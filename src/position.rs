// This file is part of the shakmaty library.
// Copyright (C) 2017-2022 Niklas Fiekas <niklas.fiekas@backscattering.de>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.

use std::{
    error::Error,
    fmt,
    hash::{Hash, Hasher},
    num::NonZeroU32,
    str::FromStr,
};

use bitflags::bitflags;

use crate::{
    attacks,
    bitboard::{Bitboard, Direction},
    board::Board,
    color::{
        ByColor, Color,
        Color::{Black, White},
    },
    material::Material,
    movelist::MoveList,
    setup::{Castles, EpSquare, Setup, SwapTurn},
    square::{Rank, Square},
    types::{CastlingMode, CastlingSide, Move, Piece, RemainingChecks, Role},
};

/// Outcome of a game.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Outcome {
    Decisive { winner: Color },
    Draw,
}

impl Outcome {
    pub fn from_winner(winner: Option<Color>) -> Outcome {
        match winner {
            Some(winner) => Outcome::Decisive { winner },
            None => Outcome::Draw,
        }
    }

    pub fn winner(self) -> Option<Color> {
        match self {
            Outcome::Decisive { winner } => Some(winner),
            Outcome::Draw => None,
        }
    }

    pub fn from_ascii(bytes: &[u8]) -> Result<Outcome, ParseOutcomeError> {
        Ok(match bytes {
            b"1-0" => Outcome::Decisive { winner: White },
            b"0-1" => Outcome::Decisive { winner: Black },
            b"1/2-1/2" => Outcome::Draw,
            b"*" => return Err(ParseOutcomeError::Unknown),
            _ => return Err(ParseOutcomeError::Invalid),
        })
    }
}

impl fmt::Display for Outcome {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            Outcome::Decisive { winner: White } => "1-0",
            Outcome::Decisive { winner: Black } => "0-1",
            Outcome::Draw => "1/2-1/2",
        })
    }
}

/// Error when parsing the outcome of a game.
#[derive(Debug, Clone)]
pub enum ParseOutcomeError {
    /// Got `*`.
    Unknown,
    /// Got a string other than `1-0`, `0-1`, `1/2-1/2`, or `*`.
    Invalid,
}

impl fmt::Display for ParseOutcomeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            ParseOutcomeError::Unknown => "unknown outcome: *",
            ParseOutcomeError::Invalid => "invalid outcome",
        })
    }
}

impl Error for ParseOutcomeError {}

impl FromStr for Outcome {
    type Err = ParseOutcomeError;

    fn from_str(s: &str) -> Result<Outcome, ParseOutcomeError> {
        Outcome::from_ascii(s.as_bytes())
    }
}

/// Error when trying to play an illegal move.
#[derive(Debug)]
pub struct PlayError<P> {
    m: Move,
    inner: P,
}

impl<P> PlayError<P> {
    /// Returns the unchanged position.
    pub fn into_inner(self) -> P {
        self.inner
    }
}

impl<P: fmt::Debug> fmt::Display for PlayError<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "illegal move {:?} in {:?}", self.m, self.inner)
    }
}

impl<P: fmt::Debug> Error for PlayError<P> {}

bitflags! {
    /// Reasons for a [`Setup`] not being a legal [`Position`].
    pub struct PositionErrorKinds: u32 {
        /// There are no pieces on the board.
        const EMPTY_BOARD = 1 << 0;

        /// A king is required but missing.
        const MISSING_KING = 1 << 1;

        /// A player has too many kings.
        const TOO_MANY_KINGS = 1 << 2;

        /// There are pawns on the backrank. Only [`Horde`] allows players to
        /// have pawns on their own backrank.
        const PAWNS_ON_BACKRANK = 1 << 3;

        /// Some castling rights are invalid.
        ///
        /// Can be recovered by discarding the invalid castling rights using
        /// [`PositionError::ignore_invalid_castling_rights()`].
        const INVALID_CASTLING_RIGHTS = 1 << 4;

        /// The en passant square is on the wrong rank, not empty, or the
        /// allegedly pushed pawn is not present.
        ///
        /// Can be recovered by discarding the invalid en passant square using
        /// [`PositionError::ignore_invalid_ep_square()`].
        const INVALID_EP_SQUARE = 1 << 5;

        /// The player not to move is in check.
        const OPPOSITE_CHECK = 1 << 6;

        /// There are impossibly many checkers, or two sliding checkers are
        /// aligned.
        ///
        /// Such a position cannot be reached by any sequence of legal moves.
        ///
        /// This can be ignored using
        /// [`PositionError::ignore_impossible_check()`], but note that other
        /// programs may not work in such a situation.
        const IMPOSSIBLE_CHECK = 1 << 7;

        /// The material configuration cannot be reached with any sequence of
        /// legal moves, because there is *too much* material.
        ///
        /// Distinguishes light-squared and dark-squared bishops,
        /// pockets and promoted pieces in Crazyhouse, and pieces on the board.
        /// Does not consider their positions. Does not consider missing pieces
        /// in Crazyhouse.
        ///
        /// This can be ignored using
        /// [`PositionError::ignore_impossible_material()`], but note that
        /// other programs may not work with too much material.
        const IMPOSSIBLE_MATERIAL = 1 << 8;

        /// A variant specific rule is violated.
        const VARIANT = 1 << 9;
    }
}

/// Error when trying to create a [`Position`] from an illegal [`Setup`].
pub struct PositionError<P> {
    pub(crate) pos: P,
    pub(crate) errors: PositionErrorKinds,
}

impl<P> PositionError<P> {
    fn ignore(mut self, ignore: PositionErrorKinds) -> Result<P, Self> {
        self.errors -= ignore;
        match self {
            PositionError { pos, errors } if errors.is_empty() => Ok(pos),
            _ => Err(self),
        }
    }

    fn strict(self) -> Result<P, Self> {
        self.ignore(PositionErrorKinds::empty())
    }

    pub fn ignore_invalid_castling_rights(self) -> Result<P, Self> {
        self.ignore(PositionErrorKinds::INVALID_CASTLING_RIGHTS)
    }

    pub fn ignore_invalid_ep_square(self) -> Result<P, Self> {
        self.ignore(PositionErrorKinds::INVALID_EP_SQUARE)
    }

    /// Get the position despite [`PositionErrorKinds::IMPOSSIBLE_MATERIAL`].
    /// Note that other programs may not work with too much material.
    pub fn ignore_impossible_material(self) -> Result<P, Self> {
        self.ignore(PositionErrorKinds::IMPOSSIBLE_MATERIAL)
    }

    /// Get the position despite [`PositionErrorKinds::IMPOSSIBLE_CHECK`].
    /// Note that other programs may not work in such a situation.
    pub fn ignore_impossible_check(self) -> Result<P, Self> {
        self.ignore(PositionErrorKinds::IMPOSSIBLE_CHECK)
    }

    pub fn kinds(&self) -> PositionErrorKinds {
        self.errors
    }
}

impl<P> fmt::Debug for PositionError<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PositionError")
            .field("errors", &self.errors)
            .finish()
    }
}

impl<P> fmt::Display for PositionError<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("illegal position: ")?;

        let mut first = true;
        let mut reason = |kind: PositionErrorKinds, display: &str| -> fmt::Result {
            if self.errors.contains(kind) {
                if !first {
                    f.write_str(", ")?;
                }
                f.write_str(display)?;
                first = false;
            }
            Ok(())
        };

        reason(PositionErrorKinds::EMPTY_BOARD, "empty board")?;
        reason(PositionErrorKinds::MISSING_KING, "missing king")?;
        reason(PositionErrorKinds::TOO_MANY_KINGS, "too many kings")?;
        reason(PositionErrorKinds::PAWNS_ON_BACKRANK, "pawns on backrank")?;
        reason(
            PositionErrorKinds::INVALID_CASTLING_RIGHTS,
            "invalid castling rights",
        )?;
        reason(PositionErrorKinds::INVALID_EP_SQUARE, "invalid ep square")?;
        reason(PositionErrorKinds::OPPOSITE_CHECK, "opposite check")?;
        reason(PositionErrorKinds::IMPOSSIBLE_CHECK, "impossible check")?;
        reason(
            PositionErrorKinds::IMPOSSIBLE_MATERIAL,
            "impossible material",
        )?;
        reason(PositionErrorKinds::VARIANT, "variant rule violated")?;
        if first {
            f.write_str("unknown reason")?;
        }

        Ok(())
    }
}

impl<P> Error for PositionError<P> {}

/// Validate and set up an arbitrary position. All provided chess variants
/// support this.
pub trait FromSetup: Sized {
    /// Set up a position.
    ///
    /// # Errors
    ///
    /// Returns [`PositionError`] if the setup does not meet basic validity
    /// requirements. Meeting the requirements does not imply that the position
    /// is actually reachable with a series of legal moves from the starting
    /// position.
    fn from_setup(setup: &dyn Setup, mode: CastlingMode) -> Result<Self, PositionError<Self>>;
}

/// A legal chess or chess variant position. See [`Chess`] for a concrete
/// implementation. Extends [`Setup`].
pub trait Position: Setup {
    /// Generates all legal moves.
    fn legal_moves(&self) -> MoveList;

    /// Generates a subset of legal moves: All piece moves and drops of type
    /// `role` to the square `to`, excluding castling moves.
    fn san_candidates(&self, role: Role, to: Square) -> MoveList {
        let mut moves = self.legal_moves();
        filter_san_candidates(role, to, &mut moves);
        moves
    }

    /// Generates legal castling moves.
    fn castling_moves(&self, side: CastlingSide) -> MoveList {
        let mut moves = self.legal_moves();
        moves.retain(|m| m.castling_side().map_or(false, |s| side == s));
        moves
    }

    /// Generates en passant moves.
    fn en_passant_moves(&self) -> MoveList {
        let mut moves = self.legal_moves();
        moves.retain(|m| m.is_en_passant());
        moves
    }

    /// Generates capture moves.
    fn capture_moves(&self) -> MoveList {
        let mut moves = self.legal_moves();
        moves.retain(|m| m.is_capture());
        moves
    }

    /// Generate promotion moves.
    fn promotion_moves(&self) -> MoveList {
        let mut moves = self.legal_moves();
        moves.retain(|m| m.is_promotion());
        moves
    }

    /// Tests if a move is irreversible.
    ///
    /// In standard chess, pawn moves, captures, moves that destroy castling
    /// rights, and moves that cede en-passant are irreversible.
    ///
    /// The implementation has false-negatives, because it does not consider
    /// forced lines. For example, a checking move that will force the king
    /// to lose castling rights is not considered irreversible, only the
    /// actual king move is.
    fn is_irreversible(&self, m: &Move) -> bool {
        (match *m {
            Move::Normal {
                role: Role::Pawn, ..
            }
            | Move::Normal {
                capture: Some(_), ..
            }
            | Move::Castle { .. }
            | Move::EnPassant { .. }
            | Move::Put { .. } => true,
            Move::Normal { role, from, to, .. } => {
                self.castling_rights().contains(from)
                    || self.castling_rights().contains(to)
                    || (role == Role::King && self.castles().has_side(self.turn()))
            }
        }) || self.ep_square().is_some()
    }

    /// Attacks that a king on `square` would have to deal with.
    fn king_attackers(&self, square: Square, attacker: Color, occupied: Bitboard) -> Bitboard {
        self.board().attacks_to(square, attacker, occupied)
    }

    /// Castling paths and unmoved rooks.
    fn castles(&self) -> &Castles;

    /// Checks if the game is over due to a special variant end condition.
    ///
    /// Note that for example stalemate is not considered a variant-specific
    /// end condition (`is_variant_end()` will return `false`), but it can have
    /// a special [`variant_outcome()`](Position::variant_outcome) in suicide
    /// chess.
    fn is_variant_end(&self) -> bool;

    /// Tests if a side has insufficient winning material.
    ///
    /// Returns `false` if there is any series of legal moves that allows
    /// `color` to win the game.
    ///
    /// The converse is not necessarily true: The position might be locked up
    /// such that `color` can never win the game (even if `!color` cooperates),
    /// or insufficient material might only become apparent after a forced
    /// sequence of moves.
    ///
    /// The current implementation can be summarized as follows: Looking
    /// only at the material configuration, taking into account if bishops
    /// are positioned on dark or light squares, but not concrete piece
    /// positions, is there a position with the same material configuration
    /// where `color` can win with a series of legal moves. If not, then
    /// `color` has insufficient winning material.
    fn has_insufficient_material(&self, color: Color) -> bool;

    /// Tests special variant winning, losing and drawing conditions.
    fn variant_outcome(&self) -> Option<Outcome>;

    /// Plays a move. It is the callers responsibility to ensure the move is
    /// legal.
    ///
    /// # Panics
    ///
    /// Illegal moves can corrupt the state of the position and may
    /// (or may not) panic or cause panics on future calls. Consider using
    /// [`Position::play()`] instead.
    fn play_unchecked(&mut self, m: &Move);

    // Implementation note: Trait methods above this comment should be made
    // available for VariantPosition. The provided methods below this comment
    // are never overwritten in implementations, but for simplicity of use
    // (especially around dyn) they are not moved to an extension trait.

    /// Swap turns. This is sometimes called "playing a null move".
    ///
    /// # Errors
    ///
    /// Returns [`PositionError`] if swapping turns is not possible (usually
    /// due to a check that has to be averted).
    fn swap_turn(self) -> Result<Self, PositionError<Self>>
    where
        Self: Sized + FromSetup,
    {
        let mode = self.castles().mode();
        Self::from_setup(&SwapTurn(self), mode)
    }

    /// Tests a move for legality.
    fn is_legal(&self, m: &Move) -> bool {
        let moves = match *m {
            Move::Normal { role, to, .. } | Move::Put { role, to } => self.san_candidates(role, to),
            Move::EnPassant { to, .. } => self.san_candidates(Role::Pawn, to),
            Move::Castle { king, rook } if king.file() < rook.file() => {
                self.castling_moves(CastlingSide::KingSide)
            }
            Move::Castle { .. } => self.castling_moves(CastlingSide::QueenSide),
        };
        moves.contains(m)
    }

    /// Bitboard of pieces giving check.
    fn checkers(&self) -> Bitboard {
        self.our(Role::King).first().map_or(Bitboard(0), |king| {
            self.king_attackers(king, !self.turn(), self.board().occupied())
        })
    }

    /// Tests if the king is in check.
    fn is_check(&self) -> bool {
        self.checkers().any()
    }

    /// Tests for checkmate.
    fn is_checkmate(&self) -> bool {
        !self.checkers().is_empty() && self.legal_moves().is_empty()
    }

    /// Tests for stalemate.
    fn is_stalemate(&self) -> bool {
        self.checkers().is_empty() && !self.is_variant_end() && self.legal_moves().is_empty()
    }

    /// Tests if both sides
    /// [have insufficient winning material](Position::has_insufficient_material).
    fn is_insufficient_material(&self) -> bool {
        self.has_insufficient_material(White) && self.has_insufficient_material(Black)
    }

    /// Tests if the game is over due to [checkmate](Position::is_checkmate()),
    /// [stalemate](Position::is_stalemate()),
    /// [insufficient material](Position::is_insufficient_material) or
    /// [variant end](Position::is_variant_end).
    fn is_game_over(&self) -> bool {
        self.legal_moves().is_empty() || self.is_insufficient_material()
    }

    /// The outcome of the game, or `None` if the game is not over.
    fn outcome(&self) -> Option<Outcome> {
        self.variant_outcome().or_else(|| {
            if self.is_checkmate() {
                Some(Outcome::Decisive {
                    winner: !self.turn(),
                })
            } else if self.is_insufficient_material() || self.is_stalemate() {
                Some(Outcome::Draw)
            } else {
                None
            }
        })
    }

    /// Plays a move.
    ///
    /// # Errors
    ///
    /// Returns a [`PlayError`] if the move is not legal.
    fn play(mut self, m: &Move) -> Result<Self, PlayError<Self>>
    where
        Self: Sized,
    {
        if self.is_legal(m) {
            self.play_unchecked(m);
            Ok(self)
        } else {
            Err(PlayError {
                m: m.clone(),
                inner: self,
            })
        }
    }
}

/// A standard Chess position.
#[derive(Clone, Debug)]
pub struct Chess {
    board: Board,
    turn: Color,
    castles: Castles,
    ep_square: Option<EpSquare>,
    halfmoves: u32,
    fullmoves: NonZeroU32,
}

impl Chess {
    #[cfg(feature = "variant")]
    fn gives_check(&self, m: &Move) -> bool {
        let mut pos = self.clone();
        pos.play_unchecked(m);
        pos.is_check()
    }

    fn from_setup_unchecked(setup: &dyn Setup, mode: CastlingMode) -> (Chess, PositionErrorKinds) {
        let mut errors = PositionErrorKinds::empty();
        let board = setup.board().clone();
        let turn = setup.turn();

        let castles = match Castles::from_setup(&board, setup.castling_rights(), mode) {
            Ok(castles) => castles,
            Err(castles) => {
                errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS;
                castles
            }
        };

        let ep_square = match EpSquare::from_setup(&board, turn, setup.ep_square()) {
            Ok(ep_square) => ep_square,
            Err(()) => {
                errors |= PositionErrorKinds::INVALID_EP_SQUARE;
                None
            }
        };

        let pos = Chess {
            board,
            turn,
            castles,
            ep_square,
            halfmoves: setup.halfmoves(),
            fullmoves: setup.fullmoves(),
        };

        errors |= validate(&pos);

        (pos, errors)
    }
}

impl Default for Chess {
    fn default() -> Chess {
        Chess {
            board: Board::default(),
            turn: White,
            castles: Castles::default(),
            ep_square: None,
            halfmoves: 0,
            fullmoves: NonZeroU32::new(1).unwrap(),
        }
    }
}

impl Hash for Chess {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.board.hash(state);
        self.turn.hash(state);
        self.castling_rights().hash(state);
        self.ep_square().hash(state);
        self.halfmoves.hash(state);
        self.fullmoves.hash(state);
    }
}

impl PartialEq for Chess {
    fn eq(&self, other: &Self) -> bool {
        self.board == other.board
            && self.turn == other.turn
            && self.castling_rights() == other.castling_rights()
            && self.ep_square() == other.ep_square()
            && self.halfmoves == other.halfmoves
            && self.fullmoves == other.fullmoves
    }
}

/// Equivalent to comparing [`Fen::from_setup()`](crate::fen::Fen::from_setup)
/// of both positions.
///
/// # Example
///
/// Note that positions with different [`CastlingMode`] may be equivalent.
///
/// ```
/// use shakmaty::{CastlingMode, Chess, fen::Fen};
///
/// let fen = "r1bqkbnr/ppp2Qpp/2np4/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4";
/// let setup: Fen = fen.parse()?;
/// let position: Chess = setup.position(CastlingMode::Standard)?;
/// let position_960: Chess = setup.position(CastlingMode::Chess960)?;
/// assert_eq!(position, position_960);
/// # Ok::<_, Box<dyn std::error::Error>>(())
/// ```
impl Eq for Chess {}

impl Setup for Chess {
    fn board(&self) -> &Board {
        &self.board
    }
    fn promoted(&self) -> Bitboard {
        Bitboard(0)
    }
    fn pockets(&self) -> Option<&Material> {
        None
    }
    fn turn(&self) -> Color {
        self.turn
    }
    fn castling_rights(&self) -> Bitboard {
        self.castles.castling_rights()
    }
    fn ep_square(&self) -> Option<Square> {
        self.ep_square.and_then(|ep| relevant_ep(ep, self))
    }
    fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> {
        None
    }
    fn halfmoves(&self) -> u32 {
        self.halfmoves
    }
    fn fullmoves(&self) -> NonZeroU32 {
        self.fullmoves
    }
}

impl FromSetup for Chess {
    fn from_setup(setup: &dyn Setup, mode: CastlingMode) -> Result<Chess, PositionError<Chess>> {
        let (pos, errors) = Chess::from_setup_unchecked(setup, mode);
        PositionError { pos, errors }.strict()
    }
}

impl Position for Chess {
    fn play_unchecked(&mut self, m: &Move) {
        do_move(
            &mut self.board,
            &mut Bitboard(0),
            &mut self.turn,
            &mut self.castles,
            &mut self.ep_square,
            &mut self.halfmoves,
            &mut self.fullmoves,
            m,
        );
    }

    fn castles(&self) -> &Castles {
        &self.castles
    }

    fn legal_moves(&self) -> MoveList {
        let mut moves = MoveList::new();

        let king = self
            .board()
            .king_of(self.turn())
            .expect("king in standard chess");

        let has_ep = gen_en_passant(self.board(), self.turn(), self.ep_square, &mut moves);

        let checkers = self.checkers();
        if checkers.is_empty() {
            let target = !self.us();
            gen_non_king(self, target, &mut moves);
            gen_safe_king(self, king, target, &mut moves);
            gen_castling_moves(
                self,
                &self.castles,
                king,
                CastlingSide::KingSide,
                &mut moves,
            );
            gen_castling_moves(
                self,
                &self.castles,
                king,
                CastlingSide::QueenSide,
                &mut moves,
            );
        } else {
            evasions(self, king, checkers, &mut moves);
        }

        let blockers = slider_blockers(self.board(), self.them(), king);
        if blockers.any() || has_ep {
            moves.retain(|m| is_safe(self, king, m, blockers));
        }

        moves
    }

    fn castling_moves(&self, side: CastlingSide) -> MoveList {
        let mut moves = MoveList::new();
        let king = self
            .board()
            .king_of(self.turn())
            .expect("king in standard chess");
        gen_castling_moves(self, &self.castles, king, side, &mut moves);
        moves
    }

    fn en_passant_moves(&self) -> MoveList {
        let mut moves = MoveList::new();

        if gen_en_passant(self.board(), self.turn(), self.ep_square, &mut moves) {
            let king = self
                .board()
                .king_of(self.turn())
                .expect("king in standard chess");
            let blockers = slider_blockers(self.board(), self.them(), king);
            moves.retain(|m| is_safe(self, king, m, blockers));
        }

        moves
    }

    fn promotion_moves(&self) -> MoveList {
        let mut moves = MoveList::new();

        let king = self
            .board()
            .king_of(self.turn())
            .expect("king in standard chess");
        let checkers = self.checkers();

        if checkers.is_empty() {
            gen_pawn_moves(self, Bitboard::BACKRANKS, &mut moves);
        } else {
            evasions(self, king, checkers, &mut moves);
            moves.retain(|m| m.is_promotion());
        }

        let blockers = slider_blockers(self.board(), self.them(), king);
        if blockers.any() {
            moves.retain(|m| is_safe(self, king, m, blockers));
        }

        moves
    }

    fn san_candidates(&self, role: Role, to: Square) -> MoveList {
        let mut moves = MoveList::new();

        let king = self
            .board()
            .king_of(self.turn())
            .expect("king in standard chess");
        let checkers = self.checkers();

        if checkers.is_empty() {
            let piece_from = match role {
                Role::Pawn | Role::King => Bitboard(0),
                Role::Knight => attacks::knight_attacks(to),
                Role::Bishop => attacks::bishop_attacks(to, self.board().occupied()),
                Role::Rook => attacks::rook_attacks(to, self.board().occupied()),
                Role::Queen => attacks::queen_attacks(to, self.board().occupied()),
            };

            if !self.us().contains(to) {
                match role {
                    Role::Pawn => gen_pawn_moves(self, Bitboard::from_square(to), &mut moves),
                    Role::King => gen_safe_king(self, king, Bitboard::from_square(to), &mut moves),
                    _ => {}
                }

                for from in piece_from & self.our(role) {
                    moves.push(Move::Normal {
                        role,
                        from,
                        capture: self.board().role_at(to),
                        to,
                        promotion: None,
                    });
                }
            }
        } else {
            evasions(self, king, checkers, &mut moves);
            filter_san_candidates(role, to, &mut moves);
        }

        let has_ep = role == Role::Pawn
            && Some(EpSquare(to)) == self.ep_square
            && gen_en_passant(self.board(), self.turn(), self.ep_square, &mut moves);

        let blockers = slider_blockers(self.board(), self.them(), king);
        if blockers.any() || has_ep {
            moves.retain(|m| is_safe(self, king, m, blockers));
        }

        moves
    }

    fn has_insufficient_material(&self, color: Color) -> bool {
        // Pawns, rooks and queens are never insufficient material.
        if (self.board.by_color(color) & (self.board.pawns() | self.board.rooks_and_queens())).any()
        {
            return false;
        }

        // Knights are only insufficient material if:
        // (1) We do not have any other pieces, including more than one knight.
        // (2) The opponent does not have pawns, knights, bishops or rooks.
        //     These would allow self mate.
        if (self.board.by_color(color) & self.board.knights()).any() {
            return self.board.by_color(color).count() <= 2
                && (self.board.by_color(!color) & !self.board.kings() & !self.board().queens())
                    .is_empty();
        }

        // Bishops are only insufficient material if:
        // (1) We do not have any other pieces, including bishops on the
        //     opposite color.
        // (2) The opponent does not have bishops on the opposite color,
        //      pawns or knights. These would allow self mate.
        if (self.board.by_color(color) & self.board.bishops()).any() {
            let same_color = (self.board().bishops() & Bitboard::DARK_SQUARES).is_empty()
                || (self.board().bishops() & Bitboard::LIGHT_SQUARES).is_empty();
            return same_color
                && self.board().knights().is_empty()
                && self.board().pawns().is_empty();
        }

        true
    }

    fn is_variant_end(&self) -> bool {
        false
    }
    fn variant_outcome(&self) -> Option<Outcome> {
        None
    }
}

#[cfg(feature = "variant")]
pub(crate) mod variant {
    use super::*;
    use crate::material::MaterialSide;

    /// An Atomic Chess position.
    #[derive(Clone, Debug)]
    pub struct Atomic {
        board: Board,
        turn: Color,
        castles: Castles,
        ep_square: Option<EpSquare>,
        halfmoves: u32,
        fullmoves: NonZeroU32,
    }

    impl Default for Atomic {
        fn default() -> Atomic {
            Atomic {
                board: Board::default(),
                turn: White,
                castles: Castles::default(),
                ep_square: None,
                halfmoves: 0,
                fullmoves: NonZeroU32::new(1).unwrap(),
            }
        }
    }

    impl Setup for Atomic {
        fn board(&self) -> &Board {
            &self.board
        }
        fn promoted(&self) -> Bitboard {
            Bitboard(0)
        }
        fn pockets(&self) -> Option<&Material> {
            None
        }
        fn turn(&self) -> Color {
            self.turn
        }
        fn castling_rights(&self) -> Bitboard {
            self.castles.castling_rights()
        }
        fn ep_square(&self) -> Option<Square> {
            self.ep_square.and_then(|ep| relevant_ep(ep, self))
        }
        fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> {
            None
        }
        fn halfmoves(&self) -> u32 {
            self.halfmoves
        }
        fn fullmoves(&self) -> NonZeroU32 {
            self.fullmoves
        }
    }

    impl FromSetup for Atomic {
        fn from_setup(
            setup: &dyn Setup,
            mode: CastlingMode,
        ) -> Result<Atomic, PositionError<Atomic>> {
            let mut errors = PositionErrorKinds::empty();
            let board = setup.board().clone();
            let turn = setup.turn();

            let castles = match Castles::from_setup(&board, setup.castling_rights(), mode) {
                Ok(castles) => castles,
                Err(castles) => {
                    errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS;
                    castles
                }
            };

            let ep_square = match EpSquare::from_setup(&board, turn, setup.ep_square()) {
                Ok(ep_square) => ep_square,
                Err(()) => {
                    errors |= PositionErrorKinds::INVALID_EP_SQUARE;
                    None
                }
            };

            let pos = Atomic {
                board,
                turn,
                castles,
                ep_square,
                halfmoves: setup.halfmoves(),
                fullmoves: setup.fullmoves(),
            };

            errors |= validate(&pos) - PositionErrorKinds::IMPOSSIBLE_CHECK;

            if (pos.them() & pos.board().kings()).any() {
                // Our king just exploded. Game over, but valid position.
                errors.remove(PositionErrorKinds::MISSING_KING);
            }

            PositionError { errors, pos }.strict()
        }
    }

    impl Position for Atomic {
        fn castles(&self) -> &Castles {
            &self.castles
        }

        fn play_unchecked(&mut self, m: &Move) {
            do_move(
                &mut self.board,
                &mut Bitboard(0),
                &mut self.turn,
                &mut self.castles,
                &mut self.ep_square,
                &mut self.halfmoves,
                &mut self.fullmoves,
                m,
            );

            match *m {
                Move::Normal {
                    capture: Some(_),
                    to,
                    ..
                }
                | Move::EnPassant { to, .. } => {
                    self.board.remove_piece_at(to);

                    let explosion_radius =
                        attacks::king_attacks(to) & self.board().occupied() & !self.board.pawns();

                    if (explosion_radius & self.board().kings() & self.us()).any() {
                        self.castles.discard_side(self.turn());
                    }

                    for explosion in explosion_radius {
                        self.board.remove_piece_at(explosion);
                        self.castles.discard_rook(explosion);
                    }
                }
                _ => (),
            }
        }

        fn legal_moves(&self) -> MoveList {
            let mut moves = MoveList::new();

            gen_en_passant(self.board(), self.turn(), self.ep_square, &mut moves);
            gen_non_king(self, !self.us(), &mut moves);
            KingTag::gen_moves(self, !self.board().occupied(), &mut moves);
            if let Some(king) = self.board().king_of(self.turn()) {
                gen_castling_moves(
                    self,
                    &self.castles,
                    king,
                    CastlingSide::KingSide,
                    &mut moves,
                );
                gen_castling_moves(
                    self,
                    &self.castles,
                    king,
                    CastlingSide::QueenSide,
                    &mut moves,
                );
            }

            // Atomic move generation could be implemented more efficiently.
            // For simplicity we filter all pseudo legal moves.
            moves.retain(|m| {
                let mut after = self.clone();
                after.play_unchecked(m);
                if let Some(our_king) = after.board().king_of(self.turn()) {
                    (after.board.kings() & after.board().by_color(!self.turn())).is_empty()
                        || after
                            .king_attackers(our_king, !self.turn(), after.board.occupied())
                            .is_empty()
                } else {
                    false
                }
            });

            moves
        }

        fn king_attackers(&self, square: Square, attacker: Color, occupied: Bitboard) -> Bitboard {
            let attacker_kings = self.board().kings() & self.board().by_color(attacker);
            if attacker_kings.is_empty() || (attacks::king_attacks(square) & attacker_kings).any() {
                Bitboard(0)
            } else {
                self.board().attacks_to(square, attacker, occupied)
            }
        }

        fn is_variant_end(&self) -> bool {
            self.variant_outcome().is_some()
        }

        fn has_insufficient_material(&self, color: Color) -> bool {
            // Remaining material does not matter if the opponents king is already
            // exploded.
            if (self.board.by_color(!color) & self.board.kings()).is_empty() {
                return false;
            }

            // Bare king can not mate.
            if (self.board.by_color(color) & !self.board.kings()).is_empty() {
                return true;
            }

            // As long as the opponent king is not alone there is always a chance
            // their own piece explodes next to it.
            if (self.board.by_color(!color) & !self.board.kings()).any() {
                // Unless there are only bishops that cannot explode each other.
                if self.board().occupied() == self.board().kings() | self.board().bishops() {
                    if (self.board().bishops() & self.board().white() & Bitboard::DARK_SQUARES)
                        .is_empty()
                    {
                        return (self.board().bishops()
                            & self.board().black()
                            & Bitboard::LIGHT_SQUARES)
                            .is_empty();
                    }
                    if (self.board().bishops() & self.board().white() & Bitboard::LIGHT_SQUARES)
                        .is_empty()
                    {
                        return (self.board().bishops()
                            & self.board().black()
                            & Bitboard::DARK_SQUARES)
                            .is_empty();
                    }
                }

                return false;
            }

            // Queen or pawn (future queen) can give mate against bare king.
            if self.board().queens().any() || self.board.pawns().any() {
                return false;
            }

            // Single knight, bishop or rook can not mate against bare king.
            if (self.board().knights() | self.board().bishops() | self.board().rooks()).count() == 1
            {
                return true;
            }

            // Two knights can not mate against bare king.
            if self.board().occupied() == self.board().kings() | self.board().knights() {
                return self.board().knights().count() <= 2;
            }

            false
        }

        fn variant_outcome(&self) -> Option<Outcome> {
            for color in Color::ALL {
                if (self.board().by_color(color) & self.board().kings()).is_empty() {
                    return Some(Outcome::Decisive { winner: !color });
                }
            }
            None
        }
    }

    /// An Antichess position. Antichess is also known as Giveaway, but players
    /// start without castling rights.
    #[derive(Clone, Debug)]
    pub struct Antichess {
        board: Board,
        turn: Color,
        castles: Castles,
        ep_square: Option<EpSquare>,
        halfmoves: u32,
        fullmoves: NonZeroU32,
    }

    impl Default for Antichess {
        fn default() -> Antichess {
            Antichess {
                board: Board::default(),
                turn: White,
                castles: Castles::empty(CastlingMode::Standard),
                ep_square: None,
                halfmoves: 0,
                fullmoves: NonZeroU32::new(1).unwrap(),
            }
        }
    }

    impl Setup for Antichess {
        fn board(&self) -> &Board {
            &self.board
        }
        fn promoted(&self) -> Bitboard {
            Bitboard(0)
        }
        fn pockets(&self) -> Option<&Material> {
            None
        }
        fn turn(&self) -> Color {
            self.turn
        }
        fn castling_rights(&self) -> Bitboard {
            Bitboard(0)
        }
        fn ep_square(&self) -> Option<Square> {
            self.ep_square.and_then(|ep| relevant_ep(ep, self))
        }
        fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> {
            None
        }
        fn halfmoves(&self) -> u32 {
            self.halfmoves
        }
        fn fullmoves(&self) -> NonZeroU32 {
            self.fullmoves
        }
    }

    impl FromSetup for Antichess {
        fn from_setup(
            setup: &dyn Setup,
            mode: CastlingMode,
        ) -> Result<Antichess, PositionError<Antichess>> {
            let mut errors = PositionErrorKinds::empty();
            let board = setup.board().clone();
            let turn = setup.turn();

            let ep_square = match EpSquare::from_setup(&board, turn, setup.ep_square()) {
                Ok(ep_square) => ep_square,
                Err(()) => {
                    errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS;
                    None
                }
            };

            let pos = Antichess {
                board,
                turn,
                castles: Castles::empty(mode),
                ep_square,
                halfmoves: setup.halfmoves(),
                fullmoves: setup.fullmoves(),
            };

            if setup.castling_rights().any() {
                errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS
            }

            errors |= validate(&pos)
                - PositionErrorKinds::MISSING_KING
                - PositionErrorKinds::TOO_MANY_KINGS
                - PositionErrorKinds::OPPOSITE_CHECK
                - PositionErrorKinds::IMPOSSIBLE_CHECK;

            PositionError { errors, pos }.strict()
        }
    }

    impl Position for Antichess {
        fn play_unchecked(&mut self, m: &Move) {
            do_move(
                &mut self.board,
                &mut Bitboard(0),
                &mut self.turn,
                &mut self.castles,
                &mut self.ep_square,
                &mut self.halfmoves,
                &mut self.fullmoves,
                m,
            );
        }

        fn castles(&self) -> &Castles {
            &self.castles
        }

        fn en_passant_moves(&self) -> MoveList {
            let mut moves = MoveList::new();
            gen_en_passant(self.board(), self.turn, self.ep_square, &mut moves);
            moves
        }

        fn capture_moves(&self) -> MoveList {
            let mut moves = self.en_passant_moves();
            let them = self.them();
            gen_non_king(self, them, &mut moves);
            add_king_promotions(&mut moves);
            KingTag::gen_moves(self, them, &mut moves);
            moves
        }

        fn legal_moves(&self) -> MoveList {
            let mut moves = self.capture_moves();

            if moves.is_empty() {
                // No compulsory captures. Generate everything else.
                gen_non_king(self, !self.board().occupied(), &mut moves);
                add_king_promotions(&mut moves);
                KingTag::gen_moves(self, !self.board().occupied(), &mut moves);
            }

            moves
        }

        fn king_attackers(
            &self,
            _square: Square,
            _attacker: Color,
            _occupied: Bitboard,
        ) -> Bitboard {
            Bitboard(0)
        }

        fn is_variant_end(&self) -> bool {
            self.board().white().is_empty() || self.board().black().is_empty()
        }

        fn has_insufficient_material(&self, color: Color) -> bool {
            // In a position with only bishops, check if all our bishops can be
            // captured.
            if self.board.occupied() == self.board.bishops() {
                let we_some_on_light = (self.board.by_color(color) & Bitboard::LIGHT_SQUARES).any();
                let we_some_on_dark = (self.board.by_color(color) & Bitboard::DARK_SQUARES).any();
                let they_all_on_dark =
                    (self.board.by_color(!color) & Bitboard::LIGHT_SQUARES).is_empty();
                let they_all_on_light =
                    (self.board.by_color(!color) & Bitboard::DARK_SQUARES).is_empty();
                (we_some_on_light && they_all_on_dark) || (we_some_on_dark && they_all_on_light)
            } else {
                false
            }
        }

        fn variant_outcome(&self) -> Option<Outcome> {
            if self.us().is_empty() || self.is_stalemate() {
                Some(Outcome::Decisive {
                    winner: self.turn(),
                })
            } else {
                None
            }
        }
    }

    /// A King of the Hill position.
    #[derive(Clone, Debug, Default)]
    pub struct KingOfTheHill {
        chess: Chess,
    }

    impl Setup for KingOfTheHill {
        fn board(&self) -> &Board {
            self.chess.board()
        }
        fn promoted(&self) -> Bitboard {
            Bitboard(0)
        }
        fn pockets(&self) -> Option<&Material> {
            None
        }
        fn turn(&self) -> Color {
            self.chess.turn()
        }
        fn castling_rights(&self) -> Bitboard {
            self.chess.castling_rights()
        }
        fn ep_square(&self) -> Option<Square> {
            self.chess.ep_square()
        }
        fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> {
            None
        }
        fn halfmoves(&self) -> u32 {
            self.chess.halfmoves()
        }
        fn fullmoves(&self) -> NonZeroU32 {
            self.chess.fullmoves()
        }
    }

    impl FromSetup for KingOfTheHill {
        fn from_setup(
            setup: &dyn Setup,
            mode: CastlingMode,
        ) -> Result<KingOfTheHill, PositionError<KingOfTheHill>> {
            let (chess, errors) = Chess::from_setup_unchecked(setup, mode);
            PositionError {
                errors,
                pos: KingOfTheHill { chess },
            }
            .strict()
        }
    }

    impl Position for KingOfTheHill {
        fn play_unchecked(&mut self, m: &Move) {
            self.chess.play_unchecked(m);
        }

        fn castles(&self) -> &Castles {
            self.chess.castles()
        }

        fn legal_moves(&self) -> MoveList {
            if self.is_variant_end() {
                MoveList::new()
            } else {
                self.chess.legal_moves()
            }
        }

        fn castling_moves(&self, side: CastlingSide) -> MoveList {
            if self.is_variant_end() {
                MoveList::new()
            } else {
                self.chess.castling_moves(side)
            }
        }

        fn en_passant_moves(&self) -> MoveList {
            if self.is_variant_end() {
                MoveList::new()
            } else {
                self.chess.en_passant_moves()
            }
        }

        fn san_candidates(&self, role: Role, to: Square) -> MoveList {
            if self.is_variant_end() {
                MoveList::new()
            } else {
                self.chess.san_candidates(role, to)
            }
        }

        fn has_insufficient_material(&self, _color: Color) -> bool {
            // Even a lone king can walk onto the hill.
            false
        }

        fn is_variant_end(&self) -> bool {
            (self.chess.board().kings() & Bitboard::CENTER).any()
        }

        fn variant_outcome(&self) -> Option<Outcome> {
            for color in Color::ALL {
                if (self.board().by_color(color) & self.board().kings() & Bitboard::CENTER).any() {
                    return Some(Outcome::Decisive { winner: color });
                }
            }
            None
        }
    }

    /// A Three-Check position.
    #[derive(Clone, Debug, Default)]
    pub struct ThreeCheck {
        chess: Chess,
        remaining_checks: ByColor<RemainingChecks>,
    }

    impl Setup for ThreeCheck {
        fn board(&self) -> &Board {
            self.chess.board()
        }
        fn promoted(&self) -> Bitboard {
            Bitboard(0)
        }
        fn pockets(&self) -> Option<&Material> {
            None
        }
        fn turn(&self) -> Color {
            self.chess.turn()
        }
        fn castling_rights(&self) -> Bitboard {
            self.chess.castling_rights()
        }
        fn ep_square(&self) -> Option<Square> {
            self.chess.ep_square()
        }
        fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> {
            Some(&self.remaining_checks)
        }
        fn halfmoves(&self) -> u32 {
            self.chess.halfmoves()
        }
        fn fullmoves(&self) -> NonZeroU32 {
            self.chess.fullmoves
        }
    }

    impl FromSetup for ThreeCheck {
        fn from_setup(
            setup: &dyn Setup,
            mode: CastlingMode,
        ) -> Result<ThreeCheck, PositionError<ThreeCheck>> {
            let (chess, mut errors) = Chess::from_setup_unchecked(setup, mode);

            let remaining_checks = setup.remaining_checks().cloned().unwrap_or_default();
            if remaining_checks.all(|remaining| remaining.is_zero()) {
                errors |= PositionErrorKinds::VARIANT;
            }

            PositionError {
                errors,
                pos: ThreeCheck {
                    chess,
                    remaining_checks,
                },
            }
            .strict()
        }
    }

    impl Position for ThreeCheck {
        fn play_unchecked(&mut self, m: &Move) {
            let turn = self.chess.turn();
            self.chess.play_unchecked(m);
            if self.is_check() {
                let checks = self.remaining_checks.by_color_mut(turn);
                *checks = RemainingChecks::minus_one(*checks);
            }
        }

        fn castles(&self) -> &Castles {
            self.chess.castles()
        }

        fn legal_moves(&self) -> MoveList {
            if self.is_variant_end() {
                MoveList::new()
            } else {
                self.chess.legal_moves()
            }
        }

        fn castling_moves(&self, side: CastlingSide) -> MoveList {
            if self.is_variant_end() {
                MoveList::new()
            } else {
                self.chess.castling_moves(side)
            }
        }

        fn en_passant_moves(&self) -> MoveList {
            if self.is_variant_end() {
                MoveList::new()
            } else {
                self.chess.en_passant_moves()
            }
        }

        fn san_candidates(&self, role: Role, to: Square) -> MoveList {
            if self.is_variant_end() {
                MoveList::new()
            } else {
                self.chess.san_candidates(role, to)
            }
        }

        fn has_insufficient_material(&self, color: Color) -> bool {
            // Any remaining piece can give check.
            (self.board().by_color(color) & !self.board().kings()).is_empty()
        }

        fn is_irreversible(&self, m: &Move) -> bool {
            self.chess.is_irreversible(m) || self.chess.gives_check(m)
        }

        fn is_variant_end(&self) -> bool {
            self.remaining_checks.any(|remaining| remaining.is_zero())
        }

        fn variant_outcome(&self) -> Option<Outcome> {
            self.remaining_checks
                .find(|remaining| remaining.is_zero())
                .map(|winner| Outcome::Decisive { winner })
        }
    }

    /// A Crazyhouse position.
    #[derive(Clone, Debug, Default)]
    pub struct Crazyhouse {
        chess: Chess,
        promoted: Bitboard,
        pockets: Material,
    }

    impl Crazyhouse {
        fn our_pocket(&self) -> &MaterialSide {
            self.pockets.by_color(self.turn())
        }

        fn our_pocket_mut(&mut self) -> &mut MaterialSide {
            let turn = self.turn();
            self.pockets.by_color_mut(turn)
        }

        fn legal_put_squares(&self) -> Bitboard {
            let checkers = self.checkers();

            if checkers.is_empty() {
                !self.board().occupied()
            } else if let Some(checker) = checkers.single_square() {
                let king = self
                    .board()
                    .king_of(self.turn())
                    .expect("king in crazyhouse");
                attacks::between(checker, king)
            } else {
                Bitboard(0)
            }
        }
    }

    impl Setup for Crazyhouse {
        fn board(&self) -> &Board {
            self.chess.board()
        }
        fn promoted(&self) -> Bitboard {
            self.promoted
        }
        fn pockets(&self) -> Option<&Material> {
            Some(&self.pockets)
        }
        fn turn(&self) -> Color {
            self.chess.turn()
        }
        fn castling_rights(&self) -> Bitboard {
            self.chess.castling_rights()
        }
        fn ep_square(&self) -> Option<Square> {
            self.chess.ep_square()
        }
        fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> {
            None
        }
        fn halfmoves(&self) -> u32 {
            self.chess.halfmoves()
        }
        fn fullmoves(&self) -> NonZeroU32 {
            self.chess.fullmoves()
        }
    }

    impl FromSetup for Crazyhouse {
        fn from_setup(
            setup: &dyn Setup,
            mode: CastlingMode,
        ) -> Result<Crazyhouse, PositionError<Crazyhouse>> {
            let (chess, mut errors) = Chess::from_setup_unchecked(setup, mode);
            let promoted = setup.promoted()
                & chess.board().occupied()
                & !chess.board().pawns()
                & !chess.board.kings();
            let pockets = setup.pockets().cloned().unwrap_or_default();

            if pockets.white.kings > 0 || pockets.black.kings > 0 {
                errors |= PositionErrorKinds::TOO_MANY_KINGS;
            }

            if pockets.count() + chess.board().occupied().count() > 64 {
                errors |= PositionErrorKinds::VARIANT;
            }

            if promoted.count()
                + chess.board().pawns().count()
                + usize::from(pockets.white.pawns)
                + usize::from(pockets.black.pawns)
                <= 16
                && (chess.board().knights() & !promoted).count()
                    + usize::from(pockets.white.knights)
                    + usize::from(pockets.black.knights)
                    <= 4
                && (chess.board().bishops() & !promoted).count()
                    + usize::from(pockets.white.bishops)
                    + usize::from(pockets.black.bishops)
                    <= 4
                && (chess.board().rooks() & !promoted).count()
                    + usize::from(pockets.white.rooks)
                    + usize::from(pockets.black.rooks)
                    <= 4
                && (chess.board().queens() & !promoted).count()
                    + usize::from(pockets.white.queens)
                    + usize::from(pockets.black.queens)
                    <= 2
            {
                errors -= PositionErrorKinds::IMPOSSIBLE_MATERIAL;
            }

            PositionError {
                errors,
                pos: Crazyhouse {
                    chess,
                    promoted,
                    pockets,
                },
            }
            .strict()
        }
    }

    impl Position for Crazyhouse {
        fn play_unchecked(&mut self, m: &Move) {
            match *m {
                Move::Normal {
                    capture: Some(capture),
                    to,
                    ..
                } => {
                    let capture = if self.promoted.contains(to) {
                        Role::Pawn
                    } else {
                        capture
                    };

                    *self.our_pocket_mut().by_role_mut(capture) += 1;
                }
                Move::EnPassant { .. } => {
                    self.our_pocket_mut().pawns += 1;
                }
                Move::Put { role, .. } => {
                    *self.our_pocket_mut().by_role_mut(role) -= 1;
                }
                _ => {}
            }

            do_move(
                &mut self.chess.board,
                &mut self.promoted,
                &mut self.chess.turn,
                &mut self.chess.castles,
                &mut self.chess.ep_square,
                &mut self.chess.halfmoves,
                &mut self.chess.fullmoves,
                m,
            );
        }

        fn castles(&self) -> &Castles {
            self.chess.castles()
        }

        fn legal_moves(&self) -> MoveList {
            let mut moves = self.chess.legal_moves();

            let pocket = self.our_pocket();
            let targets = self.legal_put_squares();

            for to in targets {
                for &role in &[Role::Knight, Role::Bishop, Role::Rook, Role::Queen] {
                    if pocket.by_role(role) > 0 {
                        moves.push(Move::Put { role, to });
                    }
                }
            }

            if pocket.pawns > 0 {
                for to in targets & !Bitboard::BACKRANKS {
                    moves.push(Move::Put {
                        role: Role::Pawn,
                        to,
                    });
                }
            }

            moves
        }

        fn castling_moves(&self, side: CastlingSide) -> MoveList {
            self.chess.castling_moves(side)
        }

        fn en_passant_moves(&self) -> MoveList {
            self.chess.en_passant_moves()
        }

        fn san_candidates(&self, role: Role, to: Square) -> MoveList {
            let mut moves = self.chess.san_candidates(role, to);

            if self.our_pocket().by_role(role) > 0
                && self.legal_put_squares().contains(to)
                && (role != Role::Pawn || !Bitboard::BACKRANKS.contains(to))
            {
                moves.push(Move::Put { role, to });
            }

            moves
        }

        fn is_irreversible(&self, m: &Move) -> bool {
            match *m {
                Move::Castle { .. } => true,
                Move::Normal { role, from, to, .. } => {
                    self.castling_rights().contains(from)
                        || self.castling_rights().contains(to)
                        || (role == Role::King && self.chess.castles.has_side(self.turn()))
                }
                _ => false,
            }
        }

        fn has_insufficient_material(&self, _color: Color) -> bool {
            // In practise no material can leave the game, but this is simple
            // to implement anyway. Bishops can be captured and put onto a
            // different color complex.
            self.board().occupied().count() + self.pockets.count() <= 3
                && self.promoted.is_empty()
                && self.board().pawns().is_empty()
                && self.board().rooks_and_queens().is_empty()
                && self.pockets.white.pawns == 0
                && self.pockets.black.pawns == 0
                && self.pockets.white.rooks == 0
                && self.pockets.black.rooks == 0
                && self.pockets.white.queens == 0
                && self.pockets.black.queens == 0
        }

        fn is_variant_end(&self) -> bool {
            false
        }
        fn variant_outcome(&self) -> Option<Outcome> {
            None
        }
    }

    /// A Racing Kings position.
    #[derive(Clone, Debug)]
    pub struct RacingKings {
        board: Board,
        turn: Color,
        castles: Castles,
        halfmoves: u32,
        fullmoves: NonZeroU32,
    }

    impl Default for RacingKings {
        fn default() -> RacingKings {
            RacingKings {
                board: Board::racing_kings(),
                turn: White,
                castles: Castles::empty(CastlingMode::Standard),
                halfmoves: 0,
                fullmoves: NonZeroU32::new(1).unwrap(),
            }
        }
    }

    impl Setup for RacingKings {
        fn board(&self) -> &Board {
            &self.board
        }
        fn promoted(&self) -> Bitboard {
            Bitboard(0)
        }
        fn pockets(&self) -> Option<&Material> {
            None
        }
        fn turn(&self) -> Color {
            self.turn
        }
        fn castling_rights(&self) -> Bitboard {
            Bitboard(0)
        }
        fn ep_square(&self) -> Option<Square> {
            None
        }
        fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> {
            None
        }
        fn halfmoves(&self) -> u32 {
            self.halfmoves
        }
        fn fullmoves(&self) -> NonZeroU32 {
            self.fullmoves
        }
    }

    impl FromSetup for RacingKings {
        fn from_setup(
            setup: &dyn Setup,
            mode: CastlingMode,
        ) -> Result<RacingKings, PositionError<RacingKings>> {
            let mut errors = PositionErrorKinds::empty();

            if setup.castling_rights().any() {
                errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS;
            }

            let board = setup.board().clone();
            if board.pawns().any() {
                errors |= PositionErrorKinds::VARIANT;
            }
            if setup.ep_square().is_some() {
                errors |= PositionErrorKinds::INVALID_EP_SQUARE;
            }

            let pos = RacingKings {
                board,
                turn: setup.turn(),
                castles: Castles::empty(mode),
                halfmoves: setup.halfmoves(),
                fullmoves: setup.fullmoves(),
            };

            if pos.is_check() {
                errors |= PositionErrorKinds::IMPOSSIBLE_CHECK;
            }

            if pos.turn().is_black()
                && (pos.board().white() & pos.board().kings() & Rank::Eighth).any()
                && (pos.board().black() & pos.board().kings() & Rank::Eighth).any()
            {
                errors |= PositionErrorKinds::VARIANT;
            }

            errors |= validate(&pos);

            PositionError { errors, pos }.strict()
        }
    }

    impl Position for RacingKings {
        fn play_unchecked(&mut self, m: &Move) {
            do_move(
                &mut self.board,
                &mut Bitboard(0),
                &mut self.turn,
                &mut self.castles,
                &mut None,
                &mut self.halfmoves,
                &mut self.fullmoves,
                m,
            );
        }

        fn legal_moves(&self) -> MoveList {
            let mut moves = MoveList::new();

            if self.is_variant_end() {
                return moves;
            }

            // Generate all legal moves (no castling, no ep).
            let target = !self.us();
            gen_non_king(self, target, &mut moves);
            let king = self
                .board()
                .king_of(self.turn())
                .expect("king in racingkings");
            gen_safe_king(self, king, target, &mut moves);

            let blockers = slider_blockers(self.board(), self.them(), king);
            if blockers.any() {
                moves.retain(|m| is_safe(self, king, m, blockers));
            }

            // Do not allow giving check. This could be implemented more
            // efficiently.
            moves.retain(|m| {
                let mut after = self.clone();
                after.play_unchecked(m);
                !after.is_check()
            });

            moves
        }

        fn castles(&self) -> &Castles {
            &self.castles
        }

        fn has_insufficient_material(&self, _color: Color) -> bool {
            // Even a lone king can win the race.
            false
        }

        fn is_variant_end(&self) -> bool {
            let in_goal = self.board().kings() & Rank::Eighth;
            if in_goal.is_empty() {
                return false;
            }

            if self.turn().is_white() || (in_goal & self.board().black()).any() {
                return true;
            }

            // White has reached the backrank. Check if black can catch up.
            let black_king = self.board().king_of(Black).expect("king in racingkings");
            for target in attacks::king_attacks(black_king) & Rank::Eighth & !self.board().black() {
                if self
                    .king_attackers(target, White, self.board().occupied())
                    .is_empty()
                {
                    return false;
                }
            }

            true
        }

        fn variant_outcome(&self) -> Option<Outcome> {
            if self.is_variant_end() {
                let in_goal = self.board().kings() & Rank::Eighth;
                if (in_goal & self.board().white()).any() && (in_goal & self.board().black()).any()
                {
                    Some(Outcome::Draw)
                } else if (in_goal & self.board().white()).any() {
                    Some(Outcome::Decisive { winner: White })
                } else {
                    Some(Outcome::Decisive { winner: Black })
                }
            } else {
                None
            }
        }
    }

    /// A Horde position.
    #[derive(Clone, Debug)]
    pub struct Horde {
        board: Board,
        turn: Color,
        castles: Castles,
        ep_square: Option<EpSquare>,
        halfmoves: u32,
        fullmoves: NonZeroU32,
    }

    impl Default for Horde {
        fn default() -> Horde {
            let mut castles = Castles::default();
            castles.discard_side(White);

            Horde {
                board: Board::horde(),
                turn: White,
                castles,
                ep_square: None,
                halfmoves: 0,
                fullmoves: NonZeroU32::new(1).unwrap(),
            }
        }
    }

    impl Setup for Horde {
        fn board(&self) -> &Board {
            &self.board
        }
        fn promoted(&self) -> Bitboard {
            Bitboard(0)
        }
        fn pockets(&self) -> Option<&Material> {
            None
        }
        fn turn(&self) -> Color {
            self.turn
        }
        fn castling_rights(&self) -> Bitboard {
            self.castles.castling_rights()
        }
        fn ep_square(&self) -> Option<Square> {
            self.ep_square.and_then(|ep| relevant_ep(ep, self))
        }
        fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> {
            None
        }
        fn halfmoves(&self) -> u32 {
            self.halfmoves
        }
        fn fullmoves(&self) -> NonZeroU32 {
            self.fullmoves
        }
    }

    impl FromSetup for Horde {
        fn from_setup(
            setup: &dyn Setup,
            mode: CastlingMode,
        ) -> Result<Horde, PositionError<Horde>> {
            let mut errors = PositionErrorKinds::empty();
            let board = setup.board().clone();
            let turn = setup.turn();

            let castles = match Castles::from_setup(&board, setup.castling_rights(), mode) {
                Ok(castles) => castles,
                Err(castles) => {
                    errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS;
                    castles
                }
            };

            let ep_square = match EpSquare::from_setup(&board, turn, setup.ep_square()) {
                Ok(ep_square) => ep_square,
                Err(()) => {
                    errors |= PositionErrorKinds::INVALID_EP_SQUARE;
                    None
                }
            };

            let pos = Horde {
                board,
                turn,
                castles,
                ep_square,
                halfmoves: setup.halfmoves(),
                fullmoves: setup.fullmoves(),
            };

            errors |= validate(&pos)
                - PositionErrorKinds::PAWNS_ON_BACKRANK
                - PositionErrorKinds::MISSING_KING
                - PositionErrorKinds::IMPOSSIBLE_MATERIAL;

            if (pos.board().kings() & pos.board.white()).is_empty() {
                if pos.board().white().count() > 36
                    || !is_standard_material(pos.board(), Color::Black)
                {
                    errors |= PositionErrorKinds::IMPOSSIBLE_MATERIAL;
                }
            } else if pos.board().black().count() > 36
                || !is_standard_material(pos.board(), Color::White)
            {
                errors |= PositionErrorKinds::IMPOSSIBLE_MATERIAL;
            }

            if (pos.board().pawns() & pos.board().white() & Rank::Eighth).any()
                || (pos.board().pawns() & pos.board().black() & Rank::First).any()
            {
                errors |= PositionErrorKinds::PAWNS_ON_BACKRANK;
            }

            if pos.board().kings().is_empty() {
                errors |= PositionErrorKinds::MISSING_KING;
            }

            if (pos.board().kings() & pos.board().white()).any()
                && (pos.board().kings() & pos.board().black()).any()
            {
                errors |= PositionErrorKinds::VARIANT;
            }

            PositionError { errors, pos }.strict()
        }
    }

    impl Position for Horde {
        fn play_unchecked(&mut self, m: &Move) {
            do_move(
                &mut self.board,
                &mut Bitboard(0),
                &mut self.turn,
                &mut self.castles,
                &mut self.ep_square,
                &mut self.halfmoves,
                &mut self.fullmoves,
                m,
            );
        }

        fn legal_moves(&self) -> MoveList {
            let mut moves = MoveList::new();

            let king = self.board().king_of(self.turn());
            let has_ep = gen_en_passant(self.board(), self.turn(), self.ep_square, &mut moves);

            let checkers = self.checkers();
            if checkers.is_empty() {
                let target = !self.us();
                gen_non_king(self, target, &mut moves);
                if let Some(king) = king {
                    gen_safe_king(self, king, target, &mut moves);
                    gen_castling_moves(
                        self,
                        &self.castles,
                        king,
                        CastlingSide::KingSide,
                        &mut moves,
                    );
                    gen_castling_moves(
                        self,
                        &self.castles,
                        king,
                        CastlingSide::QueenSide,
                        &mut moves,
                    );
                }
            } else {
                evasions(self, king.expect("king in check"), checkers, &mut moves);
            }

            if let Some(king) = king {
                let blockers = slider_blockers(self.board(), self.them(), king);
                if blockers.any() || has_ep {
                    moves.retain(|m| is_safe(self, king, m, blockers));
                }
            }

            moves
        }

        fn castles(&self) -> &Castles {
            &self.castles
        }

        fn is_variant_end(&self) -> bool {
            self.board().white().is_empty() || self.board().black().is_empty()
        }

        fn has_insufficient_material(&self, color: Color) -> bool {
            // The side with the king can always win by capturing the horde.
            if (self.board.by_color(color) & self.board.kings()).any() {
                return false;
            }

            // By this point: color == White
            let horde = self.board.material_side(color);
            let horde_darkb = (Bitboard::DARK_SQUARES
                & self.board.by_color(color)
                & self.board.bishops())
            .count() as u8;
            let horde_lightb =
                (Bitboard::LIGHT_SQUARES & self.board.by_color(color) & self.board.bishops())
                    .count() as u8;
            let horde_bishop_co = || Color::from_white(horde_lightb >= 1);
            let horde_num = horde.pawns
                + horde.knights
                + horde.rooks
                + horde.queens
                + if horde_darkb <= 2 { horde_darkb } else { 2 }
                + if horde_lightb <= 2 { horde_lightb } else { 2 };
            // Two same color bishops suffice to cover all the light and dark squares
            // around the enemy king.

            let pieces = self.board.material_side(!color);
            let pieces_darkb = || -> u8 {
                (Bitboard::DARK_SQUARES & self.board.by_color(!color) & self.board.bishops())
                    .count() as u8
            };
            let pieces_lightb = || -> u8 {
                (Bitboard::LIGHT_SQUARES & self.board.by_color(!color) & self.board.bishops())
                    .count() as u8
            };
            let pieces_num = pieces.count() as u8;
            let pieces_oppositeb_of = |square_color: Color| -> u8 {
                match square_color {
                    White => pieces_darkb(),
                    Black => pieces_lightb(),
                }
            };
            let pieces_sameb_as = |square_color: Color| -> u8 {
                match square_color {
                    White => pieces_lightb(),
                    Black => pieces_darkb(),
                }
            };
            let pieces_of_type_not = |piece: u8| -> u8 { pieces_num - piece };
            let has_bishop_pair = |side: Color| -> bool {
                match side {
                    White => horde_lightb >= 1 && horde_darkb >= 1,
                    Black => pieces_lightb() >= 1 && pieces_darkb() >= 1,
                }
            };

            if horde_num == 0 {
                return true;
            }
            if horde_num >= 4 {
                // Four or more white pieces can always deliver mate.
                return false;
            }
            if (horde.pawns >= 1 || horde.queens >= 1) && horde_num >= 2 {
                // Pawns/queens are never insufficient material when paired with any other
                // piece (a pawn promotes to a queen and delivers mate).
                return false;
            }
            if horde.rooks >= 1 && horde_num >= 2 {
                // A rook is insufficient material only when it is paired with a bishop
                // against a lone king. The horde can mate in any other case.
                // A rook on A1 and a bishop on C3 mate a king on B1 when there is a
                // friendly pawn/opposite-color-bishop/rook/queen on C2.
                // A rook on B8 and a bishop C3 mate a king on A1 when there is a friendly
                // knight on A2.
                if !(horde_num == 2
                    && horde.rooks == 1
                    && horde.bishops == 1
                    && pieces_of_type_not(pieces_sameb_as(horde_bishop_co())) == 1)
                {
                    return false;
                }
            }

            if horde_num == 1 {
                if pieces_num == 1 {
                    // A lone piece cannot mate a lone king.
                    return true;
                } else if horde.queens == 1 {
                    // The horde has a lone queen.
                    // A lone queen mates a king on A1 bounded by:
                    //  -- a pawn/rook on A2
                    //  -- two same color bishops on A2, B1
                    // We ignore every other mating case, since it can be reduced to
                    // the two previous cases (e.g. a black pawn on A2 and a black
                    // bishop on B1).
                    return !(pieces.pawns >= 1
                        || pieces.rooks >= 1
                        || pieces_lightb() >= 2
                        || pieces_darkb() >= 2);
                } else if horde.pawns == 1 {
                    // Promote the pawn to a queen or a knight and check whether white
                    // can mate.
                    let pawn_square = (self.board.pawns() & self.board.by_color(color))
                        .single_square()
                        .unwrap();
                    let mut promote_to_queen = self.clone();
                    promote_to_queen
                        .board
                        .set_piece_at(pawn_square, color.queen());
                    let mut promote_to_knight = self.clone();
                    promote_to_knight
                        .board
                        .set_piece_at(pawn_square, color.knight());
                    return promote_to_queen.has_insufficient_material(color)
                        && promote_to_knight.has_insufficient_material(color);
                } else if horde.rooks == 1 {
                    // A lone rook mates a king on A8 bounded by a pawn/rook on A7 and a
                    // pawn/knight on B7. We ignore every other case, since it can be
                    // reduced to the two previous cases.
                    // (e.g. three pawns on A7, B7, C7)
                    return !(pieces.pawns >= 2
                        || (pieces.rooks >= 1 && pieces.pawns >= 1)
                        || (pieces.rooks >= 1 && pieces.knights >= 1)
                        || (pieces.pawns >= 1 && pieces.knights >= 1));
                } else if horde.bishops == 1 {
                    // The horde has a lone bishop.
                    return !(
                        // The king can be mated on A1 if there is a pawn/opposite-color-bishop
                        // on A2 and an opposite-color-bishop on B1.
                        // If black has two or more pawns, white gets the benefit of the doubt;
                        // there is an outside chance that white promotes its pawns to
                        // opposite-color-bishops and selfmates theirself.
                        // Every other case that the king is mated by the bishop requires that
                        // black has two pawns or two opposite-color-bishop or a pawn and an
                        // opposite-color-bishop.
                        // For example a king on A3 can be mated if there is
                        // a pawn/opposite-color-bishop on A4, a pawn/opposite-color-bishop on
                        // B3, a pawn/bishop/rook/queen on A2 and any other piece on B2.
                        pieces_oppositeb_of(horde_bishop_co()) >= 2
                            || (pieces_oppositeb_of(horde_bishop_co()) >= 1 && pieces.pawns >= 1)
                            || pieces.pawns >= 2
                    );
                } else if horde.knights == 1 {
                    // The horde has a lone knight.
                    return !(
                        // The king on A1 can be smother mated by a knight on C2 if there is
                        // a pawn/knight/bishop on B2, a knight/rook on B1 and any other piece
                        // on A2.
                        // Moreover, when black has four or more pieces and two of them are
                        // pawns, black can promote their pawns and selfmate theirself.
                        pieces_num >= 4
                            && (pieces.knights >= 2
                                || pieces.pawns >= 2
                                || (pieces.rooks >= 1 && pieces.knights >= 1)
                                || (pieces.rooks >= 1 && pieces.bishops >= 1)
                                || (pieces.knights >= 1 && pieces.bishops >= 1)
                                || (pieces.rooks >= 1 && pieces.pawns >= 1)
                                || (pieces.knights >= 1 && pieces.pawns >= 1)
                                || (pieces.bishops >= 1 && pieces.pawns >= 1)
                                || (has_bishop_pair(!color) && pieces.pawns >= 1))
                            && if pieces_darkb() >= 2 {
                                pieces_of_type_not(pieces_darkb()) >= 3
                            } else {
                                true
                            }
                            && if pieces_lightb() >= 2 {
                                pieces_of_type_not(pieces_lightb()) >= 3
                            } else {
                                true
                            }
                    );
                }

            // By this point, we only need to deal with white's minor pieces.
            } else if horde_num == 2 {
                if pieces_num == 1 {
                    // Two minor pieces cannot mate a lone king.
                    return true;
                } else if horde.knights == 2 {
                    // A king on A1 is mated by two knights, if it is obstructed by a
                    // pawn/bishop/knight on B2. On the other hand, if black only has
                    // major pieces it is a draw.
                    return pieces.pawns + pieces.bishops + pieces.knights < 1;
                } else if has_bishop_pair(color) {
                    return !(
                        // A king on A1 obstructed by a pawn/bishop on A2 is mated
                        // by the bishop pair.
                        pieces.pawns >= 1 || pieces.bishops >= 1 ||
                            // A pawn/bishop/knight on B4, a pawn/bishop/rook/queen on
                            // A4 and the king on A3 enable Boden's mate by the bishop
                            // pair. In every other case white cannot win.
                            ( pieces.knights >= 1 && pieces.rooks + pieces.queens >= 1 )
                    );
                } else if horde.bishops >= 1 && horde.knights >= 1 {
                    // The horde has a bishop and a knight.
                    return !(
                        // A king on A1 obstructed by a pawn/opposite-color-bishop on
                        // A2 is mated by a knight on D2 and a bishop on C3.
                        pieces.pawns >= 1 || pieces_oppositeb_of(horde_bishop_co()) >= 1 ||
                            // A king on A1 bounded by two friendly pieces on A2 and B1 is
                            // mated when the knight moves from D4 to C2 so that both the
                            // knight and the bishop deliver check.
                            pieces_of_type_not( pieces_sameb_as(horde_bishop_co()) ) >=3
                    );
                } else {
                    // The horde has two or more bishops on the same color.
                    // White can only win if black has enough material to obstruct
                    // the squares of the opposite color around the king.
                    return !(
                        // A king on A1 obstructed by a pawn/opposite-bishop/knight
                        // on A2 and a opposite-bishop/knight on B1 is mated by two
                        // bishops on B2 and C3. This position is theoretically
                        // achievable even when black has two pawns or when they
                        // have a pawn and an opposite color bishop.
                        (pieces.pawns >= 1 && pieces_oppositeb_of(horde_bishop_co()) >= 1)
                            || (pieces.pawns >= 1 && pieces.knights >= 1)
                            || (pieces_oppositeb_of(horde_bishop_co()) >= 1 && pieces.knights >= 1)
                            || (pieces_oppositeb_of(horde_bishop_co()) >= 2)
                            || pieces.knights >= 2
                            || pieces.pawns >= 2
                        // In every other case, white can only draw.
                    );
                }
            } else if horde_num == 3 {
                // A king in the corner is mated by two knights and a bishop or three
                // knights or the bishop pair and a knight/bishop.
                if (horde.knights == 2 && horde.bishops == 1)
                    || horde.knights == 3
                    || has_bishop_pair(color)
                {
                    return false;
                } else {
                    // White has two same color bishops and a knight.
                    // A king on A1 is mated by a bishop on B2, a bishop on C1 and a
                    // knight on C3, as long as there is another black piece to waste
                    // a tempo.
                    return pieces_num == 1;
                }
            }

            true
        }

        fn variant_outcome(&self) -> Option<Outcome> {
            if self.board().occupied().is_empty() {
                Some(Outcome::Draw)
            } else if self.board().white().is_empty() {
                Some(Outcome::Decisive { winner: Black })
            } else if self.board().black().is_empty() {
                Some(Outcome::Decisive { winner: White })
            } else {
                None
            }
        }
    }

    fn add_king_promotions(moves: &mut MoveList) {
        let mut king_promotions = MoveList::new();

        for m in &moves[..] {
            if let Move::Normal {
                role,
                from,
                capture,
                to,
                promotion: Some(Role::Queen),
            } = *m
            {
                king_promotions.push(Move::Normal {
                    role,
                    from,
                    capture,
                    to,
                    promotion: Some(Role::King),
                });
            }
        }

        moves.extend(king_promotions);
    }
}

#[allow(clippy::too_many_arguments)]
fn do_move(
    board: &mut Board,
    promoted: &mut Bitboard,
    turn: &mut Color,
    castles: &mut Castles,
    ep_square: &mut Option<EpSquare>,
    halfmoves: &mut u32,
    fullmoves: &mut NonZeroU32,
    m: &Move,
) {
    let color = *turn;
    ep_square.take();

    *halfmoves = if m.is_zeroing() {
        0
    } else {
        halfmoves.saturating_add(1)
    };

    match *m {
        Move::Normal {
            role,
            from,
            capture,
            to,
            promotion,
        } => {
            if role == Role::Pawn && to - from == 16 && from.rank() == Rank::Second {
                *ep_square = from.offset(8).map(EpSquare);
            } else if role == Role::Pawn && from - to == 16 && from.rank() == Rank::Seventh {
                *ep_square = from.offset(-8).map(EpSquare);
            }

            if role == Role::King {
                castles.discard_side(color);
            } else if role == Role::Rook {
                castles.discard_rook(from);
            }

            if capture == Some(Role::Rook) {
                castles.discard_rook(to);
            }

            board.discard_piece_at(from);
            board.set_piece_at(to, promotion.map_or(role.of(color), |p| p.of(color)));

            let is_promoted = promoted.remove(from) || promotion.is_some();
            promoted.set(to, is_promoted);
        }
        Move::Castle { king, rook } => {
            let side = CastlingSide::from_queen_side(rook < king);
            board.discard_piece_at(king);
            board.discard_piece_at(rook);
            board.set_piece_at(
                Square::from_coords(side.rook_to_file(), rook.rank()),
                color.rook(),
            );
            board.set_piece_at(
                Square::from_coords(side.king_to_file(), king.rank()),
                color.king(),
            );
            castles.discard_side(color);
        }
        Move::EnPassant { from, to } => {
            board.discard_piece_at(Square::from_coords(to.file(), from.rank())); // captured pawn
            board.discard_piece_at(from);
            board.set_piece_at(to, color.pawn());
        }
        Move::Put { role, to } => {
            board.set_piece_at(to, Piece { color, role });
        }
    }

    if color.is_black() {
        *fullmoves = NonZeroU32::new(fullmoves.get().saturating_add(1)).unwrap();
    }

    *turn = !color;
}

fn validate<P: Position>(pos: &P) -> PositionErrorKinds {
    let mut errors = PositionErrorKinds::empty();

    if pos.board().occupied().is_empty() {
        errors |= PositionErrorKinds::EMPTY_BOARD;
    }

    if (pos.board().pawns() & Bitboard::BACKRANKS).any() {
        errors |= PositionErrorKinds::PAWNS_ON_BACKRANK;
    }

    for color in Color::ALL {
        let kings = pos.board().kings() & pos.board().by_color(color);
        if kings.is_empty() {
            errors |= PositionErrorKinds::MISSING_KING;
        } else if kings.more_than_one() {
            errors |= PositionErrorKinds::TOO_MANY_KINGS;
        }

        if !is_standard_material(pos.board(), color) {
            errors |= PositionErrorKinds::IMPOSSIBLE_MATERIAL;
        }
    }

    if let Some(their_king) = pos.board().king_of(!pos.turn()) {
        if pos
            .king_attackers(their_king, pos.turn(), pos.board().occupied())
            .any()
        {
            errors |= PositionErrorKinds::OPPOSITE_CHECK;
        }
    }

    if let Some(our_king) = pos.board().king_of(pos.turn()) {
        let checkers = pos.checkers();
        match (checkers.first(), checkers.last()) {
            (Some(a), Some(b))
                if a != b && (checkers.count() > 2 || attacks::aligned(a, b, our_king)) =>
            {
                errors |= PositionErrorKinds::IMPOSSIBLE_CHECK;
            }
            _ => (),
        }

        // Determining if there is a valid en passant square requires move
        // generation. We know the king exists, so its fine to call it even
        // before full validation.
        if let Some(ep_suare) = pos.ep_square() {
            for checker in checkers {
                if attacks::aligned(our_king, ep_suare, checker) {
                    errors |= PositionErrorKinds::IMPOSSIBLE_CHECK;
                }
            }
        }
    }

    errors
}

fn is_standard_material(board: &Board, color: Color) -> bool {
    let our = board.by_color(color);
    let promoted_pieces = (board.queens() & our).count().saturating_sub(1)
        + (board.rooks() & our).count().saturating_sub(2)
        + (board.knights() & our).count().saturating_sub(2)
        + (board.bishops() & our & Bitboard::LIGHT_SQUARES)
            .count()
            .saturating_sub(1)
        + (board.bishops() & our & Bitboard::DARK_SQUARES)
            .count()
            .saturating_sub(1);
    (board.pawns() & our).count() + promoted_pieces <= 8
}

fn gen_non_king<P: Position>(pos: &P, target: Bitboard, moves: &mut MoveList) {
    gen_pawn_moves(pos, target, moves);
    KnightTag::gen_moves(pos, target, moves);
    BishopTag::gen_moves(pos, target, moves);
    RookTag::gen_moves(pos, target, moves);
    QueenTag::gen_moves(pos, target, moves);
}

fn gen_safe_king<P: Position>(pos: &P, king: Square, target: Bitboard, moves: &mut MoveList) {
    for to in attacks::king_attacks(king) & target {
        if pos
            .board()
            .attacks_to(to, !pos.turn(), pos.board().occupied())
            .is_empty()
        {
            moves.push(Move::Normal {
                role: Role::King,
                from: king,
                capture: pos.board().role_at(to),
                to,
                promotion: None,
            });
        }
    }
}

fn evasions<P: Position>(pos: &P, king: Square, checkers: Bitboard, moves: &mut MoveList) {
    let sliders = checkers & pos.board().sliders();

    let mut attacked = Bitboard(0);
    for checker in sliders {
        attacked |= attacks::ray(checker, king) ^ checker;
    }

    gen_safe_king(pos, king, !pos.us() & !attacked, moves);

    if let Some(checker) = checkers.single_square() {
        let target = attacks::between(king, checker).with(checker);
        gen_non_king(pos, target, moves);
    }
}

fn gen_castling_moves<P: Position>(
    pos: &P,
    castles: &Castles,
    king: Square,
    side: CastlingSide,
    moves: &mut MoveList,
) {
    if let Some(rook) = castles.rook(pos.turn(), side) {
        let path = castles.path(pos.turn(), side);
        if (path & pos.board().occupied()).any() {
            return;
        }

        let king_to = side.king_to(pos.turn());
        let king_path = attacks::between(king, king_to).with(king);
        for sq in king_path {
            if pos
                .king_attackers(sq, !pos.turn(), pos.board().occupied() ^ king)
                .any()
            {
                return;
            }
        }

        if pos
            .king_attackers(
                king_to,
                !pos.turn(),
                pos.board().occupied() ^ king ^ rook ^ side.rook_to(pos.turn()),
            )
            .any()
        {
            return;
        }

        moves.push(Move::Castle { king, rook });
    }
}

trait Stepper {
    const ROLE: Role;

    fn attacks(from: Square) -> Bitboard;

    fn gen_moves<P: Position>(pos: &P, target: Bitboard, moves: &mut MoveList) {
        for from in pos.our(Self::ROLE) {
            for to in Self::attacks(from) & target {
                moves.push(Move::Normal {
                    role: Self::ROLE,
                    from,
                    capture: pos.board().role_at(to),
                    to,
                    promotion: None,
                });
            }
        }
    }
}

trait Slider {
    const ROLE: Role;
    fn attacks(from: Square, occupied: Bitboard) -> Bitboard;

    fn gen_moves<P: Position>(pos: &P, target: Bitboard, moves: &mut MoveList) {
        for from in pos.our(Self::ROLE) {
            for to in Self::attacks(from, pos.board().occupied()) & target {
                moves.push(Move::Normal {
                    role: Self::ROLE,
                    from,
                    capture: pos.board().role_at(to),
                    to,
                    promotion: None,
                });
            }
        }
    }
}

enum KnightTag {}
enum BishopTag {}
enum RookTag {}
enum QueenTag {}
enum KingTag {}

impl Stepper for KnightTag {
    const ROLE: Role = Role::Knight;
    fn attacks(from: Square) -> Bitboard {
        attacks::knight_attacks(from)
    }
}

impl Stepper for KingTag {
    const ROLE: Role = Role::King;
    fn attacks(from: Square) -> Bitboard {
        attacks::king_attacks(from)
    }
}

impl Slider for BishopTag {
    const ROLE: Role = Role::Bishop;
    fn attacks(from: Square, occupied: Bitboard) -> Bitboard {
        attacks::bishop_attacks(from, occupied)
    }
}

impl Slider for RookTag {
    const ROLE: Role = Role::Rook;
    fn attacks(from: Square, occupied: Bitboard) -> Bitboard {
        attacks::rook_attacks(from, occupied)
    }
}

impl Slider for QueenTag {
    const ROLE: Role = Role::Queen;
    fn attacks(from: Square, occupied: Bitboard) -> Bitboard {
        attacks::queen_attacks(from, occupied)
    }
}

fn gen_pawn_moves<P: Position>(pos: &P, target: Bitboard, moves: &mut MoveList) {
    // Safety of unchecked offset calculations: If we shift a set of squares
    // by an offset, then the negated offset is valid for all resulting
    // squares.

    // Generate captures.
    #[inline(always)]
    fn gen_pawn_captures<P: Position>(
        pos: &P,
        dir: Direction,
        target: Bitboard,
        moves: &mut MoveList,
    ) {
        let captures = dir.translate(pos.our(Role::Pawn)) & pos.them() & target;

        for to in captures & !Bitboard::BACKRANKS {
            // Safety: See above.
            let from = unsafe { to.offset_unchecked(-dir.offset()) };
            moves.push(Move::Normal {
                role: Role::Pawn,
                from,
                capture: pos.board().role_at(to),
                to,
                promotion: None,
            });
        }

        for to in captures & Bitboard::BACKRANKS {
            // Safety: See above.
            let from = unsafe { to.offset_unchecked(-dir.offset()) };
            push_promotions(moves, from, to, pos.board().role_at(to));
        }
    }
    gen_pawn_captures(
        pos,
        pos.turn()
            .fold_wb(Direction::NorthWest, Direction::SouthWest),
        target,
        moves,
    );
    gen_pawn_captures(
        pos,
        pos.turn()
            .fold_wb(Direction::NorthEast, Direction::SouthEast),
        target,
        moves,
    );

    // Generate single-step advances.
    let single_moves = pos.our(Role::Pawn).relative_shift(pos.turn(), 8) & !pos.board().occupied();

    for to in single_moves & target & !Bitboard::BACKRANKS {
        // Safety: See above.
        let from = unsafe { to.offset_unchecked(pos.turn().fold_wb(-8, 8)) };
        moves.push(Move::Normal {
            role: Role::Pawn,
            from,
            capture: None,
            to,
            promotion: None,
        });
    }

    for to in single_moves & target & Bitboard::BACKRANKS {
        // Safety: See above.
        let from = unsafe { to.offset_unchecked(pos.turn().fold_wb(-8, 8)) };
        push_promotions(moves, from, to, None);
    }

    // Generate double-step advances.
    let double_moves = single_moves.relative_shift(pos.turn(), 8)
        & pos.turn().fold_wb(Bitboard::SOUTH, Bitboard::NORTH)
        & !pos.board().occupied();

    for to in double_moves & target {
        // Safety: See above.
        let from = unsafe { to.offset_unchecked(pos.turn().fold_wb(-16, 16)) };
        moves.push(Move::Normal {
            role: Role::Pawn,
            from,
            capture: None,
            to,
            promotion: None,
        });
    }
}

fn push_promotions(moves: &mut MoveList, from: Square, to: Square, capture: Option<Role>) {
    for promotion in [Role::Queen, Role::Rook, Role::Bishop, Role::Knight] {
        moves.push(Move::Normal {
            role: Role::Pawn,
            from,
            capture,
            to,
            promotion: Some(promotion),
        });
    }
}

fn relevant_ep<P: Position>(EpSquare(ep_square): EpSquare, pos: &P) -> Option<Square> {
    if pos.en_passant_moves().is_empty() {
        None
    } else {
        Some(ep_square)
    }
}

fn gen_en_passant(
    board: &Board,
    turn: Color,
    ep_square: Option<EpSquare>,
    moves: &mut MoveList,
) -> bool {
    let mut found = false;

    if let Some(EpSquare(to)) = ep_square {
        for from in board.pawns() & board.by_color(turn) & attacks::pawn_attacks(!turn, to) {
            moves.push(Move::EnPassant { from, to });
            found = true;
        }
    }

    found
}

fn slider_blockers(board: &Board, enemy: Bitboard, king: Square) -> Bitboard {
    let snipers = (attacks::rook_attacks(king, Bitboard(0)) & board.rooks_and_queens())
        | (attacks::bishop_attacks(king, Bitboard(0)) & board.bishops_and_queens());

    let mut blockers = Bitboard(0);

    for sniper in snipers & enemy {
        let b = attacks::between(king, sniper) & board.occupied();

        if !b.more_than_one() {
            blockers.add(b);
        }
    }

    blockers
}

fn is_safe<P: Position>(pos: &P, king: Square, m: &Move, blockers: Bitboard) -> bool {
    match *m {
        Move::Normal { from, to, .. } => {
            !blockers.contains(from) || attacks::aligned(from, to, king)
        }
        Move::EnPassant { from, to } => {
            let mut occupied = pos.board().occupied();
            occupied.toggle(from);
            occupied.toggle(Square::from_coords(to.file(), from.rank())); // captured pawn
            occupied.add(to);

            (attacks::rook_attacks(king, occupied) & pos.them() & pos.board().rooks_and_queens())
                .is_empty()
                && (attacks::bishop_attacks(king, occupied)
                    & pos.them()
                    & pos.board().bishops_and_queens())
                .is_empty()
        }
        _ => true,
    }
}

fn filter_san_candidates(role: Role, to: Square, moves: &mut MoveList) {
    moves.retain(|m| match *m {
        Move::Normal { role: r, to: t, .. } | Move::Put { role: r, to: t } => to == t && role == r,
        Move::EnPassant { to: t, .. } => role == Role::Pawn && t == to,
        Move::Castle { .. } => false,
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fen::{fen, Fen};

    struct _AssertObjectSafe(Box<dyn Position>);

    fn setup_fen<T: Position + FromSetup>(fen: &str) -> T {
        fen.parse::<Fen>()
            .expect("valid fen")
            .position::<T>(CastlingMode::Chess960)
            .expect("legal position")
    }

    #[test]
    fn test_most_known_legals() {
        let pos: Chess = setup_fen("R6R/3Q4/1Q4Q1/4Q3/2Q4Q/Q4Q2/pp1Q4/kBNN1KB1 w - - 0 1");
        assert_eq!(pos.legal_moves().len(), 218);
    }

    #[test]
    fn test_pinned_san_candidate() {
        let pos: Chess = setup_fen("R2r2k1/6pp/1Np2p2/1p2pP2/4p3/4K3/3r2PP/8 b - - 5 37");

        let moves = pos.san_candidates(Role::Rook, Square::D3);

        assert_eq!(
            moves[0],
            Move::Normal {
                role: Role::Rook,
                from: Square::D2,
                capture: None,
                to: Square::D3,
                promotion: None,
            }
        );

        assert_eq!(moves.len(), 1);
    }

    #[test]
    fn test_promotion() {
        let pos: Chess = setup_fen("3r3K/6PP/8/8/8/2k5/8/8 w - - 0 1");

        let moves = pos.legal_moves();
        assert!(moves.iter().all(|m| m.role() == Role::Pawn));
        assert!(moves.iter().all(|m| m.is_promotion()));
    }

    fn assert_insufficient_material<P>(fen: &str, white: bool, black: bool)
    where
        P: Position + FromSetup,
    {
        let pos: P = setup_fen(fen);

        assert_eq!(pos.has_insufficient_material(White), white);
        assert_eq!(pos.has_insufficient_material(Black), black);
    }

    #[test]
    fn test_insufficient_material() {
        assert_insufficient_material::<Chess>("8/5k2/8/8/8/8/3K4/8 w - - 0 1", true, true);
        assert_insufficient_material::<Chess>("8/3k4/8/8/2N5/8/3K4/8 b - - 0 1", true, true);
        assert_insufficient_material::<Chess>("8/4rk2/8/8/8/8/3K4/8 w - - 0 1", true, false);
        assert_insufficient_material::<Chess>("8/4qk2/8/8/8/8/3K4/8 w - - 0 1", true, false);
        assert_insufficient_material::<Chess>("8/4bk2/8/8/8/8/3KB3/8 w - - 0 1", false, false);
        assert_insufficient_material::<Chess>("8/8/3Q4/2bK4/B7/8/1k6/8 w - - 1 68", false, false);
        assert_insufficient_material::<Chess>("8/5k2/8/8/8/4B3/3K1B2/8 w - - 0 1", true, true);
        assert_insufficient_material::<Chess>("5K2/8/8/1B6/8/k7/6b1/8 w - - 0 39", true, true);
        assert_insufficient_material::<Chess>("8/8/8/4k3/5b2/3K4/8/2B5 w - - 0 33", true, true);
        assert_insufficient_material::<Chess>("3b4/8/8/6b1/8/8/R7/K1k5 w - - 0 1", false, true);
    }

    #[test]
    fn test_eq() {
        assert_eq!(Chess::default(), Chess::default());
        assert_ne!(
            Chess::default(),
            Chess::default().swap_turn().expect("swap turn legal")
        );

        // Check that castling paths do not interfere.
        let pos: Chess = setup_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBK1BNR w KQkq - 0 1");
        let pos_after_move = pos
            .play(&Move::Normal {
                role: Role::King,
                from: Square::D1,
                to: Square::E1,
                capture: None,
                promotion: None,
            })
            .expect("Ke1 is legal");
        assert_eq!(
            pos_after_move,
            setup_fen::<Chess>("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNB1KBNR b kq - 1 1")
        );

        // Check that promoted pieces are irrelevant in standard chess.
        let pos: Chess = setup_fen("rnbqkbn1/pppppppP/8/8/8/8/PPPPPPP1/RNB~QKBNR w KQq - 0 26");
        let pos_after_queen_promotion = pos
            .clone()
            .play(&Move::Normal {
                role: Role::Pawn,
                from: Square::H7,
                to: Square::H8,
                capture: None,
                promotion: Some(Role::Queen),
            })
            .expect("h8=Q is legal");
        let pos_after_knight_promotion = pos
            .play(&Move::Normal {
                role: Role::Pawn,
                from: Square::H7,
                to: Square::H8,
                capture: None,
                promotion: Some(Role::Knight),
            })
            .expect("h8=N is legal");
        let final_pos: Chess =
            setup_fen("rnbqkbnQ/ppppppp1/8/8/8/8/PPPPPPP1/RNBQKBNR b KQq - 0 26");
        assert_eq!(pos_after_queen_promotion, final_pos);
        assert_ne!(pos_after_knight_promotion, final_pos);
    }

    #[cfg(feature = "variant")]
    #[test]
    fn test_variant_insufficient_material() {
        use super::variant::*;

        let false_negative = false;

        assert_insufficient_material::<Atomic>("8/3k4/8/8/2N5/8/3K4/8 b - - 0 1", true, true);
        assert_insufficient_material::<Atomic>("8/4rk2/8/8/8/8/3K4/8 w - - 0 1", true, true);
        assert_insufficient_material::<Atomic>("8/4qk2/8/8/8/8/3K4/8 w - - 0 1", true, false);
        assert_insufficient_material::<Atomic>("8/1k6/8/2n5/8/3NK3/8/8 b - - 0 1", false, false);
        assert_insufficient_material::<Atomic>("8/4bk2/8/8/8/8/3KB3/8 w - - 0 1", true, true);
        assert_insufficient_material::<Atomic>("4b3/5k2/8/8/8/8/3KB3/8 w - - 0 1", false, false);
        assert_insufficient_material::<Atomic>("3Q4/5kKB/8/8/8/8/8/8 b - - 0 1", false, true);
        assert_insufficient_material::<Atomic>("8/5k2/8/8/8/8/5K2/4bb2 w - - 0 1", true, false);
        assert_insufficient_material::<Atomic>("8/5k2/8/8/8/8/5K2/4nb2 w - - 0 1", true, false);

        assert_insufficient_material::<Antichess>("8/4bk2/8/8/8/8/3KB3/8 w - - 0 1", false, false);
        assert_insufficient_material::<Antichess>("4b3/5k2/8/8/8/8/3KB3/8 w - - 0 1", false, false);
        assert_insufficient_material::<Antichess>("8/8/8/6b1/8/3B4/4B3/5B2 w - - 0 1", true, true);
        assert_insufficient_material::<Antichess>("8/8/5b2/8/8/3B4/3B4/8 w - - 0 1", true, false);
        assert_insufficient_material::<Antichess>(
            "8/5p2/5P2/8/3B4/1bB5/8/8 b - - 0 1",
            false_negative,
            false_negative,
        );

        assert_insufficient_material::<KingOfTheHill>(
            "8/5k2/8/8/8/8/3K4/8 w - - 0 1",
            false,
            false,
        );

        assert_insufficient_material::<RacingKings>("8/5k2/8/8/8/8/3K4/8 w - - 0 1", false, false);

        assert_insufficient_material::<ThreeCheck>("8/5k2/8/8/8/8/3K4/8 w - - 0 1", true, true);
        assert_insufficient_material::<ThreeCheck>("8/5k2/8/8/8/8/3K2N1/8 w - - 0 1", false, true);

        assert_insufficient_material::<Crazyhouse>("8/5k2/8/8/8/8/3K2N1/8 w - - 0 1", true, true);
        assert_insufficient_material::<Crazyhouse>(
            "8/5k2/8/8/8/5B2/3KB3/8 w - - 0 1",
            false,
            false,
        );
        assert_insufficient_material::<Crazyhouse>(
            "8/8/8/8/3k4/3N~4/3K4/8 w - - 0 1",
            false,
            false,
        );

        assert_insufficient_material::<Horde>("8/5k2/8/8/8/4NN2/8/8 w - - 0 1", true, false);
    }

    #[cfg(feature = "variant")]
    #[test]
    fn test_exploded_king_loses_castling_rights() {
        use super::variant::Atomic;

        let pos: Atomic = setup_fen("rnb1kbnr/pppppppp/8/4q3/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1");

        let pos = pos
            .play(&Move::Normal {
                role: Role::Queen,
                from: Square::E5,
                to: Square::E2,
                capture: Some(Role::Pawn),
                promotion: None,
            })
            .expect("Qxe2# is legal");

        assert_eq!(
            pos.castling_rights(),
            Bitboard::from(Square::A8) | Bitboard::from(Square::H8)
        );
        assert_eq!(
            pos.castles().rook(Color::White, CastlingSide::QueenSide),
            None
        );
        assert_eq!(
            pos.castles().rook(Color::White, CastlingSide::KingSide),
            None
        );
        assert_eq!(
            pos.castles().rook(Color::Black, CastlingSide::QueenSide),
            Some(Square::A8)
        );
        assert_eq!(
            pos.castles().rook(Color::Black, CastlingSide::KingSide),
            Some(Square::H8)
        );
    }

    #[cfg(feature = "variant")]
    #[test]
    fn test_atomic_exploded_king() {
        use super::variant::Atomic;

        let pos: Atomic = setup_fen("rn5r/pp4pp/2p3Nn/5p2/1b2P1PP/8/PPP2P2/R1B1KB1R b KQ - 0 9");

        assert_eq!(
            pos.outcome(),
            Some(Outcome::Decisive {
                winner: Color::White
            })
        );
    }

    #[cfg(feature = "variant")]
    #[test]
    fn test_racing_kings_end() {
        use super::variant::RacingKings;

        // Both players reached the backrank.
        let pos: RacingKings = setup_fen("kr3NK1/1q2R3/8/8/8/5n2/2N5/1rb2B1R w - - 11 14");
        assert!(pos.is_variant_end());
        assert_eq!(pos.variant_outcome(), Some(Outcome::Draw));

        // White to move is lost because black reached the backrank.
        let pos: RacingKings = setup_fen("1k6/6K1/8/8/8/8/8/8 w - - 0 1");
        assert!(pos.is_variant_end());
        assert_eq!(
            pos.variant_outcome(),
            Some(Outcome::Decisive {
                winner: Color::Black
            })
        );

        // Black is given a chance to catch up.
        let pos: RacingKings = setup_fen("1K6/7k/8/8/8/8/8/8 b - - 0 1");
        assert_eq!(pos.variant_outcome(), None);

        // Black near backrank but cannot move there.
        let pos: RacingKings = setup_fen("2KR4/k7/2Q5/4q3/8/8/8/2N5 b - - 0 1");
        assert!(pos.is_variant_end());
        assert_eq!(
            pos.variant_outcome(),
            Some(Outcome::Decisive {
                winner: Color::White
            })
        );
    }

    #[test]
    fn test_aligned_checkers() {
        let res = "2Nq4/2K5/1b6/8/7R/3k4/7P/8 w - - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .position::<Chess>(CastlingMode::Chess960);
        assert_eq!(
            res.expect_err("impossible check").kinds(),
            PositionErrorKinds::IMPOSSIBLE_CHECK
        );

        let _ = "8/8/5k2/p1q5/PP1rp1P1/3P1N2/2RK1r2/5nN1 w - - 0 3"
            .parse::<Fen>()
            .expect("valid fen")
            .position::<Chess>(CastlingMode::Standard)
            .expect("checkers aligned with opponent king not relevant");

        let res = "8/8/8/1k6/3Pp3/8/8/4KQ2 b - d3 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .position::<Chess>(CastlingMode::Standard);
        assert_eq!(
            res.expect_err("impossible check due to ep square").kinds(),
            PositionErrorKinds::IMPOSSIBLE_CHECK
        );
    }

    #[test]
    fn test_swap_turn() {
        let pos: Chess = "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3"
            .parse::<Fen>()
            .expect("valid fen")
            .position(CastlingMode::Chess960)
            .expect("valid position");
        let swapped_fen = fen(&pos.swap_turn().expect("swap turn"));
        assert_eq!(
            swapped_fen,
            "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3"
        );
    }

    #[test]
    fn test_invalid_ep_square() {
        let fen: Fen = "4k3/8/8/8/8/8/8/4K3 w - e3 0 1".parse().expect("valid fen");
        assert_eq!(
            fen.position::<Chess>(CastlingMode::Standard)
                .expect_err("invalid ep square")
                .kinds(),
            PositionErrorKinds::INVALID_EP_SQUARE
        );
        assert_eq!(
            fen.position::<Chess>(CastlingMode::Standard)
                .or_else(PositionError::ignore_invalid_ep_square)
                .expect("now valid")
                .ep_square(),
            None
        );
    }
}
