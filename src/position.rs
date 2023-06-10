use core::{
    fmt,
    hash::{Hash, Hasher},
    num::NonZeroU32,
    str::FromStr,
};

use bitflags::bitflags;

use crate::{
    attacks,
    bitboard::{Bitboard, Direction},
    setup::{Castles, EnPassant, Setup},
    Board, ByColor, ByRole, CastlingMode, CastlingSide, Color,
    Color::{Black, White},
    EnPassantMode, Move, MoveList, Piece, Rank, RemainingChecks, Role, Square,
};

/// Outcome of a game.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Outcome {
    Decisive { winner: Color },
    Draw,
}

impl Outcome {
    pub const fn from_winner(winner: Option<Color>) -> Outcome {
        match winner {
            Some(winner) => Outcome::Decisive { winner },
            None => Outcome::Draw,
        }
    }

    pub const fn winner(self) -> Option<Color> {
        match self {
            Outcome::Decisive { winner } => Some(winner),
            Outcome::Draw => None,
        }
    }

    pub const fn from_ascii(bytes: &[u8]) -> Result<Outcome, ParseOutcomeError> {
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

#[cfg(feature = "std")]
impl std::error::Error for ParseOutcomeError {}

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

#[cfg(feature = "std")]
impl<P: fmt::Debug> std::error::Error for PlayError<P> {}

bitflags! {
    /// Reasons for a [`Setup`] not being a legal [`Position`].
    ///
    /// A position is legal if it can be reached with a sequence of legal moves
    /// from the starting position. All legal positions are accepted.
    /// However, it is not feasible (or even always deseriable) to reject all
    /// illegal positions.
    ///
    /// Instead, the validity requirements here are chosen based on
    /// practical considerations: Are shakmaty, as well as common
    /// chess software (in particular Stockfish and Lichess) able to correctly
    /// handle the position, and are they likely to continue to do so in future
    /// versions?
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct PositionErrorKinds: u32 {
        /// There are no pieces on the board.
        const EMPTY_BOARD = 1 << 0;

        /// A king is required but missing.
        const MISSING_KING = 1 << 1;

        /// A player has too many kings.
        const TOO_MANY_KINGS = 1 << 2;

        /// There are pawns on the backrank. Only [`Horde`](variant::Horde) allows players to
        /// have pawns on their own backrank.
        const PAWNS_ON_BACKRANK = 1 << 3;

        /// Some castling rights are invalid for the selected [`CastlingMode`].
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

        /// There are impossibly many checkers, two sliding checkers are
        /// aligned, or check is not possible because the en passant square
        /// implies that the last move was a double pawn push.
        ///
        /// Unlike [`PositionErrorKinds::OPPOSITE_CHECK`], this can be ignored
        /// using [`PositionError::ignore_impossible_check()`], but note that
        /// other software may not work correctly in such situations.
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
        /// [`PositionError::ignore_too_much_material()`], but note that
        /// other software may not work correctly with too much material.
        const TOO_MUCH_MATERIAL = 1 << 8;

        /// A variant specific rule is violated.
        const VARIANT = 1 << 9;

        #[deprecated = "Use `PositionErrorKinds::TOO_MUCH_MATERIAL` instead"]
        const IMPOSSIBLE_MATERIAL = PositionErrorKinds::TOO_MUCH_MATERIAL.bits();
    }
}

/// Error when trying to create a [`Position`] from an illegal [`Setup`].
///
/// See [`PositionErrorKinds`] for possible reasons.
#[derive(Clone)]
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

    /// Discards invalid castling rights to recover from
    /// [`PositionErrorKinds::INVALID_CASTLING_RIGHTS`].
    pub fn ignore_invalid_castling_rights(self) -> Result<P, Self> {
        self.ignore(PositionErrorKinds::INVALID_CASTLING_RIGHTS)
    }

    /// Discards invalid en passant squares to recover from
    /// [`PositionErrorKinds::INVALID_EP_SQUARE`].
    pub fn ignore_invalid_ep_square(self) -> Result<P, Self> {
        self.ignore(PositionErrorKinds::INVALID_EP_SQUARE)
    }

    /// Get the position despite [`PositionErrorKinds::TOO_MUCH_MATERIAL`].
    ///
    /// Note that other programs may not work with too much material.
    pub fn ignore_too_much_material(self) -> Result<P, Self> {
        self.ignore(PositionErrorKinds::TOO_MUCH_MATERIAL)
    }

    #[deprecated = "Use `PositionErrorKinds::ignore_too_much_material()`"]
    pub fn ignore_impossible_material(self) -> Result<P, Self> {
        self.ignore(PositionErrorKinds::TOO_MUCH_MATERIAL)
    }

    /// Get the position despite [`PositionErrorKinds::IMPOSSIBLE_CHECK`]
    /// (not be be confused with [`PositionErrorKinds::OPPOSITE_CHECK`]).
    ///
    /// Note that other programs may not work in such a situation.
    pub fn ignore_impossible_check(self) -> Result<P, Self> {
        self.ignore(PositionErrorKinds::IMPOSSIBLE_CHECK)
    }

    /// Returns the reasons for this error.
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
        reason(PositionErrorKinds::TOO_MUCH_MATERIAL, "too much material")?;
        reason(PositionErrorKinds::VARIANT, "variant rule violated")?;
        if first {
            f.write_str("unknown reason")?;
        }

        Ok(())
    }
}

#[cfg(feature = "std")]
impl<P> std::error::Error for PositionError<P> {}

/// Validate and set up a playable [`Position`]. All provided chess variants
/// support this.
pub trait FromSetup: Sized {
    /// Set up a playable [`Position`].
    ///
    /// # Errors
    ///
    /// Returns [`PositionError`] if the [`Setup`] does not
    /// meet [basic validity requirements](PositionErrorKinds).
    ///
    /// Meeting the requirements does not imply that the position
    /// is actually reachable with a series of legal moves from the starting
    /// position.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{CastlingMode, Chess, FromSetup, Setup, PositionError};
    ///
    /// let setup = Setup::default();
    ///
    /// let pos = Chess::from_setup(setup, CastlingMode::Standard)
    ///     .or_else(PositionError::ignore_too_much_material)
    ///     .or_else(PositionError::ignore_impossible_check)?;
    ///
    /// # Ok::<_, PositionError<_>>(())
    /// ```
    fn from_setup(setup: Setup, mode: CastlingMode) -> Result<Self, PositionError<Self>>;
}

/// A playable chess or chess variant position. See [`Chess`] for a concrete
/// implementation.
///
/// # Equality
///
/// All provided variants implement [`Hash`](std::hash::Hash),
/// [`PartialEq`](std::cmp::PartialEq), and [`Eq`](std::cmp::Eq) according
/// to FIDE rules for repeated positions. That is, considering
///
/// * piece positions
/// * promoted pieces only in Crazyhouse
/// * pockets only in Crazyhouse
/// * turn
/// * current castling rights
/// * currently available [legal](`EnPassantMode::Legal`) en passant moves
/// * remaining checks only in Three-Check
///
/// but specifally *ignoring* halfmove and fullmove counters.
///
/// Use [`Setup`] for structural equality.
pub trait Position {
    /// Piece positions on the board.
    fn board(&self) -> &Board;
    /// Positions of tracked promoted pieces. Used only for Crazyhouse.
    fn promoted(&self) -> Bitboard;
    /// Pockets in chess variants like Crazyhouse.
    fn pockets(&self) -> Option<&ByColor<ByRole<u8>>>;
    /// Side to move.
    fn turn(&self) -> Color;
    /// Castling paths and unmoved rooks.
    fn castles(&self) -> &Castles;
    /// [Unconditionally](`EnPassantMode::Always`) gets the en passant target
    /// square after a double pawn push, even if no en passant capture is
    /// actually possible.
    ///
    /// Also see [`Position::pseudo_legal_ep_square()`] and
    /// [`Position::legal_ep_square()`].
    fn maybe_ep_square(&self) -> Option<Square>;
    /// Remaining checks in Three-Check.
    fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>>;
    /// Number of half-moves since the last
    /// [capture or pawn move](super::Move::is_zeroing()).
    fn halfmoves(&self) -> u32;
    /// Move number. Starts at 1 and is increased after every black move.
    fn fullmoves(&self) -> NonZeroU32;

    /// Converts the position to the current [`Setup`].
    fn into_setup(self, mode: EnPassantMode) -> Setup;

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
                self.castles().castling_rights().contains(from)
                    || self.castles().castling_rights().contains(to)
                    || (role == Role::King && self.castles().has_color(self.turn()))
            }
        }) || self.legal_ep_square().is_some()
    }

    /// Attacks that a king on `square` would have to deal with.
    fn king_attackers(&self, square: Square, attacker: Color, occupied: Bitboard) -> Bitboard {
        self.board().attacks_to(square, attacker, occupied)
    }

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
    /// The converse is not necessarily true for this static check.
    /// The position might be locked up such that `color` can never win the
    /// game (even if `!color` cooperates), or insufficient material might only
    /// become apparent after a forced sequence of moves.
    ///
    /// The minimum guarantee is: Looking only at the material configuration,
    /// taking into account color complexes of bishops and knights, but not
    /// concrete piece positions, is there a position with the same material
    /// configuration where `color` can win with a series of legal moves?
    /// If not, then `color` has insufficient winning material.
    ///
    /// For a complete dynamic unwinnability solver see
    /// <https://chasolver.org/>.
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
    /// [`Position::play()`] if you cannot guarantee legality.
    fn play_unchecked(&mut self, m: &Move);

    // Implementation note: Trait methods above this comment should be made
    // available for VariantPosition. The provided methods below this comment
    // are never overwritten in implementations, but for simplicity of use
    // (especially around dyn) they are not moved to an extension trait.

    /// Squares occupied by the side to move.
    fn us(&self) -> Bitboard /* FINAL */ {
        self.board().by_color(self.turn())
    }

    /// Squares occupied with the given piece type by the side to move.
    fn our(&self, role: Role) -> Bitboard /* FINAL */ {
        self.board().by_piece(role.of(self.turn()))
    }

    /// Squares occupied by the opponent of the side to move.
    fn them(&self) -> Bitboard /* FINAL */ {
        self.board().by_color(!self.turn())
    }

    /// Squares occupied with the given piece type by the opponent of the side
    /// to move.
    fn their(&self, role: Role) -> Bitboard /* FINAL */ {
        self.board().by_piece(role.of(!self.turn()))
    }

    /// Tests a move for legality.
    fn is_legal(&self, m: &Move) -> bool /* FINAL */ {
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

    /// The en passant square, if it is the target of a
    /// [pseudo-legal](`EnPassantMode::PseudoLegal`) en passant move.
    fn pseudo_legal_ep_square(&self) -> Option<Square> /* FINAL */ {
        self.maybe_ep_square().filter(|ep_square| {
            (attacks::pawn_attacks(!self.turn(), *ep_square) & self.our(Role::Pawn)).any()
        })
    }

    /// The en passant square, if it really is the target of a
    /// [legal](`EnPassantMode::Legal`) en passant
    /// move.
    fn legal_ep_square(&self) -> Option<Square> /* FINAL */ {
        self.pseudo_legal_ep_square()
            .filter(|_| !self.en_passant_moves().is_empty())
    }

    /// The en passant square.
    fn ep_square(&self, mode: EnPassantMode) -> Option<Square> /* FINAL */ {
        match mode {
            EnPassantMode::Always => self.maybe_ep_square(),
            EnPassantMode::PseudoLegal => self.pseudo_legal_ep_square(),
            EnPassantMode::Legal => self.legal_ep_square(),
        }
    }

    /// Bitboard of pieces giving check.
    fn checkers(&self) -> Bitboard /* FINAL */ {
        self.our(Role::King).first().map_or(Bitboard(0), |king| {
            self.king_attackers(king, !self.turn(), self.board().occupied())
        })
    }

    /// Tests if the king is in check.
    fn is_check(&self) -> bool /* FINAL */ {
        self.checkers().any()
    }

    /// Tests for checkmate.
    fn is_checkmate(&self) -> bool /* FINAL */ {
        !self.checkers().is_empty() && self.legal_moves().is_empty()
    }

    /// Tests for stalemate.
    fn is_stalemate(&self) -> bool /* FINAL */ {
        self.checkers().is_empty() && !self.is_variant_end() && self.legal_moves().is_empty()
    }

    /// Tests if both sides
    /// [have insufficient winning material](Position::has_insufficient_material).
    fn is_insufficient_material(&self) -> bool /* FINAL */ {
        self.has_insufficient_material(White) && self.has_insufficient_material(Black)
    }

    /// Tests if the game is over due to [checkmate](Position::is_checkmate()),
    /// [stalemate](Position::is_stalemate()),
    /// [insufficient material](Position::is_insufficient_material) or
    /// [variant end](Position::is_variant_end).
    fn is_game_over(&self) -> bool /* FINAL */ {
        self.is_variant_end() || self.legal_moves().is_empty() || self.is_insufficient_material()
    }

    /// The outcome of the game, or `None` if the game is not over.
    fn outcome(&self) -> Option<Outcome> /* FINAL */ {
        self.variant_outcome().or_else(|| {
            if self.legal_moves().is_empty() {
                Some(if self.is_check() {
                    Outcome::Decisive {
                        winner: !self.turn(),
                    }
                } else {
                    Outcome::Draw // Stalemate
                })
            } else if self.is_insufficient_material() {
                Some(Outcome::Draw)
            } else {
                None
            }
        })
    }

    /// Plays a move.
    ///
    ///
    /// # Errors
    ///
    /// Returns a [`PlayError`] if the move is not legal. You can use
    /// [`Position::play_unchecked()`] if you can guarantee legality.
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
        let mut setup = self.into_setup(EnPassantMode::Always);
        setup.swap_turn();
        Self::from_setup(setup, mode)
    }
}

/// A standard Chess position.
///
/// # Equality
///
/// [`Hash`](std::hash::Hash), [`PartialEq`](std::cmp::PartialEq),
/// and [`Eq`](std::cmp::Eq) are implemented according to FIDE rules for
/// repeated positions. See [`Position`](trait.Position.html#equality).
#[derive(Clone, Debug)]
pub struct Chess {
    board: Board,
    turn: Color,
    castles: Castles,
    ep_square: Option<EnPassant>,
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

    #[allow(clippy::type_complexity)]
    fn from_setup_unchecked(
        setup: Setup,
        mode: CastlingMode,
    ) -> (
        Chess,
        Option<ByColor<ByRole<u8>>>,
        Option<ByColor<RemainingChecks>>,
        PositionErrorKinds,
    ) {
        let mut errors = PositionErrorKinds::empty();

        let castles = match Castles::from_setup(&setup, mode) {
            Ok(castles) => castles,
            Err(castles) => {
                errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS;
                castles
            }
        };

        let ep_square = match EnPassant::from_setup(&setup) {
            Ok(ep_square) => ep_square,
            Err(()) => {
                errors |= PositionErrorKinds::INVALID_EP_SQUARE;
                None
            }
        };

        let pos = Chess {
            board: setup.board,
            turn: setup.turn,
            castles,
            ep_square,
            halfmoves: setup.halfmoves,
            fullmoves: setup.fullmoves,
        };

        errors |= validate(&pos, ep_square);

        (pos, setup.pockets, setup.remaining_checks, errors)
    }

    /// Initial position of any regular chess game.
    pub const fn new() -> Chess {
        Chess {
            board: Board::new(),
            turn: White,
            castles: Castles::new(),
            ep_square: None,
            halfmoves: 0,
            fullmoves: match NonZeroU32::new(1) {
                Some(num) => num,
                _ => unreachable!(),
            },
        }
    }
}

impl Default for Chess {
    fn default() -> Chess {
        Chess::new()
    }
}

impl Hash for Chess {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.board.hash(state);
        self.turn.hash(state);
        self.castles.castling_rights().hash(state);
        // Optimization: Not hashing legal_ep_square(), but still considered
        // for equality.
    }
}

impl PartialEq for Chess {
    fn eq(&self, other: &Chess) -> bool {
        self.board == other.board
            && self.turn == other.turn
            && self.castles.castling_rights() == other.castles.castling_rights()
            && self.legal_ep_square() == other.legal_ep_square()
    }
}

/// Equivalent to comparing `Position::into_setup(EnPassantMode::Legal)`.
///
/// Positions with different [`CastlingMode`] may be equivalent. En passant
/// squares are considered only if there is a legal en passant capture.
///
/// # Example
///
/// ```
/// use shakmaty::{CastlingMode, Chess, fen::Fen};
///
/// let fen: Fen = "r1bqkbnr/ppp2Qpp/2np4/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4".parse()?;
/// let position: Chess = fen.clone().into_position(CastlingMode::Standard)?;
/// let position_960: Chess = fen.into_position(CastlingMode::Chess960)?;
/// assert_eq!(position, position_960);
///
/// # use shakmaty::{fen::ParseFenError, PositionError};
/// # #[derive(Debug)] struct CommonError;
/// # impl From<ParseFenError> for CommonError { fn from(_: ParseFenError) -> Self { Self } }
/// # impl<P> From<PositionError<P>> for CommonError { fn from(_: PositionError<P>) -> Self { Self } }
/// # Ok::<_, CommonError>(())
/// ```
impl Eq for Chess {}

impl FromSetup for Chess {
    fn from_setup(setup: Setup, mode: CastlingMode) -> Result<Chess, PositionError<Chess>> {
        let (pos, _, _, errors) = Chess::from_setup_unchecked(setup, mode);
        PositionError { pos, errors }.strict()
    }
}

impl Position for Chess {
    fn board(&self) -> &Board {
        &self.board
    }

    fn promoted(&self) -> Bitboard {
        Bitboard::EMPTY
    }

    fn pockets(&self) -> Option<&ByColor<ByRole<u8>>> {
        None
    }

    fn turn(&self) -> Color {
        self.turn
    }

    fn castles(&self) -> &Castles {
        &self.castles
    }

    fn maybe_ep_square(&self) -> Option<Square> {
        self.ep_square.map(EnPassant::square)
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

    fn into_setup(self, mode: EnPassantMode) -> Setup {
        Setup {
            ep_square: self.ep_square(mode),
            board: self.board,
            promoted: Bitboard::EMPTY,
            pockets: None,
            turn: self.turn,
            castling_rights: self.castles.castling_rights(),
            remaining_checks: None,
            halfmoves: self.halfmoves,
            fullmoves: self.fullmoves,
        }
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
            && self.ep_square.map(Square::from) == Some(to)
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
    use core::{cmp::min, ops::Not};

    use super::*;

    /// An Atomic Chess position.
    #[derive(Clone, Debug)]
    pub struct Atomic {
        board: Board,
        turn: Color,
        castles: Castles,
        ep_square: Option<EnPassant>,
        halfmoves: u32,
        fullmoves: NonZeroU32,
    }

    impl Atomic {
        pub const fn new() -> Atomic {
            Atomic {
                board: Board::new(),
                turn: White,
                castles: Castles::new(),
                ep_square: None,
                halfmoves: 0,
                fullmoves: match NonZeroU32::new(1) {
                    Some(num) => num,
                    _ => unreachable!(),
                },
            }
        }
    }

    impl Default for Atomic {
        fn default() -> Atomic {
            Atomic::new()
        }
    }

    impl Hash for Atomic {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.board.hash(state);
            self.turn.hash(state);
            self.castles.castling_rights().hash(state);
        }
    }

    impl PartialEq for Atomic {
        fn eq(&self, other: &Self) -> bool {
            self.board == other.board
                && self.turn == other.turn
                && self.castles.castling_rights() == other.castles.castling_rights()
                && self.legal_ep_square() == other.legal_ep_square()
        }
    }

    impl Eq for Atomic {}

    impl FromSetup for Atomic {
        fn from_setup(setup: Setup, mode: CastlingMode) -> Result<Atomic, PositionError<Atomic>> {
            let mut errors = PositionErrorKinds::empty();

            let castles = match Castles::from_setup(&setup, mode) {
                Ok(castles) => castles,
                Err(castles) => {
                    errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS;
                    castles
                }
            };

            let ep_square = match EnPassant::from_setup(&setup) {
                Ok(ep_square) => ep_square,
                Err(()) => {
                    errors |= PositionErrorKinds::INVALID_EP_SQUARE;
                    None
                }
            };

            let pos = Atomic {
                board: setup.board,
                turn: setup.turn,
                castles,
                ep_square,
                halfmoves: setup.halfmoves,
                fullmoves: setup.fullmoves,
            };

            errors |= validate(&pos, ep_square);

            if ep_square.is_none() {
                // Other king moving away can cause many checks to be given
                // at the same time. We do not check the details, or even that
                // the other king really is close.
                errors.remove(PositionErrorKinds::IMPOSSIBLE_CHECK);
            }

            if (pos.them() & pos.board().kings()).any() {
                // Our king just exploded. Game over, but valid position.
                errors.remove(PositionErrorKinds::MISSING_KING);
            }

            PositionError { pos, errors }.strict()
        }
    }

    impl Position for Atomic {
        fn board(&self) -> &Board {
            &self.board
        }

        fn promoted(&self) -> Bitboard {
            Bitboard::EMPTY
        }

        fn pockets(&self) -> Option<&ByColor<ByRole<u8>>> {
            None
        }

        fn turn(&self) -> Color {
            self.turn
        }

        fn castles(&self) -> &Castles {
            &self.castles
        }

        fn maybe_ep_square(&self) -> Option<Square> {
            self.ep_square.map(EnPassant::square)
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

        fn into_setup(self, mode: EnPassantMode) -> Setup {
            Setup {
                ep_square: self.ep_square(mode),
                board: self.board,
                promoted: Bitboard::EMPTY,
                pockets: None,
                turn: self.turn,
                castling_rights: self.castles.castling_rights(),
                remaining_checks: None,
                halfmoves: self.halfmoves,
                fullmoves: self.fullmoves,
            }
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
                    self.board.discard_piece_at(to);

                    let explosion_radius =
                        attacks::king_attacks(to) & self.board().occupied() & !self.board.pawns();

                    if (explosion_radius & self.board().kings() & self.us()).any() {
                        self.castles.discard_color(self.turn());
                    }

                    for explosion in explosion_radius {
                        self.board.discard_piece_at(explosion);
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
        ep_square: Option<EnPassant>,
        halfmoves: u32,
        fullmoves: NonZeroU32,
    }

    impl Antichess {
        pub const fn new() -> Antichess {
            Antichess {
                board: Board::new(),
                turn: White,
                castles: Castles::empty(CastlingMode::Standard),
                ep_square: None,
                halfmoves: 0,
                fullmoves: match NonZeroU32::new(1) {
                    Some(num) => num,
                    _ => unreachable!(),
                },
            }
        }
    }

    impl Default for Antichess {
        fn default() -> Antichess {
            Antichess::new()
        }
    }

    impl Hash for Antichess {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.board.hash(state);
            self.turn.hash(state);
            self.castles.castling_rights().hash(state);
        }
    }

    impl PartialEq for Antichess {
        fn eq(&self, other: &Self) -> bool {
            self.board == other.board
                && self.turn == other.turn
                && self.castles.castling_rights() == other.castles.castling_rights()
                && self.legal_ep_square() == other.legal_ep_square()
        }
    }

    impl Eq for Antichess {}

    impl FromSetup for Antichess {
        fn from_setup(
            setup: Setup,
            mode: CastlingMode,
        ) -> Result<Antichess, PositionError<Antichess>> {
            let mut errors = PositionErrorKinds::empty();

            let ep_square = match EnPassant::from_setup(&setup) {
                Ok(ep_square) => ep_square,
                Err(()) => {
                    errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS;
                    None
                }
            };

            let pos = Antichess {
                board: setup.board,
                turn: setup.turn,
                castles: Castles::empty(mode),
                ep_square,
                halfmoves: setup.halfmoves,
                fullmoves: setup.fullmoves,
            };

            if setup.castling_rights.any() {
                errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS;
            }

            errors |= validate(&pos, ep_square)
                - PositionErrorKinds::MISSING_KING
                - PositionErrorKinds::TOO_MANY_KINGS
                - PositionErrorKinds::OPPOSITE_CHECK
                - PositionErrorKinds::IMPOSSIBLE_CHECK;

            PositionError { pos, errors }.strict()
        }
    }

    impl Position for Antichess {
        fn board(&self) -> &Board {
            &self.board
        }

        fn promoted(&self) -> Bitboard {
            Bitboard::EMPTY
        }

        fn pockets(&self) -> Option<&ByColor<ByRole<u8>>> {
            None
        }

        fn turn(&self) -> Color {
            self.turn
        }

        fn castles(&self) -> &Castles {
            &self.castles
        }

        fn maybe_ep_square(&self) -> Option<Square> {
            self.ep_square.map(EnPassant::square)
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

        fn into_setup(self, mode: EnPassantMode) -> Setup {
            Setup {
                ep_square: self.ep_square(mode),
                board: self.board,
                promoted: Bitboard::EMPTY,
                pockets: None,
                turn: self.turn,
                castling_rights: self.castles.castling_rights(),
                remaining_checks: None,
                halfmoves: self.halfmoves,
                fullmoves: self.fullmoves,
            }
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
            if self.board.by_color(color).is_empty() {
                false
            } else if self.board.by_color(!color).is_empty() {
                true
            } else if self.board.occupied() == self.board.bishops() {
                // In a position with only bishops, check if all our bishops
                // can be captured.
                let we_some_on_light = (self.board.by_color(color) & Bitboard::LIGHT_SQUARES).any();
                let we_some_on_dark = (self.board.by_color(color) & Bitboard::DARK_SQUARES).any();
                let they_all_on_dark =
                    (self.board.by_color(!color) & Bitboard::LIGHT_SQUARES).is_empty();
                let they_all_on_light =
                    (self.board.by_color(!color) & Bitboard::DARK_SQUARES).is_empty();
                (we_some_on_light && they_all_on_dark) || (we_some_on_dark && they_all_on_light)
            } else if self.board.occupied() == self.board.knights() {
                match (
                    self.board.white().single_square(),
                    self.board.black().single_square(),
                ) {
                    (Some(white_single_knight), Some(black_single_knight)) => {
                        self.turn
                            == color
                                ^ white_single_knight.is_light()
                                ^ black_single_knight.is_dark()
                    }
                    _ => false,
                }
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
    #[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
    pub struct KingOfTheHill {
        chess: Chess,
    }

    impl KingOfTheHill {
        pub const fn new() -> KingOfTheHill {
            KingOfTheHill {
                chess: Chess::new(),
            }
        }
    }

    impl FromSetup for KingOfTheHill {
        fn from_setup(
            setup: Setup,
            mode: CastlingMode,
        ) -> Result<KingOfTheHill, PositionError<KingOfTheHill>> {
            let (chess, _, _, errors) = Chess::from_setup_unchecked(setup, mode);
            PositionError {
                errors,
                pos: KingOfTheHill { chess },
            }
            .strict()
        }
    }

    impl Position for KingOfTheHill {
        fn board(&self) -> &Board {
            self.chess.board()
        }

        fn promoted(&self) -> Bitboard {
            Bitboard::EMPTY
        }

        fn castles(&self) -> &Castles {
            self.chess.castles()
        }

        fn pockets(&self) -> Option<&ByColor<ByRole<u8>>> {
            None
        }

        fn turn(&self) -> Color {
            self.chess.turn()
        }

        fn maybe_ep_square(&self) -> Option<Square> {
            self.chess.maybe_ep_square()
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

        fn into_setup(self, mode: EnPassantMode) -> Setup {
            self.chess.into_setup(mode)
        }

        fn play_unchecked(&mut self, m: &Move) {
            self.chess.play_unchecked(m);
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
    #[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
    pub struct ThreeCheck {
        chess: Chess,
        remaining_checks: ByColor<RemainingChecks>,
    }

    impl ThreeCheck {
        pub const fn new() -> ThreeCheck {
            ThreeCheck {
                chess: Chess::new(),
                remaining_checks: ByColor {
                    black: RemainingChecks::new(3),
                    white: RemainingChecks::new(3),
                },
            }
        }
    }

    impl FromSetup for ThreeCheck {
        fn from_setup(
            setup: Setup,
            mode: CastlingMode,
        ) -> Result<ThreeCheck, PositionError<ThreeCheck>> {
            let (chess, _, remaining_checks, mut errors) = Chess::from_setup_unchecked(setup, mode);

            let remaining_checks = remaining_checks.unwrap_or_default();
            if remaining_checks.iter().all(|remaining| remaining.is_zero()) {
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
        fn board(&self) -> &Board {
            self.chess.board()
        }

        fn promoted(&self) -> Bitboard {
            Bitboard::EMPTY
        }

        fn pockets(&self) -> Option<&ByColor<ByRole<u8>>> {
            None
        }

        fn turn(&self) -> Color {
            self.chess.turn()
        }

        fn castles(&self) -> &Castles {
            self.chess.castles()
        }

        fn maybe_ep_square(&self) -> Option<Square> {
            self.chess.maybe_ep_square()
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

        fn into_setup(self, mode: EnPassantMode) -> Setup {
            Setup {
                remaining_checks: Some(self.remaining_checks),
                ..self.chess.into_setup(mode)
            }
        }

        fn play_unchecked(&mut self, m: &Move) {
            let turn = self.chess.turn();
            self.chess.play_unchecked(m);
            if self.is_check() {
                let checks = self.remaining_checks.get_mut(turn);
                *checks = checks.saturating_sub(1);
            }
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
            self.remaining_checks
                .iter()
                .any(|remaining| remaining.is_zero())
        }

        fn variant_outcome(&self) -> Option<Outcome> {
            self.remaining_checks
                .find(|remaining| remaining.is_zero())
                .map(|winner| Outcome::Decisive { winner })
        }
    }

    /// A Crazyhouse position.
    #[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
    pub struct Crazyhouse {
        chess: Chess,
        promoted: Bitboard,
        pockets: ByColor<ByRole<u8>>,
    }

    impl Crazyhouse {
        pub const fn new() -> Crazyhouse {
            Crazyhouse {
                chess: Chess::new(),
                promoted: Bitboard::EMPTY,
                pockets: ByColor {
                    black: ByRole {
                        pawn: 0,
                        knight: 0,
                        bishop: 0,
                        rook: 0,
                        queen: 0,
                        king: 0,
                    },
                    white: ByRole {
                        pawn: 0,
                        knight: 0,
                        bishop: 0,
                        rook: 0,
                        queen: 0,
                        king: 0,
                    },
                },
            }
        }

        fn our_pocket(&self) -> &ByRole<u8> {
            self.pockets.get(self.turn())
        }

        fn our_pocket_mut(&mut self) -> &mut ByRole<u8> {
            let turn = self.turn();
            self.pockets.get_mut(turn)
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

    impl FromSetup for Crazyhouse {
        fn from_setup(
            setup: Setup,
            mode: CastlingMode,
        ) -> Result<Crazyhouse, PositionError<Crazyhouse>> {
            let promoted = setup.promoted
                & setup.board.occupied()
                & !setup.board.pawns()
                & !setup.board.kings();
            let (chess, pockets, _, mut errors) = Chess::from_setup_unchecked(setup, mode);
            let pockets = pockets.unwrap_or_default();

            if pockets.white.king > 0 || pockets.black.king > 0 {
                errors |= PositionErrorKinds::TOO_MANY_KINGS;
            }

            if pockets.count() + chess.board().occupied().count() > 64 {
                errors |= PositionErrorKinds::VARIANT;
            }

            errors -= PositionErrorKinds::TOO_MUCH_MATERIAL;

            if promoted.count()
                + chess.board().pawns().count()
                + usize::from(pockets.white.pawn)
                + usize::from(pockets.black.pawn)
                > 16
                || (chess.board().knights() & !promoted).count()
                    + usize::from(pockets.white.knight)
                    + usize::from(pockets.black.knight)
                    > 4
                || (chess.board().bishops() & !promoted).count()
                    + usize::from(pockets.white.bishop)
                    + usize::from(pockets.black.bishop)
                    > 4
                || (chess.board().rooks() & !promoted).count()
                    + usize::from(pockets.white.rook)
                    + usize::from(pockets.black.rook)
                    > 4
                || (chess.board().queens() & !promoted).count()
                    + usize::from(pockets.white.queen)
                    + usize::from(pockets.black.queen)
                    > 2
            {
                errors |= PositionErrorKinds::TOO_MUCH_MATERIAL;
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
        fn board(&self) -> &Board {
            self.chess.board()
        }

        fn promoted(&self) -> Bitboard {
            self.promoted
        }

        fn pockets(&self) -> Option<&ByColor<ByRole<u8>>> {
            Some(&self.pockets)
        }

        fn turn(&self) -> Color {
            self.chess.turn()
        }

        fn castles(&self) -> &Castles {
            self.chess.castles()
        }

        fn maybe_ep_square(&self) -> Option<Square> {
            self.chess.maybe_ep_square()
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

        fn into_setup(self, mode: EnPassantMode) -> Setup {
            Setup {
                promoted: self.promoted,
                pockets: Some(self.pockets),
                ..self.chess.into_setup(mode)
            }
        }

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

                    *self.our_pocket_mut().get_mut(capture) += 1;
                }
                Move::EnPassant { .. } => {
                    self.our_pocket_mut().pawn += 1;
                }
                Move::Put { role, .. } => {
                    *self.our_pocket_mut().get_mut(role) -= 1;
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

        fn legal_moves(&self) -> MoveList {
            let mut moves = self.chess.legal_moves();

            let pocket = self.our_pocket();
            let targets = self.legal_put_squares();

            for to in targets {
                for role in [Role::Knight, Role::Bishop, Role::Rook, Role::Queen] {
                    if *pocket.get(role) > 0 {
                        moves.push(Move::Put { role, to });
                    }
                }
            }

            if pocket.pawn > 0 {
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

            if *self.our_pocket().get(role) > 0
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
                    self.chess.castles.castling_rights().contains(from)
                        || self.chess.castles.castling_rights().contains(to)
                        || (role == Role::King && self.chess.castles.has_color(self.turn()))
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
                && self.pockets.white.pawn == 0
                && self.pockets.black.pawn == 0
                && self.pockets.white.rook == 0
                && self.pockets.black.rook == 0
                && self.pockets.white.queen == 0
                && self.pockets.black.queen == 0
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

    impl RacingKings {
        pub const fn new() -> RacingKings {
            RacingKings {
                board: Board::racing_kings(),
                turn: White,
                castles: Castles::empty(CastlingMode::Standard),
                halfmoves: 0,
                fullmoves: match NonZeroU32::new(1) {
                    Some(num) => num,
                    _ => unreachable!(),
                },
            }
        }
    }

    impl Default for RacingKings {
        fn default() -> RacingKings {
            RacingKings::new()
        }
    }

    impl Hash for RacingKings {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.board.hash(state);
            self.turn.hash(state);
            self.castles.castling_rights().hash(state);
        }
    }

    impl PartialEq for RacingKings {
        fn eq(&self, other: &Self) -> bool {
            self.board == other.board
                && self.turn == other.turn
                && self.castles.castling_rights() == other.castles.castling_rights()
        }
    }

    impl Eq for RacingKings {}

    impl FromSetup for RacingKings {
        fn from_setup(
            setup: Setup,
            mode: CastlingMode,
        ) -> Result<RacingKings, PositionError<RacingKings>> {
            let mut errors = PositionErrorKinds::empty();

            if setup.castling_rights.any() {
                errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS;
            }

            if setup.board.pawns().any() {
                errors |= PositionErrorKinds::VARIANT;
            }

            for color in Color::ALL {
                let us = setup.board.by_color(color);
                if (setup.board.knights() & us).count() > 2
                    || (setup.board.bishops() & us).count() > 2
                    || (setup.board.rooks() & us).count() > 2
                    || (setup.board.queens() & us).more_than_one()
                {
                    errors |= PositionErrorKinds::TOO_MUCH_MATERIAL;
                }
            }

            if setup.ep_square.is_some() {
                errors |= PositionErrorKinds::INVALID_EP_SQUARE;
            }

            let pos = RacingKings {
                board: setup.board,
                turn: setup.turn,
                castles: Castles::empty(mode),
                halfmoves: setup.halfmoves,
                fullmoves: setup.fullmoves,
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

            errors |= validate(&pos, None);

            PositionError { pos, errors }.strict()
        }
    }

    impl Position for RacingKings {
        fn board(&self) -> &Board {
            &self.board
        }

        fn promoted(&self) -> Bitboard {
            Bitboard::EMPTY
        }

        fn pockets(&self) -> Option<&ByColor<ByRole<u8>>> {
            None
        }

        fn turn(&self) -> Color {
            self.turn
        }

        fn castles(&self) -> &Castles {
            &self.castles
        }

        fn maybe_ep_square(&self) -> Option<Square> {
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

        fn into_setup(self, _mode: EnPassantMode) -> Setup {
            Setup {
                board: self.board,
                promoted: Bitboard::EMPTY,
                pockets: None,
                turn: self.turn,
                castling_rights: self.castles.castling_rights(),
                ep_square: None,
                remaining_checks: None,
                halfmoves: self.halfmoves,
                fullmoves: self.fullmoves,
            }
        }

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
        ep_square: Option<EnPassant>,
        halfmoves: u32,
        fullmoves: NonZeroU32,
    }

    impl Default for Horde {
        fn default() -> Horde {
            let mut castles = Castles::default();
            castles.discard_color(White);

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

    impl Hash for Horde {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.board.hash(state);
            self.turn.hash(state);
            self.castles.castling_rights().hash(state);
        }
    }

    impl PartialEq for Horde {
        fn eq(&self, other: &Self) -> bool {
            self.board == other.board
                && self.turn == other.turn
                && self.castles.castling_rights() == other.castles.castling_rights()
                && self.legal_ep_square() == other.legal_ep_square()
        }
    }

    impl Eq for Horde {}

    impl FromSetup for Horde {
        fn from_setup(setup: Setup, mode: CastlingMode) -> Result<Horde, PositionError<Horde>> {
            let mut errors = PositionErrorKinds::empty();

            let castles = match Castles::from_setup(&setup, mode) {
                Ok(castles) => castles,
                Err(castles) => {
                    errors |= PositionErrorKinds::INVALID_CASTLING_RIGHTS;
                    castles
                }
            };

            let ep_square = match EnPassant::from_setup(&setup) {
                Ok(ep_square) => ep_square,
                Err(()) => {
                    errors |= PositionErrorKinds::INVALID_EP_SQUARE;
                    None
                }
            };

            let pos = Horde {
                board: setup.board,
                turn: setup.turn,
                castles,
                ep_square,
                halfmoves: setup.halfmoves,
                fullmoves: setup.fullmoves,
            };

            errors |= validate(&pos, ep_square)
                - PositionErrorKinds::PAWNS_ON_BACKRANK
                - PositionErrorKinds::MISSING_KING
                - PositionErrorKinds::TOO_MUCH_MATERIAL;

            if pos.board().kings().is_empty() {
                errors |= PositionErrorKinds::MISSING_KING;
            } else if pos.board().kings().more_than_one() {
                errors |= PositionErrorKinds::TOO_MANY_KINGS;
            }

            for color in Color::ALL {
                let us = pos.board.by_color(color);
                if (pos.board().kings() & us).any() {
                    if !is_standard_material(pos.board(), color) {
                        errors |= PositionErrorKinds::TOO_MUCH_MATERIAL;
                    }
                    if (pos.board().pawns() & us & Bitboard::BACKRANKS).any() {
                        errors |= PositionErrorKinds::PAWNS_ON_BACKRANK;
                    }
                } else {
                    if us.count() > 36 {
                        errors |= PositionErrorKinds::TOO_MUCH_MATERIAL;
                    }
                    if (pos.board().pawns() & us & (!color).backrank()).any() {
                        errors |= PositionErrorKinds::PAWNS_ON_BACKRANK;
                    }
                }
            }

            PositionError { pos, errors }.strict()
        }
    }

    impl Position for Horde {
        fn board(&self) -> &Board {
            &self.board
        }

        fn promoted(&self) -> Bitboard {
            Bitboard::EMPTY
        }

        fn pockets(&self) -> Option<&ByColor<ByRole<u8>>> {
            None
        }

        fn turn(&self) -> Color {
            self.turn
        }

        fn castles(&self) -> &Castles {
            &self.castles
        }

        fn maybe_ep_square(&self) -> Option<Square> {
            self.ep_square.map(EnPassant::square)
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

        fn into_setup(self, mode: EnPassantMode) -> Setup {
            Setup {
                ep_square: self.ep_square(mode),
                board: self.board,
                promoted: Bitboard::EMPTY,
                pockets: None,
                turn: self.turn,
                castling_rights: self.castles.castling_rights(),
                remaining_checks: None,
                halfmoves: self.halfmoves,
                fullmoves: self.fullmoves,
            }
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

        fn is_variant_end(&self) -> bool {
            self.board().white().is_empty() || self.board().black().is_empty()
        }

        #[allow(clippy::nonminimal_bool)] // Aids commentary
        fn has_insufficient_material(&self, color: Color) -> bool {
            #[derive(Copy, Clone)]
            enum SquareColor {
                Dark,
                Light,
            }
            impl From<SquareColor> for Bitboard {
                fn from(square_color: SquareColor) -> Bitboard {
                    match square_color {
                        SquareColor::Light => Bitboard::LIGHT_SQUARES,
                        SquareColor::Dark => Bitboard::DARK_SQUARES,
                    }
                }
            }
            impl Not for SquareColor {
                type Output = SquareColor;

                fn not(self) -> SquareColor {
                    match self {
                        SquareColor::Dark => SquareColor::Light,
                        SquareColor::Light => SquareColor::Dark,
                    }
                }
            }

            // The side with the king can always win by capturing the horde.
            if (self.board.by_color(color) & self.board.kings()).any() {
                return false;
            }

            let has_bishop_pair = |side: Color| -> bool {
                let bishops = self.board.bishops() & self.board.by_color(side);
                (bishops & Bitboard::DARK_SQUARES).any()
                    && (bishops & Bitboard::LIGHT_SQUARES).any()
            };

            // By this point: color is the horde.
            let horde = self.board.material_side(color);
            let horde_bishops = |square_color: SquareColor| -> u8 {
                (Bitboard::from(square_color) & self.board.by_color(color) & self.board.bishops())
                    .count() as u8
            };
            let horde_bishop_color = if horde_bishops(SquareColor::Light) >= 1 {
                SquareColor::Light
            } else {
                SquareColor::Dark
            };
            // Two same color bishops suffice to cover all the light and dark squares
            // around the enemy king.
            let horde_num = horde.pawn
                + horde.knight
                + horde.rook
                + horde.queen
                + min(horde_bishops(SquareColor::Dark), 2)
                + min(horde_bishops(SquareColor::Light), 2);

            let pieces = self.board.material_side(!color);
            let pieces_bishops = |square_color: SquareColor| -> u8 {
                (Bitboard::from(square_color) & self.board.by_color(!color) & self.board.bishops())
                    .count() as u8
            };
            let pieces_num = pieces.count() as u8;
            let pieces_of_type_not = |piece: u8| -> u8 { pieces_num - piece };

            if horde_num == 0 {
                return true;
            }
            if horde_num >= 4 {
                // Four or more white pieces can always deliver mate.
                return false;
            }
            if (horde.pawn >= 1 || horde.queen >= 1) && horde_num >= 2 {
                // Pawns/queens are never insufficient material when paired with any other
                // piece (a pawn promotes to a queen and delivers mate).
                return false;
            }
            if horde.rook >= 1 && horde_num >= 2 {
                // A rook is insufficient material only when it is paired with a bishop
                // against a lone king. The horde can mate in any other case.
                // A rook on A1 and a bishop on C3 mate a king on B1 when there is a
                // friendly pawn/opposite-color-bishop/rook/queen on C2.
                // A rook on B8 and a bishop C3 mate a king on A1 when there is a friendly
                // knight on A2.
                if !(horde_num == 2
                    && horde.rook == 1
                    && horde.bishop == 1
                    && pieces_of_type_not(pieces_bishops(horde_bishop_color)) == 1)
                {
                    return false;
                }
            }

            if horde_num == 1 {
                if pieces_num == 1 {
                    // A lone piece cannot mate a lone king.
                    return true;
                } else if horde.queen == 1 {
                    // The horde has a lone queen.
                    // A lone queen mates a king on A1 bounded by:
                    //  -- a pawn/rook on A2
                    //  -- two same color bishops on A2, B1
                    // We ignore every other mating case, since it can be reduced to
                    // the two previous cases (e.g. a black pawn on A2 and a black
                    // bishop on B1).
                    return !(pieces.pawn >= 1
                        || pieces.rook >= 1
                        || pieces_bishops(SquareColor::Light) >= 2
                        || pieces_bishops(SquareColor::Dark) >= 2);
                } else if horde.pawn == 1 {
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
                } else if horde.rook == 1 {
                    // A lone rook mates a king on A8 bounded by a pawn/rook on A7 and a
                    // pawn/knight on B7. We ignore every other case, since it can be
                    // reduced to the two previous cases.
                    // (e.g. three pawns on A7, B7, C7)
                    return !(pieces.pawn >= 2
                        || (pieces.rook >= 1 && pieces.pawn >= 1)
                        || (pieces.rook >= 1 && pieces.knight >= 1)
                        || (pieces.pawn >= 1 && pieces.knight >= 1));
                } else if horde.bishop == 1 {
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
                        pieces_bishops(!horde_bishop_color) >= 2
                            || (pieces_bishops(!horde_bishop_color) >= 1 && pieces.pawn >= 1)
                            || pieces.pawn >= 2
                    );
                } else if horde.knight == 1 {
                    // The horde has a lone knight.
                    return !(
                        // The king on A1 can be smother mated by a knight on C2 if there is
                        // a pawn/knight/bishop on B2, a knight/rook on B1 and any other piece
                        // on A2.
                        // Moreover, when black has four or more pieces and two of them are
                        // pawns, black can promote their pawns and selfmate theirself.
                        pieces_num >= 4
                            && (pieces.knight >= 2
                                || pieces.pawn >= 2
                                || (pieces.rook >= 1 && pieces.knight >= 1)
                                || (pieces.rook >= 1 && pieces.bishop >= 1)
                                || (pieces.knight >= 1 && pieces.bishop >= 1)
                                || (pieces.rook >= 1 && pieces.pawn >= 1)
                                || (pieces.knight >= 1 && pieces.pawn >= 1)
                                || (pieces.bishop >= 1 && pieces.pawn >= 1)
                                || (has_bishop_pair(!color) && pieces.pawn >= 1))
                            && (pieces_bishops(SquareColor::Dark) < 2
                                || pieces_of_type_not(pieces_bishops(SquareColor::Dark)) >= 3)
                            && (pieces_bishops(SquareColor::Light) < 2
                                || pieces_of_type_not(pieces_bishops(SquareColor::Light)) >= 3)
                    );
                }

            // By this point, we only need to deal with white's minor pieces.
            } else if horde_num == 2 {
                if pieces_num == 1 {
                    // Two minor pieces cannot mate a lone king.
                    return true;
                } else if horde.knight == 2 {
                    // A king on A1 is mated by two knights, if it is obstructed by a
                    // pawn/bishop/knight on B2. On the other hand, if black only has
                    // major pieces it is a draw.
                    return pieces.pawn + pieces.bishop + pieces.knight < 1;
                } else if has_bishop_pair(color) {
                    return !(
                        // A king on A1 obstructed by a pawn/bishop on A2 is mated
                        // by the bishop pair.
                        pieces.pawn >= 1 || pieces.bishop >= 1 ||
                            // A pawn/bishop/knight on B4, a pawn/bishop/rook/queen on
                            // A4 and the king on A3 enable Boden's mate by the bishop
                            // pair. In every other case white cannot win.
                            ( pieces.knight >= 1 && pieces.rook + pieces.queen >= 1 )
                    );
                } else if horde.bishop >= 1 && horde.knight >= 1 {
                    // The horde has a bishop and a knight.
                    return !(
                        // A king on A1 obstructed by a pawn/opposite-color-bishop on
                        // A2 is mated by a knight on D2 and a bishop on C3.
                        pieces.pawn >= 1 || pieces_bishops(!horde_bishop_color) >= 1 ||
                            // A king on A1 bounded by two friendly pieces on A2 and B1 is
                            // mated when the knight moves from D4 to C2 so that both the
                            // knight and the bishop deliver check.
                            pieces_of_type_not( pieces_bishops(horde_bishop_color) ) >=3
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
                        (pieces.pawn >= 1 && pieces_bishops(!horde_bishop_color) >= 1)
                            || (pieces.pawn >= 1 && pieces.knight >= 1)
                            || (pieces_bishops(!horde_bishop_color) >= 1 && pieces.knight >= 1)
                            || (pieces_bishops(!horde_bishop_color) >= 2)
                            || pieces.knight >= 2
                            || pieces.pawn >= 2
                        // In every other case, white can only draw.
                    );
                }
            } else if horde_num == 3 {
                // A king in the corner is mated by two knights and a bishop or three
                // knights or the bishop pair and a knight/bishop.
                if (horde.knight == 2 && horde.bishop == 1)
                    || horde.knight == 3
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

#[allow(clippy::too_many_arguments)] // But typesafe
fn do_move(
    board: &mut Board,
    promoted: &mut Bitboard,
    turn: &mut Color,
    castles: &mut Castles,
    ep_square: &mut Option<EnPassant>,
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
                *ep_square = from.offset(8).map(EnPassant);
            } else if role == Role::Pawn && from - to == 16 && from.rank() == Rank::Seventh {
                *ep_square = from.offset(-8).map(EnPassant);
            }

            if role == Role::King {
                castles.discard_color(color);
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
            castles.discard_color(color);
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

fn validate<P: Position>(pos: &P, ep_square: Option<EnPassant>) -> PositionErrorKinds {
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
            errors |= PositionErrorKinds::TOO_MUCH_MATERIAL;
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

    let checkers = pos.checkers();
    if let (Some(a), Some(b), Some(our_king)) = (
        checkers.first(),
        checkers.last(),
        pos.board().king_of(pos.turn()),
    ) {
        if let Some(ep_square) = ep_square {
            // The pushed pawn must be the only checker, or it has uncovered
            // check by a single sliding piece.
            if a != b
                || (a != ep_square.pawn_pushed_to()
                    && pos
                        .king_attackers(
                            our_king,
                            !pos.turn(),
                            pos.board()
                                .occupied()
                                .without(ep_square.pawn_pushed_to())
                                .with(ep_square.pawn_pushed_from()),
                        )
                        .any())
            {
                errors |= PositionErrorKinds::IMPOSSIBLE_CHECK;
            }
        } else {
            // There can be at most two checkers, and discovered checkers
            // cannot be aligned.
            if a != b && (checkers.count() > 2 || attacks::aligned(a, our_king, b)) {
                errors |= PositionErrorKinds::IMPOSSIBLE_CHECK;
            }
        }
    }

    errors
}

const fn is_standard_material(board: &Board, color: Color) -> bool {
    let our = board.by_color(color);
    let promoted_pieces = board.queens().intersect(our).count().saturating_sub(1)
        + board.rooks().intersect(our).count().saturating_sub(2)
        + board.knights().intersect(our).count().saturating_sub(2)
        + board
            .bishops()
            .intersect(our)
            .intersect(Bitboard::LIGHT_SQUARES)
            .count()
            .saturating_sub(1)
        + board
            .bishops()
            .intersect(our)
            .intersect(Bitboard::DARK_SQUARES)
            .count()
            .saturating_sub(1);
    board.pawns().intersect(our).count() + promoted_pieces <= 8
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
    let single_moves =
        pos.our(Role::Pawn).shift(pos.turn().fold_wb(8, -8)) & !pos.board().occupied();

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
    let double_moves = single_moves.shift(pos.turn().fold_wb(8, -8))
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

fn gen_en_passant(
    board: &Board,
    turn: Color,
    ep_square: Option<EnPassant>,
    moves: &mut MoveList,
) -> bool {
    let mut found = false;

    if let Some(EnPassant(to)) = ep_square {
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
            let capture = Square::from_coords(to.file(), from.rank());
            pos.board()
                .attacks_to(
                    king,
                    !pos.turn(),
                    pos.board()
                        .occupied()
                        .toggled(from)
                        .toggled(capture)
                        .with(to),
                )
                .without(capture)
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
    use crate::fen::Fen;

    #[cfg(feature = "alloc")]
    struct _AssertObjectSafe(alloc::boxed::Box<dyn Position>);

    fn setup_fen<T: Position + FromSetup>(fen: &str) -> T {
        fen.parse::<Fen>()
            .expect("valid fen")
            .into_position::<T>(CastlingMode::Chess960)
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

        assert_eq!(
            pos.has_insufficient_material(White),
            white,
            "expected white {}",
            if white { "cannot win" } else { "can win " }
        );
        assert_eq!(
            pos.has_insufficient_material(Black),
            black,
            "expected black {}",
            if black { "cannot win" } else { "can win" }
        );
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
    fn test_outcome() {
        for (fen, outcome) in [
            (
                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
                None,
            ),
            ("2k5/8/8/8/8/8/8/3KB3 w - - 0 1", Some(Outcome::Draw)),
            ("8/8/8/8/8/Q1K5/8/1k6 b - - 0 1", Some(Outcome::Draw)),
            (
                "8/8/8/8/8/Q7/2K5/k7 b - - 0 1",
                Some(Outcome::Decisive { winner: White }),
            ),
        ] {
            let pos: Chess = setup_fen(fen);
            assert_eq!(pos.outcome(), outcome);
        }
    }

    #[test]
    #[cfg(feature = "std")]
    fn test_eq() {
        fn hash<T: Hash>(value: &T) -> u64 {
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            value.hash(&mut hasher);
            hasher.finish()
        }

        assert_eq!(Chess::default(), Chess::default());
        assert_eq!(hash(&Chess::default()), hash(&Chess::default()));
        assert_ne!(
            Chess::default(),
            Chess::default().swap_turn().expect("swap turn legal")
        );

        // Check that castling paths do not interfere.
        let pos: Chess = setup_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBK1BNR w KQkq - 0 1");
        let pos_after_move_played = pos
            .play(&Move::Normal {
                role: Role::King,
                from: Square::D1,
                to: Square::E1,
                capture: None,
                promotion: None,
            })
            .expect("Ke1 is legal");
        let pos_after_move =
            setup_fen::<Chess>("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNB1KBNR b kq - 1 1");
        assert_eq!(pos_after_move, pos_after_move_played);
        assert_eq!(hash(&pos_after_move), hash(&pos_after_move_played));

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
        assert_eq!(hash(&pos_after_queen_promotion), hash(&final_pos));
        assert_ne!(pos_after_knight_promotion, final_pos);

        // Check that irrelevant en passant is treated as such.
        let pos: Chess = setup_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1");
        let pos_with_irrelevant_ep: Chess =
            setup_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1");
        assert_eq!(pos, pos_with_irrelevant_ep);
        assert_eq!(hash(&pos), hash(&pos_with_irrelevant_ep));
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
        assert_insufficient_material::<Antichess>(
            "8/8/6b1/8/3P4/8/5B2/8 w - - 0 1",
            false_negative,
            false,
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
        assert_insufficient_material::<Horde>("8/8/8/7k/7P/7P/8/8 b - - 0 58", false, false);
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
            pos.castles().castling_rights(),
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

    #[cfg(feature = "variant")]
    #[test]
    fn test_antichess_insufficient_material() {
        use super::variant::Antichess;

        for (fen, possible_winner) in [
            // https://lichess.org/aWVWduVV
            ("8/8/8/1n2N3/8/8/8/8 w - - 0 32", Color::Black),
            ("8/3N4/8/1n6/8/8/8/8 b - - 1 32", Color::Black),
            // https://lichess.org/EzRIcUxc
            ("8/8/8/8/2N5/8/8/n7 w - - 0 30", Color::Black),
            ("8/8/8/4N3/8/8/8/n7 b - - 1 30", Color::Black),
            ("8/8/8/4N3/8/8/2n5/8 w - - 2 31", Color::Black),
            ("8/8/6N1/8/8/8/2n5/8 b - - 3 31", Color::Black),
            ("8/8/6N1/8/8/4n3/8/8 w - - 4 32", Color::Black),
            ("5N2/8/8/8/8/4n3/8/8 b - - 5 32", Color::Black),
            ("5N2/8/8/5n2/8/8/8/8 w - - 6 33", Color::Black),
            // https://lichess.org/te3cf1wG
            ("6n1/8/8/4N3/8/8/8/8 b - - 0 27", Color::White),
            ("8/8/5n2/4N3/8/8/8/8 w - - 1 28", Color::White),
            ("8/3N4/5n2/8/8/8/8/8 b - - 2 28", Color::White),
            ("8/3n4/8/8/8/8/8/8 w - - 0 29", Color::White),
        ] {
            let pos = fen
                .parse::<Fen>()
                .expect("valid fen")
                .into_position::<Antichess>(CastlingMode::Standard)
                .expect("legal position");
            assert!(
                !pos.has_insufficient_material(possible_winner),
                "{possible_winner} can win {fen}"
            );
            assert!(
                pos.has_insufficient_material(!possible_winner),
                "{possible_winner} can not win {fen}"
            );
        }
    }

    #[test]
    fn test_aligned_checkers() {
        let res = "2Nq4/2K5/1b6/8/7R/3k4/7P/8 w - - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position::<Chess>(CastlingMode::Chess960);
        assert_eq!(
            res.expect_err("impossible check").kinds(),
            PositionErrorKinds::IMPOSSIBLE_CHECK
        );

        let _ = "8/8/5k2/p1q5/PP1rp1P1/3P1N2/2RK1r2/5nN1 w - - 0 3"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position::<Chess>(CastlingMode::Standard)
            .expect("checkers aligned with opponent king not relevant");

        let res = "8/8/8/1k6/3Pp3/8/8/4KQ2 b - d3 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position::<Chess>(CastlingMode::Standard);
        assert_eq!(
            res.expect_err("impossible check due to ep square").kinds(),
            PositionErrorKinds::IMPOSSIBLE_CHECK
        );
    }

    #[cfg(feature = "alloc")]
    #[test]
    fn test_swap_turn() {
        use alloc::string::ToString as _;
        let pos: Chess = "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position(CastlingMode::Chess960)
            .expect("valid position");
        let swapped = pos.swap_turn().expect("swap turn");
        assert_eq!(
            Fen(swapped.into_setup(EnPassantMode::Always)).to_string(),
            "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3"
        );
    }

    #[test]
    fn test_invalid_ep_square() {
        let fen: Fen = "4k3/8/8/8/8/8/8/4K3 w - e3 0 1".parse().expect("valid fen");
        let err = fen
            .into_position::<Chess>(CastlingMode::Standard)
            .expect_err("invalid ep square");
        assert_eq!(err.kinds(), PositionErrorKinds::INVALID_EP_SQUARE);
        assert_eq!(
            err.ignore_invalid_ep_square()
                .expect("now valid")
                .maybe_ep_square(),
            None
        );
    }

    #[test]
    fn test_check_with_unrelated_ep_square() {
        let fen: Fen = "rnbqk1nr/bb3p1p/1q2r3/2pPp3/3P4/7P/1PP1NpPP/R1BQKBNR w KQkq c6 0 1"
            .parse()
            .expect("valid fen");
        let pos = fen
            .into_position::<Chess>(CastlingMode::Standard)
            .expect_err("impossible check")
            .ignore_impossible_check()
            .expect("legal otherwise");
        assert!(pos.san_candidates(Role::Pawn, Square::C6).is_empty());
        assert!(pos.en_passant_moves().is_empty());
        assert_eq!(pos.legal_moves().len(), 2);
    }
}
