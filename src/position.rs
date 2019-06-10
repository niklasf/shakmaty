// This file is part of the shakmaty library.
// Copyright (C) 2017-2019 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use crate::attacks;
use crate::board::Board;
use crate::bitboard::Bitboard;
use crate::square::{Rank, Square};
use crate::types::{Black, CastlingSide, Color, Move, Piece, RemainingChecks, Role, White};
use crate::material::{Material, MaterialSide};
use crate::setup::{Castles, Setup, SwapTurn, EMPTY_CASTLES};
use crate::movelist::{ArrayVecExt, MoveList};

use bitflags::bitflags;

use std::fmt;
use std::error::Error;

/// Outcome of a game.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Outcome {
    Decisive { winner: Color },
    Draw,
}

impl Outcome {
    pub fn winner(self) -> Option<Color> {
        match self {
            Outcome::Decisive { winner } => Some(winner),
            Outcome::Draw => None,
        }
    }
}

impl fmt::Display for Outcome {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match *self {
            Outcome::Decisive { winner: White } => "1-0",
            Outcome::Decisive { winner: Black } => "0-1",
            Outcome::Draw => "1/2-1/2",
        })
    }
}

bitflags! {
    /// Reasons for a [`Setup`] not beeing a legal [`Position`].
    ///
    /// [`Setup`]: trait.Setup.html
    /// [`Position`]: trait.Position.html
    pub struct PositionError: u32 {
        const EMPTY_BOARD = 1;
        const MISSING_KING = 2;
        const TOO_MANY_KINGS = 4;
        const PAWNS_ON_BACKRANK = 8;
        const BAD_CASTLING_RIGHTS = 16;
        const INVALID_EP_SQUARE = 32;
        const OPPOSITE_CHECK = 64;
        const VARIANT = 128;
    }
}

impl fmt::Display for PositionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "illegal position".fmt(f)
    }
}

impl Error for PositionError {
    fn description(&self) -> &str {
        "illegal position"
    }
}

impl PositionError {
    fn into_result<T>(self, ok: T) -> Result<T, PositionError> {
        if self.is_empty() {
            Ok(ok)
        } else {
            Err(self)
        }
    }
}

/// Error in case of illegal moves.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IllegalMoveError;

impl fmt::Display for IllegalMoveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "illegal move".fmt(f)
    }
}

impl Error for IllegalMoveError {
    fn description(&self) -> &str {
        "illegal move"
    }
}

impl From<()> for IllegalMoveError {
    fn from(_: ()) -> IllegalMoveError {
        IllegalMoveError
    }
}

/// A legal chess or chess variant position. See [`Chess`] for a concrete
/// implementation.
///
/// [`Chess`]: struct.Chess.html
pub trait Position: Setup {
    /// Set up a position.
    ///
    /// # Errors
    ///
    /// Returns [`PositionError`] if the setup is not legal.
    ///
    /// [`PositionError`]: enum.PositionError.html
    fn from_setup<S: Setup>(setup: &S) -> Result<Self, PositionError>
    where
        Self: Sized;

    /// Swap turns. This is sometimes called "playing a null move".
    ///
    /// # Errors
    ///
    /// Returns [`PositionError`] if swapping turns is not possible (usually
    /// due to a check that has to be averted).
    ///
    /// [`PositionError`]: enum.PositionError.html
    fn swap_turn(self) -> Result<Self, PositionError>
    where
        Self: Sized,
    {
        Self::from_setup(&SwapTurn(self))
    }

    /// Generates legal moves.
    fn legals(&self) -> MoveList {
        let mut legals = MoveList::new();
        self.legal_moves(&mut legals);
        legals
    }

    /// Collects all legal moves in an existing buffer.
    fn legal_moves(&self, moves: &mut MoveList);

    /// Generates a subset of legal moves: All piece moves and drops of type
    /// `role` to the square `to`, excluding castling moves.
    fn san_candidates(&self, role: Role, to: Square, moves: &mut MoveList) {
        self.legal_moves(moves);
        filter_san_candidates(role, to, moves);
    }

    /// Generates legal castling moves.
    fn castling_moves(&self, side: CastlingSide, moves: &mut MoveList) {
        self.legal_moves(moves);
        moves.retain(|m| m.castling_side().map_or(false, |s| side == s));
    }

    /// Generates en passant moves.
    fn en_passant_moves(&self, moves: &mut MoveList) {
        self.legal_moves(moves);
        moves.retain(|m| m.is_en_passant());
    }

    /// Generates capture moves.
    fn capture_moves(&self, moves: &mut MoveList) {
        self.legal_moves(moves);
        moves.retain(|m| m.is_capture());
    }

    /// Generate promotion moves.
    fn promotion_moves(&self, moves: &mut MoveList) {
        self.legal_moves(moves);
        moves.retain(|m| m.is_promotion());
    }

    /// Tests a move for legality.
    fn is_legal(&self, m: &Move) -> bool {
        let mut moves = MoveList::new();
        match *m {
            Move::Normal { role, to, .. } | Move::Put { role, to } =>
                self.san_candidates(role, to, &mut moves),
            Move::EnPassant { to, .. } =>
                self.san_candidates(Role::Pawn, to, &mut moves),
            Move::Castle { king, rook } if king.file() < rook.file() =>
                self.castling_moves(CastlingSide::KingSide, &mut moves),
            Move::Castle { .. } =>
                self.castling_moves(CastlingSide::QueenSide, &mut moves),
        }
        moves.contains(m)
    }

    /// Tests if a move is irreversible.
    ///
    /// In standard chess pawn moves, captures and moves that destroy castling
    /// rights are irreversible.
    fn is_irreversible(&self, m: &Move) -> bool {
        match *m {
            Move::Normal { role: Role::Pawn, .. } |
                Move::Normal { capture: Some(_), .. } |
                Move::Castle { .. } |
                Move::EnPassant { .. } |
                Move::Put { .. } => true,
            Move::Normal { role, from, to, .. } =>
                self.castling_rights().contains(from) ||
                self.castling_rights().contains(to) ||
                (role == Role::King && self.castles().has_side(self.turn()))
        }
    }

    /// Attacks that a king on `square` would have to deal with.
    fn king_attackers(&self, square: Square, attacker: Color, occupied: Bitboard) -> Bitboard {
        self.board().attacks_to(square, attacker, occupied)
    }

    /// Castling paths and unmoved rooks.
    fn castles(&self) -> &Castles;

    /// Tests if the king is in check.
    fn is_check(&self) -> bool {
        self.checkers().any()
    }

    /// Bitboard of pieces giving check.
    fn checkers(&self) -> Bitboard {
        self.our(Role::King).first().map_or(Bitboard(0), |king| {
            self.king_attackers(king, !self.turn(), self.board().occupied())
        })
    }

    /// Checks if the game is over due to a special variant end condition.
    ///
    /// Note that for example stalemate is not considered a variant-specific
    /// end condition (`is_variant_end()` will return `false`), but it can have
    /// a special [`variant_outcome()`](#tymethod.variant_outcome) in suicide
    /// chess.
    fn is_variant_end(&self) -> bool;

    /// Tests for checkmate.
    fn is_checkmate(&self) -> bool {
        if self.checkers().is_empty() {
            return false;
        }

        let mut legals = MoveList::new();
        self.legal_moves(&mut legals);
        legals.is_empty()
    }

    /// Tests for stalemate.
    fn is_stalemate(&self) -> bool {
        if !self.checkers().is_empty() || self.is_variant_end() {
            false
        } else {
            let mut legals = MoveList::new();
            self.legal_moves(&mut legals);
            legals.is_empty()
        }
    }

    /// Tests if both sides
    /// [have insufficient winning material](#tymethod.has_insufficient_material).
    fn is_insufficient_material(&self) -> bool {
        self.has_insufficient_material(White) && self.has_insufficient_material(Black)
    }

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

    /// Tests if the game is over due to [checkmate](#method.is_checkmate),
    /// [stalemate](#method.is_stalemate),
    /// [insufficient material](#tymethod.is_insufficient_material) or
    /// [variant end](#tymethod.is_variant_end).
    fn is_game_over(&self) -> bool {
        let mut legals = MoveList::new();
        self.legal_moves(&mut legals);
        legals.is_empty() || self.is_insufficient_material()
    }

    /// Tests special variant winning, losing and drawing conditions.
    fn variant_outcome(&self) -> Option<Outcome>;

    /// The outcome of the game, or `None` if the game is not over.
    fn outcome(&self) -> Option<Outcome> {
        self.variant_outcome().or_else(|| {
            if self.is_checkmate() {
                Some(Outcome::Decisive { winner: !self.turn() })
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
    /// Returns [`IllegalMoveError`] if the move is not legal in the position.
    ///
    /// [`IllegalMoveError`]: struct.IllegalMoveError.html
    fn play(mut self, m: &Move) -> Result<Self, IllegalMoveError>
    where
        Self: Sized,
    {
        if self.is_legal(m) {
            self.play_unchecked(m);
            Ok(self)
        } else {
            Err(IllegalMoveError)
        }
    }

    /// Plays a move. It is the callers responsibility to ensure the move is
    /// legal.
    ///
    /// # Panics
    ///
    /// Illegal moves can corrupt the state of the position and may
    /// (or may not) panic or cause panics on future calls. Consider using
    /// [`Position::play()`](trait.Position.html#method.play) instead.
    fn play_unchecked(&mut self, m: &Move);
}

trait CastlingUncoversRankAttack {
    /// Tests the rare case where moving the rook to the other side during
    /// castling would uncover a rank attack.
    fn castling_uncovers_rank_attack(&self, rook: Square, king_to: Square) -> bool;
}

/// A standard Chess position.
#[derive(Clone, Debug)]
pub struct Chess {
    board: Board,
    turn: Color,
    castles: Castles,
    ep_square: Option<Square>,
    halfmoves: u32,
    fullmoves: u32,
}

impl Chess {
    fn gives_check(&self, m: &Move) -> bool {
        let mut pos = self.clone();
        pos.play_unchecked(m);
        pos.is_check()
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
            fullmoves: 1,
        }
    }
}

impl Setup for Chess {
    fn board(&self) -> &Board { &self.board }
    fn pockets(&self) -> Option<&Material> { None }
    fn turn(&self) -> Color { self.turn }
    fn castling_rights(&self) -> Bitboard { self.castles.castling_rights() }
    fn ep_square(&self) -> Option<Square> { self.ep_square.filter(|_| has_relevant_ep(self)) }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }
    fn halfmoves(&self) -> u32 { self.halfmoves }
    fn fullmoves(&self) -> u32 { self.fullmoves }
}

impl Position for Chess {
    fn play_unchecked(&mut self, m: &Move) {
        do_move(&mut self.board, &mut self.turn, &mut self.castles,
                &mut self.ep_square, &mut self.halfmoves,
                &mut self.fullmoves, m);
    }

    fn from_setup<S: Setup>(setup: &S) -> Result<Chess, PositionError> {
        let (castles, errors) = match Castles::from_setup(setup) {
            Ok(castles) => (castles, PositionError::empty()),
            Err(castles) => (castles, PositionError::BAD_CASTLING_RIGHTS),
        };

        let pos = Chess {
            board: setup.board().clone(),
            turn: setup.turn(),
            castles,
            ep_square: setup.ep_square(),
            halfmoves: setup.halfmoves(),
            fullmoves: setup.fullmoves(),
        };

        (validate(&pos) | errors).into_result(pos)
    }

    fn castles(&self) -> &Castles {
        &self.castles
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        moves.clear();

        let king = self.board().king_of(self.turn()).expect("king in standard chess");

        let has_ep = gen_en_passant(self.board(), self.turn(), self.ep_square, moves);

        let checkers = self.checkers();
        if checkers.is_empty() {
            let target = !self.us();
            gen_non_king(self, target, moves);
            gen_safe_king(self, king, target, moves);
            gen_castling_moves(self, &self.castles, king, CastlingSide::KingSide, moves);
            gen_castling_moves(self, &self.castles, king, CastlingSide::QueenSide, moves);
        } else {
            evasions(self, king, checkers, moves);
        }

        let blockers = slider_blockers(self.board(), self.them(), king);
        if blockers.any() || has_ep {
            moves.swap_retain(|m| is_safe(self, king, m, blockers));
        }
    }

    fn castling_moves(&self, side: CastlingSide, moves: &mut MoveList) {
        moves.clear();
        let king = self.board().king_of(self.turn()).expect("king in standard chess");
        gen_castling_moves(self, &self.castles, king, side, moves);
    }

    fn en_passant_moves(&self, moves: &mut MoveList) {
        moves.clear();

        if gen_en_passant(self.board(), self.turn(), self.ep_square, moves) {
            let king = self.board().king_of(self.turn()).expect("king in standard chess");
            let blockers = slider_blockers(self.board(), self.them(), king);
            moves.swap_retain(|m| is_safe(self, king, m, blockers));
        }
    }

    fn promotion_moves(&self, moves: &mut MoveList) {
        moves.clear();

        let king = self.board().king_of(self.turn()).expect("king in standard chess");
        let checkers = self.checkers();

        if checkers.is_empty() {
            gen_pawn_moves(self, Bitboard::BACKRANKS, moves);
        } else {
            evasions(self, king, checkers, moves);
            moves.retain(|m| m.is_promotion());
        }

        let blockers = slider_blockers(self.board(), self.them(), king);
        if blockers.any() {
            moves.swap_retain(|m| is_safe(self, king, m, blockers));
        }
    }

    fn san_candidates(&self, role: Role, to: Square, moves: &mut MoveList) {
        moves.clear();

        let king = self.board().king_of(self.turn()).expect("king in standard chess");
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
                    Role::Pawn => gen_pawn_moves(self, Bitboard::from_square(to), moves),
                    Role::King => gen_safe_king(self, king, Bitboard::from_square(to), moves),
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
            evasions(self, king, checkers, moves);
            filter_san_candidates(role, to, moves);
        }

        let has_ep =
            role == Role::Pawn &&
            Some(to) == self.ep_square &&
            gen_en_passant(self.board(), self.turn(), self.ep_square, moves);

        let blockers = slider_blockers(self.board(), self.them(), king);
        if blockers.any() || has_ep {
            moves.swap_retain(|m| is_safe(self, king, m, blockers));
        }
    }

    fn has_insufficient_material(&self, color: Color) -> bool {
        // Pawns, rooks and queens are never insufficient material.
        if (self.board.by_color(color) & (self.board.pawns() | self.board.rooks_and_queens())).any() {
            return false;
        }

        // Knights are only insufficient material if:
        // (1) We do not have any other pieces, including more than one knight.
        // (2) The opponent does not have pawns, knights, bishops or rooks.
        //     These would allow self mate.
        if (self.board.by_color(color) & self.board.knights()).any() {
            return self.board.by_color(color).count() <= 2 &&
                (self.board.by_color(!color) & !self.board.kings() & !self.board().queens()).is_empty();
        }

        // Bishops are only insufficient material if:
        // (1) We do not have any other pieces, including bishops on the
        //     opposite color.
        // (2) The opponent does not have bishops on the opposite color,
        //      pawns or knights. These would allow self mate.
        if (self.board.by_color(color) & self.board.bishops()).any() {
            let same_color =
                (self.board().bishops() & Bitboard::DARK_SQUARES).is_empty() ||
                (self.board().bishops() & Bitboard::LIGHT_SQUARES).is_empty();
            return same_color &&
                (self.board.by_color(!color) & !self.board.kings() & !self.board().rooks_and_queens()).is_empty()
        }

        true
    }

    fn is_variant_end(&self) -> bool { false }
    fn variant_outcome(&self) -> Option<Outcome> { None }
}

impl CastlingUncoversRankAttack for Chess {
    fn castling_uncovers_rank_attack(&self, rook: Square, king_to: Square) -> bool {
        self.castles.is_chess960() &&
        castling_uncovers_rank_attack(self, rook, king_to)
    }
}

/// An Atomic Chess position.
#[derive(Clone, Debug)]
pub struct Atomic {
    board: Board,
    turn: Color,
    castles: Castles,
    ep_square: Option<Square>,
    halfmoves: u32,
    fullmoves: u32,
}

impl Default for Atomic {
    fn default() -> Atomic {
        Atomic {
            board: Board::default(),
            turn: White,
            castles: Castles::default(),
            ep_square: None,
            halfmoves: 0,
            fullmoves: 1,
        }
    }
}

impl Setup for Atomic {
    fn board(&self) -> &Board { &self.board }
    fn pockets(&self) -> Option<&Material> { None }
    fn turn(&self) -> Color { self.turn }
    fn castling_rights(&self) -> Bitboard { self.castles.castling_rights() }
    fn ep_square(&self) -> Option<Square> { self.ep_square.filter(|_| has_relevant_ep(self)) }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }
    fn halfmoves(&self) -> u32 { self.halfmoves }
    fn fullmoves(&self) -> u32 { self.fullmoves }
}

impl Position for Atomic {
    fn from_setup<S: Setup>(setup: &S) -> Result<Atomic, PositionError> {
        let (castles, errors) = match Castles::from_setup(setup) {
            Ok(castles) => (castles, PositionError::empty()),
            Err(castles) => (castles, PositionError::BAD_CASTLING_RIGHTS),
        };

        let pos = Atomic {
            board: setup.board().clone(),
            turn: setup.turn(),
            castles,
            ep_square: setup.ep_square(),
            halfmoves: setup.halfmoves(),
            fullmoves: setup.fullmoves(),
        };

        let mut errors = validate(&pos) | errors;

        if (pos.them() & pos.board().kings()).any() {
            // Our king just exploded. Game over, but valid position.
            errors.remove(PositionError::MISSING_KING);
        }

        errors.into_result(pos)
    }

    fn castles(&self) -> &Castles {
        &self.castles
    }

    fn play_unchecked(&mut self, m: &Move) {
        do_move(&mut self.board, &mut self.turn, &mut self.castles,
                &mut self.ep_square, &mut self.halfmoves,
                &mut self.fullmoves, m);

        match *m {
            Move::Normal { capture: Some(_), to, .. } | Move::EnPassant { to, .. } => {
                self.board.remove_piece_at(to);

                let explosion_radius = attacks::king_attacks(to) &
                                       self.board().occupied() &
                                       !self.board.pawns();

                for explosion in explosion_radius {
                    self.board.remove_piece_at(explosion);
                    self.castles.discard_rook(explosion);
                }
            },
            _ => ()
        }
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        moves.clear();

        gen_en_passant(self.board(), self.turn(), self.ep_square, moves);
        gen_non_king(self, !self.us(), moves);
        KingTag::gen_moves(self, !self.board().occupied(), moves);
        if let Some(king) = self.board().king_of(self.turn()) {
            gen_castling_moves(self, &self.castles, king, CastlingSide::KingSide, moves);
            gen_castling_moves(self, &self.castles, king, CastlingSide::QueenSide, moves);
        }

        // Atomic move generation could be implemented more efficiently.
        // For simplicity we filter all pseudo legal moves.
        moves.swap_retain(|m| {
            let mut after = self.clone();
            after.play_unchecked(m);
            if let Some(our_king) = after.board().king_of(self.turn()) {
                (after.board.kings() & after.board().by_color(!self.turn())).is_empty() ||
                after.king_attackers(our_king, !self.turn(), after.board.occupied()).is_empty()
            } else {
                false
            }
        });
    }

    fn king_attackers(&self, square: Square, attacker: Color, occupied: Bitboard) -> Bitboard {
        if (attacks::king_attacks(square) & self.board().kings() & self.board().by_color(attacker)).any() {
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
                if (self.board().bishops() & self.board().white() & Bitboard::DARK_SQUARES).is_empty() {
                    return (self.board().bishops() & self.board().black() & Bitboard::LIGHT_SQUARES).is_empty();
                }
                if (self.board().bishops() & self.board().white() & Bitboard::LIGHT_SQUARES).is_empty() {
                    return (self.board().bishops() & self.board().black() & Bitboard::DARK_SQUARES).is_empty();
                }
            }

            return false;
        }

        // Queen or pawn (future queen) can give mate against bare king.
        if self.board().queens().any() || self.board.pawns().any() {
            return false;
        }

        // Single knight, bishop or rook can not mate against bare king.
        if (self.board().knights() | self.board().bishops() | self.board().rooks()).count() == 1 {
            return true;
        }

        // Two knights can not mate against bare king.
        if self.board().occupied() == self.board().kings() | self.board().knights() {
            return self.board().knights().count() <= 2;
        }

        false
    }

    fn variant_outcome(&self) -> Option<Outcome> {
        for &color in &[White, Black] {
            if (self.board().by_color(color) & self.board().kings()).is_empty() {
                return Some(Outcome::Decisive { winner: !color });
            }
        }
        None
    }
}

impl CastlingUncoversRankAttack for Atomic {
    fn castling_uncovers_rank_attack(&self, rook: Square, king_to: Square) -> bool {
        (attacks::king_attacks(king_to) & self.board().kings() & self.them()).is_empty() &&
        castling_uncovers_rank_attack(self, rook, king_to)
    }
}

/// A Giveaway position. Giveaway is also (somewhat ambiguously) known as
/// Antichess.
#[derive(Clone, Debug)]
pub struct Giveaway {
    board: Board,
    turn: Color,
    castles: Castles,
    ep_square: Option<Square>,
    halfmoves: u32,
    fullmoves: u32,
}

impl Default for Giveaway {
    fn default() -> Giveaway {
        Giveaway {
            board: Board::default(),
            turn: White,
            castles: Castles::empty(),
            ep_square: None,
            halfmoves: 0,
            fullmoves: 1,
        }
    }
}

impl Setup for Giveaway {
    fn board(&self) -> &Board { &self.board }
    fn pockets(&self) -> Option<&Material> { None }
    fn turn(&self) -> Color { self.turn }
    fn castling_rights(&self) -> Bitboard { self.castles.castling_rights() }
    fn ep_square(&self) -> Option<Square> { self.ep_square.filter(|_| has_relevant_ep(self)) }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }
    fn halfmoves(&self) -> u32 { self.halfmoves }
    fn fullmoves(&self) -> u32 { self.fullmoves }
}

impl Position for Giveaway {
    fn play_unchecked(&mut self, m: &Move) {
        do_move(&mut self.board, &mut self.turn, &mut self.castles,
                &mut self.ep_square, &mut self.halfmoves,
                &mut self.fullmoves, m);
    }

    fn from_setup<S: Setup>(setup: &S) -> Result<Giveaway, PositionError> {
        let (castles, errors) = match Castles::from_setup(setup) {
            Ok(castles) => (castles, PositionError::empty()),
            Err(castles) => (castles, PositionError::BAD_CASTLING_RIGHTS),
        };

        let pos = Giveaway {
            board: setup.board().clone(),
            turn: setup.turn(),
            castles,
            ep_square: setup.ep_square(),
            halfmoves: setup.halfmoves(),
            fullmoves: setup.fullmoves(),
        };

        let errors = (validate(&pos) | errors)
            - PositionError::MISSING_KING
            - PositionError::TOO_MANY_KINGS
            - PositionError::OPPOSITE_CHECK;

        errors.into_result(pos)
    }

    fn castles(&self) -> &Castles {
        &self.castles
    }

    fn en_passant_moves(&self, moves: &mut MoveList) {
        moves.clear();
        gen_en_passant(self.board(), self.turn, self.ep_square, moves);
    }

    fn capture_moves(&self, moves: &mut MoveList) {
        self.en_passant_moves(moves); // clears move list
        let them = self.them();
        gen_non_king(self, them, moves);
        add_king_promotions(moves);
        KingTag::gen_moves(self, them, moves);
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        self.capture_moves(moves); // clears move list

        if moves.is_empty() {
            // No compulsory captures. Generate everything else.
            gen_non_king(self, !self.board().occupied(), moves);
            add_king_promotions(moves);
            KingTag::gen_moves(self, !self.board().occupied(), moves);
            if let Some(king) = self.board().king_of(self.turn()) {
                gen_castling_moves(self, &self.castles, king, CastlingSide::KingSide, moves);
                gen_castling_moves(self, &self.castles, king, CastlingSide::QueenSide, moves);
            }
        }
    }

    fn king_attackers(&self, _square: Square, _attacker: Color, _occupied: Bitboard) -> Bitboard {
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
            let they_all_on_dark = (self.board.by_color(!color) & Bitboard::LIGHT_SQUARES).is_empty();
            let they_all_on_light = (self.board.by_color(!color) & Bitboard::DARK_SQUARES).is_empty();
            (we_some_on_light && they_all_on_dark) || (we_some_on_dark && they_all_on_light)
        } else {
            false
        }
    }

    fn variant_outcome(&self) -> Option<Outcome> {
        if self.us().is_empty() || self.is_stalemate() {
            Some(Outcome::Decisive { winner: self.turn() })
        } else {
            None
        }
    }
}

impl CastlingUncoversRankAttack for Giveaway {
    fn castling_uncovers_rank_attack(&self, _rook: Square, _king_to: Square) -> bool {
        false
    }
}

/// A King Of The Hill position.
#[derive(Clone, Debug, Default)]
pub struct KingOfTheHill {
    chess: Chess,
}

impl Setup for KingOfTheHill {
    fn board(&self) -> &Board { self.chess.board() }
    fn pockets(&self) -> Option<&Material> { None }
    fn turn(&self) -> Color { self.chess.turn() }
    fn castling_rights(&self) -> Bitboard { self.chess.castling_rights() }
    fn ep_square(&self) -> Option<Square> { self.chess.ep_square() }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }
    fn halfmoves(&self) -> u32 { self.chess.halfmoves() }
    fn fullmoves(&self) -> u32 { self.chess.fullmoves() }
}

impl Position for KingOfTheHill {
    fn play_unchecked(&mut self, m: &Move) {
        self.chess.play_unchecked(m);
    }

    fn from_setup<S: Setup>(setup: &S) -> Result<KingOfTheHill, PositionError> {
        Chess::from_setup(setup).map(|chess| KingOfTheHill { chess })
    }

    fn castles(&self) -> &Castles {
        self.chess.castles()
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        if self.is_variant_end() {
            moves.clear();
        } else {
            self.chess.legal_moves(moves);
        }
    }

    fn castling_moves(&self, side: CastlingSide, moves: &mut MoveList) {
        if self.is_variant_end() {
            moves.clear();
        } else {
            self.chess.castling_moves(side, moves);
        }
    }

    fn en_passant_moves(&self, moves: &mut MoveList) {
        if self.is_variant_end() {
            moves.clear();
        } else {
            self.chess.en_passant_moves(moves);
        }
    }

    fn san_candidates(&self, role: Role, to: Square, moves: &mut MoveList) {
        if self.is_variant_end() {
            moves.clear();
        } else {
            self.chess.san_candidates(role, to, moves);
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
        for &color in &[White, Black] {
            if (self.board().by_color(color) & self.board().kings() & Bitboard::CENTER).any() {
                return Some(Outcome::Decisive { winner: color });
            }
        }
        None
    }
}

impl CastlingUncoversRankAttack for KingOfTheHill {
    fn castling_uncovers_rank_attack(&self, rook: Square, king_to: Square) -> bool {
        self.chess.castling_uncovers_rank_attack(rook, king_to)
    }
}

/// A Three-Check position.
#[derive(Clone, Debug, Default)]
pub struct ThreeCheck {
    chess: Chess,
    remaining_checks: RemainingChecks,
}

impl Setup for ThreeCheck {
    fn board(&self) -> &Board { self.chess.board() }
    fn pockets(&self) -> Option<&Material> { None }
    fn turn(&self) -> Color { self.chess.turn() }
    fn castling_rights(&self) -> Bitboard { self.chess.castling_rights() }
    fn ep_square(&self) -> Option<Square> { self.chess.ep_square() }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { Some(&self.remaining_checks) }
    fn halfmoves(&self) -> u32 { self.chess.halfmoves() }
    fn fullmoves(&self) -> u32 { self.chess.fullmoves }
}

impl Position for ThreeCheck {
    fn play_unchecked(&mut self, m: &Move) {
        let turn = self.chess.turn();
        self.chess.play_unchecked(m);
        if self.is_check() {
            self.remaining_checks.decrement(turn);
        }
    }

    fn from_setup<S: Setup>(setup: &S) -> Result<ThreeCheck, PositionError> {
        let remaining_checks = setup.remaining_checks().cloned().unwrap_or_default();
        let errors = if remaining_checks.white == 0 && remaining_checks.black == 0 {
            PositionError::VARIANT
        } else {
            PositionError::empty()
        };

        match Chess::from_setup(setup) {
            Ok(chess) => errors.into_result(ThreeCheck { chess, remaining_checks }),
            Err(err) => Err(errors | err)
        }
    }

    fn castles(&self) -> &Castles {
        self.chess.castles()
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        if self.is_variant_end() {
            moves.clear();
        } else {
            self.chess.legal_moves(moves);
        }
    }

    fn castling_moves(&self, side: CastlingSide, moves: &mut MoveList) {
        if self.is_variant_end() {
            moves.clear();
        } else {
            self.chess.castling_moves(side, moves);
        }
    }

    fn en_passant_moves(&self, moves: &mut MoveList) {
        if self.is_variant_end() {
            moves.clear();
        } else {
            self.chess.en_passant_moves(moves);
        }
    }

    fn san_candidates(&self, role: Role, to: Square, moves: &mut MoveList) {
        if self.is_variant_end() {
            moves.clear();
        } else {
            self.chess.san_candidates(role, to, moves);
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
        self.remaining_checks.white == 0 || self.remaining_checks.black == 0
    }

    fn variant_outcome(&self) -> Option<Outcome> {
        if self.remaining_checks.white == 0 && self.remaining_checks.black == 0 {
            Some(Outcome::Draw)
        } else if self.remaining_checks.white == 0 {
            Some(Outcome::Decisive { winner: White })
        } else if self.remaining_checks.black == 0 {
            Some(Outcome::Decisive { winner: Black })
        } else {
            None
        }
    }
}

impl CastlingUncoversRankAttack for ThreeCheck {
    fn castling_uncovers_rank_attack(&self, rook: Square, king_to: Square) -> bool {
        self.chess.castling_uncovers_rank_attack(rook, king_to)
    }
}

/// A Crazyhouse position.
#[derive(Clone, Debug, Default)]
pub struct Crazyhouse {
    chess: Chess,
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
            let king = self.board().king_of(self.turn()).expect("king in crazyhouse");
            attacks::between(checker, king)
        } else {
            Bitboard(0)
        }
    }
}

impl Setup for Crazyhouse {
    fn board(&self) -> &Board { self.chess.board() }
    fn pockets(&self) -> Option<&Material> { Some(&self.pockets) }
    fn turn(&self) -> Color { self.chess.turn() }
    fn castling_rights(&self) -> Bitboard { self.chess.castling_rights() }
    fn ep_square(&self) -> Option<Square> { self.chess.ep_square() }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }
    fn halfmoves(&self) -> u32 { self.chess.halfmoves() }
    fn fullmoves(&self) -> u32 { self.chess.fullmoves() }
}

impl Position for Crazyhouse {
    fn play_unchecked(&mut self, m: &Move) {
        match *m {
            Move::Normal { capture: Some(capture), to, .. } => {
                let capture = if self.board().promoted().contains(to) {
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

        self.chess.play_unchecked(m);
    }

    fn from_setup<S: Setup>(setup: &S) -> Result<Crazyhouse, PositionError> {
        Chess::from_setup(setup).and_then(|chess| {
            let pockets = setup.pockets().cloned().unwrap_or_default();
            if pockets.count().saturating_add(chess.board().occupied().count()) > 64 {
                Err(PositionError::VARIANT)
            } else if pockets.white.kings > 0 || pockets.black.kings > 0 {
                Err(PositionError::TOO_MANY_KINGS)
            } else {
                Ok(Crazyhouse { chess, pockets })
            }
        })
    }

    fn castles(&self) -> &Castles {
        self.chess.castles()
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        self.chess.legal_moves(moves);

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
                moves.push(Move::Put { role: Role::Pawn, to });
            }
        }
    }

    fn castling_moves(&self, side: CastlingSide, moves: &mut MoveList) {
        self.chess.castling_moves(side, moves);
    }

    fn en_passant_moves(&self, moves: &mut MoveList) {
        self.chess.en_passant_moves(moves);
    }

    fn san_candidates(&self, role: Role, to: Square, moves: &mut MoveList) {
        self.chess.san_candidates(role, to, moves);

        if self.our_pocket().by_role(role) > 0 && self.legal_put_squares().contains(to) &&
           (role != Role::Pawn || !Bitboard::BACKRANKS.contains(to))
        {
            moves.push(Move::Put { role, to });
        }
    }

    fn is_irreversible(&self, m: &Move) -> bool {
        match *m {
            Move::Castle { .. } => true,
            Move::Normal { role, from, to, .. } =>
                self.castling_rights().contains(from) ||
                self.castling_rights().contains(to) ||
                (role == Role::King && self.chess.castles.has_side(self.turn())),
            _ => false,
        }
    }

    fn has_insufficient_material(&self, _color: Color) -> bool {
        // In practise no material can leave the game, but this is simple
        // to implement anyway. Bishops can be captured and put onto a
        // different color complex.
        self.board().occupied().count() + self.pockets.count() <= 3 &&
        self.board().pawns().is_empty() &&
        self.board().rooks_and_queens().is_empty() &&
        self.pockets.white.pawns == 0 &&
        self.pockets.black.pawns == 0 &&
        self.pockets.white.rooks == 0 &&
        self.pockets.black.rooks == 0 &&
        self.pockets.white.queens == 0 &&
        self.pockets.black.queens == 0
    }

    fn is_variant_end(&self) -> bool { false }
    fn variant_outcome(&self) -> Option<Outcome> { None }
}

impl CastlingUncoversRankAttack for Crazyhouse {
    fn castling_uncovers_rank_attack(&self, rook: Square, king_to: Square) -> bool {
        self.chess.castling_uncovers_rank_attack(rook, king_to)
    }
}

/// A Racing Kings position.
#[derive(Clone, Debug)]
pub struct RacingKings {
    board: Board,
    turn: Color,
    halfmoves: u32,
    fullmoves: u32,
}

impl Default for RacingKings {
    fn default() -> RacingKings {
        RacingKings {
            board: Board::racing_kings(),
            turn: White,
            halfmoves: 0,
            fullmoves: 1,
        }
    }
}

impl Setup for RacingKings {
    fn board(&self) -> &Board { &self.board }
    fn pockets(&self) -> Option<&Material> { None }
    fn turn(&self) -> Color { self.turn }
    fn castling_rights(&self) -> Bitboard { Bitboard(0) }
    fn ep_square(&self) -> Option<Square> { None }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }
    fn halfmoves(&self) -> u32 { self.halfmoves }
    fn fullmoves(&self) -> u32 { self.fullmoves }
}

impl Position for RacingKings {
    fn play_unchecked(&mut self, m: &Move) {
        do_move(&mut self.board, &mut self.turn, &mut Castles::empty(),
                &mut None, &mut self.halfmoves,
                &mut self.fullmoves, m);
    }

    fn from_setup<S: Setup>(setup: &S) -> Result<RacingKings, PositionError> {
        let mut errors = PositionError::empty();

        if setup.castling_rights().any() {
            errors |= PositionError::BAD_CASTLING_RIGHTS;
        }

        let board = setup.board().clone();
        if board.pawns().any() {
            errors |= PositionError::VARIANT;
        }
        if setup.ep_square().is_some() {
            errors |= PositionError::INVALID_EP_SQUARE;
        }

        let pos = RacingKings {
            board,
            turn: setup.turn(),
            halfmoves: setup.halfmoves(),
            fullmoves: setup.fullmoves(),
        };

        if pos.is_check() {
            errors |= PositionError::VARIANT;
        }

        if pos.turn().is_black() &&
           (pos.board().white() & pos.board().kings() & Rank::Eighth).any() &&
           (pos.board().black() & pos.board().kings() & Rank::Eighth).any()
        {
            errors |= PositionError::VARIANT;
        }

        (validate(&pos) | errors).into_result(pos)
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        moves.clear();

        if self.is_variant_end() {
            return;
        }

        // Generate all legal moves (no castling, no ep).
        let target = !self.us();
        gen_non_king(self, target, moves);
        let king = self.board().king_of(self.turn()).expect("king in racingkings");
        gen_safe_king(self, king, target, moves);

        let blockers = slider_blockers(self.board(), self.them(), king);
        if blockers.any() {
            moves.swap_retain(|m| is_safe(self, king, m, blockers));
        }

        // Do not allow giving check. This could be implemented more
        // efficiently.
        moves.swap_retain(|m| {
            let mut after = self.clone();
            after.play_unchecked(m);
            !after.is_check()
        });
    }

    fn castles(&self) -> &Castles {
        &EMPTY_CASTLES
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
        for target in attacks::king_attacks(black_king) & Rank::Eighth {
            if self.king_attackers(target, White, self.board().occupied()).is_empty() {
                return false;
            }
        }

        true
    }

    fn variant_outcome(&self) -> Option<Outcome> {
        if self.is_variant_end() {
            let in_goal = self.board().kings() & Rank::Eighth;
            if (in_goal & self.board().white()).any() && (in_goal & self.board().black()).any() {
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

impl CastlingUncoversRankAttack for RacingKings {
    fn castling_uncovers_rank_attack(&self, _rook: Square, _king_to: Square) -> bool {
        false
    }
}

/// A Horde position.
#[derive(Clone, Debug)]
pub struct Horde {
    board: Board,
    turn: Color,
    castles: Castles,
    ep_square: Option<Square>,
    halfmoves: u32,
    fullmoves: u32,
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
            fullmoves: 1,
        }
    }
}

impl Setup for Horde {
    fn board(&self) -> &Board { &self.board }
    fn pockets(&self) -> Option<&Material> { None }
    fn turn(&self) -> Color { self.turn }
    fn castling_rights(&self) -> Bitboard { self.castles.castling_rights() }
    fn ep_square(&self) -> Option<Square> { self.ep_square.filter(|_| has_relevant_ep(self)) }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }
    fn halfmoves(&self) -> u32 { self.halfmoves }
    fn fullmoves(&self) -> u32 { self.fullmoves }
}

impl Position for Horde {
    fn play_unchecked(&mut self, m: &Move) {
        do_move(&mut self.board, &mut self.turn, &mut self.castles,
                &mut self.ep_square, &mut self.halfmoves,
                &mut self.fullmoves, m);
    }

    fn from_setup<S: Setup>(setup: &S) -> Result<Horde, PositionError> {
        let (castles, errors) = match Castles::from_setup(setup) {
            Ok(castles) => (castles, PositionError::empty()),
            Err(castles) => (castles, PositionError::BAD_CASTLING_RIGHTS),
        };

        let pos = Horde {
            board: setup.board().clone(),
            turn: setup.turn(),
            castles,
            ep_square: setup.ep_square(),
            halfmoves: setup.halfmoves(),
            fullmoves: setup.fullmoves(),
        };

        let mut errors = (errors | validate(&pos))
            - PositionError::PAWNS_ON_BACKRANK
            - PositionError::MISSING_KING;

        if (pos.board().pawns() & pos.board().white() & Rank::Eighth).any() ||
           (pos.board().pawns() & pos.board().black() & Rank::First).any()
        {
            errors |= PositionError::PAWNS_ON_BACKRANK;
        }

        if (pos.board().kings() & !pos.board().promoted()).is_empty() {
            errors |= PositionError::MISSING_KING;
        }

        if (pos.board().kings() & pos.board().white()).any() &&
           (pos.board().kings() & pos.board().black()).any()
        {
            errors |= PositionError::VARIANT;
        }

        errors.into_result(pos)
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        moves.clear();

        let king = self.board().king_of(self.turn());
        let has_ep = gen_en_passant(self.board(), self.turn(), self.ep_square, moves);

        let checkers = self.checkers();
        if checkers.is_empty() {
            let target = !self.us();
            gen_non_king(self, target, moves);
            if let Some(king) = king {
                gen_safe_king(self, king, target, moves);
                gen_castling_moves(self, &self.castles, king, CastlingSide::KingSide, moves);
                gen_castling_moves(self, &self.castles, king, CastlingSide::QueenSide, moves);
            }
        } else {
            evasions(self, king.expect("king in check"), checkers, moves);
        }

        if let Some(king) = king {
            let blockers = slider_blockers(self.board(), self.them(), king);
            if blockers.any() || has_ep {
                moves.swap_retain(|m| is_safe(self, king, m, blockers));
            }
        }
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

        // TODO: Detect when the horde can not mate. Note that it does not have
        // a king.
        false
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

impl CastlingUncoversRankAttack for Horde {
    fn castling_uncovers_rank_attack(&self, rook: Square, king_to: Square) -> bool {
        self.castles.is_chess960() &&
        castling_uncovers_rank_attack(self, rook, king_to)
    }
}

fn do_move(board: &mut Board,
           turn: &mut Color,
           castles: &mut Castles,
           ep_square: &mut Option<Square>,
           halfmoves: &mut u32,
           fullmoves: &mut u32,
           m: &Move) {
    let color = *turn;
    ep_square.take();
    *halfmoves = halfmoves.saturating_add(1);

    match *m {
        Move::Normal { role, from, capture, to, promotion } => {
            if role == Role::Pawn || capture.is_some() {
                *halfmoves = 0;
            }

            if role == Role::Pawn && (from - to == 16 || from - to == -16) {
                *ep_square = from.offset(color.fold(8, -8));
            }

            if role == Role::King {
                castles.discard_side(color);
            } else if role == Role::Rook {
                castles.discard_rook(from);
            }

            if capture == Some(Role::Rook) {
                castles.discard_rook(to);
            }

            let promoted = board.promoted().contains(from) || promotion.is_some();

            board.discard_piece_at(from);
            board.set_piece_at(to, promotion.map_or(role.of(color), |p| p.of(color)), promoted);
        },
        Move::Castle { king, rook } => {
            let rook_to = (if rook - king < 0 { Square::D1 } else { Square::F1 }).with_rank_of(rook);
            let king_to = (if rook - king < 0 { Square::C1 } else { Square::G1 }).with_rank_of(king);

            board.discard_piece_at(king);
            board.discard_piece_at(rook);
            board.set_piece_at(rook_to, color.rook(), false);
            board.set_piece_at(king_to, color.king(), false);

            castles.discard_side(color);
        }
        Move::EnPassant { from, to } => {
            board.discard_piece_at(to.with_rank_of(from)); // captured pawn
            board.discard_piece_at(from);
            board.set_piece_at(to, color.pawn(), false);
            *halfmoves = 0;
        }
        Move::Put { role, to } => {
            board.set_piece_at(to, Piece { color, role }, false);
        }
    }

    if color.is_black() {
        *fullmoves = fullmoves.saturating_add(1);
    }

    *turn = !color;
}

fn validate<P: Position>(pos: &P) -> PositionError {
    let mut errors = PositionError::empty();

    if pos.board().occupied().is_empty() {
        errors |= PositionError::EMPTY_BOARD;
    }

    if (pos.board().pawns() & Bitboard::BACKRANKS).any() {
        errors |= PositionError::PAWNS_ON_BACKRANK;
    }

    // validate en passant square
    if let Some(ep_square) = pos.ep_square() {
        if !Bitboard::relative_rank(pos.turn(), Rank::Sixth).contains(ep_square) {
            errors |= PositionError::INVALID_EP_SQUARE;
        } else {
            let fifth_rank_sq = ep_square
                .offset(pos.turn().fold(-8, 8))
                .expect("ep square is on sixth rank");

            let seventh_rank_sq = ep_square
                .offset(pos.turn().fold(8, -8))
                .expect("ep square is on sixth rank");

            // The last move must have been a double pawn push. Check for the
            // presence of that pawn.
            if !pos.their(Role::Pawn).contains(fifth_rank_sq) {
                errors |= PositionError::INVALID_EP_SQUARE;
            }

            if pos.board().occupied().contains(ep_square) || pos.board().occupied().contains(seventh_rank_sq) {
                errors |= PositionError::INVALID_EP_SQUARE;
            }
        }
    }

    for &color in &[White, Black] {
        if pos.board().king_of(color).is_none() {
            errors |= PositionError::MISSING_KING;
        }
    }

    if (pos.board().kings() & pos.board().white()).more_than_one() ||
       (pos.board().kings() & pos.board().black()).more_than_one()
    {
        errors |= PositionError::TOO_MANY_KINGS;
    }

    if let Some(their_king) = pos.board().king_of(!pos.turn()) {
        if pos.king_attackers(their_king, pos.turn(), pos.board().occupied()).any() {
            errors |= PositionError::OPPOSITE_CHECK;
        }
    }

    errors
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
        if pos.board().attacks_to(to, !pos.turn(), pos.board().occupied()).is_empty() {
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

fn gen_castling_moves<P: Position + CastlingUncoversRankAttack>(pos: &P, castles: &Castles, king: Square, side: CastlingSide, moves: &mut MoveList) {
    if let Some(rook) = castles.rook(pos.turn(), side) {
        let path = castles.path(pos.turn(), side);
        if (path & pos.board().occupied()).any() {
            return;
        }

        let king_to = side.king_to(pos.turn());
        let king_path = attacks::between(king, king_to).with(king_to).with(king);
        for sq in king_path {
            if pos.king_attackers(sq, !pos.turn(), pos.board().occupied() ^ king).any() {
                return;
            }
        }

        if pos.castling_uncovers_rank_attack(rook, king_to) {
            return;
        }

        moves.push(Move::Castle { king, rook });
    }
}

fn castling_uncovers_rank_attack<P: Position>(pos: &P, rook: Square, king_to: Square) -> bool {
    (attacks::rook_attacks(king_to, pos.board().occupied().without(rook)) &
     pos.them() & pos.board().rooks_and_queens() &
     Bitboard::rank(king_to.rank())).any()
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

enum KnightTag { }
enum BishopTag { }
enum RookTag { }
enum QueenTag { }
enum KingTag { }

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
    let seventh = pos.our(Role::Pawn) & Bitboard::relative_rank(pos.turn(), Rank::Seventh);

    for from in pos.our(Role::Pawn) & !seventh {
        for to in attacks::pawn_attacks(pos.turn(), from) & pos.them() & target {
            moves.push(Move::Normal {
                role: Role::Pawn,
                from,
                capture: pos.board().role_at(to),
                to,
                promotion: None,
            });
        }
    }

    for from in seventh {
        for to in attacks::pawn_attacks(pos.turn(), from) & pos.them() & target {
            push_promotions(moves, from, to, pos.board().role_at(to));
        }
    }

    let single_moves = pos.our(Role::Pawn).relative_shift(pos.turn(), 8) &
                       !pos.board().occupied();

    let double_moves = single_moves.relative_shift(pos.turn(), 8) &
                       Bitboard::relative_rank(pos.turn(), Rank::Fourth) &
                       !pos.board().occupied();

    for to in single_moves & target & !Bitboard::BACKRANKS {
        if let Some(from) = to.offset(pos.turn().fold(-8, 8)) {
            moves.push(Move::Normal {
                role: Role::Pawn,
                from,
                capture: None,
                to,
                promotion: None,
            });
        }
    }

    for to in single_moves & target & Bitboard::BACKRANKS {
        if let Some(from) = to.offset(pos.turn().fold(-8, 8)) {
            push_promotions(moves, from, to, None);
        }
    }

    for to in double_moves & target {
        if let Some(from) = to.offset(pos.turn().fold(-16, 16)) {
            moves.push(Move::Normal {
                role: Role::Pawn,
                from,
                capture: None,
                to,
                promotion: None,
            });
        }
    }
}

fn push_promotions(moves: &mut MoveList, from: Square, to: Square, capture: Option<Role>) {
    moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Queen) });
    moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Rook) });
    moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Bishop) });
    moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Knight) });
}

fn add_king_promotions(moves: &mut MoveList) {
    let mut king_promotions = MoveList::new();

    for m in &moves[..] {
        if let Move::Normal { role, from, capture, to, promotion: Some(Role::Queen) } = *m {
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

fn has_relevant_ep<P: Position>(pos: &P) -> bool {
    let mut moves = MoveList::new();
    pos.en_passant_moves(&mut moves);
    !moves.is_empty()
}

fn gen_en_passant(board: &Board, turn: Color, ep_square: Option<Square>, moves: &mut MoveList) -> bool {
    let mut found = false;

    if let Some(to) = ep_square {
        for from in board.pawns() & board.by_color(turn) & attacks::pawn_attacks(!turn, to) {
            moves.push(Move::EnPassant { from, to });
            found = true;
        }
    }

    found
}

fn slider_blockers(board: &Board, enemy: Bitboard, king: Square) -> Bitboard {
    let snipers = (attacks::rook_attacks(king, Bitboard(0)) & board.rooks_and_queens()) |
                  (attacks::bishop_attacks(king, Bitboard(0)) & board.bishops_and_queens());

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
        Move::Normal { from, to, .. } =>
            !(pos.us() & blockers).contains(from) || attacks::aligned(from, to, king),
        Move::EnPassant { from, to } => {
            let mut occupied = pos.board().occupied();
            occupied.toggle(from);
            occupied.toggle(to.with_rank_of(from)); // captured pawn
            occupied.add(to);

            (attacks::rook_attacks(king, occupied) & pos.them() & pos.board().rooks_and_queens()).is_empty() &&
            (attacks::bishop_attacks(king, occupied) & pos.them() & pos.board().bishops_and_queens()).is_empty()
        },
        _ => true,
    }
}

fn filter_san_candidates(role: Role, to: Square, moves: &mut MoveList) {
    moves.retain(|m| match *m {
        Move::Normal { role: r, to: t, .. } | Move::Put { role: r, to: t } =>
            to == t && role == r,
        Move::EnPassant { to: t, .. } => role == Role::Pawn && t == to,
        Move::Castle { .. } => false,
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fen::Fen;

    struct _AssertObjectSafe(Box<dyn Position>);

    #[test]
    fn test_most_known_legals() {
        let fen = "R6R/3Q4/1Q4Q1/4Q3/2Q4Q/Q4Q2/pp1Q4/kBNN1KB1 w - - 0 1";
        let pos: Chess = fen.parse::<Fen>()
            .expect("valid fen")
            .position()
            .expect("legal position");

        let mut moves = MoveList::new();
        pos.legal_moves(&mut moves);
        assert_eq!(moves.len(), 218);
    }

    #[test]
    fn test_pinned_san_candidate() {
        let fen = "R2r2k1/6pp/1Np2p2/1p2pP2/4p3/4K3/3r2PP/8 b - - 5 37";
        let pos: Chess = fen.parse::<Fen>()
            .expect("valid fen")
            .position()
            .expect("valid position");

        let mut moves = MoveList::new();
        pos.san_candidates(Role::Rook, Square::D3, &mut moves);

        assert_eq!(moves[0], Move::Normal {
            role: Role::Rook,
            from: Square::D2,
            capture: None,
            to: Square::D3,
            promotion: None,
        });

        assert_eq!(moves.len(), 1);
    }

    #[test]
    fn test_promotion() {
        let fen = "3r3K/6PP/8/8/8/2k5/8/8 w - - 0 1";
        let pos: Chess = fen.parse::<Fen>()
            .expect("valid fen")
            .position()
            .expect("valid position");

        let mut moves = MoveList::new();
        pos.legal_moves(&mut moves);
        assert!(moves.iter().all(|m| m.role() == Role::Pawn));
        assert!(moves.iter().all(|m| m.is_promotion()));
    }

    fn assert_insufficient_material<P: Position>(fen: &str, white: bool, black: bool) {
        let pos: P = fen.parse::<Fen>()
            .expect("valid fen")
            .position()
            .expect("valid position");

        assert_eq!(pos.has_insufficient_material(White), white);
        assert_eq!(pos.has_insufficient_material(Black), black);
    }

    #[test]
    fn test_insufficient_material() {
        let false_negative = false;

        assert_insufficient_material::<Chess>("8/5k2/8/8/8/8/3K4/8 w - - 0 1", true, true);
        assert_insufficient_material::<Chess>("8/3k4/8/8/2N5/8/3K4/8 b - - 0 1", true, true);
        assert_insufficient_material::<Chess>("8/4rk2/8/8/8/8/3K4/8 w - - 0 1", true, false);
        assert_insufficient_material::<Chess>("8/4qk2/8/8/8/8/3K4/8 w - - 0 1", true, false);
        assert_insufficient_material::<Chess>("8/4bk2/8/8/8/8/3KB3/8 w - - 0 1", false, false);
        assert_insufficient_material::<Chess>("8/8/3Q4/2bK4/B7/8/1k6/8 w - - 1 68", false, false);

        assert_insufficient_material::<Atomic>("8/3k4/8/8/2N5/8/3K4/8 b - - 0 1", true, true);
        assert_insufficient_material::<Atomic>("8/4rk2/8/8/8/8/3K4/8 w - - 0 1", true, true);
        assert_insufficient_material::<Atomic>("8/4qk2/8/8/8/8/3K4/8 w - - 0 1", true, false);
        assert_insufficient_material::<Atomic>("8/1k6/8/2n5/8/3NK3/8/8 b - - 0 1", false, false);
        assert_insufficient_material::<Atomic>("8/4bk2/8/8/8/8/3KB3/8 w - - 0 1", true, true);
        assert_insufficient_material::<Atomic>("4b3/5k2/8/8/8/8/3KB3/8 w - - 0 1", false, false);
        assert_insufficient_material::<Atomic>("3Q4/5kKB/8/8/8/8/8/8 b - - 0 1", false, true);
        assert_insufficient_material::<Atomic>("8/5k2/8/8/8/8/5K2/4bb2 w - - 0 1", true, false);
        assert_insufficient_material::<Atomic>("8/5k2/8/8/8/8/5K2/4nb2 w - - 0 1", true, false);

        assert_insufficient_material::<Giveaway>("8/4bk2/8/8/8/8/3KB3/8 w - - 0 1", false, false);
        assert_insufficient_material::<Giveaway>("4b3/5k2/8/8/8/8/3KB3/8 w - - 0 1", false, false);
        assert_insufficient_material::<Giveaway>("8/8/8/6b1/8/3B4/4B3/5B2 w - - 0 1", true, true);
        assert_insufficient_material::<Giveaway>("8/8/5b2/8/8/3B4/3B4/8 w - - 0 1", true, false);
        assert_insufficient_material::<Giveaway>("8/5p2/5P2/8/3B4/1bB5/8/8 b - - 0 1", false_negative, false_negative);

        assert_insufficient_material::<KingOfTheHill>("8/5k2/8/8/8/8/3K4/8 w - - 0 1", false, false);

        assert_insufficient_material::<RacingKings>("8/5k2/8/8/8/8/3K4/8 w - - 0 1", false, false);

        assert_insufficient_material::<ThreeCheck>("8/5k2/8/8/8/8/3K4/8 w - - 0 1", true, true);
        assert_insufficient_material::<ThreeCheck>("8/5k2/8/8/8/8/3K2N1/8 w - - 0 1", false, true);

        assert_insufficient_material::<Crazyhouse>("8/5k2/8/8/8/8/3K2N1/8 w - - 0 1", true, true);
        assert_insufficient_material::<Crazyhouse>("8/5k2/8/8/8/5B2/3KB3/8 w - - 0 1", false, false);

        assert_insufficient_material::<Horde>("8/5k2/8/8/8/4NN2/8/8 w - - 0 1", false_negative, false);
    }
}
