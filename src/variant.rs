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

//! Chess variants.
//!
//! These are games played with normal chess pieces but special rules.
//! Every chess variant implements [`FromSetup`] and [`Position`].

use std::{num::NonZeroU32, str};

pub use crate::position::{
    variant::{Antichess, Atomic, Crazyhouse, Horde, KingOfTheHill, RacingKings, ThreeCheck},
    Chess,
};
use crate::{
    zobrist::{ZobristHash, ZobristValue},
    Bitboard, Board, ByColor, ByRole, Castles, CastlingMode, CastlingSide, Color, EnPassantMode,
    FromSetup, Move, MoveList, Outcome, Position, PositionError, RemainingChecks, Role, Setup,
    Square,
};

/// Discriminant of [`VariantPosition`].
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum Variant {
    /// See [`Chess`].
    Chess,
    /// See [`Atomic`].
    Atomic,
    /// See [`Antichess`].
    Antichess,
    /// See [`KingOfTheHill`].
    KingOfTheHill,
    /// See [`ThreeCheck`].
    ThreeCheck,
    /// See [`Crazyhouse`].
    Crazyhouse,
    /// See [`RacingKings`].
    RacingKings,
    /// See [`Horde`].
    Horde,
}

impl Variant {
    /// Gets the name of the variant, as expected by the `UCI_Variant` option
    /// of chess engines.
    pub fn uci(self) -> &'static str {
        match self {
            Variant::Chess => "chess",
            Variant::Atomic => "atomic",
            Variant::Antichess => "antichess",
            Variant::KingOfTheHill => "kingofthehill",
            Variant::ThreeCheck => "3check",
            Variant::Crazyhouse => "crazyhouse",
            Variant::RacingKings => "racingkings",
            Variant::Horde => "horde",
        }
    }

    /// Selects a variant based on the name used by the `UCI_Variant` option
    /// of chess engines.
    pub fn from_uci(s: &str) -> Option<Variant> {
        Some(match s {
            "chess" => Variant::Chess,
            "atomic" => Variant::Atomic,
            "antichess" => Variant::Antichess,
            "kingofthehill" => Variant::KingOfTheHill,
            "3check" => Variant::ThreeCheck,
            "crazyhouse" => Variant::Crazyhouse,
            "racingkings" => Variant::RacingKings,
            "horde" => Variant::Horde,
            _ => return None,
        })
    }

    pub fn distinguishes_promoted(self) -> bool {
        self == Variant::Crazyhouse
    }

    pub const ALL: [Variant; 8] = [
        Variant::Chess,
        Variant::Atomic,
        Variant::Antichess,
        Variant::KingOfTheHill,
        Variant::ThreeCheck,
        Variant::Crazyhouse,
        Variant::RacingKings,
        Variant::Horde,
    ];
}

impl Default for Variant {
    fn default() -> Variant {
        Variant::Chess
    }
}

/// Dynamically dispatched chess variant [`Position`].
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum VariantPosition {
    Chess(Chess),
    Atomic(Atomic),
    Antichess(Antichess),
    KingOfTheHill(KingOfTheHill),
    ThreeCheck(ThreeCheck),
    Crazyhouse(Crazyhouse),
    RacingKings(RacingKings),
    Horde(Horde),
}

impl From<Chess> for VariantPosition {
    fn from(pos: Chess) -> VariantPosition {
        VariantPosition::Chess(pos)
    }
}

impl From<Atomic> for VariantPosition {
    fn from(pos: Atomic) -> VariantPosition {
        VariantPosition::Atomic(pos)
    }
}

impl From<Antichess> for VariantPosition {
    fn from(pos: Antichess) -> VariantPosition {
        VariantPosition::Antichess(pos)
    }
}

impl From<KingOfTheHill> for VariantPosition {
    fn from(pos: KingOfTheHill) -> VariantPosition {
        VariantPosition::KingOfTheHill(pos)
    }
}

impl From<ThreeCheck> for VariantPosition {
    fn from(pos: ThreeCheck) -> VariantPosition {
        VariantPosition::ThreeCheck(pos)
    }
}

impl From<Crazyhouse> for VariantPosition {
    fn from(pos: Crazyhouse) -> VariantPosition {
        VariantPosition::Crazyhouse(pos)
    }
}

impl From<RacingKings> for VariantPosition {
    fn from(pos: RacingKings) -> VariantPosition {
        VariantPosition::RacingKings(pos)
    }
}

impl From<Horde> for VariantPosition {
    fn from(pos: Horde) -> VariantPosition {
        VariantPosition::Horde(pos)
    }
}

impl VariantPosition {
    pub fn new(variant: Variant) -> VariantPosition {
        match variant {
            Variant::Chess => Chess::default().into(),
            Variant::Atomic => Atomic::default().into(),
            Variant::Antichess => Antichess::default().into(),
            Variant::KingOfTheHill => KingOfTheHill::default().into(),
            Variant::ThreeCheck => ThreeCheck::default().into(),
            Variant::Crazyhouse => Crazyhouse::default().into(),
            Variant::RacingKings => RacingKings::default().into(),
            Variant::Horde => Horde::default().into(),
        }
    }

    pub fn from_setup(
        variant: Variant,
        setup: Setup,
        mode: CastlingMode,
    ) -> Result<VariantPosition, PositionError<VariantPosition>> {
        fn wrap<F, P, U>(result: Result<P, PositionError<P>>, f: F) -> Result<U, PositionError<U>>
        where
            F: FnOnce(P) -> U,
        {
            match result {
                Ok(p) => Ok(f(p)),
                Err(PositionError { errors, pos }) => Err(PositionError {
                    errors,
                    pos: f(pos),
                }),
            }
        }

        match variant {
            Variant::Chess => wrap(Chess::from_setup(setup, mode), VariantPosition::Chess),
            Variant::Atomic => wrap(Atomic::from_setup(setup, mode), VariantPosition::Atomic),
            Variant::Antichess => wrap(
                Antichess::from_setup(setup, mode),
                VariantPosition::Antichess,
            ),
            Variant::KingOfTheHill => wrap(
                KingOfTheHill::from_setup(setup, mode),
                VariantPosition::KingOfTheHill,
            ),
            Variant::ThreeCheck => wrap(
                ThreeCheck::from_setup(setup, mode),
                VariantPosition::ThreeCheck,
            ),
            Variant::Crazyhouse => wrap(
                Crazyhouse::from_setup(setup, mode),
                VariantPosition::Crazyhouse,
            ),
            Variant::RacingKings => wrap(
                RacingKings::from_setup(setup, mode),
                VariantPosition::RacingKings,
            ),
            Variant::Horde => wrap(Horde::from_setup(setup, mode), VariantPosition::Horde),
        }
    }

    pub fn swap_turn(self) -> Result<VariantPosition, PositionError<VariantPosition>> {
        let mode = self.castles().mode();
        let variant = self.variant();
        let mut setup = self.into_setup(EnPassantMode::Always);
        setup.swap_turn();
        VariantPosition::from_setup(variant, setup, mode)
    }

    pub fn variant(&self) -> Variant {
        match self {
            VariantPosition::Chess(_) => Variant::Chess,
            VariantPosition::Atomic(_) => Variant::Atomic,
            VariantPosition::Antichess(_) => Variant::Antichess,
            VariantPosition::KingOfTheHill(_) => Variant::KingOfTheHill,
            VariantPosition::ThreeCheck(_) => Variant::ThreeCheck,
            VariantPosition::Crazyhouse(_) => Variant::Crazyhouse,
            VariantPosition::RacingKings(_) => Variant::RacingKings,
            VariantPosition::Horde(_) => Variant::Horde,
        }
    }

    fn borrow(&self) -> &dyn Position {
        match *self {
            VariantPosition::Chess(ref pos) => pos,
            VariantPosition::Atomic(ref pos) => pos,
            VariantPosition::Antichess(ref pos) => pos,
            VariantPosition::KingOfTheHill(ref pos) => pos,
            VariantPosition::ThreeCheck(ref pos) => pos,
            VariantPosition::Crazyhouse(ref pos) => pos,
            VariantPosition::RacingKings(ref pos) => pos,
            VariantPosition::Horde(ref pos) => pos,
        }
    }

    fn borrow_mut(&mut self) -> &mut dyn Position {
        match *self {
            VariantPosition::Chess(ref mut pos) => pos,
            VariantPosition::Atomic(ref mut pos) => pos,
            VariantPosition::Antichess(ref mut pos) => pos,
            VariantPosition::KingOfTheHill(ref mut pos) => pos,
            VariantPosition::ThreeCheck(ref mut pos) => pos,
            VariantPosition::Crazyhouse(ref mut pos) => pos,
            VariantPosition::RacingKings(ref mut pos) => pos,
            VariantPosition::Horde(ref mut pos) => pos,
        }
    }
}

impl Position for VariantPosition {
    fn board(&self) -> &Board {
        self.borrow().board()
    }
    fn promoted(&self) -> Bitboard {
        self.borrow().promoted()
    }
    fn pockets(&self) -> Option<&ByColor<ByRole<u8>>> {
        self.borrow().pockets()
    }
    fn turn(&self) -> Color {
        self.borrow().turn()
    }
    fn castles(&self) -> &Castles {
        self.borrow().castles()
    }
    fn maybe_ep_square(&self) -> Option<Square> {
        self.borrow().maybe_ep_square()
    }
    fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> {
        self.borrow().remaining_checks()
    }
    fn halfmoves(&self) -> u32 {
        self.borrow().halfmoves()
    }
    fn fullmoves(&self) -> NonZeroU32 {
        self.borrow().fullmoves()
    }
    fn into_setup(self, mode: EnPassantMode) -> Setup {
        match self {
            VariantPosition::Chess(pos) => pos.into_setup(mode),
            VariantPosition::Atomic(pos) => pos.into_setup(mode),
            VariantPosition::Antichess(pos) => pos.into_setup(mode),
            VariantPosition::KingOfTheHill(pos) => pos.into_setup(mode),
            VariantPosition::ThreeCheck(pos) => pos.into_setup(mode),
            VariantPosition::Horde(pos) => pos.into_setup(mode),
            VariantPosition::RacingKings(pos) => pos.into_setup(mode),
            VariantPosition::Crazyhouse(pos) => pos.into_setup(mode),
        }
    }
    fn legal_moves(&self) -> MoveList {
        self.borrow().legal_moves()
    }
    fn san_candidates(&self, role: Role, to: Square) -> MoveList {
        self.borrow().san_candidates(role, to)
    }
    fn castling_moves(&self, side: CastlingSide) -> MoveList {
        self.borrow().castling_moves(side)
    }
    fn en_passant_moves(&self) -> MoveList {
        self.borrow().en_passant_moves()
    }
    fn capture_moves(&self) -> MoveList {
        self.borrow().capture_moves()
    }
    fn promotion_moves(&self) -> MoveList {
        self.borrow().promotion_moves()
    }
    fn is_irreversible(&self, m: &Move) -> bool {
        self.borrow().is_irreversible(m)
    }
    fn king_attackers(&self, square: Square, attacker: Color, occupied: Bitboard) -> Bitboard {
        self.borrow().king_attackers(square, attacker, occupied)
    }
    fn is_variant_end(&self) -> bool {
        self.borrow().is_variant_end()
    }
    fn has_insufficient_material(&self, color: Color) -> bool {
        self.borrow().has_insufficient_material(color)
    }
    fn variant_outcome(&self) -> Option<Outcome> {
        self.borrow().variant_outcome()
    }
    fn play_unchecked(&mut self, m: &Move) {
        self.borrow_mut().play_unchecked(m)
    }
}

impl ZobristHash for VariantPosition {
    fn zobrist_hash<V: ZobristValue>(&self) -> V {
        match self {
            VariantPosition::Chess(pos) => pos.zobrist_hash(),
            VariantPosition::Atomic(pos) => pos.zobrist_hash(),
            VariantPosition::Antichess(pos) => pos.zobrist_hash(),
            VariantPosition::KingOfTheHill(pos) => pos.zobrist_hash(),
            VariantPosition::ThreeCheck(pos) => pos.zobrist_hash(),
            VariantPosition::Crazyhouse(pos) => pos.zobrist_hash(),
            VariantPosition::RacingKings(pos) => pos.zobrist_hash(),
            VariantPosition::Horde(pos) => pos.zobrist_hash(),
        }
    }

    fn prepare_incremental_zobrist_hash<V: ZobristValue>(
        &self,
        previous: V,
        m: &Move,
    ) -> Option<V> {
        match self {
            VariantPosition::Chess(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            VariantPosition::Atomic(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            VariantPosition::Antichess(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            VariantPosition::KingOfTheHill(pos) => {
                pos.prepare_incremental_zobrist_hash(previous, m)
            }
            VariantPosition::ThreeCheck(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            VariantPosition::Crazyhouse(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            VariantPosition::RacingKings(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            VariantPosition::Horde(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
        }
    }

    fn finalize_incremental_zobrist_hash<V: ZobristValue>(
        &self,
        intermediate: V,
        m: &Move,
    ) -> Option<V> {
        match self {
            VariantPosition::Chess(pos) => pos.finalize_incremental_zobrist_hash(intermediate, m),
            VariantPosition::Atomic(pos) => pos.finalize_incremental_zobrist_hash(intermediate, m),
            VariantPosition::Antichess(pos) => {
                pos.finalize_incremental_zobrist_hash(intermediate, m)
            }
            VariantPosition::KingOfTheHill(pos) => {
                pos.finalize_incremental_zobrist_hash(intermediate, m)
            }
            VariantPosition::ThreeCheck(pos) => {
                pos.finalize_incremental_zobrist_hash(intermediate, m)
            }
            VariantPosition::Crazyhouse(pos) => {
                pos.finalize_incremental_zobrist_hash(intermediate, m)
            }
            VariantPosition::RacingKings(pos) => {
                pos.finalize_incremental_zobrist_hash(intermediate, m)
            }
            VariantPosition::Horde(pos) => pos.finalize_incremental_zobrist_hash(intermediate, m),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variant_position_play() {
        let pos = VariantPosition::new(Variant::Chess);
        let pos = pos
            .play(&Move::Normal {
                role: Role::Knight,
                from: Square::G1,
                to: Square::F3,
                capture: None,
                promotion: None,
            })
            .expect("legal move");
        assert_eq!(pos.variant(), Variant::Chess);
    }
}
