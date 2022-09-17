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

use core::{num::NonZeroU32, str};

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
    pub const fn uci(self) -> &'static str {
        match self {
            Self::Chess => "chess",
            Self::Atomic => "atomic",
            Self::Antichess => "antichess",
            Self::KingOfTheHill => "kingofthehill",
            Self::ThreeCheck => "3check",
            Self::Crazyhouse => "crazyhouse",
            Self::RacingKings => "racingkings",
            Self::Horde => "horde",
        }
    }

    /// Selects a variant based on the name used by the `UCI_Variant` option
    /// of chess engines.
    pub fn from_uci(s: &str) -> Option<Self> {
        Some(match s {
            "chess" => Self::Chess,
            "atomic" => Self::Atomic,
            "antichess" => Self::Antichess,
            "kingofthehill" => Self::KingOfTheHill,
            "3check" => Self::ThreeCheck,
            "crazyhouse" => Self::Crazyhouse,
            "racingkings" => Self::RacingKings,
            "horde" => Self::Horde,
            _ => return None,
        })
    }

    pub fn distinguishes_promoted(self) -> bool {
        matches!(self, Self::Crazyhouse)
    }

    pub const ALL: [Self; 8] = [
        Self::Chess,
        Self::Atomic,
        Self::Antichess,
        Self::KingOfTheHill,
        Self::ThreeCheck,
        Self::Crazyhouse,
        Self::RacingKings,
        Self::Horde,
    ];
}

impl Default for Variant {
    fn default() -> Self {
        Self::Chess
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
    fn from(pos: Chess) -> Self {
        Self::Chess(pos)
    }
}

impl From<Atomic> for VariantPosition {
    fn from(pos: Atomic) -> Self {
        Self::Atomic(pos)
    }
}

impl From<Antichess> for VariantPosition {
    fn from(pos: Antichess) -> Self {
        Self::Antichess(pos)
    }
}

impl From<KingOfTheHill> for VariantPosition {
    fn from(pos: KingOfTheHill) -> Self {
        Self::KingOfTheHill(pos)
    }
}

impl From<ThreeCheck> for VariantPosition {
    fn from(pos: ThreeCheck) -> Self {
        Self::ThreeCheck(pos)
    }
}

impl From<Crazyhouse> for VariantPosition {
    fn from(pos: Crazyhouse) -> Self {
        Self::Crazyhouse(pos)
    }
}

impl From<RacingKings> for VariantPosition {
    fn from(pos: RacingKings) -> Self {
        Self::RacingKings(pos)
    }
}

impl From<Horde> for VariantPosition {
    fn from(pos: Horde) -> Self {
        Self::Horde(pos)
    }
}

impl VariantPosition {
    pub fn new(variant: Variant) -> Self {
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
    ) -> Result<Self, PositionError<Self>> {
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

    pub fn swap_turn(self) -> Result<Self, PositionError<Self>> {
        let mode = self.castles().mode();
        let variant = self.variant();
        let mut setup = self.into_setup(EnPassantMode::Always);
        setup.swap_turn();
        Self::from_setup(variant, setup, mode)
    }

    pub fn variant(&self) -> Variant {
        match self {
            Self::Chess(_) => Variant::Chess,
            Self::Atomic(_) => Variant::Atomic,
            Self::Antichess(_) => Variant::Antichess,
            Self::KingOfTheHill(_) => Variant::KingOfTheHill,
            Self::ThreeCheck(_) => Variant::ThreeCheck,
            Self::Crazyhouse(_) => Variant::Crazyhouse,
            Self::RacingKings(_) => Variant::RacingKings,
            Self::Horde(_) => Variant::Horde,
        }
    }

    fn borrow(&self) -> &dyn Position {
        match *self {
            Self::Chess(ref pos) => pos,
            Self::Atomic(ref pos) => pos,
            Self::Antichess(ref pos) => pos,
            Self::KingOfTheHill(ref pos) => pos,
            Self::ThreeCheck(ref pos) => pos,
            Self::Crazyhouse(ref pos) => pos,
            Self::RacingKings(ref pos) => pos,
            Self::Horde(ref pos) => pos,
        }
    }

    fn borrow_mut(&mut self) -> &mut dyn Position {
        match *self {
            Self::Chess(ref mut pos) => pos,
            Self::Atomic(ref mut pos) => pos,
            Self::Antichess(ref mut pos) => pos,
            Self::KingOfTheHill(ref mut pos) => pos,
            Self::ThreeCheck(ref mut pos) => pos,
            Self::Crazyhouse(ref mut pos) => pos,
            Self::RacingKings(ref mut pos) => pos,
            Self::Horde(ref mut pos) => pos,
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
            Self::Chess(pos) => pos.into_setup(mode),
            Self::Atomic(pos) => pos.into_setup(mode),
            Self::Antichess(pos) => pos.into_setup(mode),
            Self::KingOfTheHill(pos) => pos.into_setup(mode),
            Self::ThreeCheck(pos) => pos.into_setup(mode),
            Self::Horde(pos) => pos.into_setup(mode),
            Self::RacingKings(pos) => pos.into_setup(mode),
            Self::Crazyhouse(pos) => pos.into_setup(mode),
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
            Self::Chess(pos) => pos.zobrist_hash(),
            Self::Atomic(pos) => pos.zobrist_hash(),
            Self::Antichess(pos) => pos.zobrist_hash(),
            Self::KingOfTheHill(pos) => pos.zobrist_hash(),
            Self::ThreeCheck(pos) => pos.zobrist_hash(),
            Self::Crazyhouse(pos) => pos.zobrist_hash(),
            Self::RacingKings(pos) => pos.zobrist_hash(),
            Self::Horde(pos) => pos.zobrist_hash(),
        }
    }

    fn prepare_incremental_zobrist_hash<V: ZobristValue>(
        &self,
        previous: V,
        m: &Move,
    ) -> Option<V> {
        match self {
            Self::Chess(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            Self::Atomic(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            Self::Antichess(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            Self::KingOfTheHill(pos) => {
                pos.prepare_incremental_zobrist_hash(previous, m)
            }
            Self::ThreeCheck(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            Self::Crazyhouse(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            Self::RacingKings(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
            Self::Horde(pos) => pos.prepare_incremental_zobrist_hash(previous, m),
        }
    }

    fn finalize_incremental_zobrist_hash<V: ZobristValue>(
        &self,
        intermediate: V,
        m: &Move,
    ) -> Option<V> {
        match self {
            Self::Chess(pos) => pos.finalize_incremental_zobrist_hash(intermediate, m),
            Self::Atomic(pos) => pos.finalize_incremental_zobrist_hash(intermediate, m),
            Self::Antichess(pos) => {
                pos.finalize_incremental_zobrist_hash(intermediate, m)
            }
            Self::KingOfTheHill(pos) => {
                pos.finalize_incremental_zobrist_hash(intermediate, m)
            }
            Self::ThreeCheck(pos) => {
                pos.finalize_incremental_zobrist_hash(intermediate, m)
            }
            Self::Crazyhouse(pos) => {
                pos.finalize_incremental_zobrist_hash(intermediate, m)
            }
            Self::RacingKings(pos) => {
                pos.finalize_incremental_zobrist_hash(intermediate, m)
            }
            Self::Horde(pos) => pos.finalize_incremental_zobrist_hash(intermediate, m),
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
