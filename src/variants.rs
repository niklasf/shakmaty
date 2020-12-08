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

//! Chess variants.
//!
//! These are games played with normal chess pieces but special rules.
//! Every chess variant implements [`Setup`], [`FromSetup`] and [`Position`].
//!
//! [`Setup`]: ../trait.Setup.html
//! [`FromSetup`]: ../trait.FromSetup.html
//! [`Position`]: ../trait.Position.html

use std::num::NonZeroU32;

pub use crate::Chess;
pub use crate::position::Atomic;
pub use crate::position::Antichess;
pub use crate::position::KingOfTheHill;
pub use crate::position::ThreeCheck;
pub use crate::position::Crazyhouse;
pub use crate::position::RacingKings;
pub use crate::position::Horde;

use crate::{Board, Color, Bitboard, Square, Material, RemainingChecks};
use crate::{Role, Move, MoveList, CastlingSide, Outcome, Castles};
use crate::{Setup, FromSetup, Position, PositionError};
use crate::setup::SwapTurn;

/// Discriminant of [`VariantPosition`].
///
/// [`VariantPosition`]: enum.VariantPosition.html
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum Variant {
    Chess,
    Atomic,
    Antichess,
    KingOfTheHill,
    ThreeCheck,
    Crazyhouse,
    RacingKings,
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

    pub fn distinguishes_promoted(self) -> bool {
        self == Variant::Crazyhouse
    }
}

/// Dynamically dispatched chess variant [`Position`].
///
/// [`Position`]: ../trait.Position.html
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

    pub fn from_setup(variant: Variant, setup: &dyn Setup) -> Result<VariantPosition, PositionError> {
        match variant {
            Variant::Chess => Chess::from_setup(setup).map(VariantPosition::Chess),
            Variant::Atomic => Atomic::from_setup(setup).map(VariantPosition::Atomic),
            Variant::Antichess => Antichess::from_setup(setup).map(VariantPosition::Antichess),
            Variant::KingOfTheHill => KingOfTheHill::from_setup(setup).map(VariantPosition::KingOfTheHill),
            Variant::ThreeCheck => ThreeCheck::from_setup(setup).map(VariantPosition::ThreeCheck),
            Variant::Crazyhouse => Crazyhouse::from_setup(setup).map(VariantPosition::Crazyhouse),
            Variant::RacingKings => RacingKings::from_setup(setup).map(VariantPosition::RacingKings),
            Variant::Horde => Horde::from_setup(setup).map(VariantPosition::Horde),
        }
    }

    pub fn swap_turn(self) -> Result<VariantPosition, PositionError> {
        VariantPosition::from_setup(self.variant(), &SwapTurn(self))
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

impl Setup for VariantPosition {
    fn board(&self) -> &Board { self.borrow().board() }
    fn pockets(&self) -> Option<&Material> { self.borrow().pockets() }
    fn turn(&self) -> Color { self.borrow().turn() }
    fn castling_rights(&self) -> Bitboard { self.borrow().castling_rights() }
    fn ep_square(&self) -> Option<Square> { self.borrow().ep_square() }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { self.borrow().remaining_checks() }
    fn halfmoves(&self) -> u32 { self.borrow().halfmoves() }
    fn fullmoves(&self) -> NonZeroU32 { self.borrow().fullmoves() }
}

impl Position for VariantPosition {
    fn legal_moves(&self, moves: &mut MoveList) { self.borrow().legal_moves(moves) }
    fn san_candidates(&self, role: Role, to: Square, moves: &mut MoveList) { self.borrow().san_candidates(role, to, moves) }
    fn castling_moves(&self, side: CastlingSide, moves: &mut MoveList) { self.borrow().castling_moves(side, moves) }
    fn en_passant_moves(&self, moves: &mut MoveList) { self.borrow().en_passant_moves(moves) }
    fn capture_moves(&self, moves: &mut MoveList) { self.borrow().capture_moves(moves) }
    fn promotion_moves(&self, moves: &mut MoveList) { self.borrow().promotion_moves(moves) }
    fn is_irreversible(&self, m: &Move) -> bool { self.borrow().is_irreversible(m) }
    fn king_attackers(&self, square: Square, attacker: Color, occupied: Bitboard) -> Bitboard { self.borrow().king_attackers(square, attacker, occupied) }
    fn castles(&self) -> &Castles { self.borrow().castles() }
    fn checkers(&self) -> Bitboard { self.borrow().checkers() }
    fn is_variant_end(&self) -> bool { self.borrow().is_variant_end() }
    fn has_insufficient_material(&self, color: Color) -> bool { self.borrow().has_insufficient_material(color) }
    fn variant_outcome(&self) -> Option<Outcome> { self.borrow().variant_outcome() }
    fn play_unchecked(&mut self, m: &Move) { self.borrow_mut().play_unchecked(m) }
}
