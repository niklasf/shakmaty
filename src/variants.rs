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

pub use crate::Chess;
pub use crate::position::Atomic;
pub use crate::position::Giveaway;
pub use crate::position::KingOfTheHill;
pub use crate::position::ThreeCheck;
pub use crate::position::Crazyhouse;
pub use crate::position::RacingKings;
pub use crate::position::Horde;

use crate::Setup;
use crate::FromSetup;
use crate::PositionError;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum Variant {
    Chess,
    Atomic,
    Giveaway,
    KingOfTheHill,
    ThreeCheck,
    Crazyhouse,
    RacingKings,
    Horde,
}

#[derive(Debug, Clone)]
pub enum VariantPosition {
    Chess(Chess),
    Atomic(Atomic),
    Giveaway(Giveaway),
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

impl From<Giveaway> for VariantPosition {
    fn from(pos: Giveaway) -> VariantPosition {
        VariantPosition::Giveaway(pos)
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
    fn new(variant: Variant) -> VariantPosition {
        match variant {
            Variant::Chess => Chess::default().into(),
            Variant::Atomic => Atomic::default().into(),
            Variant::Giveaway => Giveaway::default().into(),
            Variant::KingOfTheHill => KingOfTheHill::default().into(),
            Variant::ThreeCheck => ThreeCheck::default().into(),
            Variant::Crazyhouse => Crazyhouse::default().into(),
            Variant::RacingKings => RacingKings::default().into(),
            Variant::Horde => Horde::default().into(),
        }
    }

    fn from_setup(variant: Variant, setup: &dyn Setup) -> Result<VariantPosition, PositionError> {
        match variant {
            Variant::Chess => Chess::from_setup(setup).map(VariantPosition::Chess),
            Variant::Atomic => Atomic::from_setup(setup).map(VariantPosition::Atomic),
            Variant::Giveaway => Giveaway::from_setup(setup).map(VariantPosition::Giveaway),
            Variant::KingOfTheHill => KingOfTheHill::from_setup(setup).map(VariantPosition::KingOfTheHill),
            Variant::ThreeCheck => ThreeCheck::from_setup(setup).map(VariantPosition::ThreeCheck),
            Variant::Crazyhouse => Crazyhouse::from_setup(setup).map(VariantPosition::Crazyhouse),
            Variant::RacingKings => RacingKings::from_setup(setup).map(VariantPosition::RacingKings),
            Variant::Horde => Horde::from_setup(setup).map(VariantPosition::Horde),
        }
    }
}
