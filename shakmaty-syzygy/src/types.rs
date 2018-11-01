// This file is part of the shakmaty-syzygy library.
// Copyright (C) 2017-2018 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use std::fmt;
use std::ops::{Add, AddAssign, Neg, Sub, SubAssign};

use arrayvec::ArrayVec;

use shakmaty::variants::{Atomic, Chess, Giveaway};
use shakmaty::{Color, Outcome, Piece};

/// File extension and magic header bytes of Syzygy tables.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TableType {
    /// File extension, e.g. `rtbw`.
    pub ext: &'static str,
    /// Magic header bytes.
    pub magic: [u8; 4],
}

/// A chess variant with Syzygy support.
pub trait Syzygy {
    /// WDL table type.
    const TBW: TableType;
    /// DTZ table type.
    const TBZ: TableType;

    /// Alternative WDL table type for pawnless endgames.
    const PAWNLESS_TBW: Option<TableType> = None;
    /// Alternative DTZ table type for pawnless endgames.
    const PAWNLESS_TBZ: Option<TableType> = None;

    /// Whether both players will have exactly one king unless the game
    /// is over.
    const ONE_KING: bool;
    /// Wether kings are allowed to be on adjacent squares.
    const CONNECTED_KINGS: bool;
    /// Whether captures are compulsory.
    const CAPTURES_COMPULSORY: bool;

    /// Maximum number of supported pieces.
    const MAX_PIECES: usize = 6;
}

impl Syzygy for Chess {
    const TBW: TableType = TableType { ext: "rtbw", magic: [0x71, 0xe8, 0x23, 0x5d] };
    const TBZ: TableType = TableType { ext: "rtbz", magic: [0xd7, 0x66, 0x0c, 0xa5] };

    const ONE_KING: bool = true;
    const CONNECTED_KINGS: bool = false;
    const CAPTURES_COMPULSORY: bool = false;

    const MAX_PIECES: usize = 7;
}

impl Syzygy for Atomic {
    const TBW: TableType = TableType { ext: "atbw", magic: [0x55, 0x8d, 0xa4, 0x49] };
    const TBZ: TableType = TableType { ext: "atbz", magic: [0x91, 0xa9, 0x5e, 0xeb] };

    const ONE_KING: bool = true;
    const CONNECTED_KINGS: bool = true;
    const CAPTURES_COMPULSORY: bool = false;
}

impl Syzygy for Giveaway {
    const TBW: TableType = TableType { ext: "gtbw", magic: [0xbc, 0x55, 0xbc, 0x21] };
    const TBZ: TableType = TableType { ext: "gtbz", magic: [0xd6, 0xf5, 0x1b, 0x50] };

    const PAWNLESS_TBW: Option<TableType> = Some(TableType { ext: "stbw", magic: [0x7b, 0xf6, 0x93, 0x15] });
    const PAWNLESS_TBZ: Option<TableType> = Some(TableType { ext: "stbz", magic: [0xe4, 0xcf, 0xe7, 0x23] });

    const ONE_KING: bool = false;
    const CONNECTED_KINGS: bool = true;
    const CAPTURES_COMPULSORY: bool = true;
}

/// 5-valued evaluation of a position in the context of the 50-move drawing
/// rule.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(i8)]
pub enum Wdl {
    /// Unconditional loss for the side to move.
    Loss = -2,
    /// Loss that can be saved by the 50-move rule.
    BlessedLoss = -1,
    /// Unconditional draw.
    Draw = 0,
    /// Win that can be frustrated by the 50-move rule.
    CursedWin = 1,
    /// Unconditional win.
    Win = 2,
}

impl Wdl {
    /// Converts `outcome` to a `Wdl` from the given point of view.
    pub fn from_outcome(outcome: Outcome, pov: Color) -> Wdl {
        match outcome {
            Outcome::Draw => Wdl::Draw,
            Outcome::Decisive { winner } if winner == pov => Wdl::Win,
            _ => Wdl::Loss,
        }
    }

    /// Converts `dtz` to a `Wdl`.
    ///
    /// Typically the result would be ambiguous for absolute DTZ values 100.
    /// This conversion assumes that such values were given immediately after
    /// a capture or pawn move, in which case the outcome is an unconditional
    /// win or loss.
    pub fn from_dtz_after_zeroing(dtz: Dtz) -> Wdl {
        match dtz.0 {
            n if -100 <= n && n <= -1 => Wdl::Loss,
            n if n < -100 => Wdl::BlessedLoss,
            0 => Wdl::Draw,
            n if 100 < n => Wdl::CursedWin,
            _ => Wdl::Win,
        }
    }

    pub(crate) fn decisive(self) -> Option<DecisiveWdl> {
        Some(match self {
            Wdl::Loss => DecisiveWdl::Loss,
            Wdl::BlessedLoss => DecisiveWdl::BlessedLoss,
            Wdl::Draw => return None,
            Wdl::CursedWin => DecisiveWdl::CursedWin,
            Wdl::Win => DecisiveWdl::Win,
        })
    }
}

impl Neg for Wdl {
    type Output = Wdl;

    fn neg(self) -> Wdl {
        match self {
            Wdl::Loss => Wdl::Win,
            Wdl::BlessedLoss => Wdl::CursedWin,
            Wdl::Draw => Wdl::Draw,
            Wdl::CursedWin => Wdl::BlessedLoss,
            Wdl::Win => Wdl::Loss,
        }
    }
}

impl From<DecisiveWdl> for Wdl {
    fn from(wdl: DecisiveWdl) -> Wdl {
        match wdl {
            DecisiveWdl::Loss => Wdl::Loss,
            DecisiveWdl::BlessedLoss => Wdl::BlessedLoss,
            DecisiveWdl::CursedWin => Wdl::CursedWin,
            DecisiveWdl::Win => Wdl::Win,
        }
    }
}

macro_rules! from_wdl_impl {
    ($wdl:ty, $($t:ty)+) => {
        $(impl From<$wdl> for $t {
            #[inline]
            fn from(wdl: $wdl) -> $t {
                wdl as $t
            }
        })+
    }
}

from_wdl_impl! { Wdl, i8 i16 i32 i64 i128 isize }

#[cfg(feature = "serde-1")]
impl ::serde::Serialize for Wdl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ::serde::Serializer,
    {
        serializer.serialize_i8(i8::from(*self))
    }
}

#[cfg(feature = "serde-1")]
impl<'de> ::serde::Deserialize<'de> for Wdl {
    fn deserialize<D>(deserializer: D) -> Result<Wdl, D::Error>
    where
        D: ::serde::Deserializer<'de>,
    {
        struct Visitor;

        impl<'de> ::serde::de::Visitor<'de> for Visitor {
            type Value = Wdl;

            fn expecting(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                formatter.write_str("wdl value (-2, -1, 0, 1, 2)")
            }

            fn visit_i64<E>(self, value: i64) -> Result<Wdl, E>
            where
                E: ::serde::de::Error,
            {
                if -2 <= value && value <= 2 {
                    Ok(unsafe { ::std::mem::transmute(value as i8) })
                } else {
                    Err(E::custom(format!("wdl out of range: {}", value)))
                }
            }

            fn visit_u64<E>(self, value: u64) -> Result<Wdl, E>
            where
                E: ::serde::de::Error,
            {
                if value <= 2 {
                    Ok(unsafe { ::std::mem::transmute(value as i8) })
                } else {
                    Err(E::custom(format!("wdl out of range: {}", value)))
                }
            }
        }

        deserializer.deserialize_i8(Visitor)
    }
}

/// 4-valued evaluation of a decisive (not drawn) position in the context of
/// the 50-move rule.
#[doc(hidden)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum DecisiveWdl {
    /// Unconditional loss for the side to move.
    Loss = -2,
    /// Loss that can be saved by the 50-move rule.
    BlessedLoss = -1,
    /// Win that can be frustrated by the 50-move rule.
    CursedWin = 1,
    /// Unconditional win.
    Win = 2,
}

impl Neg for DecisiveWdl {
    type Output = DecisiveWdl;

    fn neg(self) -> DecisiveWdl {
        match self {
            DecisiveWdl::Loss => DecisiveWdl::Win,
            DecisiveWdl::BlessedLoss => DecisiveWdl::CursedWin,
            DecisiveWdl::CursedWin => DecisiveWdl::BlessedLoss,
            DecisiveWdl::Win => DecisiveWdl::Loss,
        }
    }
}

from_wdl_impl! { DecisiveWdl, i8 i16 i32 i64 i128 isize }

/// Distance to zeroing of the half-move clock.
///
/// Zeroing the half-move clock while keeping the game theoretical result in
/// hand guarantees making progress.
///
/// Can be off by one: `Dtz(-n)` can mean a loss in `n + 1` plies and `Dtz(n)`
/// can mean a win in `n + 1` plies. This is guaranteed not to happen for
/// positions exactly on the edge of the 50-move rule, so that (with some care)
/// this never impacts results of practical play.
///
/// | DTZ | WDL | |
/// | --- | --- | --- |
/// | `-100 <= n <= -1` | Loss | Unconditional loss (assuming the 50-move counter is zero). Zeroing move can be forced in `-n` plies. |
/// | `n < -100` | Blessed loss | Loss, but draw under the 50-move rule. A zeroing move can be forced in `-n` plies or `-n - 100` plies (if a later phase is responsible for the blessing). |
/// | 0 | Draw | |
/// | `100 < n` | Cursed win | Win, but draw under the 50-move rule. A zeroing move can be forced in `n` or `n - 100` plies (if a later phase is responsible for the curse). |
/// | `1 <= n <= 100` | Win | Unconditional win (assuming the 50-move counter is zero). Zeroing move can be forced in `n` plies. |
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Dtz(pub i32);

impl Dtz {
    /// Converts `wdl` to a DTZ, given that the best move is zeroing.
    ///
    /// | WDL | DTZ |
    /// | --- | --- |
    /// | Loss | -1 |
    /// | Blessed loss | -101 |
    /// | Draw | 0 |
    /// | Cursed win | 101 |
    /// | Win | 1 |
    pub fn before_zeroing<T: Into<Wdl>>(wdl: T) -> Dtz {
        match wdl.into() {
            Wdl::Loss => Dtz(-1),
            Wdl::BlessedLoss => Dtz(-101),
            Wdl::Draw => Dtz(0),
            Wdl::CursedWin => Dtz(101),
            Wdl::Win => Dtz(1),
        }
    }

    /// Increases the absolute value by `plies`.
    pub fn add_plies(self, plies: i32) -> Dtz {
        let new_dtz = self.0.signum() * (self.0.abs() + plies);
        debug_assert!(self.0.signum() == new_dtz.signum());
        Dtz(new_dtz)
    }
}

macro_rules! from_dtz_impl {
    ($($t:ty)+) => {
        $(impl From<Dtz> for $t {
            #[inline]
            fn from(wdl: Dtz) -> $t {
                wdl.0.into()
            }
        })+
    }
}

from_dtz_impl! { i32 i64 i128 }

macro_rules! dtz_from_impl {
    ($($t:ty)+) => {
        $(impl From<$t> for Dtz {
            #[inline]
            fn from(dtz: $t) -> Dtz {
                Dtz(i32::from(dtz))
            }
        })+
    }
}

dtz_from_impl! { u8 i8 u16 i16 i32 }

impl Neg for Dtz {
    type Output = Dtz;

    #[inline]
    fn neg(self) -> Dtz {
        Dtz(-self.0)
    }
}

impl Add for Dtz {
    type Output = Dtz;

    #[inline]
    fn add(self, other: Dtz) -> Dtz {
        Dtz(self.0 + other.0)
    }
}

impl AddAssign for Dtz {
    #[inline]
    fn add_assign(&mut self, other: Dtz) {
        self.0 += other.0;
    }
}

impl Sub for Dtz {
    type Output = Dtz;

    #[inline]
    fn sub(self, other: Dtz) -> Dtz {
        Dtz(self.0 - other.0)
    }
}

impl SubAssign for Dtz {
    #[inline]
    fn sub_assign(&mut self, other: Dtz) {
        self.0 -= other.0;
    }
}

#[cfg(feature = "serde-1")]
impl ::serde::Serialize for Dtz {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ::serde::Serializer,
    {
        serializer.serialize_i32(i32::from(*self))
    }
}

#[cfg(feature = "serde-1")]
impl<'de> ::serde::Deserialize<'de> for Dtz {
    fn deserialize<D>(deserializer: D) -> Result<Dtz, D::Error>
    where
        D: ::serde::Deserializer<'de>,
    {
        struct Visitor;

        impl<'de> ::serde::de::Visitor<'de> for Visitor {
            type Value = Dtz;

            fn expecting(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                formatter.write_str("dtz value (i32)")
            }

            fn visit_i64<E>(self, value: i64) -> Result<Dtz, E>
            where
                E: ::serde::de::Error,
            {
                if i64::from(i32::min_value()) <= value && value <= i64::from(i32::max_value()) {
                    Ok(Dtz(value as i32))
                } else {
                    Err(E::custom(format!("dtz out of range: {}", value)))
                }
            }

            fn visit_u64<E>(self, value: u64) -> Result<Dtz, E>
            where
                E: ::serde::de::Error,
            {
                if value <= i32::max_value() as u64 {
                    Ok(Dtz(value as i32))
                } else {
                    Err(E::custom(format!("dtz out of range: {}", value)))
                }
            }
        }

        deserializer.deserialize_i32(Visitor)
    }
}

/// Syzygy tables are available for up to 7 pieces.
pub const MAX_PIECES: usize = 7;

/// List of up to `MAX_PIECES` pieces.
pub type Pieces = ArrayVec<[Piece; MAX_PIECES]>;

/// Metric stored in a table: WDL or DTZ.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Metric {
    Wdl,
    Dtz,
}

impl fmt::Display for Metric {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Metric::Wdl => f.write_str("wdl"),
            Metric::Dtz => f.write_str("dtz"),
        }
    }
}
