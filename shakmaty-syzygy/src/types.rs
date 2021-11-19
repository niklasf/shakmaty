// This file is part of the shakmaty-syzygy library.
// Copyright (C) 2017-2021 Niklas Fiekas <niklas.fiekas@backscattering.de>
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
use std::ops::Neg;

use arrayvec::ArrayVec;

use shakmaty::{Chess, Color, Outcome, Piece};

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

#[cfg(feature = "variant")]
impl Syzygy for shakmaty::variant::Atomic {
    const TBW: TableType = TableType { ext: "atbw", magic: [0x55, 0x8d, 0xa4, 0x49] };
    const TBZ: TableType = TableType { ext: "atbz", magic: [0x91, 0xa9, 0x5e, 0xeb] };

    const ONE_KING: bool = true;
    const CONNECTED_KINGS: bool = true;
    const CAPTURES_COMPULSORY: bool = false;
}

#[cfg(feature = "variant")]
impl Syzygy for shakmaty::variant::Antichess {
    const TBW: TableType = TableType { ext: "gtbw", magic: [0xbc, 0x55, 0xbc, 0x21] };
    const TBZ: TableType = TableType { ext: "gtbz", magic: [0xd6, 0xf5, 0x1b, 0x50] };

    const PAWNLESS_TBW: Option<TableType> = Some(TableType { ext: "stbw", magic: [0x7b, 0xf6, 0x93, 0x15] });
    const PAWNLESS_TBZ: Option<TableType> = Some(TableType { ext: "stbz", magic: [0xe4, 0xcf, 0xe7, 0x23] });

    const ONE_KING: bool = false;
    const CONNECTED_KINGS: bool = true;
    const CAPTURES_COMPULSORY: bool = true;
}

/// WDL<sub>50</sub>. 5-valued evaluation of a position in the context of the
/// 50-move drawing rule.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(i32)]
pub enum Wdl {
    /// Unconditional loss.
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
            n if n < -100 => Wdl::BlessedLoss,
            n if n < 0 => Wdl::Loss,
            0 => Wdl::Draw,
            n if n <= 100 => Wdl::Win,
            _ => Wdl::CursedWin,
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

    /// Returns a number representing the sign of `self`.
    ///
    /// * `1` if `self > Wdl::Draw`
    /// * `0` if `self == Wdl::Draw`
    /// * `-1` if `self < Wdl::Draw`
    pub fn signum(self) -> i32 {
        i32::from(self).signum()
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

/// 4-valued evaluation of a decisive (not drawn) position in the context of
/// the 50-move rule.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(i32)]
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

impl DecisiveWdl {
    pub fn signum(self) -> i32 {
        i32::from(self).signum()
    }
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

/// DTZ<sub>50</sub>′′ with rounding. Based on the distance to zeroing of the
/// half-move clock.
///
/// Zeroing the half-move clock while keeping the game theoretical result in
/// hand guarantees making progress.
///
/// Can be off by one due to
/// [rounding](http://www.talkchess.com/forum3/viewtopic.php?f=7&t=58488#p651293):
/// `Dtz(-n)` can mean a loss in `n + 1` plies and
/// `Dtz(n)` can mean a win in `n + 1` plies.
/// This implies some primary tablebase lines may waste up to 1 ply.
/// Rounding is never used for endgame phases where it would change the game
/// theoretical outcome.
///
/// This means users need to be careful in positions that are nearly drawn
/// under the 50-move rule! Carelessly wasting 1 more ply by not following the
/// tablebase recommendation, for a total of 2 wasted plies, may change the
/// outcome of the game.
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
    #[must_use]
    pub fn add_plies(self, plies: i32) -> Dtz {
        let new_dtz = self.0.signum() * (self.0.abs() + plies);
        debug_assert!(self.0.signum() == new_dtz.signum());
        Dtz(new_dtz)
    }

    /// Returns a number representing the sign of `self`.
    ///
    /// * `1` if `self > Dtz(0)`
    /// * `0` if `self == Dtz(0)`
    /// * `-1` if `self < Dtz(0)`
    pub fn signum(self) -> i32 {
        self.0.signum()
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

/// Syzygy tables are available for up to 7 pieces.
pub const MAX_PIECES: usize = 7;

/// List of up to `MAX_PIECES` pieces.
pub type Pieces = ArrayVec<Piece, MAX_PIECES>;

/// Metric stored in a table: WDL or DTZ.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Metric {
    Wdl,
    Dtz,
}

impl fmt::Display for Metric {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            Metric::Wdl => "wdl",
            Metric::Dtz => "dtz",
        })
    }
}
