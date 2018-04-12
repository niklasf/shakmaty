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

use std::ops::{Neg, Add, AddAssign, Sub, SubAssign};
use std::option::NoneError;
use std::io;

use arrayvec::ArrayVec;

use shakmaty::{Color, Piece, Outcome, Chess};

/// A chess variant with Syzygy support.
pub trait Syzygy {
    /// Extension of WDL table files, e.g. `rtbw`.
    const TBW_EXTENSION: &'static str;
    /// Extension of DTZ table files, e.g. `rtbz`.
    const TBZ_EXTENSION: &'static str;
    /// Alternative extension of WDL table files.
    const PAWNLESS_TBW_EXTENSION: &'static str;
    /// Alternative extension of DTZ table files.
    const PAWNLESS_TBZ_EXTENSION: &'static str;

    /// Magic initial bytes of a WDL table.
    const WDL_MAGIC: [u8; 4];
    /// Magic initial bytes of a DTZ table.
    const DTZ_MAGIC: [u8; 4];
    /// Alternative WDL magic.
    const PAWNLESS_WDL_MAGIC: [u8; 4];
    /// Alternative DTZ magic.
    const PAWNLESS_DTZ_MAGIC: [u8; 4];

    /// Whether both players will have exactly one king unless the game
    /// is over.
    const ONE_KING: bool;
    /// Wether kings are allowed to be on adjacent squares.
    const CONNECTED_KINGS: bool;
    /// Whether captures are compulsory.
    const CAPTURES_COMPULSORY: bool;
}

impl Syzygy for Chess {
    const TBW_EXTENSION: &'static str = "rtbw";
    const TBZ_EXTENSION: &'static str = "rtbz";
    const PAWNLESS_TBW_EXTENSION: &'static str = "rtbw";
    const PAWNLESS_TBZ_EXTENSION: &'static str = "rtbz";

    const WDL_MAGIC: [u8; 4] = [0x71, 0xe8, 0x23, 0x5d];
    const DTZ_MAGIC: [u8; 4] = [0xd7, 0x66, 0x0c, 0xa5];
    const PAWNLESS_WDL_MAGIC: [u8; 4] = [0x71, 0xe8, 0x23, 0x5d];
    const PAWNLESS_DTZ_MAGIC: [u8; 4] = [0xd7, 0x66, 0x0c, 0xa5];

    const ONE_KING: bool = true;
    const CONNECTED_KINGS: bool = false;
    const CAPTURES_COMPULSORY: bool = false;
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
    pub fn from_outcome(outcome: &Outcome, pov: Color) -> Wdl {
        match *outcome {
            Outcome::Draw => Wdl::Draw,
            Outcome::Decisive { winner } if winner == pov => Wdl::Win,
            _ => Wdl::Loss,
        }
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

macro_rules! from_wdl_impl {
    ($($t:ty)+) => {
        $(impl From<Wdl> for $t {
            #[inline]
            fn from(wdl: Wdl) -> $t {
                wdl as $t
            }
        })+
    }
}

from_wdl_impl! { i8 i16 i32 i64 isize }

/// Distance to zeroing.
///
/// Can be off by one: `Dtz(-n)` can mean a loss in `n + 1` plies and `Dtz(n)`
/// can mean a win in `n + 1` plies. This is guaranteed not to happen for
/// positions exactly on the edge of the 50-move rule, so that this never
/// impacts results of practical play.
///
/// | DTZ | WDL | |
/// | --- | --- | --- |
/// | `-100 <= n <= -1` | Loss | Unconditional loss (assuming the 50-move counter is zero). Zeroing move can be forced in `-n` plies. |
/// | `n < -100` | Blessed loss | Loss, but draw under the 50-move rule. A zeroing move can be forced in `-n` plies or `-n - 100` plies (if a later phase is responsible for the blessing). |
/// | 0 | Draw | |
/// | `100 < n` | Cursed win | Win, but draw under the 50-move rule. A zeroing move can be forced in `n` or `n - 100` plies (if a later phase is responsible for the curse). |
/// | `1 <= n <= 100` | Win | Unconditional win (assuming the 50-move counter is zero). Zeroing move can be forced in `n` plies. |
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Dtz(pub i16);

impl Dtz {
    pub fn before_zeroing(wdl: Wdl) -> Dtz {
        match wdl {
            Wdl::Loss => Dtz(-1),
            Wdl::BlessedLoss => Dtz(-101),
            Wdl::Draw => Dtz(0),
            Wdl::CursedWin => Dtz(101),
            Wdl::Win => Dtz(1),
        }
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

from_dtz_impl! { i16 i32 i64 isize }

macro_rules! dtz_from_impl {
    ($($t:ty)+) => {
        $(impl From<$t> for Dtz {
            #[inline]
            fn from(dtz: $t) -> Dtz {
                Dtz(i16::from(dtz))
            }
        })+
    }
}

dtz_from_impl! { u8 i8 i16 }

impl From<Dtz> for Wdl {
    fn from(dtz: Dtz) -> Wdl {
        match dtz.0 {
            n if -100 <= n && n <= -1 => Wdl::Loss,
            n if n < -100 => Wdl::BlessedLoss,
            0 => Wdl::Draw,
            n if 100 < n => Wdl::CursedWin,
            _ => Wdl::Win,
        }
    }
}

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

/// Syzygy tables are available for up to 7 pieces.
pub const MAX_PIECES: usize = 7;

/// List of up to 7 pieces.
pub type Pieces = ArrayVec<[Piece; MAX_PIECES]>;

/// Error when probing a table.
///
/// Possible causes:
///
/// * Position has castling rights or too many pieces
/// * Missing table
/// * I/O error
/// * Unexpected magic header bytes
/// * Corrupted table
#[derive(Debug, Clone, PartialEq, Eq, Fail)]
pub enum SyzygyError {
    #[fail(display = "syzygy tables do not contain positions with castling rights")]
    Castling,
    #[fail(display = "syzygy tables only contain positions with up to 7 pieces")]
    TooManyPieces,
    #[fail(display = "required table not found")]
    MissingTable,
    #[fail(display = "i/o error when reading a table")]
    Read,
    #[fail(display = "invalid magic bytes")]
    Magic,
    #[fail(display = "corrupted table")]
    CorruptedTable,
}

impl From<NoneError> for SyzygyError {
    fn from(_: NoneError) -> SyzygyError {
        SyzygyError::CorruptedTable
    }
}

impl From<io::Error> for SyzygyError {
    fn from(error: io::Error) -> SyzygyError {
        match error.kind() {
            io::ErrorKind::UnexpectedEof => SyzygyError::CorruptedTable,
            _ => SyzygyError::Read,
        }
    }
}

/// A [`Result`] type for Syzygy tablebase probes.
///
/// [`Result`]: https://doc.rust-lang.org/std/result/enum.Result.html
pub type SyzygyResult<T> = Result<T, SyzygyError>;
