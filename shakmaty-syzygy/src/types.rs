// This file is part of the shakmaty-syzygy library.
// Copyright (C) 2017 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use std::ops::Neg;
use std::option::NoneError;
use std::error::Error;
use std::io;
use std::fmt;

use arrayvec::ArrayVec;

use shakmaty::{Color, Piece, Outcome, Chess};

/// A chess variant with Syzygy support.
pub trait Syzygy {
    /// Extension of WDL table files, e.g. `rtbw`.
    const TBW_EXTENSION: &'static str;
    /// Extension of DTZ table files, e.g. `rtbz`.
    const TBZ_EXTENSION: &'static str;

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

    const WDL_MAGIC: [u8; 4] = [0x71, 0xe8, 0x23, 0x5d];
    const DTZ_MAGIC: [u8; 4] = [0xd7, 0x66, 0x0c, 0xa5];
    const PAWNLESS_WDL_MAGIC: [u8; 4] = [0x71, 0xe8, 0x23, 0x5d];
    const PAWNLESS_DTZ_MAGIC: [u8; 4] = [0xd7, 0x66, 0x0c, 0xa5];

    const ONE_KING: bool = true;
    const CONNECTED_KINGS: bool = false;
    const CAPTURES_COMPULSORY: bool = false;
}

/// A 5-valued evaluation of a position in the context of the 50-move drawing
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
    pub(crate) fn from_outcome(outcome: Outcome, pov: Color) -> Wdl {
        match outcome {
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

impl From<Wdl> for i8 {
    #[inline]
    fn from(wdl: Wdl) -> i8 {
        wdl as i8
    }
}

/// Syzygy tables are available for up to 6 pieces.
pub const MAX_PIECES: usize = 6;

pub type Pieces = ArrayVec<[Piece; MAX_PIECES]>;

/// Error initializing or probing a table.
///
/// * Unexpected magic header bytes
/// * Corrupted table
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyzygyError {
    kind: ErrorKind,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum ErrorKind {
    Magic,
    CorruptedTable,
    Read,
    MissingTable,
    Castling,
    TooManyPieces,
}

impl SyzygyError {
    pub(crate) fn new(kind: ErrorKind) -> SyzygyError {
        SyzygyError { kind }
    }

    fn desc(&self) -> &str {
        match self.kind {
            ErrorKind::Magic => "invalid magic bytes",
            ErrorKind::CorruptedTable => "corrupted table",
            ErrorKind::Read => "i/o error when reading a table",
            ErrorKind::MissingTable => "required table not found",
            ErrorKind::Castling => "syzygy tables do not contain positions with castling rights",
            ErrorKind::TooManyPieces => "syzygy tables only contain positions with up to 6 pieces",
        }
    }
}

impl fmt::Display for SyzygyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.desc().fmt(f)
    }
}

impl Error for SyzygyError {
    fn description(&self) -> &str {
        self.desc()
    }
}

impl From<NoneError> for SyzygyError {
    fn from(_: NoneError) -> SyzygyError {
        SyzygyError { kind: ErrorKind::CorruptedTable }
    }
}

impl From<io::Error> for SyzygyError {
    fn from(error: io::Error) -> SyzygyError {
        match error.kind() {
            io::ErrorKind::UnexpectedEof => SyzygyError { kind: ErrorKind::CorruptedTable },
            _ => SyzygyError { kind: ErrorKind::Read },
        }
    }
}

pub type SyzygyResult<T> = Result<T, SyzygyError>;
