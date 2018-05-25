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

use std::io;

use material::Material;

/// A [`Result`] type for Syzygy tablebase probes.
///
/// [`Result`]: https://doc.rust-lang.org/std/result/enum.Result.html
pub type SyzygyResult<T> = Result<T, SyzygyError>;

/// Error when probing a table.
#[derive(Debug, Fail)]
pub enum SyzygyError {
    /// Position has castling rights, but Syzygy tables do not contain
    /// positions with castling rights.
    #[fail(display = "syzygy tables do not contain positions with castling rights")]
    Castling,
    /// Position has too many pieces. Syzygy tables only support up to
    /// 7 pieces.
    #[fail(display = "syzygy tables only contain positions with up to 7 pieces")]
    TooManyPieces,
    /// Missing table.
    #[fail(display = "required table not found: {}", material)]
    MissingTable {
        material: Material
    },
    /// I/O error.
    #[fail(display = "i/o error when reading a table: {}", error)]
    Read {
        #[cause]
        error: io::Error
    },
    /// Table file has unexpected magic header bytes.
    #[fail(display = "table file has invalid magic bytes")]
    Magic,
    /// Corrupted table.
    #[fail(display = "corrupted table (detected in {} l. {})", file, line)]
    CorruptedTable {
        file: &'static str,
        line: u32
    },
}

impl From<io::Error> for SyzygyError {
    fn from(error: io::Error) -> SyzygyError {
        match error.kind() {
            io::ErrorKind::UnexpectedEof => SyzygyError::CorruptedTable { file: file!(), line: line!() },
            _ => SyzygyError::Read { error },
        }
    }
}

/// Return a `CorruptedTable` error.
macro_rules! throw {
    () => {
        return Err(SyzygyError::CorruptedTable {
            file: file!(),
            line: line!(),
        })
    }
}

/// Unwrap an `Option` or return a `CorruptedTable` error.
macro_rules! u {
    ($e:expr) => {
        match $e {
            Some(ok) => ok,
            None => throw!(),
        }
    }
}

/// Ensure that a condition holds. Otherwise return a `CorruptedTable` error.
macro_rules! ensure {
    ($cond:expr) => {
        if !$cond {
            throw!();
        }
    }
}
