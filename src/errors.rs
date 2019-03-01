// This file is part of the shakmaty library.
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
use std::error::Error;

/// Error when integer is out of range.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TryFromIntError;

impl fmt::Display for TryFromIntError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "out of range integral type conversion attempted".fmt(f)
    }
}

impl Error for TryFromIntError {
    fn description(&self) -> &str {
        "out of range integral type conversion attempted"
    }
}

impl From<()> for TryFromIntError {
    fn from(_: ()) ->TryFromIntError {
        TryFromIntError
    }
}

/// Error when float is out of range.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TryFromFloatError;

impl fmt::Display for TryFromFloatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "out of range floating point conversion attempted".fmt(f)
    }
}

impl Error for TryFromFloatError {
    fn description(&self) -> &str {
        "out of range floating point conversion attempted"
    }
}

impl From<()> for TryFromFloatError {
    fn from(_: ()) ->TryFromFloatError {
        TryFromFloatError
    }
}
