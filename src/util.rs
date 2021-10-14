// This file is part of the shakmaty library.
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

use std::{convert::TryFrom as _, num::TryFromIntError};

pub(crate) fn overflow_error() -> TryFromIntError {
    // This is a hack to construct TryFromIntError despite its private
    // constructor. The standard library keeps it private intentionally,
    // to be able to provide error details in the future, but it is unlikely
    // that something more specific than "overflow" will be added.
    u32::try_from(u64::MAX).unwrap_err()
}
