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

use crate::types::Move;

use arrayvec::ArrayVec;

/// A container for moves that can be stored inline on the stack.
///
/// The capacity is limited, but there is enough space to hold the legal
/// moves of any chess position, including any of the supported chess variants.
///
/// # Example
///
/// ```
/// use shakmaty::{Chess, Position, Role};
///
/// let pos = Chess::default();
/// let mut moves = pos.legal_moves();
/// moves.retain(|m| m.role() == Role::Pawn);
/// assert_eq!(moves.len(), 16);
/// ```
pub type MoveList = ArrayVec<[Move; 512]>;
