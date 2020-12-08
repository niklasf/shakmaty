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

//! A library for chess move generation.
//!
//! # Examples
//!
//! Generate legal moves in the starting position:
//!
//! ```
//! use shakmaty::{Chess, Position, PositionExt};
//!
//! let pos = Chess::default();
//! let legals = pos.legals();
//! assert_eq!(legals.len(), 20);
//! ```
//!
//! Play moves:
//!
//! ```
//! # use std::error::Error;
//! #
//! use shakmaty::{Square, Move, Role};
//! # use shakmaty::{Chess, Position, PositionExt};
//! # let pos = Chess::default();
//!
//! // 1. e4
//! let pos = pos.play(&Move::Normal {
//!     role: Role::Pawn,
//!     from: Square::E2,
//!     to: Square::E4,
//!     capture: None,
//!     promotion: None,
//! })?;
//! #
//! # Ok::<_, Box<dyn Error>>(())
//! ```
//!
//! Detect game end conditions:
//!
//! ```
//! # use shakmaty::{Chess, Position, PositionExt};
//! # let pos = Chess::default();
//! assert!(!pos.is_checkmate());
//! assert!(!pos.is_stalemate());
//! assert!(!pos.is_insufficient_material());
//!
//! assert_eq!(pos.outcome(), None); // no winner yet
//! ```
//!
//! Also supports [FEN](fen/index.html), [SAN](san/index.html) and
//! [UCI](uci/index.html) formats for positions and moves.

#![doc(html_root_url = "https://docs.rs/shakmaty/0.16.4")]

#![warn(missing_debug_implementations)]
#![warn(rust_2018_idioms)]

mod errors;
mod square;
mod types;
mod material;
mod board;
mod position;
mod setup;
mod movelist;
mod magics;
mod perft;

pub mod attacks;
pub mod bitboard;
pub mod fen;
pub mod uci;
pub mod san;
pub mod variants;

pub use crate::errors::{TryFromIntError, TryFromFloatError};
pub use crate::square::{ParseSquareError, File, Rank, Square};
pub use crate::types::{CastlingSide, CastlingMode, Color, Move, Piece, RemainingChecks, Role};
pub use crate::material::{Material, MaterialSide, ParseMaterialError};
pub use crate::bitboard::Bitboard;
pub use crate::board::{Board, Pieces};
pub use crate::setup::{Castles, Setup};
pub use crate::movelist::MoveList;
pub use crate::position::{Chess, IllegalMoveError, Outcome, Position, PositionExt, FromSetup, PositionError};
pub use crate::perft::perft;
