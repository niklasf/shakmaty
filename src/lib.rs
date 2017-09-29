// This file is part of the shakmaty library.
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

//! A library for chess move generation.
//!
//! # Examples
//!
//! Generate legal moves in the starting position:
//!
//! ```
//! use shakmaty::{Chess, Position, MoveList};
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
//! # fn try_main() -> Result<(), Box<Error>> {
//! use shakmaty::{Move, Role};
//! use shakmaty::square;
//! # use shakmaty::{Chess, Position};
//! # let pos = Chess::default();
//!
//! // 1. e4
//! let pos = pos.play(&Move::Normal {
//!     role: Role::Pawn,
//!     from: square::E2,
//!     to: square::E4,
//!     capture: None,
//!     promotion: None,
//! })?;
//! #
//! #     Ok(())
//! # }
//! #
//! # fn main() {
//! #     try_main().unwrap();
//! # }
//! ```
//!
//! Detect game end conditions:
//!
//! ```
//! # use shakmaty::{Chess, Position};
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

#![doc(html_root_url = "https://docs.rs/shakmaty/0.0.10")]

#![cfg_attr(test, feature(test))]
#![feature(const_fn)]
#![feature(ascii_ctype)]
#![feature(exact_size_is_empty)]
#![feature(cfg_target_feature)]
#![feature(platform_intrinsics)]

#[cfg(test)]
extern crate test;

extern crate arrayvec;
extern crate option_filter;
extern crate btoi;

mod types;
mod bitboard;
mod board;
mod position;
mod setup;
mod movelist;
mod magics;
mod perft;

pub mod square;
pub mod attacks;
pub mod fen;
pub mod uci;
pub mod san;

pub use square::Square;
pub use types::{Color, Role, Piece, Move, Pocket, Pockets, RemainingChecks};
pub use bitboard::{Bitboard, CarryRippler};
pub use board::{Board, Pieces};
pub use setup::{Setup, CastlingSide};
pub use movelist::MoveList;
pub use position::{IllegalMove, Outcome, Position, PositionError, Chess};
pub use perft::perft;
