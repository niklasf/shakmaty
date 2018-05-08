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
//! use shakmaty::{Chess, Position};
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
//! use shakmaty::{Square, Move, Role};
//! # use shakmaty::{Chess, Position};
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

#![doc(html_root_url = "https://docs.rs/shakmaty/0.6.6")]

#![warn(missing_debug_implementations)]

#![cfg_attr(nightly, feature(exact_size_is_empty))]
#![cfg_attr(nightly, feature(fused))]
#![cfg_attr(nightly, feature(trusted_len))]
#![cfg_attr(nightly, feature(option_filter))]

extern crate arrayvec;
#[macro_use]
extern crate bitflags;
extern crate btoi;
extern crate option_filter;

mod square;
mod types;
mod bitboard;
mod board;
mod position;
mod setup;
mod movelist;
mod magics;
mod perft;

pub mod attacks;
pub mod fen;
pub mod uci;
pub mod san;

pub use square::{InvalidSquareName, Square};
pub use types::{CastlingSide, Color, Move, Piece, Pocket, Pockets, RemainingChecks, Role};
pub use bitboard::{Bitboard, CarryRippler};
pub use board::{Board, Pieces};
pub use setup::Setup;
pub use movelist::MoveList;
pub use position::{Chess, IllegalMove, Outcome, Position, PositionError};
pub use perft::perft;

pub mod variants {
    //! Chess variants.
    pub use Chess;
    pub use position::Atomic;
    pub use position::Giveaway;
    pub use position::KingOfTheHill;
}
