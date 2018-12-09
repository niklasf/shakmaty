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

#![doc(html_root_url = "https://docs.rs/shakmaty/0.13.0")]

#![warn(missing_debug_implementations)]
#![warn(bare_trait_objects)]

use btoi;

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

pub use crate::square::{ParseSquareError, File, Rank, Square};
pub use crate::types::{CastlingSide, Color, Move, Piece, RemainingChecks, Role};
pub use crate::material::{Material, MaterialSide, ParseMaterialError};
pub use crate::bitboard::Bitboard;
pub use crate::board::{Board, Pieces};
pub use crate::setup::{Castles, Setup};
pub use crate::movelist::MoveList;
pub use crate::position::{Chess, IllegalMoveError, Outcome, Position, PositionError};
pub use crate::perft::perft;

pub mod variants {
    //! Chess variants.
    //!
    //! These are games played with normal chess pieces but special rules.
    //! Every chess variant implements [`Setup`] and [`Position`].
    //!
    //! [`Setup`]: ../trait.Setup.html
    //! [`Position`]: ../trait.Position.html
    pub use crate::Chess;
    pub use crate::position::Atomic;
    pub use crate::position::Giveaway;
    pub use crate::position::KingOfTheHill;
    pub use crate::position::ThreeCheck;
    pub use crate::position::Crazyhouse;
    pub use crate::position::RacingKings;
    pub use crate::position::Horde;
}
