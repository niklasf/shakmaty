// This file is part of the pgn-reader library.
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

//! A fast non-allocating and streaming reader for chess games in PGN notation.
//!
//! [`BufferedReader`] parses games and calls methods of a user provided
//! [`Visitor`]. Implementing custom visitors allows for maximum flexibility:
//!
//! * The reader itself does not allocate (besides a single fixed-size buffer).
//!   The visitor can decide if and how to represent games in memory.
//! * The reader does not validate move legality. This allows implementing
//!   support for custom chess variants, or delaying move validation.
//! * The visitor can signal to the reader that it does not care about a game
//!   or variation.
//!
//! # Flow
//!
//! Visitor methods are called in this order:
//!
//! ![Flow](https://github.com/niklasf/rust-pgn-reader/blob/master/docs/visitor.png?raw=true)
//!
//! # Examples
//!
//! A visitor that counts the number of syntactically valid moves in mainline
//! of each game.
//!
//! ```
//! use std::io;
//! use pgn_reader::{Visitor, Skip, BufferedReader, SanPlus};
//!
//! struct MoveCounter {
//!     moves: usize,
//! }
//!
//! impl MoveCounter {
//!     fn new() -> MoveCounter {
//!         MoveCounter { moves: 0 }
//!     }
//! }
//!
//! impl Visitor for MoveCounter {
//!     type Result = usize;
//!
//!     fn begin_game(&mut self) {
//!         self.moves = 0;
//!     }
//!
//!     fn san(&mut self, _san_plus: SanPlus) {
//!         self.moves += 1;
//!     }
//!
//!     fn begin_variation(&mut self) -> Skip {
//!         Skip(true) // stay in the mainline
//!     }
//!
//!     fn end_game(&mut self) -> Self::Result {
//!         self.moves
//!     }
//! }
//!
//! fn main() -> io::Result<()> {
//!     let pgn = b"1. e4 e5 2. Nf3 (2. f4)
//!                 { game paused due to bad weather }
//!                 2... Nf6 *";
//!
//!     let mut reader = BufferedReader::new_cursor(&pgn[..]);
//!
//!     let mut counter = MoveCounter::new();
//!     let moves = reader.read_game(&mut counter)?;
//!
//!     assert_eq!(moves, Some(4));
//!     Ok(())
//! }
//! ```
//!
//! A visitor that returns the final position using [Shakmaty].
//!
//! ```
//! use std::io;
//!
//! use shakmaty::{Chess, Position};
//! use shakmaty::fen::Fen;
//!
//! use pgn_reader::{Visitor, Skip, RawHeader, BufferedReader, SanPlus};
//!
//! struct LastPosition {
//!     pos: Chess,
//! }
//!
//! impl LastPosition {
//!     fn new() -> LastPosition {
//!         LastPosition { pos: Chess::default() }
//!     }
//! }
//!
//! impl Visitor for LastPosition {
//!     type Result = Chess;
//!
//!     fn header(&mut self, key: &[u8], value: RawHeader<'_>) {
//!         // Support games from a non-standard starting position.
//!         if key == b"FEN" {
//!             let pos = Fen::from_ascii(value.as_bytes()).ok()
//!                 .and_then(|f| f.position().ok());
//!
//!             if let Some(pos) = pos {
//!                 self.pos = pos;
//!             }
//!         }
//!     }
//!
//!     fn begin_variation(&mut self) -> Skip {
//!         Skip(true) // stay in the mainline
//!     }
//!
//!     fn san(&mut self, san_plus: SanPlus) {
//!         if let Ok(m) = san_plus.san.to_move(&self.pos) {
//!             self.pos.play_unchecked(&m);
//!         }
//!     }
//!
//!     fn end_game(&mut self) -> Self::Result {
//!         ::std::mem::replace(&mut self.pos, Chess::default())
//!     }
//! }
//!
//! fn main() -> io::Result<()> {
//!     let pgn = b"1. f3 e5 2. g4 Qh4#";
//!
//!     let mut reader = BufferedReader::new_cursor(&pgn[..]);
//!
//!     let mut visitor = LastPosition::new();
//!     let pos = reader.read_game(&mut visitor)?;
//!
//!     assert!(pos.map_or(false, |p| p.is_checkmate()));
//!     Ok(())
//! }
//! ```
//!
//! [`BufferedReader`]: struct.BufferedReader.html
//! [`Visitor`]: trait.Visitor.html
//! [Shakmaty]: ../shakmaty/index.html

#![doc(html_root_url = "https://docs.rs/pgn-reader/0.15.0")]

#![warn(missing_debug_implementations)]

mod types;
mod visitor;
mod reader;

pub use shakmaty::{Color, Role, CastlingSide, Outcome, Square, File, Rank};
pub use shakmaty::san::{San, SanPlus};

pub use types::{Skip, Nag, RawHeader, RawComment};
pub use visitor::Visitor;
pub use reader::{BufferedReader, IntoIter};
