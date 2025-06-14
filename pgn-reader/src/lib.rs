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
//! <img src="https://github.com/tigerros/rust-pgn-reader/blob/clearer-phases/docs/visitor.svg?raw=true" alt="Flow" width="500"/>
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
//! use shakmaty::{CastlingMode, Chess, Position};
//! use shakmaty::fen::Fen;
//!
//! use pgn_reader::{Visitor, Skip, RawTag, BufferedReader, SanPlus};
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
//!     fn tag(&mut self, name: &[u8], value: RawTag<'_>) {
//!         // Support games from a non-standard starting position.
//!         if name == b"FEN" {
//!             let pos = Fen::from_ascii(value.as_bytes()).ok()
//!                 .and_then(|f| f.into_position(CastlingMode::Standard).ok());
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
//!             self.pos.play_unchecked(m);
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
//! [Shakmaty]: ../shakmaty/index.html

#![doc(html_root_url = "https://docs.rs/pgn-reader/0.27.0")]
#![forbid(unsafe_op_in_unsafe_fn)]
#![warn(missing_debug_implementations)]

mod reader;
mod types;
mod visitor;

pub use reader::{BufferedReader, IntoIter};
pub use shakmaty::{
    san::{San, SanPlus},
    CastlingSide, Color, File, Outcome, Rank, Role, Square,
};
pub use types::{Nag, RawComment, RawTag, Skip};
pub use visitor::Visitor;
