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
//! use std::convert::Infallible;
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
//!     type Output = usize;
//!     type Error = Infallible;
//!
//!     fn begin_tags(&mut self) -> Result<(), Self::Error> {
//!         self.moves = 0;
//!
//!         Ok(())
//!     }
//!
//!     fn san(&mut self, _san_plus: SanPlus) -> Result<(), Self::Error> {
//!         self.moves += 1;
//!
//!         Ok(())
//!     }
//!
//!     fn begin_variation(&mut self) -> Result<Skip, Self::Error> {
//!         Ok(Skip(true)) // stay in the mainline
//!     }
//!
//!     fn end_game(&mut self) -> Self::Output {
//!         self.moves
//!     }
//! }
//!
//! fn main() -> io::Result<()> {
//!     let pgn = b"1. e4 e5 2. Nf3 (2. f4)
//!                 { game paused due to bad weather }
//!                 2... Nf6 *";
//!
//!     let mut reader = BufferedReader::new(io::Cursor::new(&pgn));
//!
//!     let mut counter = MoveCounter::new();
//!     let moves = reader.read_game(&mut counter)?;
//!
//!     assert_eq!(moves, Some(Ok(4)));
//!     Ok(())
//! }
//! ```
//!
//! A visitor that returns the final position using [Shakmaty].
//!
//! ```
//! use std::io;
//! use std::convert::Infallible;
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
//!     type Output = Chess;
//!     type Error = Infallible;
//!
//!     fn tag(&mut self, name: &[u8], value: RawTag<'_>) -> Result<(), Self::Error> {
//!         // Support games from a non-standard starting position.
//!         if name == b"FEN" {
//!             let pos = Fen::from_ascii(value.as_bytes()).ok()
//!                 .and_then(|f| f.into_position(CastlingMode::Standard).ok());
//!
//!             if let Some(pos) = pos {
//!                 self.pos = pos;
//!             }
//!         }
//!
//!         Ok(())
//!     }
//!
//!     fn begin_variation(&mut self) -> Result<Skip, Self::Error> {
//!         Ok(Skip(true)) // stay in the mainline
//!     }
//!
//!     fn san(&mut self, san_plus: SanPlus) -> Result<(), Self::Error> {
//!         if let Ok(m) = san_plus.san.to_move(&self.pos) {
//!             self.pos.play_unchecked(m);
//!         }
//!
//!         Ok(())
//!     }
//!
//!     fn end_game(&mut self) -> Self::Output {
//!         ::std::mem::replace(&mut self.pos, Chess::default())
//!     }
//! }
//!
//! fn main() -> io::Result<()> {
//!     let pgn = b"1. f3 e5 2. g4 Qh4#";
//!
//!     let mut reader = BufferedReader::new(io::Cursor::new(&pgn));
//!
//!     let mut visitor = LastPosition::new();
//!     let pos = reader.read_game(&mut visitor)?;
//!
//!     assert!(pos.map_or(false, |p| p.unwrap().is_checkmate()));
//!     Ok(())
//! }
//! ```
//!
//! [Shakmaty]: ../shakmaty/index.html

#![forbid(unsafe_op_in_unsafe_fn)]
#![warn(missing_debug_implementations)]
#![warn(missing_copy_implementations)]

mod buffer;
mod reader;
mod types;
mod visitor;

pub use buffer::Buffer;
pub use reader::{BufferedReader, IntoIter};
pub use shakmaty::{
    CastlingSide, Color, File, KnownOutcome, Outcome, Rank, Role, Square,
    san::{San, SanPlus},
};
pub use types::{Nag, RawComment, RawTag, Skip};
pub use visitor::Visitor;
