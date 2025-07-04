//! A fast non-allocating and streaming reader for chess games in PGN notation.
//!
//! [`Reader`] parses games and calls methods of a user provided
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
//! use std::{io, ops::ControlFlow};
//! use pgn_reader::{Visitor, Skip, Reader, SanPlus};
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
//!
//!     fn begin_tags(&mut self) -> ControlFlow<Self::Output> {
//!         self.moves = 0;
//!         ControlFlow::Continue(())
//!     }
//!
//!     fn san(&mut self, _san_plus: SanPlus) -> ControlFlow<Self::Output> {
//!         self.moves += 1;
//!         ControlFlow::Continue(())
//!     }
//!
//!     fn begin_variation(&mut self) -> ControlFlow<Self::Output, Skip> {
//!         ControlFlow::Continue(Skip(true)) // stay in the mainline
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
//!     let mut reader = Reader::new(io::Cursor::new(&pgn));
//!
//!     let mut counter = MoveCounter::new();
//!     let moves = reader.read_game(&mut counter)?;
//!
//!     assert_eq!(moves, Some(4));
//!     Ok(())
//! }
//! ```
//!
//! A visitor that returns the final position using [`shakmaty`].
//!
//! ```
//! use std::{error::Error, io, mem, ops::ControlFlow};
//!
//! use shakmaty::{CastlingMode, Chess, Position};
//! use shakmaty::fen::Fen;
//!
//! use pgn_reader::{Visitor, Skip, RawTag, Reader, SanPlus};
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
//!     type Output = Result<Chess, Box<dyn Error>>;
//!
//!     fn tag(&mut self, name: &[u8], value: RawTag<'_>) -> ControlFlow<Self::Output> {
//!         // Support games from a non-standard starting position.
//!         if name == b"FEN" {
//!             let fen = match Fen::from_ascii(value.as_bytes()) {
//!                 Ok(fen) => fen,
//!                 Err(err) => return ControlFlow::Break(Err(err.into())),
//!             };
//!             let pos = Fen::from_ascii(value.as_bytes()).ok()
//!                 .and_then(|f| f.into_position(CastlingMode::Standard).ok());
//!
//!             if let Some(pos) = pos {
//!                 self.pos = pos;
//!             }
//!         }
//!         ControlFlow::Continue(())
//!     }
//!
//!     fn begin_variation(&mut self) -> ControlFlow<Self::Output, Skip> {
//!         ControlFlow::Continue(Skip(true)) // stay in the mainline
//!     }
//!
//!     fn san(&mut self, san_plus: SanPlus) -> ControlFlow<Self::Output> {
//!         match san_plus.san.to_move(&self.pos) {
//!             Ok(m) => {
//!                 self.pos.play_unchecked(m);
//!                 ControlFlow::Continue(())
//!             }
//!             Err(err) => ControlFlow::Break(Err(err.into()))
//!         }
//!     }
//!
//!     fn end_game(&mut self) -> Self::Output {
//!         Ok(mem::replace(&mut self.pos, Chess::default()))
//!     }
//! }
//!
//! fn main() -> io::Result<()> {
//!     let pgn = b"1. f3 e5 2. g4 Qh4#";
//!
//!     let mut reader = Reader::new(io::Cursor::new(&pgn));
//!
//!     let mut visitor = LastPosition::new();
//!
//!     let pos = reader
//!        .read_game(&mut visitor)?
//!        .expect("game found")
//!        .expect("valid");
//!
//!     assert!(pos.is_checkmate());
//!     Ok(())
//! }
//! ```

#![warn(missing_debug_implementations)]

mod buffer;
pub mod comment;
pub mod nag;
pub mod reader;
mod tag;
mod visitor;

pub use comment::RawComment;
pub use nag::Nag;
pub use reader::Reader;
pub use shakmaty::{self, KnownOutcome, Outcome, san::SanPlus};
pub use tag::RawTag;
pub use visitor::{Skip, Visitor};
