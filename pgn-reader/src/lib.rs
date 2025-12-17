//! A fast non-allocating and streaming reader for chess games in PGN notation.
//!
//! [`Reader`] parses games and calls methods of a user provided
//! [`Visitor`]. Implementing custom visitors allows for maximum flexibility:
//!
//! * The reader itself does not allocate (besides a single fixed-size buffer).
//!   The visitor can decide if and how to represent games in memory.
//! * The reader does not validate move legality. This allows implementing
//!   support for custom chess variants, or delaying move validation.
//! * The visitor can short-circuit and let the reader use a fast path for
//!   skipping games or variations.
//!
//! # Flow
//!
//! Visitor methods are called in these phases:
//!
//! 1. [`Visitor::begin_tags()`]
//!    - [`Visitor::tag()`]
//! 2. [`Visitor::begin_movetext()`]
//!    - [`Visitor::san()`]
//!    - [`Visitor::nag()`]
//!    - [`Visitor::comment()`]
//!    - [`Visitor::begin_variation()`] or skip
//!      - [`Visitor::variation_san()`]
//!    - [`Visitor::end_variation()`]
//!    - [`Visitor::outcome()`]
//! 3. [`Visitor::end_game()`]
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
//! struct MoveCounter;
//!
//! impl Visitor for MoveCounter {
//!     type Tags = ();
//!     type Movetext = usize;
//!     type Output = usize;
//!
//!     fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
//!         ControlFlow::Continue(())
//!     }
//!
//!     fn begin_movetext(&mut self, _tags: Self::Tags) -> ControlFlow<Self::Output, Self::Movetext> {
//!         ControlFlow::Continue(0)
//!     }
//!
//!     fn san(&mut self, movetext: &mut Self::Movetext, _san_plus: SanPlus) -> ControlFlow<Self::Output> {
//!         *movetext += 1;
//!         ControlFlow::Continue(())
//!     }
//!
//!     fn begin_variation(&mut self, _movetext: &mut Self::Movetext) -> ControlFlow<Self::Output, Skip> {
//!         ControlFlow::Continue(Skip(true)) // stay in the mainline
//!     }
//!
//!     fn end_game(&mut self, movetext: Self::Movetext) -> Self::Output {
//!         movetext
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
//!     let moves = reader.read_game(&mut MoveCounter)?;
//!     assert_eq!(moves, Some(4));
//!
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
//! struct LastPosition;
//!
//! impl Visitor for LastPosition {
//!     type Tags = Option<Chess>;
//!     type Movetext = Chess;
//!     type Output = Result<Chess, Box<dyn Error>>;
//!
//!     fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
//!         ControlFlow::Continue(None)
//!     }
//!
//!     fn tag(&mut self, tags: &mut Self::Tags, name: &[u8], value: RawTag<'_>) -> ControlFlow<Self::Output> {
//!         // Support games from a non-standard starting position.
//!         if name == b"FEN" {
//!             let fen = match Fen::from_ascii(value.as_bytes()) {
//!                 Ok(fen) => fen,
//!                 Err(err) => return ControlFlow::Break(Err(err.into())),
//!             };
//!             let pos = match fen.into_position(CastlingMode::Standard) {
//!                 Ok(pos) => pos,
//!                 Err(err) => return ControlFlow::Break(Err(err.into())),
//!             };
//!             tags.replace(pos);
//!         }
//!         ControlFlow::Continue(())
//!     }
//!
//!     fn begin_movetext(&mut self, tags: Self::Tags) -> ControlFlow<Self::Output, Self::Movetext> {
//!         ControlFlow::Continue(tags.unwrap_or_default())
//!     }
//!
//!     fn begin_variation(&mut self, _movetext: &mut Self::Movetext) -> ControlFlow<Self::Output, Skip> {
//!         ControlFlow::Continue(Skip(true)) // stay in the mainline
//!     }
//!
//!     fn san(&mut self, movetext: &mut Self::Movetext, san_plus: SanPlus) -> ControlFlow<Self::Output> {
//!         match san_plus.san.to_move(movetext) {
//!             Ok(m) => {
//!                 movetext.play_unchecked(m);
//!                 ControlFlow::Continue(())
//!             }
//!             Err(err) => ControlFlow::Break(Err(err.into()))
//!         }
//!     }
//!
//!     fn end_game(&mut self, movetext: Self::Movetext) -> Self::Output {
//!         Ok(movetext)
//!     }
//! }
//!
//! fn main() -> io::Result<()> {
//!     let pgn = b"1. f3 e5 2. g4 Qh4#";
//!
//!     let mut reader = Reader::new(io::Cursor::new(&pgn));
//!
//!     let pos = reader
//!        .read_game(&mut LastPosition)?
//!        .expect("game found")
//!        .expect("valid and legal");
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
