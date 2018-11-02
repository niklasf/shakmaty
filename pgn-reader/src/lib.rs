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

//! A fast non-allocating reader for chess games in PGN notation.
//!
//! [`Reader`] parses games and calls methods of a user provided [`Visitor`].
//! Implementing custom visitors allows for maximum flexibility:
//!
//! * The reader itself does not allocate. The visitor can decide if and
//!   how to represent games in memory.
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
//! extern crate pgn_reader;
//!
//! use pgn_reader::{Visitor, Skip, RawHeader, Reader, San};
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
//! impl<'pgn> Visitor<'pgn> for MoveCounter {
//!     type Result = usize;
//!
//!     fn begin_game(&mut self) {
//!         self.moves = 0;
//!     }
//!
//!     fn san(&mut self, _san: San) {
//!         self.moves += 1;
//!     }
//!
//!     fn begin_variation(&mut self) -> Skip {
//!         Skip(true) // stay in the mainline
//!     }
//!
//!     fn end_game(&mut self, _game: &'pgn [u8]) -> Self::Result {
//!         self.moves
//!     }
//! }
//!
//! fn main() {
//!     let pgn = b"1. e4 e5 2. Nf3 (2. f4)
//!                 { game paused due to bad weather }
//!                 2... Nf6 *";
//!
//!     let mut counter = MoveCounter::new();
//!     let reader = Reader::new(&mut counter, pgn);
//!
//!     let moves: usize = reader.into_iter().sum();
//!     assert_eq!(moves, 4);
//! }
//! ```
//!
//! A visitor that returns the final position using [Shakmaty].
//!
//! ```
//! extern crate pgn_reader;
//! extern crate shakmaty;
//!
//! use pgn_reader::{Visitor, Skip, RawHeader, Reader, San};
//!
//! use shakmaty::{Chess, Position};
//! use shakmaty::fen::Fen;
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
//! impl<'pgn> Visitor<'pgn> for LastPosition {
//!     type Result = Chess;
//!
//!     fn header(&mut self, key: &'pgn [u8], value: RawHeader<'pgn>) {
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
//!     fn san(&mut self, san: San) {
//!         if let Ok(m) = san.to_move(&self.pos) {
//!             self.pos.play_unchecked(&m);
//!         }
//!     }
//!
//!     fn end_game(&mut self, _game: &'pgn [u8]) -> Self::Result {
//!         ::std::mem::replace(&mut self.pos, Chess::default())
//!     }
//! }
//!
//! fn main() {
//!     let pgn = b"1. f3 e5 2. g4 Qh4#";
//!
//!     let mut visitor = LastPosition::new();
//!     let mut reader = Reader::new(&mut visitor, pgn);
//!
//!     let pos = reader.read_game();
//!     assert!(pos.map_or(false, |p| p.is_checkmate()));
//! }
//! ```
//!
//! [`Reader`]: struct.Reader.html
//! [`Visitor`]: trait.Visitor.html
//! [Shakmaty]: ../shakmaty/index.html

#![doc(html_root_url = "https://docs.rs/pgn-reader/0.11.0")]

#![warn(missing_debug_implementations)]

extern crate memchr;
extern crate btoi;
extern crate shakmaty;
extern crate slice_deque;

mod types;
mod visitor;
mod reader;

pub use visitor::Visitor;
pub use reader::{BufferedReader, SliceReader as Cursor};

/* impl<'a, 'pgn, V: Visitor<'pgn>> Reader<'a, 'pgn, V> {
    /// Creates a new reader with a custom [`Visitor`].
    ///
    /// [`Visitor`]: trait.Visitor.html
    pub fn new(visitor: &'a mut V, pgn: &'pgn [u8]) -> Reader<'a, 'pgn, V> {
        // Skip BOM.
        let pos = if pgn.starts_with(b"\xef\xbb\xbf") { 3 } else { 0 };

        // Skip leading whitespace.
        let (_, pgn) = split_after_pgn_space(pgn, pos);
        Reader { visitor, pgn }
    }

    /// Read the next game, returning the result from the visitor, or `None`
    /// if there was no further game.
    pub fn read_game(&mut self) -> Option<V::Result> {
        if self.pgn.is_empty() {
            return None;
        }

        // Scan game.
        self.visitor.begin_game();
        self.visitor.begin_headers();
        let pos = self.scan_headers();
        let pos = if let Skip(false) = self.visitor.end_headers() {
            self.scan_movetext(pos)
        } else {
            self.skip_movetext(pos)
        };

        // Skip trailing whitespace.
        let (head, tail) = split_after_pgn_space(self.pgn, pos);

        let result = self.visitor.end_game(head);
        self.pgn = tail;
        Some(result)
    }

    /// Skip the next game without calling methods of the visitor.
    pub fn skip_game(&mut self) {
        let pos = self.scan_headers();
        let pos = self.skip_movetext(pos);
        let (_, tail) = split_after_pgn_space(self.pgn, pos);
        self.pgn = tail;
    }

    /// Reads all games.
    pub fn read_all(mut self) {
        while let Some(_) = self.read_game() { }
    }

    /// Returns a slice containing the not yet fully parsed games.
    pub fn remaining_pgn(&self) -> &'pgn [u8] {
        self.pgn
    }

impl<'a, 'pgn, V: Visitor<'pgn>> IntoIterator for Reader<'a, 'pgn, V> {
    type Item = V::Result;
    type IntoIter = Iter<'a, 'pgn, V>;

    fn into_iter(self) -> Self::IntoIter {
        Iter { reader: self }
    }
}

/// View a [`Reader`] as an iterator.
///
/// [`Reader`]: struct.Reader.html
pub struct Iter<'a, 'pgn, V: Visitor<'pgn>> where V: 'a {
    reader: Reader<'a, 'pgn, V>,
}

impl<'a, 'pgn, V: Visitor<'pgn>> fmt::Debug for Iter<'a, 'pgn, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Iter").finish()
    }
}

impl<'a, 'pgn, V: Visitor<'pgn>> Iterator for Iter<'a, 'pgn, V> {
    type Item = V::Result;

    fn next(&mut self) -> Option<Self::Item> {
        self.reader.read_game()
    }
} */
