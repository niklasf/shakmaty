// This file is part of the pgn-reader library.
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
//! use pgn_reader::{Visitor, Skip, Reader, San};
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
//! use pgn_reader::{Visitor, Skip, Reader, San};
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
//!     fn header(&mut self, key: &'pgn [u8], value: &'pgn [u8]) {
//!         // Support games from a non-standard starting position.
//!         if key == b"FEN" {
//!             let pos = Fen::from_ascii(value).ok()
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

#![doc(html_root_url = "https://docs.rs/pgn-reader/0.7.0")]

#![warn(missing_debug_implementations)]

extern crate memchr;
extern crate btoi;
extern crate shakmaty;

use std::fmt;
use std::cmp::max;
use std::str::FromStr;
use std::error::Error;

pub use shakmaty::san::San;
pub use shakmaty::{Color, CastlingSide, Outcome, Role, Square};

use btoi::btou;

/// Tell the reader to skip over a game or variation.
#[derive(Clone, Eq, PartialEq, Debug)]
#[must_use]
pub struct Skip(pub bool);

/// A numeric annotation glyph like `?`, `!!` or `$42`.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct Nag(pub u8);

impl Nag {
    /// Tries to parse a NAG from ASCII.
    ///
    /// # Examples
    ///
    /// ```
    /// use pgn_reader::Nag;
    ///
    /// assert_eq!(Nag::from_bytes(b"??"), Ok(Nag(4)));
    /// assert_eq!(Nag::from_bytes(b"$24"), Ok(Nag(24)));
    /// ```
    ///
    /// # Errors
    ///
    /// Returns an [`InvalidNag`] error if the input is neither a known glyph
    /// (`?!`, `!`, ...) nor a valid numeric annotation (`$0`, ..., `$255`).
    ///
    ///
    /// [`InvalidNag`]: struct.InvalidNag.html
    pub fn from_bytes(s: &[u8]) -> Result<Nag, InvalidNag> {
        if s == b"?!" {
            Ok(Nag(6))
        } else if s == b"?" {
            Ok(Nag(2))
        } else if s == b"??" {
            Ok(Nag(4))
        } else if s == b"!" {
            Ok(Nag(1))
        } else if s == b"!!" {
            Ok(Nag(3))
        } else if s == b"!?" {
            Ok(Nag(5))
        } else if s.len() > 1 && s[0] == b'$' {
            btou(&s[1..]).ok().map(Nag).ok_or(InvalidNag { _priv: () })
        } else {
            Err(InvalidNag { _priv: () })
        }
    }
}

impl fmt::Display for Nag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl From<u8> for Nag {
    fn from(nag: u8) -> Nag {
        Nag(nag)
    }
}

/// Error when parsing an invalid NAG.
#[derive(Clone, Eq, PartialEq)]
pub struct InvalidNag {
    _priv: (),
}

impl fmt::Debug for InvalidNag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("InvalidNag").finish()
    }
}

impl fmt::Display for InvalidNag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "invalid nag".fmt(f)
    }
}

impl Error for InvalidNag {
    fn description(&self) -> &str {
        "invalid nag"
    }
}

impl FromStr for Nag {
    type Err = InvalidNag;

    fn from_str(s: &str) -> Result<Nag, InvalidNag> {
        Nag::from_bytes(s.as_bytes())
    }
}

/// Consumes games from a reader.
///
/// ![Flow](https://github.com/niklasf/rust-pgn-reader/blob/master/docs/visitor.png?raw=true)
pub trait Visitor<'pgn> {
    type Result;

    /// Called at the start of a game.
    fn begin_game(&mut self) { }

    /// Called directly before reading game headers.
    fn begin_headers(&mut self) { }
    /// Called when parsing a game header like `[White "Deep Blue"]`.
    fn header(&mut self, _key: &'pgn [u8], _value: &'pgn [u8]) { }
    /// Called after reading the headers of a game. May skip quickly over the
    /// following move text directly to `end_game`.
    fn end_headers(&mut self) -> Skip { Skip(false) }

    /// Called for each move, like `Nf3`.
    fn san(&mut self, _san: San) { }
    /// Called for each numeric annotation glyph, like `!?` or `$7`.
    fn nag(&mut self, _nag: Nag) { }
    /// Called for each `{ comment }` with the whole comment as a byte slice,
    /// excluding the braces.
    fn comment(&mut self, _comment: &'pgn [u8]) { }
    /// Called for each `(`. May skip over the following variation directly
    /// to `end_variation` (or to `end_game` if no `)` follows before the end of
    /// the game).
    fn begin_variation(&mut self) -> Skip { Skip(false) }
    /// Called for each `)`. It is **not** guaranteed that there was a
    /// matching `(`.
    fn end_variation(&mut self) { }
    /// Called for each game termination, like `1-0`.
    fn outcome(&mut self, _outcome: Outcome) { }

    /// Called after parsing a game. Can return a custom result.
    fn end_game(&mut self, game: &'pgn [u8]) -> Self::Result;
}

fn split_after_pgn_space(pgn: &[u8], mut pos: usize) -> (&[u8], &[u8]) {
    while pos < pgn.len() {
        match pgn[pos] {
            b' ' | b'\t' | b'\r' => pos += 1,
            b'\n' => {
                // Also skip % comments.
                pos += 1;
                if pos < pgn.len() && pgn[pos] == b'%' {
                    pos += 1;
                    pos = memchr::memchr(b'\n', &pgn[pos..]).map_or_else(|| pgn.len(), |p| pos + p + 1);
                }
            },
            _ => break
        }
    }

    pgn.split_at(pos)
}

/// Reads a PGN.
pub struct Reader<'a, 'pgn, V: Visitor<'pgn>> where V: 'a {
    visitor: &'a mut V,
    pgn: &'pgn [u8],
}

impl<'a, 'pgn, V: Visitor<'pgn>> fmt::Debug for Reader<'a, 'pgn, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Reader").finish()
    }
}

impl<'a, 'pgn, V: Visitor<'pgn>> Reader<'a, 'pgn, V> {
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

    fn scan_headers(&mut self) -> usize {
        let mut pos = 0;

        while pos < self.pgn.len() {
            match self.pgn[pos] {
                b'[' => {
                    pos += 1;
                    let key_pos = pos;
                    match memchr::memchr2(b'"', b'\n', &self.pgn[pos..]) {
                        Some(delta) if self.pgn[pos + delta] == b'"' => {
                            pos += delta;
                            let key_end_pos = if self.pgn[pos - 1] == b' ' {
                                pos - 1
                            } else {
                                pos
                            };
                            pos += 1;
                            let value_pos = pos;

                            pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p + 1);
                            let eol_pos = if self.pgn[pos - 1] == b'\n' {
                                if self.pgn[pos - 2] == b'\r' {
                                    pos - 2
                                } else {
                                    pos - 1
                                }
                            } else {
                                continue;
                            };

                            let value_end_pos = if self.pgn[eol_pos - 1] == b']' && self.pgn[eol_pos - 2] == b'"' {
                                max(value_pos + 1, eol_pos - 2)
                            } else {
                                continue;
                            };

                            self.visitor.header(&self.pgn[key_pos..key_end_pos],
                                                &self.pgn[value_pos..value_end_pos]);
                        },
                        Some(delta) => pos += delta + 1,
                        None => pos = self.pgn.len(),
                    }
                },
                b'%' => {
                    pos += 1;
                    pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p + 1);
                },
                _ => break
            }
        }

        pos
    }

    fn skip_movetext(&mut self, mut pos: usize) -> usize {
        while pos < self.pgn.len() {
            match self.pgn[pos] {
                b'{' => {
                    pos += 1;
                    pos = memchr::memchr(b'}', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p + 1);
                },
                b';' => {
                    pos += 1;
                    pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p);
                },
                b'\n' => {
                    pos += 1;
                    if pos >= self.pgn.len() {
                        break;
                    }
                    match self.pgn[pos] {
                        b'%' => {
                            pos += 1;
                            pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p);
                        },
                        b'[' | b'\n' => {
                            break
                        },
                        b'\r' => {
                            pos += 1;
                            if pos < self.pgn.len() && self.pgn[pos] == b'\n' {
                                break;
                            }
                        },
                        _ => continue,
                    }
                },
                _ => {
                    pos += 1;
                    pos = memchr::memchr3(b'\n', b'{', b';', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p);
                },
            }
        }

        pos
    }

    fn skip_variation(&mut self, mut pos: usize) -> usize {
        let mut depth = 0usize;

        while pos < self.pgn.len() {
            match self.pgn[pos] {
                b'(' => {
                    depth += 1;
                    pos += 1;
                },
                b')' => {
                    if let Some(d) = depth.checked_sub(1) {
                        depth = d;
                    } else {
                        break;
                    }
                },
                b'{' => {
                    pos += 1;
                    pos = memchr::memchr(b'}', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p + 1);
                },
                b';' => {
                    pos += 1;
                    pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p);
                },
                b'\n' => {
                    pos += 1;
                    if pos >= self.pgn.len() {
                        break;
                    }
                    match self.pgn[pos] {
                        b'%' => {
                            pos += 1;
                            pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p);
                        },
                        b'[' | b'\n' => {
                            // Return to the previous parser and let it handle
                            // the first line break again (to end the game).
                            return pos - 1;
                        },
                        b'\r' => {
                            pos += 1;
                            if pos < self.pgn.len() && self.pgn[pos] == b'\n' {
                                return pos - 2;
                            }
                        },
                        _ => continue,
                    }
                },
                _ => {
                    pos += 1;
                }
            }
        }

        pos
    }

    fn skip_token(&mut self, mut pos: usize) -> usize {
        while pos < self.pgn.len() {
            match self.pgn[pos] {
                b' ' | b'\t' | b'\n' | b'\r' | b'{' | b'}' | b'(' | b')' | b'!' | b'?' | b'$' | b';' | b'.' => break,
                _ => pos += 1,
            }
        }

        pos
    }

    fn scan_movetext(&mut self, mut pos: usize) -> usize {
        while pos < self.pgn.len() {
            match self.pgn[pos] {
                b'{' => {
                    pos += 1;
                    pos = if let Some(delta) = memchr::memchr(b'}', &self.pgn[pos..]) {
                        let end = pos + delta;
                        self.visitor.comment(&self.pgn[pos..end]);
                        end + 1
                    } else {
                        self.visitor.comment(&self.pgn[pos..]);
                        self.pgn.len()
                    };
                },
                b'\n' => {
                    pos += 1;
                    if pos >= self.pgn.len() {
                        break;
                    }
                    match self.pgn[pos] {
                        b'%' => {
                            pos += 1;
                            pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p);
                        },
                        b'[' | b'\n' => {
                            break
                        },
                        b'\r' => {
                            pos += 1;
                            if pos < self.pgn.len() && self.pgn[pos] == b'\n' {
                                break
                            }
                        },
                        _ => continue,
                    }
                },
                b';' => {
                    pos += 1;
                    pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p);
                },
                b'1' => {
                    pos += 1;
                    if self.pgn[pos..].starts_with(b"-0") {
                        pos += 2;
                        self.visitor.outcome(Outcome::Decisive { winner: Color::White });
                    } else if self.pgn[pos..].starts_with(b"/2-1/2") {
                        pos += 6;
                        self.visitor.outcome(Outcome::Draw);
                    } else {
                        pos = self.skip_token(pos);
                    }
                },
                b'0' => {
                    pos += 1;
                    if self.pgn[pos..].starts_with(b"-1") {
                        pos += 2;
                        self.visitor.outcome(Outcome::Decisive { winner: Color::Black });
                    } else if self.pgn[pos..].starts_with(b"-0-0") {
                        pos += 4;
                        self.visitor.san(San::Castle(CastlingSide::QueenSide));
                    } else if self.pgn[pos..].starts_with(b"-0") {
                        pos += 2;
                        self.visitor.san(San::Castle(CastlingSide::KingSide));
                    } else {
                        pos = self.skip_token(pos);
                    }
                },
                b'(' => {
                    pos += 1;
                    if let Skip(true) = self.visitor.begin_variation() {
                        pos = self.skip_variation(pos);
                    }
                },
                b')' => {
                    pos += 1;
                    self.visitor.end_variation();
                },
                b'!' | b'?' | b'$' => {
                    let start = pos;
                    pos = self.skip_token(pos + 1);
                    if let Ok(nag) = Nag::from_bytes(&self.pgn[start..pos]) {
                        self.visitor.nag(nag);
                    }
                },
                b' ' | b'\t' | b'\r' | b'P' | b'.' => {
                    pos += 1;
                },
                _ => {
                    let end = self.skip_token(pos + 1);
                    if self.pgn[pos] > b'9' {
                        if let Ok(san) = San::from_ascii(&self.pgn[pos..end]) {
                            self.visitor.san(san);
                        }
                    }
                    pos = end;
                },
            }
        }

        pos
    }
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
}

#[cfg(test)]
mod tests {
    use super::*;

    struct _AssertObjectSafe<'pgn, R>(Box<Visitor<'pgn, Result=R>>);

    struct GameCounter {
        count: usize,
    }

    impl Default for GameCounter {
        fn default() -> GameCounter {
            GameCounter { count: 0 }
        }
    }

    impl<'pgn> Visitor<'pgn> for GameCounter {
        type Result = ();

        fn end_game(&mut self, _pgn: &'pgn [u8]) {
            self.count += 1;
        }
    }

    #[test]
    fn test_empty_game() {
        let mut counter = GameCounter::default();
        Reader::new(&mut counter, b"  ").read_all();
        assert_eq!(counter.count, 0);
    }

    #[test]
    fn test_trailing_space() {
        let mut counter = GameCounter::default();
        Reader::new(&mut counter, b"1. e4 1-0\n\n\n\n\n  \n").read_all();
        assert_eq!(counter.count, 1);
    }
}
