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

//! Read and write Standard Algebraic Notation.
//!
//! # Examples
//!
//! Parse and write SANs:
//!
//! ```
//! # use std::error::Error;
//! #
//! # fn try_main() -> Result<(), Box<Error>> {
//! use shakmaty::{Chess, Position};
//! use shakmaty::san::San;
//!
//! let san: San = "Nf3".parse()?;
//! assert_eq!(san.to_string(), "Nf3");
//! #
//! #     Ok(())
//! # }
//! #
//! # fn main() {
//! #     try_main().unwrap();
//! # }
//! ```
//!
//! Converting to a move:
//!
//! ```
//! # use std::error::Error;
//! #
//! # fn try_main() -> Result<(), Box<Error>> {
//! # use shakmaty::{Square, Chess, Position, Role, Move};
//! # use shakmaty::san::San;
//! # let san: San = "Nf3".parse()?;
//! let pos = Chess::default();
//! let m = san.to_move(&pos)?;
//!
//! assert_eq!(m, Move::Normal {
//!     role: Role::Knight,
//!     from: Square::G1,
//!     capture: None,
//!     to: Square::F3,
//!     promotion: None,
//! });
//! #
//! #     Ok(())
//! # }
//! #
//! # fn main() {
//! #     try_main().unwrap();
//! # }
//! ```
//!
//! Back to a (possibly disambiguated) SAN:
//!
//! ```
//! # use std::error::Error;
//! #
//! # fn try_main() -> Result<(), Box<Error>> {
//! # use shakmaty::{Chess, Position, Role};
//! # use shakmaty::san::San;
//! # let pos = Chess::default();
//! # let san: San = "Nf3".parse()?;
//! # let m = san.to_move(&pos)?;
//! assert_eq!(San::from_move(&pos, &m).to_string(), "Nf3");
//! #
//! #     Ok(())
//! # }
//! #
//! # fn main() {
//! #     try_main().unwrap();
//! # }
//! ```

use square::Square;
use types::{CastlingSide, Move, Role};
use position::{Outcome, Position};
use movelist::MoveList;

use std::fmt;
use std::str::FromStr;
use std::error::Error;

/// Error when parsing a syntactially invalid SAN.
#[derive(Debug, Eq, PartialEq)]
pub struct InvalidSan;

impl fmt::Display for InvalidSan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "invalid san".fmt(f)
    }
}

impl Error for InvalidSan {
    fn description(&self) -> &str {
        "invalid san"
    }
}

impl From<()> for InvalidSan {
    fn from(_: ()) -> InvalidSan {
        InvalidSan
    }
}

/// `IllegalSan` or `AmbiguousSan`.
#[derive(Debug)]
pub enum SanError {
    /// Standard algebraic notation does not match a legal move.
    IllegalSan,
    /// Standard algebraic notation matches multiple legal moves.
    AmbiguousSan,
}

impl SanError {
    fn desc(&self) -> &str {
        match *self {
            SanError::IllegalSan => "illegal san",
            SanError::AmbiguousSan => "ambiguous san",
        }
    }
}

impl fmt::Display for SanError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.desc().fmt(f)
    }
}

impl Error for SanError {
    fn description(&self) -> &str {
        self.desc()
    }
}

/// A move in Standard Algebraic Notation.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum San {
    Normal {
        role: Role,
        file: Option<i8>,
        rank: Option<i8>,
        capture: bool,
        to: Square,
        promotion: Option<Role>,
    },
    Castle(CastlingSide),
    Put { role: Role, to: Square },
    Null,
}

fn rank_from_char(ch: u8) -> Option<i8> {
    if b'1' <= ch && ch <= b'8' {
        Some((ch - b'1') as i8)
    } else {
        None
    }
}

fn file_from_char(ch: u8) -> Option<i8> {
    if b'a' <= ch && ch <= b'h' {
        Some((ch - b'a') as i8)
    } else {
        None
    }
}

impl San {
    /// Parses a SAN. Ignores a possible check or checkmate suffix.
    ///
    /// # Errors
    ///
    /// Returns [`InvalidSan`] if `san` is not syntactically valid.
    ///
    /// [`InvalidSan`]: struct.InvalidSan.html
    pub fn from_ascii(mut san: &[u8]) -> Result<San, InvalidSan> {
        if san.ends_with(b"#") || san.ends_with(b"+") {
            san = &san[0..(san.len() - 1)];
        }

        if san == b"--" {
            Ok(San::Null)
        } else if san == b"O-O" {
            Ok(San::Castle(CastlingSide::KingSide))
        } else if san == b"O-O-O" {
            Ok(San::Castle(CastlingSide::QueenSide))
        } else if san.len() == 3 && san[0] == b'@' {
            Ok(San::Put {
                role: Role::Pawn,
                to: Square::from_ascii(&san[1..]).map_err(|_| ())?,
            })
        } else if san.len() == 4 && san[1] == b'@' {
            Ok(San::Put {
                role: Role::from_char(san[0] as char).ok_or(())?,
                to: Square::from_ascii(&san[2..]).map_err(|_| ())?,
            })
        } else {
            let mut chars = san.iter().cloned();

            let (role, next) = {
                let ch = chars.next().ok_or(())?;
                if ch >= b'a' {
                    (Role::Pawn, ch)
                } else {
                    (Role::from_char(ch as char).ok_or(())?, chars.next().ok_or(())?)
                }
            };

            let (file, next) = if b'a' <= next && next <= b'h' {
                (Some((next - b'a') as i8), chars.next().ok_or(())?)
            } else {
                (None, next)
            };

            let (rank, next) = if b'1' <= next && next <= b'8' {
                (Some((next - b'1') as i8), chars.next())
            } else {
                (None, Some(next))
            };

            // This section is safe, because coordinates are already validated
            // by file_from_char or rank_from_char.
            let (capture, file, rank, to, next) = if let Some(next) = next {
                if next == b'x' {
                    let to_file = chars.next().and_then(file_from_char).ok_or(())?;
                    let to_rank = chars.next().and_then(rank_from_char).ok_or(())?;
                    let square = unsafe { Square::from_coords_unchecked(to_file, to_rank) };
                    (true, file, rank, square, chars.next())
                } else if next == b'=' {
                    let square = unsafe { Square::from_coords_unchecked(file.ok_or(())?, rank.ok_or(())?) };
                    (false, None, None, square, Some(b'='))
                } else {
                    let to_file = file_from_char(next).ok_or(())?;
                    let to_rank = chars.next().and_then(rank_from_char).ok_or(())?;
                    let square = unsafe { Square::from_coords_unchecked(to_file, to_rank) };
                    (false, file, rank, square, chars.next())
                }
            } else {
                let square = unsafe { Square::from_coords_unchecked(file.ok_or(())?, rank.ok_or(())?) };
                (false, None, None, square, None)
            };

            let promotion = match next {
                Some(b'=') =>
                    Some(chars.next().and_then(|r| Role::from_char(r as char)).ok_or(())?),
                Some(_) => return Err(InvalidSan),
                None => None,
            };

            Ok(San::Normal { role, file, rank, capture, to, promotion })
        }
    }

    /// Converts a move to Standard Algebraic Notation.
    pub fn from_move<P: Position>(pos: &P, m: &Move) -> San {
        match *m {
            Move::Normal { role: Role::Pawn, from, capture, to, promotion } =>
                San::Normal {
                    role: Role::Pawn,
                    file: if capture.is_some() { Some(from.file()) } else { None },
                    rank: None,
                    capture: capture.is_some(),
                    to,
                    promotion,
                },
            Move::Normal { role, from, capture, to, promotion } => {
                let mut legals = MoveList::new();
                pos.san_candidates(role, to, &mut legals);

                // Disambiguate.
                let (rank, file) = legals.iter().fold((false, false), |(rank, file), c| match *c {
                    Move::Normal { from: candidate, .. } =>
                        if from == candidate {
                            (rank, file)
                        } else if from.rank() == candidate.rank() || from.file() != candidate.file() {
                            (rank, true)
                        } else {
                            (true, file)
                        },
                    _ => (rank, file)
                });

                San::Normal {
                    role,
                    file: if file { Some(from.file()) } else { None },
                    rank: if rank { Some(from.rank()) } else { None },
                    capture: capture.is_some(),
                    to,
                    promotion
                }
            },
            Move::EnPassant { from, to, .. } => San::Normal {
                role: Role::Pawn, file: Some(from.file()), rank: None, capture: true, to, promotion: None },
            Move::Castle { rook, king } if rook.file() < king.file() => San::Castle(CastlingSide::QueenSide),
            Move::Castle { .. } => San::Castle(CastlingSide::KingSide),
            Move::Put { role, to } => San::Put { role, to },
        }
    }

    /// Tries to convert the `San` to a legal move in the context of a
    /// position.
    ///
    /// # Errors
    ///
    /// Returns [`SanError`] if there is no unique matching legal move.
    ///
    /// [`SanError`]: enum.SanError.html
    pub fn to_move<P: Position>(&self, pos: &P) -> Result<Move, SanError> {
        let mut legals = MoveList::new();

        match *self {
            San::Normal { role, file, rank, capture, to, promotion } => {
                pos.san_candidates(role, to, &mut legals);
                legals.retain(|m| match *m {
                    Move::Normal { from, capture: c, promotion: p, .. } =>
                        file.map_or(true, |f| f == from.file()) &&
                        rank.map_or(true, |r| r == from.rank()) &&
                        capture == c.is_some() &&
                        promotion == p,
                    Move::EnPassant { from, .. } =>
                        file.map_or(true, |f| f == from.file()) &&
                        rank.map_or(true, |r| r == from.rank()) &&
                        capture &&
                        promotion.is_none(),
                    _ => false,
                });
            },
            San::Castle(side) => pos.castling_moves(side, &mut legals),
            San::Put { role, to } => {
                pos.san_candidates(role, to, &mut legals);
                legals.retain(|m| match *m {
                    Move::Put { .. } => true,
                    _ => false,
                });
            },
            San::Null => return Err(SanError::IllegalSan),
        }

        legals.split_first().map_or(Err(SanError::IllegalSan), |(m, others)| {
            if others.is_empty() {
                Ok(m.clone())
            } else {
                Err(SanError::AmbiguousSan)
            }
        })
    }

    /// Searches a [`MoveList`] for a unique matching move.
    ///
    /// # Errors
    ///
    /// Returns [`SanError`] if there is no unique matching legal move.
    ///
    /// [`MoveList`]: type.MoveList.html
    /// [`SanError`]: enum.SanError.html
    pub fn find_move<'a>(&self, moves: &'a MoveList) -> Result<&'a Move, SanError> {
        let mut filtered = moves.iter().filter(|m| self.matches(m));

        let m = match filtered.next() {
            Some(m) => m,
            None => return Err(SanError::IllegalSan),
        };

        if filtered.next().is_some() {
            Err(SanError::AmbiguousSan)
        } else {
            Ok(m)
        }
    }

    /// Test if the `San` can match the `Move` (in any position).
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error::Error;
    /// #
    /// # fn try_main() -> Result<(), Box<Error>> {
    /// use shakmaty::{Square, Role, Move};
    /// use shakmaty::san::San;
    ///
    /// let m = Move::Normal {
    ///     role: Role::Knight,
    ///     from: Square::G1,
    ///     to: Square::F3,
    ///     capture: None,
    ///     promotion: None,
    /// };
    ///
    /// let nf3 = San::from_ascii(b"Nf3")?;
    /// assert!(nf3.matches(&m));
    ///
    /// let ng1f3 = San::from_ascii(b"Ng1f3")?;
    /// assert!(ng1f3.matches(&m));
    ///
    /// // capture does not match
    /// let nxf3 = San::from_ascii(b"Nxf3")?;
    /// assert!(!nxf3.matches(&m));
    ///
    /// // other file does not match
    /// let nef3 = San::from_ascii(b"Nef3")?;
    /// assert!(!nef3.matches(&m));
    /// #
    /// #     Ok(())
    /// # }
    /// #
    /// # fn main() {
    /// #     try_main().unwrap();
    /// # }
    /// ```
    pub fn matches(&self, m: &Move) -> bool {
        match *self {
            San::Normal { role, file, rank, capture, to, promotion } => {
                match *m {
                    Move::Normal { role: r, from, capture: c, to: t, promotion: pr } =>
                        role == r &&
                        file.map_or(true, |f| f == from.file()) &&
                        rank.map_or(true, |r| r == from.rank()) &&
                        capture == c.is_some() &&
                        to == t &&
                        promotion == pr,
                    Move::EnPassant { from, to: t } =>
                        role == Role::Pawn &&
                        file.map_or(true, |f| f == from.file()) &&
                        rank.map_or(true, |r| r == from.rank()) &&
                        capture &&
                        to == t &&
                        promotion.is_none(),
                    _ => false,
                }
            },
            San::Castle(side) => m.castling_side().map_or(false, |s| side == s),
            San::Put { role, to } => {
                match *m {
                    Move::Put { role: r, to: t } =>
                        r == role && to == t,
                    _ => false,
                }
            },
            San::Null => false
        }
    }
}


impl FromStr for San {
    type Err = InvalidSan;

    fn from_str(san: &str) -> Result<San, InvalidSan> {
        San::from_ascii(san.as_bytes())
    }
}

impl fmt::Display for San {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            San::Normal { role, file, rank, capture, to, promotion } => {
                if role != Role::Pawn {
                    write!(f, "{}", role.upper_char())?;
                }
                if let Some(file) = file {
                    write!(f, "{}", (b'a' + file as u8) as char)?;
                }
                if let Some(rank) = rank {
                    write!(f, "{}", (b'1' + rank as u8) as char)?;
                }
                if capture {
                    write!(f, "x")?;
                }
                write!(f, "{}", to)?;
                if let Some(promotion) = promotion {
                    write!(f, "={}", promotion.upper_char())?;
                }
                Ok(())
            },
            San::Castle(CastlingSide::KingSide) => write!(f, "O-O"),
            San::Castle(CastlingSide::QueenSide) => write!(f, "O-O-O"),
            San::Put { role: Role::Pawn, to } => write!(f, "@{}", to),
            San::Put { role, to } => write!(f, "{}@{}", role.upper_char(), to),
            San::Null => write!(f, "--"),
        }
    }
}

/// A [`San`] and possible check and checkmate suffixes.
///
/// [`San`]: enum.San.html
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SanPlus {
    pub san: San,
    pub check: bool,
    pub checkmate: bool,
}

impl SanPlus {
    /// Parses a SAN and possible check and checkmate suffix.
    ///
    /// # Errors
    ///
    /// Returns [`InvalidSan`] if `san` is not syntactically valid.
    ///
    /// [`InvalidSan`]: struct.InvalidSan.html
    pub fn from_ascii(san: &[u8]) -> Result<SanPlus, InvalidSan> {
        San::from_ascii(san).map(|result| SanPlus {
            san: result,
            checkmate: san.ends_with(b"#"),
            check: san.ends_with(b"+"),
        })
    }

    /// Converts a move to Standard Algebraic Notation including possible
    /// check and checkmate suffixes.
    pub fn from_move<P: Position>(mut pos: P, m: &Move) -> SanPlus {
        let san = San::from_move(&pos, m);
        pos.play_unchecked(m);
        let checkmate = match pos.outcome() {
            Some(Outcome::Decisive { .. }) => true,
            _ => false,
        };
        SanPlus { san, checkmate, check: !checkmate && pos.checkers().any() }
    }
}

impl FromStr for SanPlus {
    type Err = InvalidSan;

    fn from_str(san: &str) -> Result<SanPlus, InvalidSan> {
        SanPlus::from_ascii(san.as_bytes())
    }
}

impl fmt::Display for SanPlus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.checkmate {
            write!(f, "{}#", self.san)
        } else if self.check {
            write!(f, "{}+", self.san)
        } else {
            write!(f, "{}", self.san)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_write() {
        for san in &["a1", "a8", "h1", "h8", "e4", "b6", "e4=Q", "f1=N#",
                     "hxg7", "bxc1", "axe4", "bxc1+", "bxa8=R+",
                     "Nf3", "Ba5", "Qh8", "Kh1", "Qh1=K", "Ba5", "Bba5",
                     "N2c4", "Red3", "Qh1=K", "d1=N", "@e4#",
                     "K@b3", "Ba5", "Bba5",
                     "Ra1a8", "--", "O-O", "O-O-O+"] {
            let result = san.parse::<SanPlus>().expect("valid san").to_string();
            assert_eq!(*san, result, "read {} write {}", san, result);
        }
    }
}
