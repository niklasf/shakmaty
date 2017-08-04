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

//! Read and write Standard Algebraic Notation.
//!
//! # Examples
//!
//! Parse and write SANs:
//!
//! ```
//! use shakmaty::Chess;
//! use shakmaty::Position;
//! use shakmaty::san::San;
//!
//! let san: San = "Nf3".parse().expect("valid san");
//! assert_eq!(san.to_string(), "Nf3");
//! ```
//!
//! Converting to a move:
//!
//! ```
//! # use shakmaty::{Chess, Position, Role, Move};
//! # use shakmaty::san::San;
//! # use shakmaty::square;
//! # let san: San = "Nf3".parse().expect("valid san");
//! let pos = Chess::default();
//! let m = san.to_move(&pos).expect("legal move");
//!
//! assert_eq!(m, Move::Normal {
//!     role: Role::Knight,
//!     from: square::G1,
//!     capture: None,
//!     to: square::F3,
//!     promotion: None,
//! });
//! ```
//!
//! Back to a (possibly disambiguated) SAN:
//!
//! ```
//! use shakmaty::san;
//! # use shakmaty::{Chess, Position, Role};
//! # use shakmaty::san::San;
//! # let pos = Chess::default();
//! # let san: San = "Nf3".parse().expect("valid san");
//! # let m = san.to_move(&pos).expect("legal move");
//!
//! assert_eq!(san::san(&pos, &m).to_string(), "Nf3");
//! ```

use square;
use square::Square;
use types::{Move, Role};
use position::{Position, Outcome};
use movelist::MoveList;

use std::fmt;
use std::ascii::AsciiExt;
use option_filter::OptionFilterExt;
use std::str::FromStr;
use std::error::Error;

/// Error when parsing a syntactially invalid SAN.
pub struct InvalidSan { _priv: () }

impl fmt::Debug for InvalidSan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("InvalidSan").finish()
    }
}

impl fmt::Display for InvalidSan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "invalid san".fmt(f)
    }
}

impl Error for InvalidSan {
    fn description(&self) -> &str { "invalid san" }
}

impl From<()> for InvalidSan {
    fn from(_: ()) -> InvalidSan {
        InvalidSan { _priv: () }
    }
}

/// `IllegalSan` or `AmbiguousSan`.
#[derive(Debug)]
pub enum SanError {
    IllegalSan,
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
    fn description(&self) -> &str { self.desc() }
}


/// A move in Standard Algebraic Notation.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum San {
    Normal { role: Role, file: Option<i8>, rank: Option<i8>, capture: bool, to: Square, promotion: Option<Role> },
    CastleShort,
    CastleLong,
    Null,
}

/// A `San` and possible check and checkmate suffixes.
pub struct SanPlus {
    pub san: San,
    pub check: bool,
    pub checkmate: bool,
}

impl FromStr for San {
    type Err = InvalidSan;

    fn from_str(san: &str) -> Result<San, InvalidSan> {
        let san = san.trim_right_matches('#').trim_right_matches('+');
        if san == "--" {
            Ok(San::Null)
        } else if san == "O-O" {
            Ok(San::CastleShort)
        } else if san == "O-O-O" {
            Ok(San::CastleLong)
        } else {
            let mut chars = san.chars();

            let (role, next) = {
                let ch = chars.next().ok_or(())?;
                if ch.is_uppercase() {
                    (Role::from_char(ch.to_ascii_lowercase()).ok_or(())?, chars.next().ok_or(())?)
                } else {
                    (Role::Pawn, ch)
                }
            };

            let (file, next) = if 'a' <= next && next <= 'h' {
                (Some(next as i8 - b'a' as i8), chars.next().ok_or(())?)
            } else {
                (None, next)
            };

            let (rank, next) = if '1' <= next && next <= '8' {
                (Some(next as i8 - b'1' as i8), chars.next())
            } else {
                (None, Some(next))
            };

            let (capture, file, rank, to, next) = if let Some(next) = next {
                if next == 'x' {
                    let to_file = chars.next().and_then(square::file_from_char).ok_or(())?;
                    let to_rank = chars.next().and_then(square::rank_from_char).ok_or(())?;
                    (true, file, rank, Square::from_coords(to_file, to_rank).ok_or(())?, chars.next())
                } else if next == '=' {
                    (false, None, None, Square::from_coords(file.ok_or(())?, rank.ok_or(())?).ok_or(())?, Some('='))
                } else {
                    let to_file = square::file_from_char(next).ok_or(())?;
                    let to_rank = chars.next().and_then(square::rank_from_char).ok_or(())?;
                    (false, file, rank, Square::from_coords(to_file, to_rank).ok_or(())?, chars.next())
                }
            } else {
                (false, None, None, Square::from_coords(file.ok_or(())?, rank.ok_or(())?).ok_or(())?, None)
            };

            let promotion = match next {
                Some('=') =>
                    Some(chars.next().and_then(|r| Role::from_char(r.to_ascii_lowercase())).ok_or(())?),
                Some(_) => return Err(InvalidSan { _priv: () }),
                None => None,
            };

            Ok(San::Normal { role, file, rank, capture, to, promotion })
        }
    }
}

impl fmt::Display for San {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            San::Normal { role, file, rank, capture, to, promotion } => {
                if role != Role::Pawn {
                    write!(f, "{}", role.char().to_ascii_uppercase())?;
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
                    write!(f, "={}", promotion.char().to_ascii_uppercase())?;
                }
                Ok(())
            },
            San::CastleShort => write!(f, "O-O"),
            San::CastleLong => write!(f, "O-O-O"),
            San::Null => write!(f, "--"),
        }
    }
}

impl FromStr for SanPlus {
    type Err = InvalidSan;

    fn from_str(san: &str) -> Result<SanPlus, InvalidSan> {
        San::from_str(san).map(|result| {
            SanPlus {
                san: result,
                checkmate: san.ends_with('#'),
                check: san.ends_with('+'),
            }
        })
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

impl San {
    /// Tries to convert the `San` to a legal move in the context of a
    /// position.
    pub fn to_move<P: Position>(&self, pos: &P) -> Result<Move, SanError> {
        let mut legals = MoveList::new();

        match *self {
            San::Normal { role, file, rank, capture, to, promotion } => {
                pos.san_candidates(role, to, &mut legals);
                legals.swap_retain(|m| match *m {
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
            San::CastleShort => {
                pos.legal_moves(&mut legals);
                legals.swap_retain(|m| match *m {
                    Move::Castle { king, rook } => king.file() < rook.file(),
                    _ => false,
                });
            },
            San::CastleLong => {
                pos.legal_moves(&mut legals);
                legals.swap_retain(|m| match *m {
                    Move::Castle { king, rook } => rook.file() < king.file(),
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
}

/// Converts a move to Standard Algebraic Notation including possible
/// check and checkmate suffixes.
pub fn san_plus<P: Position>(pos: P, m: &Move) -> SanPlus {
    let san = san(&pos, m);
    let after = pos.play_unchecked(m);
    let checkmate = match after.outcome() {
        Some(Outcome::Decisive { .. }) => true,
        _ => false,
    };
    SanPlus { san, checkmate, check: !checkmate && after.checkers().any() }
}

/// Converts a move to Standard Algebraic Notation.
pub fn san<P: Position>(pos: &P, m: &Move) -> San {
    match *m {
        Move::Normal { role: Role::Pawn, from, capture, to, promotion } =>
            San::Normal {
                role: Role::Pawn,
                file: Some(from.file()).filter(|_| capture.is_some()),
                rank: None,
                capture: capture.is_some(),
                to,
                promotion,
            },
        Move::Normal { role, from, capture, to, promotion } => {
            let mut legals = MoveList::new();
            pos.legal_moves(&mut legals);

            // Disambiguate.
            let (rank, file) = legals.iter().fold((false, false), |(rank, file), c| match *c {
                Move::Normal { role: r, to: t, from: candidate, .. } =>
                    if role != r || to != t || from == candidate {
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
                file: Some(from.file()).filter(|_| file),
                rank: Some(from.rank()).filter(|_| rank),
                capture: capture.is_some(),
                to,
                promotion
            }
        },
        Move::EnPassant { from, to, .. } => San::Normal {
            role: Role::Pawn, file: Some(from.file()), rank: None, capture: true, to, promotion: None },
        Move::Castle { rook, king } if rook.file() < king.file() => San::CastleLong,
        Move::Castle { .. } => San::CastleShort,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_write() {
        for san in &["e4", "hxg7", "N2c4", "Qh1=K", "d1=N",
                     "Ba5", "Bba5", "Ra1a8", "--", "O-O", "O-O-O+"] {
            assert_eq!(san.parse::<SanPlus>().expect("valid san").to_string(), *san);
        }
    }
}
