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

//! Parse and write moves in Universal Chess Interface representation.
//!
//! # Examples
//!
//! Parsing UCIs:
//!
//! ```
//! # use std::error::Error;
//! #
//! # fn try_main() -> Result<(), Box<Error>> {
//! # use shakmaty::square;
//! use shakmaty::uci::Uci;
//!
//! let uci: Uci = "g1f3".parse()?;
//!
//! assert_eq!(uci, Uci::Normal {
//!     from: square::G1,
//!     to: square::F3,
//!     promotion: None
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
//! Converting to a legal move in the context of a position:
//!
//! ```
//! # use std::error::Error;
//! #
//! # fn try_main() -> Result<(), Box<Error>> {
//! # use shakmaty::square;
//! # use shakmaty::Color::White;
//! # use shakmaty::uci::Uci;
//! use shakmaty::{Chess, Setup, Position};
//!
//! # let uci: Uci = "g1f3".parse()?;
//! let pos = Chess::default();
//! let m = uci.to_move(&pos)?;
//!
//! let after = pos.play_unchecked(&m);
//! assert_eq!(after.board().piece_at(square::F3), Some(White.knight()));
//! #
//! #     Ok(())
//! # }
//! #
//! # fn main() {
//! #     try_main().unwrap();
//! # }
//! ```
//!
//! Converting from `Move` to `Uci`:
//!
//! ```
//! # use shakmaty::{Move, Role};
//! # use shakmaty::square;
//! # use shakmaty::uci::Uci;
//! use std::convert::From;
//!
//! let m = Move::Normal {
//!     role: Role::Queen,
//!     from: square::A1,
//!     to: square::H8,
//!     capture: Some(Role::Rook),
//!     promotion: None,
//! };
//!
//! let uci: Uci = m.into();
//! assert_eq!(uci.to_string(), "a1h8");
//! ```

use std::fmt;
use std::ascii::AsciiExt;
use std::str::FromStr;
use std::error::Error;

use square;
use square::Square;
use types::{Role, Move};
use position::{Position, IllegalMove};

/// Error when parsing an invalid UCI.
pub struct InvalidUci { _priv: () }

impl fmt::Debug for InvalidUci {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("InvalidUci").finish()
    }
}

impl fmt::Display for InvalidUci {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "invalid uci".fmt(f)
    }
}

impl Error for InvalidUci {
    fn description(&self) -> &str { "invalid uci" }
}

/// A move as represented in the UCI protocol.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Uci {
    Normal { from: Square, to: Square, promotion: Option<Role> },
    Put { role: Role, to: Square },
    Null
}

impl FromStr for Uci {
    type Err = InvalidUci;

    fn from_str(uci: &str) -> Result<Uci, InvalidUci> {
        // Checking is_ascii() will allow us to safely slice at byte
        // boundaries.
        if uci.len() < 4 || uci.len() > 5 || !uci.is_ascii() {
            return Err(InvalidUci { _priv: () })
        }

        match (Square::from_str(&uci[0..2]), Square::from_str(&uci[2..4]), uci.chars().nth(4)) {
            (Ok(from), Ok(to), Some(promotion)) =>
                return Role::from_char(promotion).map(|role| {
                    Uci::Normal { from, to, promotion: Some(role) }
                }).ok_or(InvalidUci { _priv: () }),
            (Ok(from), Ok(to), None) =>
                return Ok(Uci::Normal { from, to, promotion: None }),
            _ => (),
        }

        if let (Some(piece), Some('@'), Ok(to)) =
               (uci.chars().nth(0), uci.chars().nth(1), Square::from_str(&uci[2..4])) {
            return Role::from_char(piece.to_ascii_lowercase()).map(|role| {
                Uci::Put { role, to }
            }).ok_or(InvalidUci { _priv: () });
        }

        if uci == "0000" {
            return Ok(Uci::Null)
        }

        Err(InvalidUci { _priv: () })
    }
}

impl fmt::Display for Uci {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Uci::Normal { from, to, promotion: None } =>
                write!(f, "{}{}", from, to),
            Uci::Normal { from, to, promotion: Some(promotion) } =>
                write!(f, "{}{}{}", from, to, promotion.char()),
            Uci::Put { to, role } =>
                write!(f, "{}@{}", role.char().to_ascii_uppercase(), to),
            Uci::Null =>
                write!(f, "0000")
        }
    }
}

impl<'a> From<&'a Move> for Uci {
    fn from(m: &'a Move) -> Uci {
        match *m {
            Move::Normal { from, to, promotion, .. } =>
                Uci::Normal { from, to, promotion },
            Move::EnPassant { from, to, .. } =>
                Uci::Normal { from, to, promotion: None },
            Move::Castle { king, rook } =>
                Uci::Normal { from: king, to: rook, promotion: None },  // Chess960-style
            Move::Put { role, to } =>
                Uci::Put { role, to },
        }
    }
}

impl From<Move> for Uci {
    fn from(m: Move) -> Uci {
        (&m).into()
    }
}

impl Uci {
    /// Tries to convert the `Uci` to a legal `Move` in the context of a
    /// position.
    ///
    /// # Errors
    ///
    /// Errors if the move is not legal.
    pub fn to_move<P: Position>(&self, pos: &P) -> Result<Move, IllegalMove> {
        let candidate = match *self {
            Uci::Normal { from, to, promotion } => {
                let role = pos.board().role_at(from).ok_or(IllegalMove {})?;

                if promotion.is_some() && role != Role::Pawn {
                    return Err(IllegalMove {})
                }

                if role == Role::King && pos.castling_rights().contains(to) {
                    Move::Castle { king: from, rook: to }
                } else if role == Role::King &&
                          from == pos.turn().fold(square::E1, square::E8) &&
                          to.rank() == pos.turn().fold(0, 7) &&
                          from.distance(to) == 2 {
                    if from.file() < to.file() {
                        Move::Castle { king: from, rook: pos.turn().fold(square::H1, square::H8) }
                    } else {
                        Move::Castle { king: from, rook: pos.turn().fold(square::A1, square::A8) }
                    }
                } else {
                    Move::Normal { role, from, capture: pos.board().role_at(to), to, promotion }
                }
            },
            Uci::Put { role, to } => Move::Put { role, to },
            Uci::Null => return Err(IllegalMove {})
        };

        if pos.is_legal(&candidate) {
            Ok(candidate)
        } else {
            Err(IllegalMove {})
        }
    }
}
