// This file is part of the shakmaty library.
// Copyright (C) 2017-2022 Niklas Fiekas <niklas.fiekas@backscattering.de>
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
//! use shakmaty::{Square, uci::Uci};
//!
//! let uci: Uci = "g1f3".parse()?;
//!
//! assert_eq!(uci, Uci::Normal {
//!     from: Square::G1,
//!     to: Square::F3,
//!     promotion: None,
//! });
//!
//! # Ok::<_, shakmaty::uci::ParseUciError>(())
//! ```
//!
//! Converting to a legal move in the context of a position:
//!
//! ```
//! # use shakmaty::{Square, uci::{IllegalUciError, ParseUciError, Uci}};
//! use shakmaty::{Color::White, Chess, Setup, Position};
//!
//! # let uci: Uci = "g1f3".parse()?;
//! let mut pos = Chess::default();
//! let m = uci.to_move(&pos)?;
//!
//! pos.play_unchecked(&m);
//! assert_eq!(pos.board().piece_at(Square::F3), Some(White.knight()));
//!
//! # #[derive(Debug)] struct CommonError;
//! # impl From<IllegalUciError> for CommonError { fn from(_: IllegalUciError) -> Self { Self } }
//! # impl From<ParseUciError> for CommonError { fn from(_: ParseUciError) -> Self { Self } }
//! # Ok::<_, CommonError>(())
//! ```
//!
//! Converting from [`Move`] to [`Uci`]:
//!
//! ```
//! # use shakmaty::{Square, Move, Role, Chess, Position, uci::Uci};
//! #
//! let pos = Chess::default();
//!
//! let m = Move::Normal {
//!     role: Role::Knight,
//!     from: Square::B1,
//!     to: Square::C3,
//!     capture: None,
//!     promotion: None,
//! };
//!
//! let uci = m.to_uci(pos.castles().mode());
//! assert_eq!(uci.to_string(), "b1c3");
//!
//! let uci = Uci::from_standard(&m);
//! assert_eq!(uci.to_string(), "b1c3");
//!
//! let uci = Uci::from_chess960(&m);
//! assert_eq!(uci.to_string(), "b1c3");
//! ```
//!
//! [`Move`]: super::Move

use core::{fmt, str::FromStr};

use crate::{CastlingMode, CastlingSide, Move, Position, Rank, Role, Square};

/// Error when parsing an invalid UCI.
#[derive(Clone, Debug)]
pub struct ParseUciError;

impl fmt::Display for ParseUciError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("invalid uci")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ParseUciError {}

/// Error when UCI is illegal.
#[derive(Clone, Debug)]
pub struct IllegalUciError;

impl fmt::Display for IllegalUciError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("illegal uci")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for IllegalUciError {}

/// A move as represented in the UCI protocol.
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum Uci {
    /// A normal move, e.g. `e2e4` or `h2h1q`.
    Normal {
        from: Square,
        to: Square,
        promotion: Option<Role>,
    },
    /// A piece drop, e.g. `Q@f7`.
    Put { role: Role, to: Square },
    /// A null move (`0000`).
    Null,
}

impl FromStr for Uci {
    type Err = ParseUciError;

    fn from_str(uci: &str) -> Result<Self, ParseUciError> {
        Self::from_ascii(uci.as_bytes())
    }
}

impl fmt::Display for Uci {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Normal {
                from,
                to,
                promotion: None,
            } => write!(f, "{}{}", from, to),
            Self::Normal {
                from,
                to,
                promotion: Some(promotion),
            } => write!(f, "{}{}{}", from, to, promotion.char()),
            Self::Put { to, role } => write!(f, "{}@{}", role.upper_char(), to),
            Self::Null => f.write_str("0000"),
        }
    }
}

impl Uci {
    /// Parses a move in UCI notation.
    ///
    /// # Errors
    ///
    /// Returns [`ParseUciError`] if `uci` is not syntactically valid.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Square, uci::Uci};
    ///
    /// let uci = Uci::from_ascii(b"e4e5")?;
    ///
    /// assert_eq!(uci, Uci::Normal {
    ///     from: Square::E4,
    ///     to: Square::E5,
    ///     promotion: None,
    /// });
    ///
    /// # Ok::<_, shakmaty::uci::ParseUciError>(())
    /// ```
    pub fn from_ascii(uci: &[u8]) -> Result<Self, ParseUciError> {
        if uci.len() != 4 && uci.len() != 5 {
            return Err(ParseUciError);
        }

        if uci == b"0000" {
            return Ok(Self::Null);
        }

        let to = Square::from_ascii(&uci[2..4]).map_err(|_| ParseUciError)?;

        Ok(if uci[1] == b'@' {
            Self::Put {
                role: Role::from_char(char::from(uci[0])).ok_or(ParseUciError)?,
                to,
            }
        } else {
            let from = Square::from_ascii(&uci[0..2]).map_err(|_| ParseUciError)?;
            if uci.len() == 5 {
                Self::Normal {
                    from,
                    to,
                    promotion: Some(Role::from_char(char::from(uci[4])).ok_or(ParseUciError)?),
                }
            } else {
                Self::Normal {
                    from,
                    to,
                    promotion: None,
                }
            }
        })
    }

    /// Converts a move to UCI notation. Castling moves are represented as
    /// a move of the king to its new position.
    ///
    /// Warning: Using standard notation for castling moves in Chess960 may
    /// create moves that are illegal or moves that can be confused with
    /// king moves.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Move, Square, uci::Uci};
    ///
    /// let m = Move::Castle {
    ///     king: Square::E8,
    ///     rook: Square::H8,
    /// };
    ///
    /// let uci = Uci::from_standard(&m);
    /// assert_eq!(uci.to_string(), "e8g8");
    /// ```
    pub fn from_standard(m: &Move) -> Self {
        match *m {
            Move::Castle { king, rook } => {
                let side = CastlingSide::from_king_side(king < rook);
                Self::Normal {
                    from: king,
                    to: Square::from_coords(side.king_to_file(), king.rank()),
                    promotion: None,
                }
            }
            _ => Self::from_chess960(m),
        }
    }

    /// Converts a move to UCI notation. Castling moves are represented as
    /// a move of the king to the corresponding rook square, independently of
    /// the position.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Move, Square, uci::Uci};
    ///
    /// let m = Move::Castle {
    ///     king: Square::E8,
    ///     rook: Square::H8,
    /// };
    ///
    /// let uci = Uci::from_chess960(&m);
    /// assert_eq!(uci.to_string(), "e8h8");
    /// ```
    pub const fn from_chess960(m: &Move) -> Self {
        match *m {
            Move::Normal {
                from,
                to,
                promotion,
                ..
            } => Self::Normal {
                from,
                to,
                promotion,
            },
            Move::EnPassant { from, to, .. } => Self::Normal {
                from,
                to,
                promotion: None,
            },
            Move::Castle { king, rook } => Self::Normal {
                from: king,
                to: rook,
                promotion: None,
            }, // Chess960-style
            Move::Put { role, to } => Self::Put { role, to },
        }
    }

    /// See [`Uci::from_standard()`] or [`Uci::from_chess960()`].
    pub fn from_move(m: &Move, mode: CastlingMode) -> Self {
        match mode {
            CastlingMode::Standard => Self::from_standard(m),
            CastlingMode::Chess960 => Self::from_chess960(m),
        }
    }

    /// Tries to convert the `Uci` to a legal [`Move`] in the context of a
    /// position.
    ///
    /// # Errors
    ///
    /// Returns [`IllegalUciError`] if the move is not legal.
    ///
    /// [`Move`]: super::Move
    pub fn to_move<P: Position>(&self, pos: &P) -> Result<Move, IllegalUciError> {
        let candidate = match *self {
            Self::Normal {
                from,
                to,
                promotion,
            } => {
                let role = pos.board().role_at(from).ok_or(IllegalUciError)?;

                if promotion.is_some() && role != Role::Pawn {
                    return Err(IllegalUciError);
                }

                if role == Role::King && (pos.castles().castling_rights() & pos.us()).contains(to) {
                    Move::Castle {
                        king: from,
                        rook: to,
                    }
                } else if role == Role::King
                    && from == pos.turn().fold_wb(Square::E1, Square::E8)
                    && to.rank() == pos.turn().fold_wb(Rank::First, Rank::Eighth)
                    && from.distance(to) == 2
                {
                    if from.file() < to.file() {
                        Move::Castle {
                            king: from,
                            rook: pos.turn().fold_wb(Square::H1, Square::H8),
                        }
                    } else {
                        Move::Castle {
                            king: from,
                            rook: pos.turn().fold_wb(Square::A1, Square::A8),
                        }
                    }
                } else if role == Role::Pawn
                    && from.file() != to.file()
                    && !pos.board().occupied().contains(to)
                {
                    Move::EnPassant { from, to }
                } else {
                    Move::Normal {
                        role,
                        from,
                        capture: pos.board().role_at(to),
                        to,
                        promotion,
                    }
                }
            }
            Self::Put { role, to } => Move::Put { role, to },
            Self::Null => return Err(IllegalUciError),
        };

        if pos.is_legal(&candidate) {
            Ok(candidate)
        } else {
            Err(IllegalUciError)
        }
    }
}

impl Move {
    /// See [`Uci::from_move()`].
    pub fn to_uci(&self, mode: CastlingMode) -> Uci {
        Uci::from_move(self, mode)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{fen::Fen, Chess};

    #[test]
    pub fn test_uci_to_en_passant() {
        let mut pos = Chess::default();
        let e4 = "e2e4"
            .parse::<Uci>()
            .expect("e4")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&e4);
        let nc6 = "b8c6"
            .parse::<Uci>()
            .expect("Nc6")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&nc6);
        let e5 = "e4e5"
            .parse::<Uci>()
            .expect("e5")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&e5);
        let d5 = "d7d5"
            .parse::<Uci>()
            .expect("d5")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&d5);
        let exd5 = "e5d6"
            .parse::<Uci>()
            .expect("exd6")
            .to_move(&pos)
            .expect("legal en passant");
        assert!(exd5.is_en_passant());
    }

    #[cfg(feature = "variant")]
    #[test]
    pub fn test_uci_to_crazyhouse() {
        use crate::position::variant::Crazyhouse;

        let mut pos = Crazyhouse::default();
        let e4 = "e2e4"
            .parse::<Uci>()
            .expect("e4")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&e4);
        let d5 = "d7d5"
            .parse::<Uci>()
            .expect("d5")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&d5);
        let exd5 = "e4d5"
            .parse::<Uci>()
            .expect("exd5")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&exd5);
        let qxd5 = "d8d5"
            .parse::<Uci>()
            .expect("Qxd5")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&qxd5);
        let p_at_d7 = "P@d7"
            .parse::<Uci>()
            .expect("P@d7+")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&p_at_d7);
        assert!(pos.is_check());
    }

    #[test]
    fn test_king_captures_ummoved_rook() {
        let pos: Chess = "8/8/8/B2p3Q/2qPp1P1/b7/2P2PkP/4K2R b K - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position(CastlingMode::Standard)
            .expect("valid position");
        let uci = "g2h1".parse::<Uci>().expect("valid uci");
        let m = uci.to_move(&pos).expect("legal uci");
        assert_eq!(
            m,
            Move::Normal {
                role: Role::King,
                from: Square::G2,
                capture: Some(Role::Rook),
                to: Square::H1,
                promotion: None,
            }
        );
    }

    #[cfg(feature = "alloc")]
    #[test]
    fn test_uci_to_castles() {
        use alloc::string::ToString;
        let mut pos: Chess = "nbqrknbr/pppppppp/8/8/8/8/PPPPPPPP/NBQRKNBR w KQkq - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position(CastlingMode::Chess960)
            .expect("valid position");
        for uci in &["f2f4", "d7d6", "f1g3", "c8g4", "g1f2", "e8d8", "e1g1"] {
            let m = uci
                .parse::<Uci>()
                .expect("valid uci")
                .to_move(&pos)
                .expect("legal");
            pos.play_unchecked(&m);
        }
        assert_eq!(
            Fen::from_position(pos, crate::EnPassantMode::Legal).to_string(),
            "nbkr1nbr/ppp1pppp/3p4/8/5Pq1/6N1/PPPPPBPP/NBQR1RK1 b - - 5 4"
        );
    }
}
