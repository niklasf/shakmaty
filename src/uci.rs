//! Parse and write moves in Universal Chess Interface representation.
//!
//! # Examples
//!
//! Parsing UCI moves:
//!
//! ```
//! use shakmaty::{Square, uci::UciMove};
//!
//! let uci: UciMove = "g1f3".parse()?;
//!
//! assert_eq!(uci, UciMove::Normal {
//!     from: Square::G1,
//!     to: Square::F3,
//!     promotion: None,
//! });
//!
//! # Ok::<_, shakmaty::uci::ParseUciMoveError>(())
//! ```
//!
//! Converting to a legal move in the context of a position:
//!
//! ```
//! # use shakmaty::{Square, uci::{IllegalUciMoveError, ParseUciMoveError, UciMove}};
//! use shakmaty::{Color::White, Chess, Setup, Position};
//!
//! # let uci: UciMove = "g1f3".parse()?;
//! let mut pos = Chess::default();
//! let m = uci.to_move(&pos)?;
//!
//! pos.play_unchecked(&m);
//! assert_eq!(pos.board().piece_at(Square::F3), Some(White.knight()));
//!
//! # #[derive(Debug)] struct CommonError;
//! # impl From<IllegalUciMoveError> for CommonError { fn from(_: IllegalUciMoveError) -> Self { Self } }
//! # impl From<ParseUciMoveError> for CommonError { fn from(_: ParseUciMoveError) -> Self { Self } }
//! # Ok::<_, CommonError>(())
//! ```
//!
//! Converting from [`Move`] to [`UciMove`]:
//!
//! ```
//! # use shakmaty::{Square, Move, Role, Chess, Position, uci::UciMove};
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
//! let uci = UciMove::from_standard(&m);
//! assert_eq!(uci.to_string(), "b1c3");
//!
//! let uci = UciMove::from_chess960(&m);
//! assert_eq!(uci.to_string(), "b1c3");
//! ```
//!
//! [`Move`]: super::Move

use core::{fmt, str::FromStr};

use crate::{util::AppendAscii, CastlingMode, CastlingSide, Move, Position, Rank, Role, Square};

/// Error when parsing an invalid UCI move.
#[derive(Clone, Debug)]
pub struct ParseUciMoveError;

#[deprecated = "Use `ParseUciMoveError` instead"]
pub type ParseUciError = ParseUciMoveError;

impl fmt::Display for ParseUciMoveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("invalid uci")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ParseUciMoveError {}

/// Error when UCI move is illegal.
#[derive(Clone, Debug)]
pub struct IllegalUciMoveError;

#[deprecated = "Use `ParseUciMoveError` instead"]
pub type IllegalUciError = IllegalUciMoveError;

impl fmt::Display for IllegalUciMoveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("illegal uci")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for IllegalUciMoveError {}

#[deprecated = "Use `UciMove` instead"]
pub type Uci = UciMove;

/// A move as represented in the UCI protocol.
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum UciMove {
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

impl FromStr for UciMove {
    type Err = ParseUciMoveError;

    fn from_str(uci: &str) -> Result<UciMove, ParseUciMoveError> {
        UciMove::from_ascii(uci.as_bytes())
    }
}

impl fmt::Display for UciMove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.append_to(f)
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for UciMove {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut s = arrayvec::ArrayString::<5>::new();
        let _ = self.append_to(&mut s);
        serializer.serialize_str(&s)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for UciMove {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct UciMoveVisitor;

        impl<'de> serde::de::Visitor<'de> for UciMoveVisitor {
            type Value = UciMove;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("UCI move string")
            }

            fn visit_str<E>(self, value: &str) -> Result<UciMove, E>
            where
                E: serde::de::Error,
            {
                value.parse().map_err(serde::de::Error::custom)
            }
        }

        deserializer.deserialize_str(UciMoveVisitor)
    }
}

impl UciMove {
    /// Parses a move in UCI notation.
    ///
    /// # Errors
    ///
    /// Returns [`ParseUciMoveError`] if `uci` is not syntactically valid.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Square, uci::UciMove};
    ///
    /// let uci = UciMove::from_ascii(b"e4e5")?;
    ///
    /// assert_eq!(uci, UciMove::Normal {
    ///     from: Square::E4,
    ///     to: Square::E5,
    ///     promotion: None,
    /// });
    ///
    /// # Ok::<_, shakmaty::uci::ParseUciMoveError>(())
    /// ```
    pub fn from_ascii(uci: &[u8]) -> Result<UciMove, ParseUciMoveError> {
        if uci.len() != 4 && uci.len() != 5 {
            return Err(ParseUciMoveError);
        }

        if uci == b"0000" {
            return Ok(UciMove::Null);
        }

        let to = Square::from_ascii(&uci[2..4]).map_err(|_| ParseUciMoveError)?;

        if uci[1] == b'@' {
            Ok(UciMove::Put {
                role: Role::from_char(char::from(uci[0])).ok_or(ParseUciMoveError)?,
                to,
            })
        } else {
            let from = Square::from_ascii(&uci[0..2]).map_err(|_| ParseUciMoveError)?;
            if uci.len() == 5 {
                Ok(UciMove::Normal {
                    from,
                    to,
                    promotion: Some(Role::from_char(char::from(uci[4])).ok_or(ParseUciMoveError)?),
                })
            } else {
                Ok(UciMove::Normal {
                    from,
                    to,
                    promotion: None,
                })
            }
        }
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
    /// use shakmaty::{Move, Square, uci::UciMove};
    ///
    /// let m = Move::Castle {
    ///     king: Square::E8,
    ///     rook: Square::H8,
    /// };
    ///
    /// let uci = UciMove::from_standard(&m);
    /// assert_eq!(uci.to_string(), "e8g8");
    /// ```
    pub fn from_standard(m: &Move) -> UciMove {
        match *m {
            Move::Castle { king, rook } => {
                let side = CastlingSide::from_king_side(king < rook);
                UciMove::Normal {
                    from: king,
                    to: Square::from_coords(side.king_to_file(), king.rank()),
                    promotion: None,
                }
            }
            _ => UciMove::from_chess960(m),
        }
    }

    /// Converts a move to UCI notation. Castling moves are represented as
    /// a move of the king to the corresponding rook square, independently of
    /// the position.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Move, Square, uci::UciMove};
    ///
    /// let m = Move::Castle {
    ///     king: Square::E8,
    ///     rook: Square::H8,
    /// };
    ///
    /// let uci = UciMove::from_chess960(&m);
    /// assert_eq!(uci.to_string(), "e8h8");
    /// ```
    pub const fn from_chess960(m: &Move) -> UciMove {
        match *m {
            Move::Normal {
                from,
                to,
                promotion,
                ..
            } => UciMove::Normal {
                from,
                to,
                promotion,
            },
            Move::EnPassant { from, to, .. } => UciMove::Normal {
                from,
                to,
                promotion: None,
            },
            Move::Castle { king, rook } => UciMove::Normal {
                from: king,
                to: rook,
                promotion: None,
            }, // Chess960-style
            Move::Put { role, to } => UciMove::Put { role, to },
        }
    }

    /// See [`UciMove::from_standard()`] or [`UciMove::from_chess960()`].
    pub fn from_move(m: &Move, mode: CastlingMode) -> UciMove {
        match mode {
            CastlingMode::Standard => UciMove::from_standard(m),
            CastlingMode::Chess960 => UciMove::from_chess960(m),
        }
    }

    /// Tries to convert the `Uci` to a legal [`Move`] in the context of a
    /// position.
    ///
    /// # Errors
    ///
    /// Returns [`IllegalUciMoveError`] if the move is not legal.
    ///
    /// [`Move`]: super::Move
    pub fn to_move<P: Position>(&self, pos: &P) -> Result<Move, IllegalUciMoveError> {
        let candidate = match *self {
            UciMove::Normal {
                from,
                to,
                promotion,
            } => {
                let role = pos.board().role_at(from).ok_or(IllegalUciMoveError)?;

                if promotion.is_some() && role != Role::Pawn {
                    return Err(IllegalUciMoveError);
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
            UciMove::Put { role, to } => Move::Put { role, to },
            UciMove::Null => return Err(IllegalUciMoveError),
        };

        if pos.is_legal(&candidate) {
            Ok(candidate)
        } else {
            Err(IllegalUciMoveError)
        }
    }

    #[must_use]
    pub fn to_mirrored(&self) -> UciMove {
        match *self {
            UciMove::Normal {
                from,
                to,
                promotion,
            } => UciMove::Normal {
                from: from.flip_vertical(),
                to: to.flip_vertical(),
                promotion,
            },
            UciMove::Put { role, to } => UciMove::Put {
                role,
                to: to.flip_vertical(),
            },
            UciMove::Null => UciMove::Null,
        }
    }

    fn append_to<W: AppendAscii>(&self, f: &mut W) -> Result<(), W::Error> {
        match *self {
            UciMove::Normal {
                from,
                to,
                promotion,
            } => {
                from.append_to(f)?;
                to.append_to(f)?;
                if let Some(promotion) = promotion {
                    f.append_ascii(promotion.char())?;
                }
            }
            UciMove::Put { role, to } => {
                f.append_ascii(role.upper_char())?;
                f.append_ascii('@')?;
                to.append_to(f)?;
            }
            UciMove::Null => {
                f.append_ascii('0')?;
                f.append_ascii('0')?;
                f.append_ascii('0')?;
                f.append_ascii('0')?;
            }
        }
        Ok(())
    }

    #[cfg(feature = "alloc")]
    pub fn append_to_string(&self, s: &mut alloc::string::String) {
        let _ = self.append_to(s);
    }

    #[cfg(feature = "alloc")]
    pub fn append_ascii_to(&self, buf: &mut alloc::vec::Vec<u8>) {
        let _ = self.append_to(buf);
    }

    #[cfg(feature = "std")]
    pub fn write_ascii_to<W: std::io::Write>(&self, w: W) -> std::io::Result<()> {
        self.append_to(&mut crate::util::WriteAscii(w))
    }
}

impl Move {
    /// See [`UciMove::from_move()`].
    pub fn to_uci(&self, mode: CastlingMode) -> UciMove {
        UciMove::from_move(self, mode)
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
            .parse::<UciMove>()
            .expect("e4")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&e4);
        let nc6 = "b8c6"
            .parse::<UciMove>()
            .expect("Nc6")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&nc6);
        let e5 = "e4e5"
            .parse::<UciMove>()
            .expect("e5")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&e5);
        let d5 = "d7d5"
            .parse::<UciMove>()
            .expect("d5")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&d5);
        let exd5 = "e5d6"
            .parse::<UciMove>()
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
            .parse::<UciMove>()
            .expect("e4")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&e4);
        let d5 = "d7d5"
            .parse::<UciMove>()
            .expect("d5")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&d5);
        let exd5 = "e4d5"
            .parse::<UciMove>()
            .expect("exd5")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&exd5);
        let qxd5 = "d8d5"
            .parse::<UciMove>()
            .expect("Qxd5")
            .to_move(&pos)
            .expect("legal");
        pos.play_unchecked(&qxd5);
        let p_at_d7 = "P@d7"
            .parse::<UciMove>()
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
        let uci = "g2h1".parse::<UciMove>().expect("valid uci");
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
        use alloc::string::ToString as _;
        let mut pos: Chess = "nbqrknbr/pppppppp/8/8/8/8/PPPPPPPP/NBQRKNBR w KQkq - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position(CastlingMode::Chess960)
            .expect("valid position");
        for uci in &["f2f4", "d7d6", "f1g3", "c8g4", "g1f2", "e8d8", "e1g1"] {
            let m = uci
                .parse::<UciMove>()
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
