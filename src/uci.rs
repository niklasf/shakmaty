//! Parse and write moves in Universal Chess Interface representation.
//!
//! # Examples
//!
//! Parsing UCIs:
//!
//! ```
//! # use shakmaty::square;
//! use shakmaty::uci::Uci;
//!
//! let uci: Uci = "g1f3".parse().expect("valid uci");
//! assert_eq!(uci, Uci::Normal { from: square::G1, to: square::F3, promotion: None });
//! ```
//!
//! Converting to a legal move in the context of a position:
//!
//! ```
//! # use shakmaty::square;
//! # use shakmaty::Color::White;
//! # use shakmaty::uci::Uci;
//! use shakmaty::{Chess, Setup, Position};
//!
//! # let uci: Uci = "g1f3".parse().expect("valid uci");
//! let pos = Chess::default();
//! let m = uci.to_move(&pos).expect("legal move");
//!
//! let after = pos.play_unchecked(&m);
//! assert_eq!(after.board().piece_at(square::F3), Some(White.knight()));
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

use square;
use square::Square;
use types::{Role, Move};
use position::{Position, MoveError};

/// A move as represented in the UCI protocol.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Uci {
    Normal { from: Square, to: Square, promotion: Option<Role> },
    Put { role: Role, to: Square },
    Null
}

impl FromStr for Uci {
    type Err = ();

    fn from_str(uci: &str) -> Result<Uci, ()> {
        if uci.len() < 4 || uci.len() > 5 {
            return Err(())
        }

        match (Square::from_str(&uci[0..2]), Square::from_str(&uci[2..4]), uci.chars().nth(4)) {
            (Ok(from), Ok(to), Some(promotion)) =>
                return Role::from_char(promotion).map(|role| {
                    Uci::Normal { from, to, promotion: Some(role) }
                }).ok_or(()),
            (Ok(from), Ok(to), None) =>
                return Ok(Uci::Normal { from, to, promotion: None }),
            _ => (),
        }

        if let (Some(piece), Some('@'), Ok(to)) =
               (uci.chars().nth(0), uci.chars().nth(1), Square::from_str(&uci[2..4])) {
            return Role::from_char(piece.to_ascii_lowercase()).map(|role| {
                Uci::Put { role, to }
            }).ok_or(());
        }

        if uci == "0000" {
            return Ok(Uci::Null)
        }

        Err(())
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

impl<'a> Into<Uci> for &'a Move {
    fn into(self) -> Uci {
        match *self {
            Move::Normal { from, to, promotion, .. } =>
                Uci::Normal { from, to, promotion },
            Move::EnPassant { from, to, .. } =>
                Uci::Normal { from, to, promotion: None },
            Move::Castle { king, rook } =>
                Uci::Normal { from: king, to: rook, promotion: None },  // Chess960-style
            Move::Put { role, to } =>
                Uci::Put { role, to },
            Move::Null =>
                Uci::Null
        }
    }
}

impl Into<Uci> for Move {
    fn into(self) -> Uci {
        (&self).into()
    }
}

impl Uci {
    /// Tries to convert the `Uci` to a legal `Move` in the context of a
    /// position.
    pub fn to_move<P: Position>(&self, pos: &P) -> Result<Move, MoveError> {
        let candidate = match *self {
            Uci::Normal { from, to, promotion } => {
                let role = pos.board().role_at(from).ok_or(())?;

                if promotion.is_some() && role != Role::Pawn {
                    return Err(())
                }

                if role == Role::King && pos.castling_rights().contains(to) {
                    Move::Castle { king: from, rook: to }
                } else if role == Role::King &&
                          from == pos.turn().fold(square::E1, square::E8) &&
                          to.rank() == pos.turn().fold(0, 7) &&
                          square::distance(from, to) == 2 {
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
            Uci::Null => return Ok(Move::Null)
        };

        if pos.is_legal(&candidate) {
            Ok(candidate)
        } else {
            Err(())
        }
    }
}
