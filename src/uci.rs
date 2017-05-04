use std::fmt;
use std::ascii::AsciiExt;
use std::str::FromStr;

use square::Square;
use types::{Role, Move};

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
            _ => ()
        }

        match (uci.chars().nth(0), uci.chars().nth(1), Square::from_str(&uci[2..4])) {
            (Some(piece), Some('@'), Ok(to)) =>
                return Role::from_char(piece.to_ascii_lowercase()).map(|role| {
                    Uci::Put { role, to }
                }).ok_or(()),
            _ => ()
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
