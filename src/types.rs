use std::fmt;
use std::ascii::AsciiExt;
use std::char;
use std::ops;

use square::Square;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Color {
    Black,
    White,
}

impl Color {
    pub fn fold<T>(self, white: T, black: T) -> T {
        match self {
            Color::Black => black,
            Color::White => white
        }
    }

    pub fn pawn(self)   -> Piece { Role::Pawn.of(self) }
    pub fn knight(self) -> Piece { Role::Knight.of(self) }
    pub fn bishop(self) -> Piece { Role::Bishop.of(self) }
    pub fn rook(self)   -> Piece { Role::Rook.of(self) }
    pub fn queen(self)  -> Piece { Role::Queen.of(self) }
    pub fn king(self)   -> Piece { Role::King.of(self) }
}

impl ops::Not for Color {
    type Output = Color;

    fn not(self) -> Color {
        self.fold(Color::Black, Color::White)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Role {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl Role {
    pub fn of(self, color: Color) -> Piece {
        Piece { color, role: self }
    }

    pub fn chr(self) -> char {
        match self {
            Role::Pawn =>   'p',
            Role::Knight => 'n',
            Role::Bishop => 'b',
            Role::Rook =>   'r',
            Role::Queen =>  'q',
            Role::King =>   'k'
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Piece {
    pub color: Color,
    pub role: Role,
}

impl Piece {
    pub fn chr(self) -> char {
        self.color.fold(self.role.chr().to_ascii_uppercase(), self.role.chr())
    }
}

#[derive(Debug, Clone)]
pub enum Move {
    Normal { from: Square, to: Square, promotion: Option<Role> },
    Put { to: Square, role: Role },
}

impl Move {
    pub fn from_uci(uci: &str) -> Option<Move> {
        if uci.len() < 4 || uci.len() > 5 {
            return None
        }

        Square::from_str(&uci[2..4]).and_then(|to| {
            Square::from_str(&uci[0..2]).map(|from| {
                // TODO: Promotions
                Move::Normal { from, to, promotion: None }
            })
            // TODO: Drops
       })
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Move::Normal { from, to, promotion: None } =>
                write!(f, "{}{}", from, to),
            &Move::Normal { from, to, promotion: Some(promotion) } =>
                write!(f, "{}{}{}", from, to, promotion.chr()),
            &Move::Put { to, role } =>
                write!(f, "{}@{}", role.chr().to_ascii_uppercase(), to)
        }
    }
}
