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

use std::fmt;
use std::ascii::AsciiExt;
use std::char;
use std::ops;
use std::str;

use square::Square;

pub use self::Color::{Black, White};
pub use self::Role::{Pawn, Knight, Bishop, Rook, Queen, King};

/// `White` or `Black`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Color {
    Black,
    White,
}

impl Color {
    pub fn from_char(ch: char) -> Option<Color> {
        match ch {
            'w' => Some(Color::White),
            'b' => Some(Color::Black),
            _ => None
        }
    }

    pub fn from_bool(white: bool) -> Color {
        if white { Color::White } else { Color::Black }
    }

    pub fn fold<T>(self, white: T, black: T) -> T {
        match self {
            Color::White => white,
            Color::Black => black
        }
    }

    pub fn is_white(self) -> bool { self == Color::White }
    pub fn is_black(self) -> bool { self == Color::Black }

    pub fn char(self) -> char { self.fold('w', 'b') }

    pub const fn pawn(self)   -> Piece { Pawn.of(self) }
    pub const fn knight(self) -> Piece { Knight.of(self) }
    pub const fn bishop(self) -> Piece { Bishop.of(self) }
    pub const fn rook(self)   -> Piece { Rook.of(self) }
    pub const fn queen(self)  -> Piece { Queen.of(self) }
    pub const fn king(self)   -> Piece { King.of(self) }
}

impl ops::Not for Color {
    type Output = Color;

    fn not(self) -> Color {
        self.fold(Color::Black, Color::White)
    }
}

/// Piece types: `Pawn`, `Knight`, `Bishop`, `Rook`, `Queen`, `King`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Role {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl Role {
    pub fn from_char(ch: char) -> Option<Role> {
        match ch {
            'p' => Some(Role::Pawn),
            'n' => Some(Role::Knight),
            'b' => Some(Role::Bishop),
            'r' => Some(Role::Rook),
            'q' => Some(Role::Queen),
            'k' => Some(Role::King),
            _ => None
        }
    }

    pub const fn of(self, color: Color) -> Piece {
        Piece { color, role: self }
    }

    pub fn char(self) -> char {
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

/// A piece with `Color` and `Role`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Piece {
    pub color: Color,
    pub role: Role,
}

impl Piece {
    pub fn char(&self) -> char {
        self.color.fold(self.role.char().to_ascii_uppercase(), self.role.char())
    }

    pub fn from_char(ch: char) -> Option<Piece> {
        if ch == ch.to_ascii_lowercase() {
            Role::from_char(ch).map(|role| role.of(Color::Black))
        } else {
            Role::from_char(ch.to_ascii_lowercase()).map(|role| role.of(Color::White))
        }
    }
}

/// Information about a move.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Move {
    Normal { role: Role, from: Square, capture: Option<Role>, to: Square, promotion: Option<Role> },
    EnPassant { from: Square, to: Square },
    Castle { king: Square, rook: Square },
    Put { role: Role, to: Square },
}

impl Move {
    pub fn capture(&self) -> Option<Role> {
        match *self {
            Move::Normal { capture, .. } => capture,
            Move::EnPassant { .. } => Some(Pawn),
            _ => None
        }
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Move::Normal { role, from, capture, to, promotion } => {
                if role != Role::Pawn {
                    write!(f, "{}", role.char().to_ascii_uppercase())?;
                }

                write!(f, "{}{}{}", from, if capture.is_some() { 'x' } else { '-' }, to)?;

                if let Some(p) = promotion {
                    write!(f, "={}", p.char().to_ascii_uppercase())?;
                }

                Ok(())
            },
            Move::EnPassant { from, to, .. } => {
                write!(f, "{}x{}", from, to)
            },
            Move::Castle { king, rook } => {
                if king < rook {
                    write!(f, "O-O")
                } else {
                    write!(f, "O-O-O")
                }
            },
            Move::Put { role, to } => {
                write!(f, "{}@{}", role.char().to_ascii_uppercase(), to)
            },
        }
    }
}

/// The number of checks the respective side needs to give in order to in
/// (in a game of Three-Check).
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct RemainingChecks {
    pub white: u8,
    pub black: u8,
}

impl Default for RemainingChecks {
    fn default() -> RemainingChecks {
        RemainingChecks { white: 3, black: 3 }
    }
}

impl RemainingChecks {
    pub fn by_color(&self, color: Color) -> u8 {
        color.fold(self.white, self.black)
    }

    pub fn by_color_mut(&mut self, color: Color) -> &mut u8 {
        color.fold(&mut self.white, &mut self.black)
    }

    pub fn subtract(&mut self, color: Color) {
        *self.by_color_mut(color) = self.by_color(color).saturating_sub(1);
    }
}

impl fmt::Display for RemainingChecks {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}+{}", self.white, self.black)
    }
}
