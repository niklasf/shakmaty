// This file is part of the shakmaty library.
// Copyright (C) 2017-2019 Niklas Fiekas <niklas.fiekas@backscattering.de>
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
use std::char;
use std::ops;

use crate::square::Square;

pub use self::Color::{Black, White};
pub use self::Role::{Bishop, King, Knight, Pawn, Queen, Rook};

/// `White` or `Black`.
#[allow(missing_docs)]
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum Color {
    Black = 0,
    White = 1,
}

impl Color {
    pub fn from_char(ch: char) -> Option<Color> {
        match ch {
            'w' => Some(Color::White),
            'b' => Some(Color::Black),
            _ => None,
        }
    }

    #[inline]
    pub fn from_white(white: bool) -> Color {
        if white { Color::White } else { Color::Black }
    }

    #[inline]
    pub fn from_black(black: bool) -> Color {
        if black { Color::Black } else { Color::White }
    }

    #[inline]
    pub fn fold<T>(self, white: T, black: T) -> T {
        match self {
            Color::White => white,
            Color::Black => black
        }
    }

    #[inline]
    pub fn is_white(self) -> bool { self == Color::White }
    #[inline]
    pub fn is_black(self) -> bool { self == Color::Black }

    pub fn char(self) -> char { self.fold('w', 'b') }

    #[inline]
    pub fn pawn(self)   -> Piece { Pawn.of(self) }
    #[inline]
    pub fn knight(self) -> Piece { Knight.of(self) }
    #[inline]
    pub fn bishop(self) -> Piece { Bishop.of(self) }
    #[inline]
    pub fn rook(self)   -> Piece { Rook.of(self) }
    #[inline]
    pub fn queen(self)  -> Piece { Queen.of(self) }
    #[inline]
    pub fn king(self)   -> Piece { King.of(self) }
}

impl ops::Not for Color {
    type Output = Color;

    #[inline]
    fn not(self) -> Color {
        self.fold(Color::Black, Color::White)
    }
}

impl ops::BitXor<bool> for Color {
    type Output = Color;

    #[inline]
    fn bitxor(self, flip: bool) -> Color {
        Color::from_white(self.is_white() ^ flip)
    }
}

/// Piece types: `Pawn`, `Knight`, `Bishop`, `Rook`, `Queen`, `King`.
///
/// # Examples
///
/// ```
/// use shakmaty::Role;
///
/// // Piece types are indexed from 1 to 6.
/// assert_eq!(u32::from(Role::Pawn), 1);
/// assert_eq!(u32::from(Role::King), 6);
/// ```
#[allow(missing_docs)]
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub enum Role {
    Pawn = 1,
    Knight = 2,
    Bishop = 3,
    Rook = 4,
    Queen = 5,
    King = 6,
}

impl Role {
    /// Gets the piece type from its English letter.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Role;
    ///
    /// assert_eq!(Role::from_char('K'), Some(Role::King));
    /// assert_eq!(Role::from_char('n'), Some(Role::Knight));
    ///
    /// assert_eq!(Role::from_char('X'), None);
    /// ```
    pub fn from_char(ch: char) -> Option<Role> {
        match ch {
            'P' | 'p' => Some(Role::Pawn),
            'N' | 'n' => Some(Role::Knight),
            'B' | 'b' => Some(Role::Bishop),
            'R' | 'r' => Some(Role::Rook),
            'Q' | 'q' => Some(Role::Queen),
            'K' | 'k' => Some(Role::King),
            _ => None,
        }
    }

    /// Gets a [`Piece`](struct.Piece.html) of the given color.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Color, Role};
    ///
    /// assert_eq!(Role::King.of(Color::Black), Color::Black.king());
    /// ```
    #[inline]
    pub fn of(self, color: Color) -> Piece {
        Piece { color, role: self }
    }

    /// Gets the English letter for the piece type.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Role;
    ///
    /// assert_eq!(Role::Rook.char(), 'r');
    /// ```
    pub fn char(self) -> char {
        match self {
            Role::Pawn => 'p',
            Role::Knight => 'n',
            Role::Bishop => 'b',
            Role::Rook => 'r',
            Role::Queen => 'q',
            Role::King => 'k',
        }
    }

    /// Gets the uppercase English letter for the piece type.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Role;
    ///
    /// assert_eq!(Role::Rook.upper_char(), 'R');
    /// ```
    pub fn upper_char(self) -> char {
        match self {
            Role::Pawn => 'P',
            Role::Knight => 'N',
            Role::Bishop => 'B',
            Role::Rook => 'R',
            Role::Queen => 'Q',
            Role::King => 'K',
        }
    }
}

macro_rules! int_from_role_impl {
    ($($t:ty)+) => {
        $(impl From<Role> for $t {
            #[inline]
            fn from(role: Role) -> $t {
                role as $t
            }
        })+
    }
}

int_from_role_impl! { u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize }

macro_rules! try_role_from_int_impl {
    ($($t:ty)+) => {
        $(#[cfg(nightly)] impl std::convert::TryFrom<$t> for Role {
            type Error = crate::errors::TryFromIntError;

            #[inline]
            fn try_from(value: $t) -> Result<Role, Self::Error> {
                Ok(match value {
                    1 => Role::Pawn,
                    2 => Role::Knight,
                    3 => Role::Bishop,
                    4 => Role::Rook,
                    5 => Role::Queen,
                    6 => Role::King,
                    _ => return Err(crate::errors::TryFromIntError),
                })
            }
        })+
    }
}

try_role_from_int_impl! { u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize }

pub const ROLES: [Role; 6] = [Pawn, Knight, Bishop, Rook, Queen, King];

/// A piece with [`Color`] and [`Role`].
///
/// [`Color`]: enum.Color.html
/// [`Role`]: enum.Role.html
#[allow(missing_docs)]
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Piece {
    pub color: Color,
    pub role: Role,
}

impl Piece {
    pub fn char(self) -> char {
        self.color.fold(self.role.upper_char(), self.role.char())
    }

    pub fn from_char(ch: char) -> Option<Piece> {
        Role::from_char(ch).map(|role| {
            role.of(Color::from_white(32 & ch as u8 == 0))
        })
    }
}

/// Information about a move.
#[derive(Clone, Eq, PartialEq, Debug)]
#[cfg_attr(nightly, repr(align(4)))]
pub enum Move {
    Normal {
        role: Role,
        from: Square,
        capture: Option<Role>,
        to: Square,
        promotion: Option<Role>,
    },
    EnPassant { from: Square, to: Square },
    Castle { king: Square, rook: Square },
    Put { role: Role, to: Square },
}

impl Move {
    /// Gets the role of the moved piece.
    pub fn role(&self) -> Role {
        match *self {
            Move::Normal { role, .. } | Move::Put { role, .. } => role,
            Move::EnPassant { .. } => Role::Pawn,
            Move::Castle { .. } => Role::King,
        }
    }

    /// Gets the origin square or `None` for drops.
    pub fn from(&self) -> Option<Square> {
        match *self {
            Move::Normal { from, .. } | Move::EnPassant { from, .. } => Some(from),
            Move::Castle { king, .. } => Some(king),
            Move::Put { .. } => None,
        }
    }

    /// Gets the target square. For castling moves this is the corresponding
    /// rook square.
    pub fn to(&self) -> Square {
        match *self {
            Move::Normal { to, .. } | Move::EnPassant { to, .. } | Move::Put { to, .. } => to,
            Move::Castle { rook, .. } => rook,
        }
    }

    /// Gets the role of the captured piece or `None`.
    pub fn capture(&self) -> Option<Role> {
        match *self {
            Move::Normal { capture, .. } => capture,
            Move::EnPassant { .. } => Some(Pawn),
            _ => None,
        }
    }

    /// Checks if the move is a capture.
    pub fn is_capture(&self) -> bool {
        match *self {
            Move::Normal { capture: Some(_), .. } | Move::EnPassant { .. } => true,
            _ => false,
        }
    }

    /// Checks if the move is en passant.
    pub fn is_en_passant(&self) -> bool {
        match *self {
            Move::EnPassant { .. } => true,
            _ => false,
        }
    }

    /// Checks if the move zeros the half-move clock.
    pub fn is_zeroing(&self) -> bool {
        match *self {
            Move::Normal { role: Role::Pawn, ..} | Move::Normal { capture: Some(_), .. } | Move::EnPassant { .. } => true,
            _ => false,
        }
    }

    /// Gets the castling side.
    pub fn castling_side(&self) -> Option<CastlingSide> {
        match *self {
            Move::Castle { king, rook } if king < rook => Some(CastlingSide::KingSide),
            Move::Castle { .. } => Some(CastlingSide::QueenSide),
            _ => None,
        }
    }

    /// Checks if the move is a castling move.
    pub fn is_castle(&self) -> bool {
        match *self {
            Move::Castle { .. } => true,
            _ => false,
        }
    }

    /// Gets the promotion role.
    pub fn promotion(&self) -> Option<Role> {
        match *self {
            Move::Normal { promotion, .. } => promotion,
            _ => None,
        }
    }

    /// Checks if the move is a promotion.
    pub fn is_promotion(&self) -> bool {
        match *self {
            Move::Normal { promotion: Some(_), .. } => true,
            _ => false,
        }
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Move::Normal { role, from, capture, to, promotion } => {
                if role != Role::Pawn {
                    write!(f, "{}", role.upper_char())?;
                }

                write!(f, "{}{}{}", from, if capture.is_some() { 'x' } else { '-' }, to)?;

                if let Some(p) = promotion {
                    write!(f, "={}", p.upper_char())?;
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
                if role != Role::Pawn {
                    write!(f, "{}", role.upper_char())?;
                }
                write!(f, "@{}", to)
            },
        }
    }
}

/// The number of checks the respective side needs to give in order to win
/// (in a game of Three-Check).
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
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

    pub fn decrement(&mut self, color: Color) {
        *self.by_color_mut(color) = self.by_color(color).saturating_sub(1);
    }
}

impl fmt::Display for RemainingChecks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}+{}", self.white, self.black)
    }
}

/// `KingSide` (O-O) or `QueenSide` (O-O-O).
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum CastlingSide {
    KingSide = 0,
    QueenSide = 1,
}

impl CastlingSide {
    pub fn is_queen_side(self) -> bool {
        match self {
            CastlingSide::KingSide => false,
            CastlingSide::QueenSide => true,
        }
    }

    pub fn is_king_side(self) -> bool {
        !self.is_queen_side()
    }

    pub fn from_queen_side(queen_side: bool) -> CastlingSide {
        if queen_side { CastlingSide::QueenSide } else { CastlingSide::KingSide }
    }

    pub fn from_king_side(king_side: bool) -> CastlingSide {
        if king_side { CastlingSide::KingSide } else { CastlingSide::QueenSide }
    }

    pub fn king_to(self, color: Color) -> Square {
        match self {
            CastlingSide::KingSide => color.fold(Square::G1, Square::G8),
            CastlingSide::QueenSide => color.fold(Square::C1, Square::C8),
        }
    }

    pub fn rook_to(self, color: Color) -> Square {
        match self {
            CastlingSide::KingSide => color.fold(Square::F1, Square::F8),
            CastlingSide::QueenSide => color.fold(Square::D1, Square::D8),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_role_order() {
        assert!(Role::Pawn < Role::Knight);
        assert!(Role::Knight < Role::Bishop);
        assert!(Role::Bishop < Role::Rook);
        assert!(Role::Rook < Role::Queen);
        assert!(Role::Queen < Role::King);
    }
}
