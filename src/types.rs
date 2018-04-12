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
use std::char;
use std::ops;

use square::Square;

pub use self::Color::{Black, White};
pub use self::Role::{Bishop, King, Knight, Pawn, Queen, Rook};

/// `White` or `Black`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
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
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum Role {
    Pawn = 0,
    Knight = 1,
    Bishop = 2,
    Rook = 3,
    Queen = 4,
    King = 5,
}

impl Role {
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

    #[inline]
    pub fn of(self, color: Color) -> Piece {
        Piece { color, role: self }
    }

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

pub const ROLES: [Role; 6] = [Pawn, Knight, Bishop, Rook, Queen, King];

/// A piece with [`Color`] and [`Role`].
///
/// [`Color`]: enum.Color.html
/// [`Role`]: enum.Role.html
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Piece {
    pub color: Color,
    pub role: Role,
}

impl Piece {
    pub fn char(&self) -> char {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

/// A players Crazyhouse pocket.
#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct Pocket {
    pub pawns: u8,
    pub knights: u8,
    pub bishops: u8,
    pub rooks: u8,
    pub queens: u8,
    pub kings: u8,
}

impl Pocket {
    pub fn by_role(&self, role: Role) -> u8 {
        match role {
            Role::Pawn => self.pawns,
            Role::Knight => self.knights,
            Role::Bishop => self.bishops,
            Role::Rook => self.rooks,
            Role::Queen => self.queens,
            Role::King => self.kings,
        }
    }

    pub fn by_role_mut(&mut self, role: Role) -> &mut u8 {
        match role {
            Role::Pawn => &mut self.pawns,
            Role::Knight => &mut self.knights,
            Role::Bishop => &mut self.bishops,
            Role::Rook => &mut self.rooks,
            Role::Queen => &mut self.queens,
            Role::King => &mut self.kings,
        }
    }

    pub fn count(&self) -> u8 {
        self.pawns
            .saturating_add(self.knights)
            .saturating_add(self.bishops)
            .saturating_add(self.rooks)
            .saturating_add(self.queens)
            .saturating_add(self.kings)
    }
}

/// Pockets to hold captured pieces for both sides (in Crazyhouse).
#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct Pockets {
    pub white: Pocket,
    pub black: Pocket,
}

impl Pockets {
    pub fn by_color(&self, color: Color) -> &Pocket {
        color.fold(&self.white, &self.black)
    }

    pub fn by_color_mut(&mut self, color: Color) -> &mut Pocket {
        color.fold(&mut self.white, &mut self.black)
    }

    pub fn by_piece(&self, piece: Piece) -> u8 {
        self.by_color(piece.color).by_role(piece.role)
    }

    pub fn by_piece_mut(&mut self, piece: Piece) -> &mut u8 {
        self.by_color_mut(piece.color).by_role_mut(piece.role)
    }

    pub fn add(&mut self, piece: Piece) {
        *self.by_piece_mut(piece) = self.by_piece(piece).saturating_add(1);
    }

    pub fn remove(&mut self, piece: Piece) {
        *self.by_piece_mut(piece) = self.by_piece(piece).saturating_sub(1);
    }

    pub fn count(&self) -> u8 {
        self.black.count().saturating_add(self.white.count())
    }
}

impl fmt::Display for Pockets {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for &color in &[White, Black] {
            for &role in &ROLES {
                let piece = Piece { color, role };
                write!(f, "{}", piece.char().to_string().repeat(self.by_piece(piece) as usize))?;
            }
        }
        Ok(())
    }
}

/// The number of checks the respective side needs to give in order to win
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

    pub fn decrement(&mut self, color: Color) {
        *self.by_color_mut(color) = self.by_color(color).saturating_sub(1);
    }
}

impl fmt::Display for RemainingChecks {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}+{}", self.white, self.black)
    }
}

/// `KingSide` (O-O) or `QueenSide` (O-O-O).
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum CastlingSide {
    KingSide = 0,
    QueenSide = 1,
}

impl CastlingSide {
    pub fn is_queen_side(&self) -> bool {
        match *self {
            CastlingSide::KingSide => false,
            CastlingSide::QueenSide => true,
        }
    }

    pub fn is_king_side(&self) -> bool {
        !self.is_queen_side()
    }

    pub fn king_to(&self, color: Color) -> Square {
        match *self {
            CastlingSide::KingSide => color.fold(Square::G1, Square::G8),
            CastlingSide::QueenSide => color.fold(Square::C1, Square::C8),
        }
    }

    pub fn rook_to(&self, color: Color) -> Square {
        match *self {
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
