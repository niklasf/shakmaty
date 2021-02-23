// This file is part of the shakmaty library.
// Copyright (C) 2017-2021 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use std::ops;

use crate::types::Piece;
use crate::types::Role;
use crate::square::Rank;

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
            Color::Black => black,
        }
    }

    #[inline]
    pub fn is_white(self) -> bool { self == Color::White }
    #[inline]
    pub fn is_black(self) -> bool { self == Color::Black }

    #[inline]
    pub fn backrank(self) -> Rank { self.fold(Rank::First, Rank::Eighth) }

    pub fn char(self) -> char { self.fold('w', 'b') }

    #[inline]
    pub fn pawn(self)   -> Piece { Role::Pawn.of(self) }
    #[inline]
    pub fn knight(self) -> Piece { Role::Knight.of(self) }
    #[inline]
    pub fn bishop(self) -> Piece { Role::Bishop.of(self) }
    #[inline]
    pub fn rook(self)   -> Piece { Role::Rook.of(self) }
    #[inline]
    pub fn queen(self)  -> Piece { Role::Queen.of(self) }
    #[inline]
    pub fn king(self)   -> Piece { Role::King.of(self) }
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

/// Container with values for each [`Color`].
#[derive(Clone, Default, Eq, PartialEq, Debug, Hash)]
pub struct ByColor<T> {
    pub white: T,
    pub black: T,
}

impl<T> ByColor<T> {
    #[inline]
    pub fn new_with<F>(self, mut init: F) -> ByColor<T>
    where
        F: FnMut(Color) -> T,
    {
        ByColor {
            white: init(Color::White),
            black: init(Color::Black),
        }
    }

    #[inline]
    pub fn by_color(&self, color: Color) -> &T {
        match color {
            Color::White => &self.white,
            Color::Black => &self.black,
        }
    }

    #[inline]
    pub fn by_color_mut(&mut self, color: Color) -> &mut T {
        match color {
            Color::White => &mut self.white,
            Color::Black => &mut self.black,
        }
    }

    #[inline]
    pub fn into_color(self, color: Color) -> T {
        match color {
            Color::White => self.white,
            Color::Black => self.black,
        }
    }

    #[inline]
    pub fn map<U, F>(self, mut f: F) -> ByColor<U>
    where
        F: FnMut(T) -> U,
    {
        ByColor {
            white: f(self.white),
            black: f(self.black),
        }
    }

    #[inline]
    pub fn any<F>(&self, mut predicate: F) -> bool
    where
        F: FnMut(&T) -> bool,
    {
        predicate(&self.white) || predicate(&self.black)
    }

    #[inline]
    pub fn all<F>(&self, mut predicate: F) -> bool
    where
        F: FnMut(&T) -> bool,
    {
        predicate(&self.white) && predicate(&self.black)
    }

    #[inline]
    pub fn find<F>(&self, mut predicate: F) -> Option<Color>
    where
        F: FnMut(&T) -> bool,
    {
        if predicate(&self.white) {
            Some(Color::White)
        } else if predicate(&self.black) {
            Some(Color::Black)
        } else {
            None
        }
    }

    pub fn as_ref(&self) -> ByColor<&T> {
        ByColor {
            white: &self.white,
            black: &self.black,
        }
    }
}

impl<T: Copy> ByColor<&T> {
    pub fn copied(self) -> ByColor<T> {
        ByColor {
            white: *self.white,
            black: *self.black,
        }
    }
}

impl<T: Clone> ByColor<&T> {
    pub fn cloned(self) -> ByColor<T> {
        ByColor {
            white: self.white.clone(),
            black: self.black.clone(),
        }
    }
}
