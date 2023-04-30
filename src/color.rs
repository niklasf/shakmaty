//! White or black.

use core::{array, convert::identity, fmt, mem, ops, str::FromStr};

use crate::{
    role::{ByRole, Role},
    square::Rank,
    types::Piece,
};

/// `White` or `Black`.
#[allow(missing_docs)]
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum Color {
    Black = 0,
    White = 1,
}

impl Color {
    pub const fn from_char(ch: char) -> Option<Color> {
        Some(match ch {
            'w' => Color::White,
            'b' => Color::Black,
            _ => return None,
        })
    }

    pub fn char(self) -> char {
        self.fold_wb('w', 'b')
    }

    fn from_name(name: &str) -> Option<Color> {
        Some(match name {
            "white" => Color::White,
            "black" => Color::Black,
            _ => return None,
        })
    }

    const fn name(self) -> &'static str {
        match self {
            Color::Black => "black",
            Color::White => "white",
        }
    }

    #[inline]
    pub const fn from_white(white: bool) -> Color {
        if white {
            Color::White
        } else {
            Color::Black
        }
    }

    #[inline]
    pub const fn from_black(black: bool) -> Color {
        if black {
            Color::Black
        } else {
            Color::White
        }
    }

    #[inline]
    pub fn fold_wb<T>(self, white: T, black: T) -> T {
        match self {
            Color::White => white,
            Color::Black => black,
        }
    }

    #[inline]
    pub const fn is_white(self) -> bool {
        matches!(self, Color::White)
    }
    #[inline]
    pub const fn is_black(self) -> bool {
        matches!(self, Color::Black)
    }

    /// Same as `!self`, but usable in `const` contexts.
    #[must_use]
    pub const fn other(self) -> Color {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }

    #[inline]
    pub const fn backrank(self) -> Rank {
        match self {
            Color::White => Rank::First,
            Color::Black => Rank::Eighth,
        }
    }

    #[inline]
    pub fn relative_rank(self, rank: Rank) -> Rank {
        match self {
            Color::White => rank,
            Color::Black => rank.flip_vertical(),
        }
    }

    #[inline]
    pub const fn pawn(self) -> Piece {
        Role::Pawn.of(self)
    }

    #[inline]
    pub const fn knight(self) -> Piece {
        Role::Knight.of(self)
    }

    #[inline]
    pub const fn bishop(self) -> Piece {
        Role::Bishop.of(self)
    }

    #[inline]
    pub const fn rook(self) -> Piece {
        Role::Rook.of(self)
    }

    #[inline]
    pub const fn queen(self) -> Piece {
        Role::Queen.of(self)
    }

    #[inline]
    pub const fn king(self) -> Piece {
        Role::King.of(self)
    }

    /// `White` and `Black`, in this order.
    pub const ALL: [Color; 2] = [Color::White, Color::Black];
}

impl ops::Not for Color {
    type Output = Color;

    #[inline]
    fn not(self) -> Color {
        self.fold_wb(Color::Black, Color::White)
    }
}

impl ops::BitXor<bool> for Color {
    type Output = Color;

    #[inline]
    fn bitxor(self, flip: bool) -> Color {
        Color::from_white(self.is_white() ^ flip)
    }
}

impl ops::BitXorAssign<bool> for Color {
    fn bitxor_assign(&mut self, flip: bool) {
        *self = *self ^ flip;
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// Error when parsing an invalid color name.
#[derive(Clone, Debug)]
pub struct ParseColorError;

impl fmt::Display for ParseColorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("invalid color")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ParseColorError {}

impl FromStr for Color {
    type Err = ParseColorError;

    fn from_str(s: &str) -> Result<Color, ParseColorError> {
        Color::from_name(s).ok_or(ParseColorError)
    }
}

/// Container with values for each [`Color`].
#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash)]
#[repr(C)]
pub struct ByColor<T> {
    pub black: T,
    pub white: T,
}

impl<T> ByColor<T> {
    #[inline]
    pub fn new_with<F>(mut init: F) -> ByColor<T>
    where
        F: FnMut(Color) -> T,
    {
        ByColor {
            white: init(Color::White),
            black: init(Color::Black),
        }
    }

    #[inline]
    pub const fn get(&self, color: Color) -> &T {
        // Safety: Trivial offset into #[repr(C)] struct.
        unsafe {
            &*(self as *const ByColor<T>)
                .cast::<T>()
                .offset(color as isize)
        }
    }

    #[inline]
    pub fn get_mut(&mut self, color: Color) -> &mut T {
        // Safety: Trivial offset into #[repr(C)] struct.
        unsafe { &mut *(self as *mut ByColor<T>).cast::<T>().offset(color as isize) }
    }

    pub fn flip(&mut self) {
        mem::swap(&mut self.white, &mut self.black);
    }

    #[must_use]
    pub fn into_flipped(self) -> ByColor<T> {
        ByColor {
            black: self.white,
            white: self.black,
        }
    }

    #[inline]
    pub fn for_each<F>(self, mut f: F)
    where
        F: FnMut(T),
    {
        f(self.white);
        f(self.black);
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

    #[inline]
    pub const fn as_ref(&self) -> ByColor<&T> {
        ByColor {
            black: &self.black,
            white: &self.white,
        }
    }

    #[inline]
    pub fn as_mut(&mut self) -> ByColor<&mut T> {
        ByColor {
            black: &mut self.black,
            white: &mut self.white,
        }
    }

    pub fn zip<U>(self, other: ByColor<U>) -> ByColor<(T, U)> {
        ByColor {
            black: (self.black, other.black),
            white: (self.white, other.white),
        }
    }

    pub fn zip_color(self) -> ByColor<(Color, T)> {
        ByColor::new_with(identity).zip(self)
    }

    pub fn iter(&self) -> array::IntoIter<&T, 2> {
        self.as_ref().into_iter()
    }

    pub fn iter_mut(&mut self) -> array::IntoIter<&mut T, 2> {
        self.as_mut().into_iter()
    }
}

impl<T> ByColor<ByRole<T>> {
    pub const fn piece(&self, piece: Piece) -> &T {
        self.get(piece.color).get(piece.role)
    }

    pub fn piece_mut(&mut self, piece: Piece) -> &mut T {
        self.get_mut(piece.color).get_mut(piece.role)
    }
}

#[cfg(feature = "variant")]
impl ByColor<ByRole<u8>> {
    pub(crate) fn count(&self) -> usize {
        self.iter().map(ByRole::count).sum()
    }
}

impl<T: PartialOrd> ByColor<T> {
    pub fn normalize(&mut self) {
        if self.white < self.black {
            self.flip();
        }
    }

    #[must_use]
    pub fn into_normalized(mut self) -> ByColor<T> {
        self.normalize();
        self
    }
}

impl<T: PartialEq> ByColor<T> {
    pub fn is_symmetric(&self) -> bool {
        self.white == self.black
    }
}

impl<T: Copy> ByColor<&T> {
    pub fn copied(self) -> ByColor<T> {
        self.map(|item| *item)
    }
}

impl<T: Clone> ByColor<&T> {
    pub fn cloned(self) -> ByColor<T> {
        self.map(Clone::clone)
    }
}

impl<T> IntoIterator for ByColor<T> {
    type Item = T;
    type IntoIter = array::IntoIter<T, 2>;

    fn into_iter(self) -> Self::IntoIter {
        [self.white, self.black].into_iter()
    }
}
