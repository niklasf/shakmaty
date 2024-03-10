use core::{array, convert::identity, mem, ops};

use crate::{
    color::Color,
    square::{File, Square},
};

/// `KingSide` (O-O) or `QueenSide` (O-O-O).
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum CastlingSide {
    KingSide,
    QueenSide,
}

impl CastlingSide {
    pub const fn is_queen_side(self) -> bool {
        matches!(self, CastlingSide::QueenSide)
    }

    pub const fn is_king_side(self) -> bool {
        matches!(self, CastlingSide::KingSide)
    }

    pub const fn from_queen_side(queen_side: bool) -> CastlingSide {
        if queen_side {
            CastlingSide::QueenSide
        } else {
            CastlingSide::KingSide
        }
    }

    pub const fn from_king_side(king_side: bool) -> CastlingSide {
        if king_side {
            CastlingSide::KingSide
        } else {
            CastlingSide::QueenSide
        }
    }

    pub const fn king_to_file(self) -> File {
        match self {
            CastlingSide::KingSide => File::G,
            CastlingSide::QueenSide => File::C,
        }
    }

    pub const fn rook_to_file(self) -> File {
        match self {
            CastlingSide::KingSide => File::F,
            CastlingSide::QueenSide => File::D,
        }
    }

    pub fn king_to(self, color: Color) -> Square {
        Square::from_coords(self.king_to_file(), color.backrank())
    }

    pub fn rook_to(self, color: Color) -> Square {
        Square::from_coords(self.rook_to_file(), color.backrank())
    }

    pub const fn other(self) -> CastlingSide {
        match self {
            CastlingSide::KingSide => CastlingSide::QueenSide,
            CastlingSide::QueenSide => CastlingSide::KingSide,
        }
    }

    /// `KingSide` and `QueenSide`, in this order.
    pub const ALL: [CastlingSide; 2] = [CastlingSide::KingSide, CastlingSide::QueenSide];
}

impl ops::Not for CastlingSide {
    type Output = CastlingSide;

    fn not(self) -> CastlingSide {
        self.other()
    }
}

impl ops::BitXor<bool> for CastlingSide {
    type Output = CastlingSide;

    fn bitxor(self, flip: bool) -> CastlingSide {
        CastlingSide::from_king_side(self.is_king_side() ^ flip)
    }
}

impl ops::BitXorAssign<bool> for CastlingSide {
    fn bitxor_assign(&mut self, flip: bool) {
        *self = *self ^ flip;
    }
}

// Container with values for each [`CastlingSide`].
#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash)]
#[repr(C)]
pub struct ByCastlingSide<T> {
    pub king_side: T,
    pub queen_side: T,
}

impl<T> ByCastlingSide<T> {
    pub fn new_with<F>(mut init: F) -> ByCastlingSide<T>
    where
        F: FnMut(CastlingSide) -> T,
    {
        ByCastlingSide {
            king_side: init(CastlingSide::KingSide),
            queen_side: init(CastlingSide::QueenSide),
        }
    }

    pub const fn get(&self, side: CastlingSide) -> &T {
        match side {
            CastlingSide::KingSide => &self.king_side,
            CastlingSide::QueenSide => &self.queen_side,
        }
    }

    pub fn get_mut(&mut self, side: CastlingSide) -> &mut T {
        match side {
            CastlingSide::KingSide => &mut self.king_side,
            CastlingSide::QueenSide => &mut self.queen_side,
        }
    }

    pub fn swap(&mut self) {
        mem::swap(&mut self.king_side, &mut self.queen_side);
    }

    #[must_use]
    pub fn into_flipped(self) -> ByCastlingSide<T> {
        ByCastlingSide {
            king_side: self.queen_side,
            queen_side: self.king_side,
        }
    }

    pub fn for_each<F>(self, mut f: F)
    where
        F: FnMut(T),
    {
        f(self.king_side);
        f(self.queen_side);
    }

    pub fn map<U, F>(self, mut f: F) -> ByCastlingSide<U>
    where
        F: FnMut(T) -> U,
    {
        ByCastlingSide {
            king_side: f(self.king_side),
            queen_side: f(self.queen_side),
        }
    }

    pub fn find<F>(&self, mut predicate: F) -> Option<CastlingSide>
    where
        F: FnMut(&T) -> bool,
    {
        if predicate(&self.king_side) {
            Some(CastlingSide::KingSide)
        } else if predicate(&self.queen_side) {
            Some(CastlingSide::QueenSide)
        } else {
            None
        }
    }

    pub const fn as_ref(&self) -> ByCastlingSide<&T> {
        ByCastlingSide {
            king_side: &self.king_side,
            queen_side: &self.queen_side,
        }
    }

    pub fn as_mut(&mut self) -> ByCastlingSide<&mut T> {
        ByCastlingSide {
            king_side: &mut self.king_side,
            queen_side: &mut self.queen_side,
        }
    }

    pub fn zip<U>(self, other: ByCastlingSide<U>) -> ByCastlingSide<(T, U)> {
        ByCastlingSide {
            king_side: (self.king_side, other.king_side),
            queen_side: (self.queen_side, other.queen_side),
        }
    }

    pub fn zip_castling_side(self) -> ByCastlingSide<(CastlingSide, T)> {
        ByCastlingSide::new_with(identity).zip(self)
    }

    pub fn iter(&self) -> array::IntoIter<&T, 2> {
        self.as_ref().into_iter()
    }

    pub fn iter_mut(&mut self) -> array::IntoIter<&mut T, 2> {
        self.as_mut().into_iter()
    }
}

impl<T: Copy> ByCastlingSide<&T> {
    pub fn copied(self) -> ByCastlingSide<T> {
        self.map(|item| *item)
    }
}

impl<T: Clone> ByCastlingSide<&T> {
    pub fn cloned(self) -> ByCastlingSide<T> {
        self.map(Clone::clone)
    }
}

impl<T> IntoIterator for ByCastlingSide<T> {
    type Item = T;
    type IntoIter = array::IntoIter<T, 2>;

    fn into_iter(self) -> Self::IntoIter {
        [self.king_side, self.queen_side].into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_castling_side_discriminants() {
        // Fixed but not publicly guaranteed discriminants.
        assert_eq!(CastlingSide::KingSide as usize, 0);
        assert_eq!(CastlingSide::QueenSide as usize, 1);
    }
}
