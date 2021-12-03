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

//! Sets of squares.

use std::{
    fmt,
    fmt::Write,
    iter::{FromIterator, FusedIterator},
    ops,
};

use crate::{
    color::Color,
    square::{File, Rank, Square},
};

/// A set of [squares](super::Square) represented by a 64 bit
/// integer mask.
///
/// # Examples
///
/// ```
/// use shakmaty::Bitboard;
///
/// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
/// // . 1 1 1 1 . . .
/// // . 1 . . . 1 . .
/// // . 1 . . . 1 . .
/// // . 1 . . 1 . . .
/// // . 1 1 1 . . . .
/// // . 1 . 1 . . . .
/// // . 1 . . 1 . . .
/// // . 1 . . . 1 . .
/// ```
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Bitboard(pub u64);

impl Bitboard {
    /// A bitboard with a single square.
    #[inline]
    pub fn from_square(sq: Square) -> Bitboard {
        Bitboard(SQUARES[usize::from(sq)])
    }

    /// Returns the bitboard containing all squares of the given rank.
    #[inline]
    pub fn rank(rank: Rank) -> Bitboard {
        Bitboard(RANKS[usize::from(rank)])
    }

    /// Returns the bitboard containing all squares of the given file.
    #[inline]
    pub fn file(file: File) -> Bitboard {
        Bitboard(FILES[usize::from(file)])
    }

    /// Like `rank()`, but from the point of view of `color`.
    #[inline]
    pub fn relative_rank(color: Color, rank: Rank) -> Bitboard {
        Bitboard::rank(color.fold_wb(rank, rank.flip_vertical()))
    }

    /// Shift using `<<` for `White` and `>>` for `Black`.
    #[must_use]
    #[inline]
    pub fn relative_shift(self, color: Color, shift: u32) -> Bitboard {
        match color {
            Color::White => Bitboard(self.0 << shift),
            Color::Black => Bitboard(self.0 >> shift),
        }
    }

    #[must_use]
    #[inline]
    pub fn any(self) -> bool {
        self.0 != 0
    }

    #[inline]
    pub fn is_empty(self) -> bool {
        self.0 == 0
    }

    #[inline]
    pub fn contains(self, sq: Square) -> bool {
        !(self & Bitboard::from_square(sq)).is_empty()
    }

    #[inline]
    pub fn add<T: Into<Bitboard>>(&mut self, squares: T) {
        *self |= squares;
    }

    #[inline]
    pub fn toggle<T: Into<Bitboard>>(&mut self, squares: T) {
        *self ^= squares;
    }

    #[inline]
    pub fn discard<T: Into<Bitboard>>(&mut self, squares: T) {
        *self &= !squares.into();
    }

    /// Removes a square from the bitboard.
    ///
    /// Returns `true` if the square was in the set. Use
    /// [`Bitboard::discard()`] if you do not care about the return value.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Bitboard, Square};
    ///
    /// let mut bitboard = Bitboard::FULL;
    /// assert_eq!(bitboard.remove(Square::E4), true);
    /// assert_eq!(bitboard.remove(Square::E4), false);
    /// ```
    #[must_use]
    #[inline]
    pub fn remove(&mut self, sq: Square) -> bool {
        if self.contains(sq) {
            self.toggle(sq);
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn set(&mut self, sq: Square, v: bool) {
        if v {
            self.add(sq);
        } else {
            self.discard(sq);
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.0 = 0;
    }

    #[must_use]
    #[inline]
    pub fn with<T: Into<Bitboard>>(self, squares: T) -> Bitboard {
        self | squares
    }

    #[must_use]
    #[inline]
    pub fn without<T: Into<Bitboard>>(self, squares: T) -> Bitboard {
        self & !squares.into()
    }

    #[inline]
    pub fn is_disjoint<T: Into<Bitboard>>(self, other: T) -> bool {
        (self & other).is_empty()
    }

    #[inline]
    pub fn is_subset<T: Into<Bitboard>>(self, other: T) -> bool {
        (self & !other.into()).is_empty()
    }

    #[inline]
    pub fn is_superset<T: Into<Bitboard>>(self, other: T) -> bool {
        other.into().is_subset(self)
    }

    #[inline]
    pub fn pop_front(&mut self) -> Option<Square> {
        let square = self.first();
        self.0 &= self.0.wrapping_sub(1);
        square
    }

    #[inline]
    pub fn first(self) -> Option<Square> {
        if self.is_empty() {
            None
        } else {
            Some(Square::new(self.0.trailing_zeros()))
        }
    }

    #[inline]
    pub fn pop_back(&mut self) -> Option<Square> {
        let square = self.last();
        *self ^= Bitboard::from_iter(square);
        square
    }

    #[inline]
    pub fn last(self) -> Option<Square> {
        if self.is_empty() {
            None
        } else {
            Some(Square::new(63 - self.0.leading_zeros()))
        }
    }

    #[inline]
    pub fn count(self) -> usize {
        self.0.count_ones() as usize
    }

    #[inline]
    pub fn more_than_one(self) -> bool {
        self.0 & self.0.wrapping_sub(1) != 0
    }

    #[inline]
    pub fn single_square(self) -> Option<Square> {
        if self.more_than_one() {
            None
        } else {
            self.first()
        }
    }

    /// An iterator over the subsets of this bitboard.
    #[inline]
    pub fn carry_rippler(self) -> CarryRippler {
        CarryRippler {
            bb: self.0,
            subset: 0,
            first: true,
        }
    }

    /// Mirror the bitboard vertically.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.flip_vertical(), Bitboard(0x2212_0a0e_1222_221e));
    /// // . 1 . . . 1 . .
    /// // . 1 . . 1 . . .
    /// // . 1 . 1 . . . .
    /// // . 1 1 1 . . . .
    /// // . 1 . . 1 . . .
    /// // . 1 . . . 1 . .
    /// // . 1 . . . 1 . .
    /// // . 1 1 1 1 . . .
    /// ```
    #[must_use]
    #[inline]
    pub fn flip_vertical(self) -> Bitboard {
        Bitboard(self.0.swap_bytes())
    }

    /// Mirror the bitboard horizontally.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.flip_horizontal(), Bitboard(0x7844_4448_7050_4844));
    /// // . . . 1 1 1 1 .
    /// // . . 1 . . . 1 .
    /// // . . 1 . . . 1 .
    /// // . . . 1 . . 1 .
    /// // . . . . 1 1 1 .
    /// // . . . . 1 . 1 .
    /// // . . . 1 . . 1 .
    /// // . . 1 . . . 1 .
    /// ```
    #[must_use]
    pub fn flip_horizontal(self) -> Bitboard {
        // https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#Horizontal
        let k1 = 0x5555_5555_5555_5555;
        let k2 = 0x3333_3333_3333_3333;
        let k4 = 0x0f0f_0f0f_0f0f_0f0f;
        let x = self.0;
        let x = ((x >> 1) & k1) | ((x & k1) << 1);
        let x = ((x >> 2) & k2) | ((x & k2) << 2);
        let x = ((x >> 4) & k4) | ((x & k4) << 4);
        Bitboard(x)
    }

    /// Mirror the bitboard at the a1-h8 diagonal.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.flip_diagonal(), Bitboard(0x0000_6192_8c88_ff00));
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // 1 . . . . 1 1 .
    /// // . 1 . . 1 . . 1
    /// // . . 1 1 . . . 1
    /// // . . . 1 . . . 1
    /// // 1 1 1 1 1 1 1 1
    /// // . . . . . . . .
    /// ```
    #[must_use]
    pub fn flip_diagonal(self) -> Bitboard {
        // https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#Diagonal
        let k1 = 0x5500_5500_5500_5500;
        let k2 = 0x3333_0000_3333_0000;
        let k4 = 0x0f0f_0f0f_0000_0000;
        let mut x = self.0;
        let t = k4 & (x ^ (x << 28));
        x ^= t ^ (t >> 28);
        let t = k2 & (x ^ (x << 14));
        x ^= t ^ (t >> 14);
        let t = k1 & (x ^ (x << 7));
        x ^= t ^ (t >> 7);
        Bitboard(x)
    }

    /// Mirror the bitboard at the h1-a8 diagonal.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.flip_anti_diagonal(), Bitboard(0x00ff_1131_4986_0000));
    /// // . . . . . . . .
    /// // 1 1 1 1 1 1 1 1
    /// // 1 . . . 1 . . .
    /// // 1 . . . 1 1 . .
    /// // 1 . . 1 . . 1 .
    /// // . 1 1 . . . . 1
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// ```
    #[must_use]
    pub fn flip_anti_diagonal(self) -> Bitboard {
        // https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#Anti-Diagonal
        let k1 = 0xaa00_aa00_aa00_aa00;
        let k2 = 0xcccc_0000_cccc_0000;
        let k4 = 0xf0f0_f0f0_0f0f_0f0f;
        let mut x = self.0;
        let t = x ^ (x << 36);
        x ^= k4 & (t ^ (x >> 36));
        let t = k2 & (x ^ (x << 18));
        x ^= t ^ (t >> 18);
        let t = k1 & (x ^ (x << 9));
        x ^= t ^ (t >> 9);
        Bitboard(x)
    }

    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.rotate_90(), Bitboard(0x00ff_888c_9261_0000));
    /// // . . . . . . . .
    /// // 1 1 1 1 1 1 1 1
    /// // . . . 1 . . . 1
    /// // . . 1 1 . . . 1
    /// // . 1 . . 1 . . 1
    /// // 1 . . . . 1 1 .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// ```
    #[must_use]
    pub fn rotate_90(self) -> Bitboard {
        self.flip_diagonal().flip_vertical()
    }

    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.rotate_180(), Bitboard(0x4448_5070_4844_4478));
    /// // . . 1 . . . 1 .
    /// // . . . 1 . . 1 .
    /// // . . . . 1 . 1 .
    /// // . . . . 1 1 1 .
    /// // . . . 1 . . 1 .
    /// // . . 1 . . . 1 .
    /// // . . 1 . . . 1 .
    /// // . . . 1 1 1 1 .
    /// ```
    #[must_use]
    #[inline]
    pub fn rotate_180(self) -> Bitboard {
        Bitboard(self.0.reverse_bits())
    }

    /// ```
    /// use shakmaty::Bitboard;
    ///
    /// let bitboard = Bitboard(0x1e22_2212_0e0a_1222);
    /// assert_eq!(bitboard.rotate_270(), Bitboard(0x0000_8649_3111_ff00));
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . 1 1 . . . . 1
    /// // 1 . . 1 . . 1 .
    /// // 1 . . . 1 1 . .
    /// // 1 . . . 1 . . .
    /// // 1 1 1 1 1 1 1 1
    /// // . . . . . . . .
    /// ```
    #[must_use]
    pub fn rotate_270(self) -> Bitboard {
        self.flip_vertical().flip_diagonal()
    }

    /// An empty bitboard.
    pub const EMPTY: Bitboard = Bitboard(0);

    /// A bitboard containing all squares.
    pub const FULL: Bitboard = Bitboard(!0);

    /// All dark squares.
    pub const DARK_SQUARES: Bitboard = Bitboard(0xaa55_aa55_aa55_aa55);

    /// All light squares.
    pub const LIGHT_SQUARES: Bitboard = Bitboard(0x55aa_55aa_55aa_55aa);

    /// The four corner squares.
    pub const CORNERS: Bitboard = Bitboard(0x8100_0000_0000_0081);

    /// The backranks.
    pub const BACKRANKS: Bitboard = Bitboard(0xff00_0000_0000_00ff);

    /// The four center squares.
    pub const CENTER: Bitboard = Bitboard(0x0000_0018_1800_0000);

    #[deprecated(since = "0.20.1", note = "Renamed to Bitboard::FULL for consistency")]
    pub const ALL: Bitboard = Bitboard::FULL;
}

/// Square masks.
static SQUARES: [u64; 64] = {
    let mut masks = [0; 64];
    let mut i = 0;
    while i < 64 {
        masks[i] = 1 << i;
        i += 1;
    }
    masks
};

/// Rank masks.
static RANKS: [u64; 8] = {
    let mut masks = [0; 8];
    let mut i = 0;
    while i < 8 {
        masks[i] = 0xff << (i * 8);
        i += 1;
    }
    masks
};

/// File masks.
static FILES: [u64; 8] = {
    let mut masks = [0; 8];
    let mut i = 0;
    while i < 8 {
        masks[i] = 0x0101_0101_0101_0101 << i;
        i += 1;
    }
    masks
};

impl fmt::Debug for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for rank in (0..8).map(Rank::new).rev() {
            for file in (0..8).map(File::new) {
                let sq = Square::from_coords(file, rank);
                f.write_char(if self.contains(sq) { '1' } else { '.' })?;
                f.write_char(if file < File::H { ' ' } else { '\n' })?;
            }
        }

        Ok(())
    }
}

impl fmt::UpperHex for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::UpperHex::fmt(&self.0, f)
    }
}

impl fmt::LowerHex for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::LowerHex::fmt(&self.0, f)
    }
}

impl fmt::Octal for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Octal::fmt(&self.0, f)
    }
}

impl fmt::Binary for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Binary::fmt(&self.0, f)
    }
}

impl From<Square> for Bitboard {
    #[inline]
    fn from(sq: Square) -> Bitboard {
        Bitboard::from_square(sq)
    }
}

impl From<Rank> for Bitboard {
    #[inline]
    fn from(rank: Rank) -> Bitboard {
        Bitboard::rank(rank)
    }
}

impl From<File> for Bitboard {
    #[inline]
    fn from(file: File) -> Bitboard {
        Bitboard::file(file)
    }
}

impl From<u64> for Bitboard {
    #[inline]
    fn from(bb: u64) -> Bitboard {
        Bitboard(bb)
    }
}

impl From<Bitboard> for u64 {
    #[inline]
    fn from(bb: Bitboard) -> u64 {
        bb.0
    }
}

impl<T> ops::BitAnd<T> for Bitboard
where
    T: Into<Bitboard>,
{
    type Output = Bitboard;

    #[inline]
    fn bitand(self, rhs: T) -> Bitboard {
        let Bitboard(rhs) = rhs.into();
        Bitboard(self.0 & rhs)
    }
}

impl<T> ops::BitAndAssign<T> for Bitboard
where
    T: Into<Bitboard>,
{
    #[inline]
    fn bitand_assign(&mut self, rhs: T) {
        let Bitboard(rhs) = rhs.into();
        self.0 &= rhs;
    }
}

impl<T> ops::BitOr<T> for Bitboard
where
    T: Into<Bitboard>,
{
    type Output = Bitboard;

    #[inline]
    fn bitor(self, rhs: T) -> Bitboard {
        let Bitboard(rhs) = rhs.into();
        Bitboard(self.0 | rhs)
    }
}

impl<T> ops::BitOrAssign<T> for Bitboard
where
    T: Into<Bitboard>,
{
    #[inline]
    fn bitor_assign(&mut self, rhs: T) {
        let Bitboard(rhs) = rhs.into();
        self.0 |= rhs;
    }
}

impl<T> ops::BitXor<T> for Bitboard
where
    T: Into<Bitboard>,
{
    type Output = Bitboard;

    #[inline]
    fn bitxor(self, rhs: T) -> Bitboard {
        let Bitboard(rhs) = rhs.into();
        Bitboard(self.0 ^ rhs)
    }
}

impl<T> ops::BitXorAssign<T> for Bitboard
where
    T: Into<Bitboard>,
{
    #[inline]
    fn bitxor_assign(&mut self, rhs: T) {
        let Bitboard(rhs) = rhs.into();
        self.0 ^= rhs;
    }
}

impl ops::Not for Bitboard {
    type Output = Bitboard;

    #[inline]
    fn not(self) -> Bitboard {
        Bitboard(!self.0)
    }
}

impl FromIterator<Square> for Bitboard {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Square>,
    {
        let mut result = Bitboard(0);
        result.extend(iter);
        result
    }
}

impl Extend<Square> for Bitboard {
    fn extend<T: IntoIterator<Item = Square>>(&mut self, iter: T) {
        for square in iter {
            self.add(square);
        }
    }
}

impl IntoIterator for Bitboard {
    type Item = Square;
    type IntoIter = IntoIter;

    #[inline]
    fn into_iter(self) -> IntoIter {
        IntoIter(self)
    }
}

/// Iterator over the squares of a [`Bitboard`].
#[derive(Debug, Clone)]
pub struct IntoIter(Bitboard);

impl Iterator for IntoIter {
    type Item = Square;

    #[inline]
    fn next(&mut self) -> Option<Square> {
        self.0.pop_front()
    }

    #[inline]
    fn count(self) -> usize {
        self.0.count()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.0.count();
        (len, Some(len))
    }

    #[inline]
    fn last(self) -> Option<Square> {
        self.0.last()
    }
}

impl ExactSizeIterator for IntoIter {
    #[inline]
    fn len(&self) -> usize {
        self.0.count()
    }
}

impl FusedIterator for IntoIter {}

impl DoubleEndedIterator for IntoIter {
    #[inline]
    fn next_back(&mut self) -> Option<Square> {
        self.0.pop_back()
    }
}

/// Iterator over the subsets of a [`Bitboard`].
#[derive(Debug, Clone)]
pub struct CarryRippler {
    bb: u64,
    subset: u64,
    first: bool,
}

impl Iterator for CarryRippler {
    type Item = Bitboard;

    #[inline]
    fn next(&mut self) -> Option<Bitboard> {
        let subset = self.subset;
        if subset != 0 || self.first {
            self.first = false;
            self.subset = self.subset.wrapping_sub(self.bb) & self.bb;
            Some(Bitboard(subset))
        } else {
            None
        }
    }

    #[inline]
    fn last(self) -> Option<Bitboard> {
        if self.subset != 0 || self.first {
            Some(Bitboard(self.bb))
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, 1_usize.checked_shl(self.bb.count_ones()))
    }
}

impl FusedIterator for CarryRippler {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_more_than_one() {
        assert_eq!(Bitboard(0).more_than_one(), false);
        assert_eq!(Bitboard(1).more_than_one(), false);
        assert_eq!(Bitboard(2).more_than_one(), false);
        assert_eq!(Bitboard(3).more_than_one(), true);
        assert_eq!(Bitboard::FULL.more_than_one(), true);
    }

    #[test]
    fn test_first() {
        assert_eq!(Bitboard::from_square(Square::A1).first(), Some(Square::A1));
        assert_eq!(Bitboard::from_square(Square::D2).first(), Some(Square::D2));
        assert_eq!(Bitboard(0).first(), None);
    }

    #[test]
    fn test_last() {
        assert_eq!(Bitboard::from_square(Square::A1).last(), Some(Square::A1));
        assert_eq!(
            Bitboard(0).with(Square::A1).with(Square::H1).last(),
            Some(Square::H1)
        );
        assert_eq!(Bitboard(0).last(), None);
    }

    #[test]
    fn test_is_empty() {
        assert!(Bitboard(0).is_empty());
        assert!(!Bitboard(1).is_empty());
    }

    #[test]
    fn test_rank() {
        assert_eq!(Bitboard::rank(Rank::Fourth), Bitboard(0xff00_0000));
    }

    #[test]
    fn test_from_iter() {
        assert_eq!(Bitboard::from_iter(None), Bitboard(0));
        assert_eq!(
            Bitboard::from_iter(Some(Square::D2)),
            Bitboard::from_square(Square::D2)
        );
    }

    #[test]
    fn test_upper_hex() {
        assert_eq!(format!("{:#0X}", Bitboard(42)), format!("{:#0X}", 42));
    }

    #[test]
    fn test_lower_hex() {
        assert_eq!(format!("{:#0x}", Bitboard(42)), format!("{:#0x}", 42));
    }

    #[test]
    fn test_octal() {
        assert_eq!(format!("{:#0o}", Bitboard(42)), format!("{:#0o}", 42));
    }

    #[test]
    fn test_binary() {
        assert_eq!(format!("{:#0b}", Bitboard(42)), format!("{:#0b}", 42));
    }
}
