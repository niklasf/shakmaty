// This file is part of the shakmaty library.
// Copyright (C) 2017-2018 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use std::ops;
use std::fmt;
use std::fmt::Write;
use std::iter::FromIterator;

use square::{File, Rank, Square};
use types::Color;

/// A set of [squares](../enum.Square.html) represented by a 64 bit
/// integer mask.
///
/// # Examples
///
/// ```
/// # use shakmaty::{Rank, Square, Bitboard};
/// let mask = Bitboard::from(Rank::Third).with(Square::E5);
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . 1 . . .
/// // . . . . . . . .
/// // 1 1 1 1 1 1 1 1
/// // . . . . . . . .
/// // . . . . . . . .
///
/// assert_eq!(mask.first(), Some(Square::A3));
/// ```
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Bitboard(pub u64);

impl Bitboard {
    /// A bitboard with a single square.
    #[inline]
    pub fn from_square(sq: Square) -> Bitboard {
        // This is safe because valid square indexes are in bounds.
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
        Bitboard::rank(color.fold(rank, rank.flip_vertical()))
    }

    /// Shift using `<<` for `White` and `>>` for `Black`.
    #[inline]
    pub fn relative_shift(self, color: Color, shift: u8) -> Bitboard {
        match color {
            Color::White => Bitboard(self.0 << shift),
            Color::Black => Bitboard(self.0 >> shift),
        }
    }

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
    pub fn add(&mut self, sq: Square) {
        *self |= Bitboard::from_square(sq);
    }

    #[inline]
    pub fn add_all(&mut self, Bitboard(bb): Bitboard) {
        self.0 |= bb;
    }

    #[inline]
    pub fn flip(&mut self, sq: Square) {
        *self ^= Bitboard::from_square(sq)
    }

    #[inline]
    pub fn discard(&mut self, sq: Square) {
        *self &= !Bitboard::from_square(sq);
    }

    #[inline]
    pub fn discard_all(&mut self, Bitboard(bb): Bitboard) {
        self.0 &= !bb;
    }

    #[inline]
    pub fn remove(&mut self, sq: Square) -> bool {
        if self.contains(sq) {
            self.flip(sq);
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

    #[inline]
    pub fn with(self, sq: Square) -> Bitboard {
        self | Bitboard::from_square(sq)
    }

    #[inline]
    pub fn without(self, sq: Square) -> Bitboard {
        self & !Bitboard::from_square(sq)
    }

    #[inline]
    pub fn without_all(self, bb: Bitboard) -> Bitboard {
        self & !bb
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
            // This is safe, because a non-zero u64 can have at most
            // 63 trailing zeros.
            Some(unsafe { Square::from_index_unchecked(self.0.trailing_zeros() as i8) })
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
            // This is safe because a non-zero u64 has between 0 and
            // 63 (included) leading zeros.
            Some(unsafe { Square::from_index_unchecked(63 ^ self.0.leading_zeros() as i8) })
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

    /// An empty bitboard.
    pub const EMPTY: Bitboard = Bitboard(0);

    /// A bitboard containing all squares.
    pub const ALL: Bitboard = Bitboard(!0u64);

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
}

/// Square masks.
#[cfg_attr(feature = "cargo-clippy", allow(unreadable_literal))]
static SQUARES: [u64; 64] = [0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80, 0x100,
    0x200, 0x400, 0x800, 0x1000, 0x2000, 0x4000, 0x8000, 0x10000, 0x20000,
    0x40000, 0x80000, 0x100000, 0x200000, 0x400000, 0x800000, 0x1000000,
    0x2000000, 0x4000000, 0x8000000, 0x10000000, 0x20000000, 0x40000000,
    0x80000000, 0x100000000, 0x200000000, 0x400000000, 0x800000000,
    0x1000000000, 0x2000000000, 0x4000000000, 0x8000000000, 0x10000000000,
    0x20000000000, 0x40000000000, 0x80000000000, 0x100000000000,
    0x200000000000, 0x400000000000, 0x800000000000, 0x1000000000000,
    0x2000000000000, 0x4000000000000, 0x8000000000000, 0x10000000000000,
    0x20000000000000, 0x40000000000000, 0x80000000000000, 0x100000000000000,
    0x200000000000000, 0x400000000000000, 0x800000000000000,
    0x1000000000000000, 0x2000000000000000, 0x4000000000000000,
    0x8000000000000000
];

/// Rank masks.
#[cfg_attr(feature = "cargo-clippy", allow(unreadable_literal))]
static RANKS: [u64; 8] = [0xff, 0xff00, 0xff0000, 0xff000000, 0xff00000000, 0xff0000000000, 0xff000000000000, 0xff00000000000000];

/// File masks.
#[cfg_attr(feature = "cargo-clippy", allow(unreadable_literal))]
static FILES: [u64; 8] = [0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080];

impl fmt::Debug for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for rank in (0..8).rev() {
            for file in 0..8 {
                let sq = Square::from_coords(File::new(file), Rank::new(rank));
                f.write_char(if self.contains(sq) { '1' } else { '.' })?;
                f.write_char(if file < 7 { ' ' } else { '\n' })?;
            }
        }

        Ok(())
    }
}

impl fmt::UpperHex for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:X}", self.0)
    }
}

impl fmt::LowerHex for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:x}", self.0)
    }
}

impl fmt::Octal for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:o}", self.0)
    }
}

impl fmt::Binary for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:b}", self.0)
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

    fn into_iter(self) -> IntoIter {
        IntoIter(self)
    }
}

/// Iterator over the squares of a [`Bitboard`].
///
/// [`Bitboard`]: struct.Bitboard.html
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

    #[cfg(nightly)]
    #[inline]
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl ::std::iter::FusedIterator for IntoIter {}

impl DoubleEndedIterator for IntoIter {
    #[inline]
    fn next_back(&mut self) -> Option<Square> {
        self.0.pop_back()
    }
}

/// Iterator over the subsets of a [`Bitboard`].
///
/// [`Bitboard`]: struct.Bitboard.html
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
        (0, 1usize.checked_shl(self.bb.count_ones()))
    }
}

impl ::std::iter::FusedIterator for CarryRippler {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_more_than_one() {
        assert_eq!(Bitboard(0).more_than_one(), false);
        assert_eq!(Bitboard(1).more_than_one(), false);
        assert_eq!(Bitboard(2).more_than_one(), false);
        assert_eq!(Bitboard(3).more_than_one(), true);
        assert_eq!(Bitboard::ALL.more_than_one(), true);
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
        assert_eq!(Bitboard(0).with(Square::A1).with(Square::H1).last(),
                   Some(Square::H1));
        assert_eq!(Bitboard(0).last(), None);
    }

    #[test]
    fn test_is_empty() {
        assert!(Bitboard(0).is_empty());
        assert!(!Bitboard(1).is_empty());
    }

    #[test]
    fn test_rank() {
        assert_eq!(Bitboard::rank(Rank::Fourth), Bitboard(0xff000000));
    }

    #[test]
    fn test_from_iter() {
        assert_eq!(Bitboard::from_iter(None), Bitboard(0));
        assert_eq!(Bitboard::from_iter(Some(Square::D2)),
                   Bitboard::from_square(Square::D2));
    }
}
