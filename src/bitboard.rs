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

use std::ops;
use std::fmt;
use std::fmt::Write;
use std::iter::FromIterator;

use square::Square;
use types::Color;

pub static SQUARES: [u64; 64] = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824, 2147483648, 4294967296, 8589934592, 17179869184, 34359738368, 68719476736, 137438953472, 274877906944, 549755813888, 1099511627776, 2199023255552, 4398046511104, 8796093022208, 17592186044416, 35184372088832, 70368744177664, 140737488355328, 281474976710656, 562949953421312, 1125899906842624, 2251799813685248, 4503599627370496, 9007199254740992, 18014398509481984, 36028797018963968, 72057594037927936, 144115188075855872, 288230376151711744, 576460752303423488, 1152921504606846976, 2305843009213693952, 4611686018427387904, 9223372036854775808];

/// A set of squares represented by a 64 bit integer mask.
///
/// # Examples
///
/// ```
/// # use shakmaty::Bitboard;
/// # use shakmaty::square;
/// let mask = Bitboard::rank(2).with(square::E5);
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . 1 . . .
/// // . . . . . . . .
/// // 1 1 1 1 1 1 1 1
/// // . . . . . . . .
/// // . . . . . . . .
///
/// assert_eq!(mask.first(), Some(square::A3));
/// ```
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Bitboard(pub u64);

impl Bitboard {
    /// A bitboard with a single square.
    #[inline]
    pub fn from_square(sq: Square) -> Bitboard {
        Bitboard(unsafe { *SQUARES.get_unchecked(sq.index() as usize) })
    }

    /// A bitboard containing all squares.
    #[inline]
    pub const fn all() -> Bitboard {
        Bitboard(!0u64)
    }

    /// Returns the bitboard containing all squares of the given rank
    /// (or an empty bitboard if the rank index is out of range).
    #[inline]
    pub fn rank(rank: i8) -> Bitboard {
        if 0 <= rank && rank < 8 {
            Bitboard(0xff << (8 * rank))
        } else {
            Bitboard(0)
        }
    }

    /// Returns the bitboard containing all squares of the given file
    /// (or an empty bitboard if the file index is out of range).
    #[inline]
    pub fn file(file: i8) -> Bitboard {
        if 0 <= file && file < 8 {
            Bitboard(0x0101_0101_0101_0101 << file)
        } else {
            Bitboard(0)
        }
    }

    /// Like `rank()`, but from the point of view of `color`.
    #[inline]
    pub fn relative_rank(color: Color, rank: i8) -> Bitboard {
        if 0 <= rank && rank < 8 {
            match color {
                Color::White => Bitboard(0xff << (8 * rank)),
                Color::Black => Bitboard(0xff00_0000_0000_0000 >> (8 * rank)),
            }
        } else {
            Bitboard(0)
        }
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
    pub fn contains(self, sq: Square) -> bool {
        !(self & Bitboard::from_square(sq)).is_empty()
    }

    #[inline]
    pub fn add(&mut self, sq: Square) {
        self.0 |= unsafe { *SQUARES.get_unchecked(sq.index() as usize) };
    }

    #[inline]
    pub fn add_all(&mut self, Bitboard(bb): Bitboard) {
        self.0 |= bb;
    }

    #[inline]
    pub fn flip(&mut self, sq: Square) {
        self.0 ^= unsafe { *SQUARES.get_unchecked(sq.index() as usize) };
    }

    #[inline]
    pub fn discard(&mut self, sq: Square) {
        self.0 &= unsafe { !*SQUARES.get_unchecked(sq.index() as usize) };
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
}

/// All dark squares.
pub const DARK_SQUARES: Bitboard = Bitboard(0xaa55_aa55_aa55_aa55);

/// All light squares.
pub const LIGHT_SQUARES: Bitboard = Bitboard(0x55aa_55aa_55aa_55aa);

/// The four corner squares.
pub const CORNERS: Bitboard = Bitboard(0x8100_0000_0000_0081);

/// The backranks.
pub const BACKRANKS: Bitboard = Bitboard(0xff00_0000_0000_00ff);

impl fmt::Debug for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for rank in (0..8).rev() {
            for file in 0..8 {
                let sq = Square::from_coords(file, rank).unwrap();
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
    fn from(sq: Square) -> Bitboard {
        Bitboard::from_square(sq)
    }
}

impl<T> ops::BitAnd<T> for Bitboard
    where T: Into<Bitboard>
{
    type Output = Bitboard;

    #[inline]
    fn bitand(self, rhs: T) -> Bitboard {
        let Bitboard(rhs) = rhs.into();
        Bitboard(self.0 & rhs)
    }
}

impl<T> ops::BitAndAssign<T> for Bitboard
    where T: Into<Bitboard>
{
    #[inline]
    fn bitand_assign(&mut self, rhs: T) {
        let Bitboard(rhs) = rhs.into();
        self.0 &= rhs;
    }
}

impl<T> ops::BitOr<T> for Bitboard
    where T: Into<Bitboard>
{
    type Output = Bitboard;

    #[inline]
    fn bitor(self, rhs: T) -> Bitboard {
        let Bitboard(rhs) = rhs.into();
        Bitboard(self.0 | rhs)
    }
}

impl<T> ops::BitOrAssign<T> for Bitboard
    where T: Into<Bitboard>
{
    #[inline]
    fn bitor_assign(&mut self, rhs: T) {
        let Bitboard(rhs) = rhs.into();
        self.0 |= rhs;
    }
}

impl<T> ops::BitXor<T> for Bitboard
    where T: Into<Bitboard>
{
    type Output = Bitboard;

    #[inline]
    fn bitxor(self, rhs: T) -> Bitboard {
        let Bitboard(rhs) = rhs.into();
        Bitboard(self.0 ^ rhs)
    }
}

impl<T> ops::BitXorAssign<T> for Bitboard
    where T: Into<Bitboard>
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
        where T: IntoIterator<Item = Square>
    {
        let mut result = Bitboard(0);
        for square in iter {
            result.add(square);
        }
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

impl Iterator for Bitboard {
    type Item = Square;

    #[inline]
    fn next(&mut self) -> Option<Square> {
        let square = self.first();
        self.0 &= self.0.wrapping_sub(1);
        square
    }

    #[inline]
    fn count(self) -> usize {
        self.len()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }

    #[inline]
    fn last(self) -> Option<Square> {
        if self.is_empty() {
            None
        } else {
            // This is safe because a non-zero u64 has between 0 and
            // 63 (included) leading zeros.
            Some(unsafe { Square::from_index_unchecked(63 ^ self.0.leading_zeros() as i8) })
        }
    }
}

impl ExactSizeIterator for Bitboard {
    #[inline]
    fn len(&self) -> usize {
        self.0.count_ones() as usize
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.0 == 0
    }
}

impl DoubleEndedIterator for Bitboard {
    #[inline]
    fn next_back(&mut self) -> Option<Square> {
        if self.is_empty() {
            None
        } else {
            // This is safe because a non-zero u64 has between 0 and
            // 63 (included) leading zeros.
            let sq = unsafe { Square::from_index_unchecked(63 ^ self.0.leading_zeros() as i8) };
            self.0 ^= unsafe { *SQUARES.get_unchecked(sq.index() as usize) };
            Some(sq)
        }
    }
}

/// Iterator over the subsets of a `Bitboard`.
#[derive(Debug)]
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use square;

    #[test]
    fn test_more_than_one() {
        assert_eq!(Bitboard(0).more_than_one(), false);
        assert_eq!(Bitboard(1).more_than_one(), false);
        assert_eq!(Bitboard(2).more_than_one(), false);
        assert_eq!(Bitboard(3).more_than_one(), true);
        assert_eq!(Bitboard::all().more_than_one(), true);
    }

    #[test]
    fn test_first() {
        assert_eq!(Bitboard::from_square(square::A1).first(), Some(square::A1));
        assert_eq!(Bitboard::from_square(square::D2).first(), Some(square::D2));
        assert_eq!(Bitboard(0).first(), None);
    }

    #[test]
    fn test_last() {
        assert_eq!(Bitboard::from_square(square::A1).last(), Some(square::A1));
        assert_eq!(Bitboard(0).with(square::A1).with(square::H1).last(),
                   Some(square::H1));
        assert_eq!(Bitboard(0).last(), None);
    }

    #[test]
    fn test_rank() {
        assert_eq!(Bitboard::rank(3), Bitboard(0xff000000));
    }

    #[test]
    fn test_from_iter() {
        assert_eq!(Bitboard::from_iter(None), Bitboard(0));
        assert_eq!(Bitboard::from_iter(Some(square::D2)),
                   Bitboard::from_square(square::D2));
    }
}
