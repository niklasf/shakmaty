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

use std::cmp::max;
use std::fmt;
use std::str;
use std::error::Error;
use std::ops::Sub;

/// Error when parsing an invalid square name.
pub struct InvalidSquareName {
    _priv: (),
}

impl fmt::Debug for InvalidSquareName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("InvalidSquareName").finish()
    }
}

impl fmt::Display for InvalidSquareName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "invalid square name".fmt(f)
    }
}

impl Error for InvalidSquareName {
    fn description(&self) -> &str {
        "invalid square name"
    }
}

/// A square index.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(i8)]
pub enum Square {
    A1 = 0, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
}

impl Square {
    /// Creates a `Square` from an integer index.
    ///
    /// # Panics
    ///
    /// Panics if the index is not in the range `0..=63`.
    #[inline]
    pub fn new(index: i8) -> Square {
        assert!(0 <= index && index < 64);
        unsafe { Square::from_index_unchecked(index) }
    }

    /// Tries to create a `Square` from an integer index in the range `0..=63`.
    #[inline]
    pub fn from_index(index: i8) -> Option<Square> {
        if 0 <= index && index < 64 {
            Some(unsafe { Square::from_index_unchecked(index) })
        } else {
            None
        }
    }

    /// Creates a `Square` from an integer index.
    ///
    /// # Unsafety
    ///
    /// It is the callers responsibility to ensure it is in the range `0..=63`.
    #[inline]
    pub unsafe fn from_index_unchecked(index: i8) -> Square {
        debug_assert!(0 <= index && index < 64);
        ::std::mem::transmute(index)
    }

    /// Tries to create a square from zero-based file and rank indexes.
    #[inline]
    pub fn from_coords(file: i8, rank: i8) -> Option<Square> {
        if 0 <= file && file < 8 && 0 <= rank && rank < 8 {
            Some(unsafe { Square::from_coords_unchecked(file, rank) })
        } else {
            None
        }
    }

    /// Creates a `Square` from zero-based file and rank indexes.
    ///
    /// # Unsafety
    ///
    /// It is the callers responsibility to ensure that file and rank are in
    /// the range `0..=7`.
    #[inline]
    pub unsafe fn from_coords_unchecked(file: i8, rank: i8) -> Square {
        debug_assert!(0 <= file && file < 8 && 0 <= rank && rank < 8);
        Square::from_index_unchecked(file | (rank << 3))
    }

    /// Parses a square name.
    ///
    /// # Errors
    ///
    /// Returns [`InvalidSquareName`] if the input is not a valid square name
    /// in lowercase ASCII characters.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::error::Error;
    /// #
    /// # fn try_main() -> Result<(), Box<Error>> {
    /// use shakmaty::Square;
    ///
    /// let sq = Square::from_bytes(b"a5")?;
    /// assert_eq!(sq, Square::A5);
    /// #
    /// #     Ok(())
    /// # }
    /// #
    /// # fn main() {
    /// #     try_main().unwrap();
    /// # }
    /// ```
    ///
    /// [`InvalidSquareName`]: struct.InvalidSquareName.html
    #[inline]
    pub fn from_bytes(s: &[u8]) -> Result<Square, InvalidSquareName> {
        if s.len() == 2 && b'a' <= s[0] && s[0] <= b'h' && b'1' <= s[1] && s[1] <= b'8' {
            Ok(unsafe { Square::from_coords_unchecked((s[0] - b'a') as i8, (s[1] - b'1') as i8) })
        } else {
            Err(InvalidSquareName { _priv: () })
        }
    }

    #[inline]
    pub fn file(self) -> i8 {
        (self as i8) & 7
    }

    #[inline]
    pub fn file_char(self) -> char {
        (b'a' + self.file() as u8) as char
    }

    #[inline]
    pub fn rank(self) -> i8 {
        (self as i8) >> 3
    }

    #[inline]
    pub fn rank_char(self) -> char {
        (b'1' + self.rank() as u8) as char
    }

    #[inline]
    pub fn offset(self, delta: i8) -> Option<Square> {
        (self as i8).checked_add(delta).and_then(Square::from_index)
    }

    /// Flip the square horizontally.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::H1.flip_horizontal(), Square::A1);
    /// assert_eq!(Square::D3.flip_horizontal(), Square::E3);
    /// ```
    #[inline]
    pub fn flip_horizontal(self) -> Square {
        unsafe { Square::from_index_unchecked((self as i8) ^ 0x07) }
    }

    /// Flip the square vertically.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::A8.flip_vertical(), Square::A1);
    /// assert_eq!(Square::D3.flip_vertical(), Square::D6);
    /// ```
    #[inline]
    pub fn flip_vertical(self) -> Square {
        unsafe { Square::from_index_unchecked((self as i8) ^ 0x38) }
    }

    /// Flip at the a1-h8 diagonal by swapping file and rank.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::A1.flip_diagonal(), Square::A1);
    /// assert_eq!(Square::A3.flip_diagonal(), Square::C1);
    /// ```
    pub fn flip_diagonal(self) -> Square {
        unsafe { Square::from_coords_unchecked(self.rank(), self.file()) }
    }

    /// Tests is the square is a light square.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert!(Square::D1.is_light());
    /// assert!(!Square::D8.is_light());
    /// ```
    #[inline]
    pub fn is_light(self) -> bool {
        (self.rank() + self.file()) % 2 == 1
    }

    /// Tests is the square is a dark square.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert!(Square::E1.is_dark());
    /// assert!(!Square::E8.is_dark());
    /// ```
    #[inline]
    pub fn is_dark(self) -> bool {
        (self.rank() + self.file()) % 2 == 0
    }

    /// The distance between the two squares, i.e. the number of king steps
    /// to get from one square to the other.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::A2.distance(Square::B5), 3);
    /// ```
    pub fn distance(self, other: Square) -> i8 {
        max((self.file() - other.file()).abs(),
            (self.rank() - other.rank()).abs())
    }

    /// Combines two squares, taking the file from the first and the rank from
    /// the second.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::D3.combine(Square::F5), Square::D5);
    /// ```
    #[inline]
    pub fn combine(self, rank: Square) -> Square {
        unsafe { Square::from_coords_unchecked(self.file(), rank.rank()) }
    }
}

macro_rules! from_square_impl {
    ($($t:ty)+) => {
        $(impl From<Square> for $t {
            #[inline]
            fn from(sq: Square) -> $t {
                sq as $t
            }
        })+
    }
}

from_square_impl! { u8 i8 u16 i16 u32 i32 u64 i64 usize isize }

impl Sub for Square {
    type Output = i8;

    #[inline]
    fn sub(self, other: Square) -> i8 {
        self as i8 - other as i8
    }
}

impl str::FromStr for Square {
    type Err = InvalidSquareName;

    fn from_str(s: &str) -> Result<Square, InvalidSquareName> {
        Square::from_bytes(s.as_bytes())
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.file_char(), self.rank_char())
    }
}

impl fmt::Debug for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string().to_uppercase())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_square() {
        for file in 0..8 {
            for rank in 0..8 {
                let square = Square::from_coords(file, rank).unwrap();
                assert_eq!(square.file(), file);
                assert_eq!(square.rank(), rank);
            }
        }
    }
}
