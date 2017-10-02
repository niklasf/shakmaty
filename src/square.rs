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
#[repr(u8)]
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
    /// Create a `Square` from in integer index.
    ///
    /// # Panics
    ///
    /// Panics if the index is not in the range `0..=63`.
    #[inline]
    pub fn new(index: u8) -> Square {
        assert!(index < 64);
        unsafe { Square::from_index_unchecked(index) }
    }

    /// Tries to create a `Square` from an integer index in the range `0..=63`.
    #[inline]
    pub fn from_index(index: u8) -> Option<Square> {
        if index < 64 {
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
    pub unsafe fn from_index_unchecked(index: u8) -> Square {
        debug_assert!(index < 64);
        ::std::mem::transmute(index)
    }

    /// Tries to create a square from zero-based file and rank indexes.
    #[inline]
    pub fn from_coords(file: u8, rank: u8) -> Option<Square> {
        if file < 8 && rank < 8 {
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
    pub unsafe fn from_coords_unchecked(file: u8, rank: u8) -> Square {
        debug_assert!(file < 8);
        debug_assert!(rank < 8);
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
            Ok(unsafe { Square::from_coords_unchecked(s[0] - b'a', s[1] - b'1') })
        } else {
            Err(InvalidSquareName { _priv: () })
        }
    }

    #[inline]
    pub fn index(self) -> u8 {
        self as u8
    }

    #[inline]
    pub fn file(self) -> u8 {
        (self as u8) & 7
    }

    #[inline]
    pub fn file_char(self) -> char {
        (b'a' + self.file()) as char
    }

    #[inline]
    pub fn rank(self) -> u8 {
        (self as u8) >> 3
    }

    #[inline]
    pub fn rank_char(self) -> char {
        (b'1' + self.rank()) as char
    }

    #[inline]
    pub fn offset(self, delta: i8) -> Option<Square> {
        (self as i8).checked_add(delta).and_then(|s| Square::from_index(s as u8))
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
    pub fn distance(self, other: Square) -> u8 {
        max((self.file() as i8 - other.file() as i8).abs(),
            (self.rank() as i8 - other.rank() as i8).abs()) as u8
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
        unsafe { Square::from_index_unchecked(self.file() | (rank.rank() << 3)) }
    }
}

impl From<Square> for i8 {
    #[inline]
    fn from(sq: Square) -> i8 {
        sq as i8
    }
}

impl From<Square> for u8 {
    #[inline]
    fn from(sq: Square) -> u8 {
        sq as u8
    }
}

impl From<Square> for usize {
    #[inline]
    fn from(sq: Square) -> usize {
        sq as usize
    }
}

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
