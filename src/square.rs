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

//! Square constants.

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
pub struct Square(i8);

impl Square {
    /// Create a `Square` from in integer index.
    ///
    /// # Panics
    ///
    /// Panics if the index is not in the range `0..=63`.
    #[inline]
    pub fn new(index: i8) -> Square {
        assert!(0 <= index && index < 64);
        Square(index)
    }

    /// Tries to create a `Square` from an integer index in the range `0..=63`.
    #[inline]
    pub fn from_index(index: i8) -> Option<Square> {
        if 0 <= index && index < 64 {
            Some(Square(index))
        } else {
            None
        }
    }

    /// Creates a `Square` from an integer index. It is the callers
    /// responsibility to ensure it is in the range `0..=63`.
    #[inline]
    pub unsafe fn from_index_unchecked(index: i8) -> Square {
        debug_assert!(0 <= index && index < 64);
        Square(index)
    }

    /// Tries to create a square from zero-based file and rank indexes.
    #[inline]
    pub fn from_coords(file: i8, rank: i8) -> Option<Square> {
        if 0 <= file && file < 8 && 0 <= rank && rank < 8 {
            Some(Square(file | (rank << 3)))
        } else {
            None
        }
    }

    /// Parse a square name.
    #[inline]
    pub fn from_bytes(s: &[u8]) -> Result<Square, InvalidSquareName> {
        if s.len() == 2 && b'a' <= s[0] && s[0] <= b'h' && b'1' <= s[1] && s[1] <= b'8' {
            Ok(Square((s[0] - b'a') as i8 | ((s[1] - b'1') << 3) as i8))
        } else {
            Err(InvalidSquareName { _priv: () })
        }
    }

    #[inline]
    pub fn index(self) -> i8 {
        self.0
    }

    #[inline]
    pub fn file(self) -> i8 {
        let Square(sq) = self;
        sq & 7
    }

    #[inline]
    pub fn file_char(self) -> char {
        (b'a' + self.file() as u8) as char
    }

    #[inline]
    pub fn rank(self) -> i8 {
        let Square(sq) = self;
        sq >> 3
    }

    #[inline]
    pub fn rank_char(self) -> char {
        (b'1' + self.rank() as u8) as char
    }

    #[inline]
    pub fn offset(self, delta: i8) -> Option<Square> {
        let Square(sq) = self;
        if 0 <= sq + delta && sq + delta < 64 {
            Some(Square(sq + delta))
        } else {
            None
        }
    }

    /// Tests is the square is a light square.
    ///
    /// ```
    /// use shakmaty::square;
    ///
    /// assert!(square::D1.is_light());
    /// assert!(!square::D8.is_light());
    /// ```
    #[inline]
    pub fn is_light(self) -> bool {
        (self.rank() + self.file()) % 2 == 1
    }

    /// Tests is the square is a dark square.
    ///
    /// ```
    /// use shakmaty::square;
    ///
    /// assert!(square::E1.is_dark());
    /// assert!(!square::E8.is_dark());
    /// ```
    #[inline]
    pub fn is_dark(self) -> bool {
        (self.rank() + self.file()) % 2 == 0
    }

    /// The distance between the two squares, i.e. the number of king steps
    /// to get from one square to the other.
    ///
    /// ```
    /// use shakmaty::square;
    ///
    /// assert_eq!(square::A2.distance(square::B5), 3);
    /// ```
    pub fn distance(self, other: Square) -> i8 {
        max((self.file() - other.file()).abs(),
            (self.rank() - other.rank()).abs())
    }

    /// Combines two squares, taking the file from the first and the rank from
    /// the second.
    ///
    /// ```
    /// use shakmaty::square;
    ///
    /// assert_eq!(square::D3.combine(square::F5), square::D5);
    /// ```
    #[inline]
    pub fn combine(self, rank: Square) -> Square {
        Square(self.file() | (rank.rank() << 3))
    }
}

impl From<Square> for i8 {
    #[inline]
    fn from(sq: Square) -> i8 {
        sq.0
    }
}

impl Sub for Square {
    type Output = i8;

    #[inline]
    fn sub(self, Square(other): Square) -> i8 {
        self.0 - other
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

pub const A1: Square = Square(0);
pub const B1: Square = Square(1);
pub const C1: Square = Square(2);
pub const D1: Square = Square(3);
pub const E1: Square = Square(4);
pub const F1: Square = Square(5);
pub const G1: Square = Square(6);
pub const H1: Square = Square(7);
pub const A2: Square = Square(8);
pub const B2: Square = Square(9);
pub const C2: Square = Square(10);
pub const D2: Square = Square(11);
pub const E2: Square = Square(12);
pub const F2: Square = Square(13);
pub const G2: Square = Square(14);
pub const H2: Square = Square(15);
pub const A3: Square = Square(16);
pub const B3: Square = Square(17);
pub const C3: Square = Square(18);
pub const D3: Square = Square(19);
pub const E3: Square = Square(20);
pub const F3: Square = Square(21);
pub const G3: Square = Square(22);
pub const H3: Square = Square(23);
pub const A4: Square = Square(24);
pub const B4: Square = Square(25);
pub const C4: Square = Square(26);
pub const D4: Square = Square(27);
pub const E4: Square = Square(28);
pub const F4: Square = Square(29);
pub const G4: Square = Square(30);
pub const H4: Square = Square(31);
pub const A5: Square = Square(32);
pub const B5: Square = Square(33);
pub const C5: Square = Square(34);
pub const D5: Square = Square(35);
pub const E5: Square = Square(36);
pub const F5: Square = Square(37);
pub const G5: Square = Square(38);
pub const H5: Square = Square(39);
pub const A6: Square = Square(40);
pub const B6: Square = Square(41);
pub const C6: Square = Square(42);
pub const D6: Square = Square(43);
pub const E6: Square = Square(44);
pub const F6: Square = Square(45);
pub const G6: Square = Square(46);
pub const H6: Square = Square(47);
pub const A7: Square = Square(48);
pub const B7: Square = Square(49);
pub const C7: Square = Square(50);
pub const D7: Square = Square(51);
pub const E7: Square = Square(52);
pub const F7: Square = Square(53);
pub const G7: Square = Square(54);
pub const H7: Square = Square(55);
pub const A8: Square = Square(56);
pub const B8: Square = Square(57);
pub const C8: Square = Square(58);
pub const D8: Square = Square(59);
pub const E8: Square = Square(60);
pub const F8: Square = Square(61);
pub const G8: Square = Square(62);
pub const H8: Square = Square(63);

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
