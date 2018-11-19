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

use std::cmp::max;
use std::fmt;
use std::str;
use std::error::Error;
use std::ops::Sub;

/// A file of the chessboard.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(i8)]
pub enum File {
    A = 0, B, C, D, E, F, G, H
}

impl File {
    /// Gets a `File` from an integer index.
    ///
    /// # Panics
    ///
    /// Panics if the index is not in the range `0..=7`.
    #[inline]
    pub fn new(index: i8) -> File {
        assert!(0 <= index && index < 8);
        unsafe { File::from_index_unchecked(index) }
    }

    #[inline]
    pub fn from_char(ch: char) -> Option<File> {
        if 'a' <= ch && ch <= 'h' {
            Some(File::new((ch as u8 - b'a') as i8))
        } else {
            None
        }
    }

    #[inline]
    pub fn from_index(index: i8) -> Option<File> {
        if 0 <= index && index < 8 {
            Some(File::new(index))
        } else {
            None
        }
    }

    /// Gets a `File` from an integer index.
    ///
    /// # Unsafety
    ///
    /// It is the callers responsibility to ensure the index is in the range
    /// `0..=7`.
    #[inline]
    pub unsafe fn from_index_unchecked(index: i8) -> File {
        debug_assert!(0 <= index && index < 8);
        ::std::mem::transmute(index)
    }

    #[inline]
    pub fn char(self) -> char {
        char::from(b'a' + u8::from(self))
    }

    #[inline]
    pub fn rotate(self) -> Rank {
        Rank::new(i8::from(self))
    }

    #[inline]
    pub fn offset(self, delta: i8) -> Option<Rank> {
        i8::from(self).checked_add(delta).and_then(Rank::from_index)
    }

    #[inline]
    pub fn flip_horizontal(self) -> File {
        File::new(7 - i8::from(self))
    }
}

impl Sub for File {
    type Output = i8;

    #[inline]
    fn sub(self, other: File) -> i8 {
        i8::from(self) - i8::from(other)
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.char())
    }
}

macro_rules! from_file_impl {
    ($($t:ty)+) => {
        $(impl From<File> for $t {
            #[inline]
            #[cfg_attr(nightly, allow(clippy::cast_lossless))]
            fn from(file: File) -> $t {
                file as i8 as $t
            }
        })+
    }
}

from_file_impl! { u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize f32 f64 }

/// A rank of the chessboard.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(i8)]
pub enum Rank {
    First = 0, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth
}

impl Rank {
    /// Gets a `Rank` from an integer index.
    ///
    /// # Panics
    ///
    /// Panics if the index is not in the range `0..=7`.
    #[inline]
    pub fn new(index: i8) -> Rank {
        assert!(0 <= index && index < 8);
        unsafe { Rank::from_index_unchecked(index) }
    }

    #[inline]
    pub fn from_char(ch: char) -> Option<Rank> {
        if '1' <= ch && ch <= '8' {
            Some(Rank::new((ch as u8 - b'1') as i8))
        } else {
            None
        }
    }

    #[inline]
    pub fn from_index(index: i8) -> Option<Rank> {
        if 0 <= index && index < 8 {
            Some(Rank::new(index))
        } else {
            None
        }
    }

    /// Gets a `Rank` from an integer index.
    ///
    /// # Unsafety
    ///
    /// It is the callers responsibility to ensure the index is in the range
    /// `0..=7`.
    #[inline]
    pub unsafe fn from_index_unchecked(index: i8) -> Rank {
        debug_assert!(0 <= index && index < 8);
        ::std::mem::transmute(index)
    }

    #[inline]
    pub fn char(self) -> char {
        char::from(b'1' + u8::from(self))
    }

    #[inline]
    pub fn rotate(self) -> File {
        File::new(i8::from(self))
    }

    #[inline]
    pub fn offset(self, delta: i8) -> Option<Rank> {
        i8::from(self).checked_add(delta).and_then(Rank::from_index)
    }

    #[inline]
    pub fn flip_vertical(self) -> Rank {
        Rank::new(7 - i8::from(self))
    }
}

impl Sub for Rank {
    type Output = i8;

    #[inline]
    fn sub(self, other: Rank) -> i8 {
        i8::from(self) - i8::from(other)
    }
}

impl fmt::Display for Rank {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.char())
    }
}

macro_rules! from_rank_impl {
    ($($t:ty)+) => {
        $(impl From<Rank> for $t {
            #[inline]
            #[cfg_attr(nightly, allow(clippy::cast_lossless))]
            fn from(rank: Rank) -> $t {
                rank as i8 as $t
            }
        })+
    }
}

from_rank_impl! { u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize f32 f64 }

/// Error when parsing an invalid square name.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseSquareError;

impl fmt::Display for ParseSquareError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "invalid square name".fmt(f)
    }
}

impl Error for ParseSquareError {
    fn description(&self) -> &str {
        "invalid square name"
    }
}

impl From<()> for ParseSquareError {
    fn from(_: ()) -> ParseSquareError {
        ParseSquareError
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
    /// Gets a `Square` from an integer index.
    ///
    /// # Panics
    ///
    /// Panics if the index is not in the range `0..=63`.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::new(0), Square::A1);
    /// assert_eq!(Square::new(63), Square::H8);
    /// ```
    #[inline]
    pub fn new(index: i8) -> Square {
        assert!(0 <= index && index < 64);
        unsafe { Square::from_index_unchecked(index) }
    }

    /// Tries to get a `Square` from an integer index in the range `0..=63`.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::from_index(0), Some(Square::A1));
    /// assert_eq!(Square::from_index(63), Some(Square::H8));
    ///
    /// assert_eq!(Square::from_index(64), None);
    /// ```
    #[inline]
    pub fn from_index(index: i8) -> Option<Square> {
        if 0 <= index && index < 64 {
            Some(Square::new(index))
        } else {
            None
        }
    }

    /// Gets a `Square` from an integer index.
    ///
    /// # Unsafety
    ///
    /// It is the callers responsibility to ensure it is in the range `0..=63`.
    #[inline]
    pub unsafe fn from_index_unchecked(index: i8) -> Square {
        debug_assert!(0 <= index && index < 64);
        ::std::mem::transmute(index)
    }

    /// Tries to get a square from file and rank.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Square, File, Rank};
    ///
    /// assert_eq!(Square::from_coords(File::A, Rank::First), Square::A1);
    /// ```
    #[inline]
    pub fn from_coords(file: File, rank: Rank) -> Square {
        unsafe { Square::from_index_unchecked(i8::from(file) | (i8::from(rank) << 3)) }
    }

    /// Parses a square name.
    ///
    /// # Errors
    ///
    /// Returns [`ParseSquareError`] if the input is not a valid square name
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
    /// let sq = Square::from_ascii(b"a5")?;
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
    /// [`ParseSquareError`]: struct.ParseSquareError.html
    #[inline]
    pub fn from_ascii(s: &[u8]) -> Result<Square, ParseSquareError> {
        if s.len() == 2 {
            match (File::from_char(char::from(s[0])), Rank::from_char(char::from(s[1]))) {
                (Some(file), Some(rank)) => Ok(Square::from_coords(file, rank)),
                _ => Err(ParseSquareError),
            }
        } else {
            Err(ParseSquareError)
        }
    }

    /// Gets the file.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Square, File};
    ///
    /// assert_eq!(Square::A1.file(), File::A);
    /// assert_eq!(Square::B2.file(), File::B);
    /// ```
    #[inline]
    pub fn file(self) -> File {
        File::new(i8::from(self) & 7)
    }

    /// Gets the rank.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Square, Rank};
    ///
    /// assert_eq!(Square::A1.rank(), Rank::First);
    /// assert_eq!(Square::B2.rank(), Rank::Second);
    /// ```
    #[inline]
    pub fn rank(self) -> Rank {
        Rank::new(i8::from(self) >> 3)
    }

    /// Gets file and rank.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Square, File, Rank};
    ///
    /// assert_eq!(Square::A1.coords(), (File::A, Rank::First));
    /// assert_eq!(Square::H8.coords(), (File::H, Rank::Eighth));
    /// ```
    #[inline]
    pub fn coords(self) -> (File, Rank) {
        (self.file(), self.rank())
    }

    /// Calculates the offset from a square index.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::F3.offset(8), Some(Square::F4));
    /// assert_eq!(Square::F3.offset(-1), Some(Square::E3));
    ///
    /// assert_eq!(Square::F3.offset(48), None);
    /// ```
    #[inline]
    pub fn offset(self, delta: i8) -> Option<Square> {
        i8::from(self).checked_add(delta).and_then(Square::from_index)
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
        // This is safe because all 6 bit values are in the range 0..=63.
        unsafe { Square::from_index_unchecked(i8::from(self) ^ 0b000_111) }
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
        // This is safe because all 6 bit values are in the range 0..=63.
        unsafe { Square::from_index_unchecked(i8::from(self) ^ 0b111_000) }
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
        Square::from_coords(self.rank().rotate(), self.file().rotate())
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
        (i8::from(self.rank()) + i8::from(self.file())) % 2 == 1
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
        (i8::from(self.rank()) + i8::from(self.file())) % 2 == 0
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
        Square::from_coords(self.file(), rank.rank())
    }
}

macro_rules! from_square_impl {
    ($($t:ty)+) => {
        $(impl From<Square> for $t {
            #[inline]
            #[cfg_attr(nightly, allow(clippy::cast_lossless))]
            fn from(sq: Square) -> $t {
                sq as $t
            }
        })+
    }
}

from_square_impl! { u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize }

impl Sub for Square {
    type Output = i8;

    #[inline]
    fn sub(self, other: Square) -> i8 {
        i8::from(self) - i8::from(other)
    }
}

impl From<(File, Rank)> for Square {
    #[inline]
    fn from((file, rank): (File, Rank)) -> Square {
        Square::from_coords(file, rank)
    }
}

impl str::FromStr for Square {
    type Err = ParseSquareError;

    fn from_str(s: &str) -> Result<Square, ParseSquareError> {
        Square::from_ascii(s.as_bytes())
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.file().char(), self.rank().char())
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
        for file in (0..8).map(File::new) {
            for rank in (0..8).map(Rank::new) {
                let square = Square::from_coords(file, rank);
                assert_eq!(square.file(), file);
                assert_eq!(square.rank(), rank);
            }
        }
    }
}
