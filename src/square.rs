use core::{
    cmp::max,
    convert::TryInto,
    fmt::{self, Write as _},
    mem,
    num::TryFromIntError,
    ops::Sub,
    str,
};

use crate::util::overflow_error;

macro_rules! from_repr_u8_impl {
    ($from:ty, $($t:ty)+) => {
        $(impl From<$from> for $t {
            #[inline]
            fn from(value: $from) -> $t {
                value as u8 as $t
            }
        })+
    }
}

macro_rules! try_from_int_impl {
    ($type:ty, $lower:expr, $upper:expr, $($t:ty)+) => {
        $(impl core::convert::TryFrom<$t> for $type {
            type Error = TryFromIntError;

            #[inline]
            #[allow(unused_comparisons)]
            fn try_from(value: $t) -> Result<$type, Self::Error> {
                if ($lower..$upper).contains(&value) {
                    Ok(<$type>::new(value as u32))
                } else {
                    Err(overflow_error())
                }
            }
        })+
    }
}

/// A file of the chessboard.
#[allow(missing_docs)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u8)]
pub enum File {
    A = 0,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}

impl File {
    /// Gets a `File` from an integer index.
    ///
    /// # Panics
    ///
    /// Panics if the index is not in the range `0..=7`.
    #[track_caller]
    #[inline]
    pub const fn new(index: u32) -> File {
        assert!(index < 8);
        unsafe { File::new_unchecked(index) }
    }

    /// Gets a `File` from an integer index.
    ///
    /// # Safety
    ///
    /// It is the callers responsibility to ensure the index is in the range
    /// `0..=7`.
    #[inline]
    pub const unsafe fn new_unchecked(index: u32) -> File {
        debug_assert!(index < 8);
        unsafe { mem::transmute(index as u8) }
    }

    #[inline]
    pub fn from_char(ch: char) -> Option<File> {
        if ('a'..='h').contains(&ch) {
            Some(File::new(u32::from(ch as u8 - b'a')))
        } else {
            None
        }
    }

    #[inline]
    pub fn char(self) -> char {
        char::from(b'a' + u8::from(self))
    }

    #[inline]
    pub fn upper_char(self) -> char {
        char::from(b'A' + u8::from(self))
    }

    #[must_use]
    #[inline]
    pub fn offset(self, delta: i32) -> Option<File> {
        i32::from(self)
            .checked_add(delta)
            .and_then(|index| index.try_into().ok())
    }

    #[inline]
    pub fn distance(self, other: File) -> u32 {
        u32::from(self).abs_diff(u32::from(other))
    }

    #[must_use]
    #[inline]
    pub fn flip_horizontal(self) -> File {
        File::new(7 - u32::from(self))
    }

    #[must_use]
    #[inline]
    pub fn flip_diagonal(self) -> Rank {
        Rank::new(u32::from(self))
    }

    #[must_use]
    #[inline]
    pub fn flip_anti_diagonal(self) -> Rank {
        Rank::new(7 - u32::from(self))
    }

    /// `A`, ..., `H`.
    pub const ALL: [File; 8] = [
        File::A,
        File::B,
        File::C,
        File::D,
        File::E,
        File::F,
        File::G,
        File::H,
    ];
}

impl Sub for File {
    type Output = i32;

    #[inline]
    fn sub(self, other: File) -> i32 {
        i32::from(self) - i32::from(other)
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(self.char())
    }
}

from_repr_u8_impl! { File, u8 i8 u16 i16 u32 i32 u64 i64 usize isize }
try_from_int_impl! { File, 0, 8, u8 i8 u16 i16 u32 i32 u64 i64 usize isize }

/// A rank of the chessboard.
#[allow(missing_docs)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u8)]
pub enum Rank {
    First = 0,
    Second,
    Third,
    Fourth,
    Fifth,
    Sixth,
    Seventh,
    Eighth,
}

impl Rank {
    /// Gets a `Rank` from an integer index.
    ///
    /// # Panics
    ///
    /// Panics if the index is not in the range `0..=7`.
    #[track_caller]
    #[inline]
    pub fn new(index: u32) -> Rank {
        assert!(index < 8);
        unsafe { Rank::new_unchecked(index) }
    }

    /// Gets a `Rank` from an integer index.
    ///
    /// # Safety
    ///
    /// It is the callers responsibility to ensure the index is in the range
    /// `0..=7`.
    #[inline]
    pub unsafe fn new_unchecked(index: u32) -> Rank {
        debug_assert!(index < 8);
        unsafe { mem::transmute(index as u8) }
    }

    #[inline]
    pub fn from_char(ch: char) -> Option<Rank> {
        if ('1'..='8').contains(&ch) {
            Some(Rank::new(u32::from(ch as u8 - b'1')))
        } else {
            None
        }
    }

    #[inline]
    pub fn char(self) -> char {
        char::from(b'1' + u8::from(self))
    }

    #[must_use]
    #[inline]
    pub fn offset(self, delta: i32) -> Option<Rank> {
        i32::from(self)
            .checked_add(delta)
            .and_then(|index| index.try_into().ok())
    }

    #[inline]
    pub fn distance(self, other: Rank) -> u32 {
        u32::from(self).abs_diff(u32::from(other))
    }

    #[must_use]
    #[inline]
    pub fn flip_vertical(self) -> Rank {
        Rank::new(7 - u32::from(self))
    }

    #[must_use]
    #[inline]
    pub fn flip_diagonal(self) -> File {
        File::new(u32::from(self))
    }

    #[must_use]
    #[inline]
    pub fn flip_anti_diagonal(self) -> File {
        File::new(7 - u32::from(self))
    }

    /// `First`, ..., `Eighth`.
    pub const ALL: [Rank; 8] = [
        Rank::First,
        Rank::Second,
        Rank::Third,
        Rank::Fourth,
        Rank::Fifth,
        Rank::Sixth,
        Rank::Seventh,
        Rank::Eighth,
    ];
}

impl Sub for Rank {
    type Output = i32;

    #[inline]
    fn sub(self, other: Rank) -> i32 {
        i32::from(self) - i32::from(other)
    }
}

impl fmt::Display for Rank {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(self.char())
    }
}

from_repr_u8_impl! { Rank, u8 i8 u16 i16 u32 i32 u64 i64 usize isize }
try_from_int_impl! { Rank, 0, 8, u8 i8 u16 i16 u32 i32 u64 i64 usize isize }

/// Error when parsing an invalid square name.
#[derive(Clone, Debug)]
pub struct ParseSquareError;

impl fmt::Display for ParseSquareError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("invalid square name")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for ParseSquareError {}

/// A square of the chessboard.
#[rustfmt::skip]
#[allow(missing_docs)]
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
    #[track_caller]
    #[inline]
    pub const fn new(index: u32) -> Square {
        assert!(index < 64);
        unsafe { Square::new_unchecked(index) }
    }

    /// Gets a `Square` from an integer index.
    ///
    /// # Safety
    ///
    /// It is the callers responsibility to ensure it is in the range `0..=63`.
    #[inline]
    pub const unsafe fn new_unchecked(index: u32) -> Square {
        debug_assert!(index < 64);
        unsafe { mem::transmute(index as u8) }
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
        // Safety: Files and ranks are represented with 3 bits each, and all
        // 6 bit values are in the range 0..=63.
        unsafe { Square::new_unchecked(u32::from(file) | (u32::from(rank) << 3)) }
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
    /// use shakmaty::Square;
    /// let sq = Square::from_ascii(b"a5")?;
    /// assert_eq!(sq, Square::A5);
    /// # Ok::<_, shakmaty::ParseSquareError>(())
    /// ```
    #[inline]
    pub fn from_ascii(s: &[u8]) -> Result<Square, ParseSquareError> {
        if s.len() == 2 {
            match (
                File::from_char(char::from(s[0])),
                Rank::from_char(char::from(s[1])),
            ) {
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
        File::new(u32::from(self) & 7)
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
        Rank::new(u32::from(self) >> 3)
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
    #[must_use]
    #[inline]
    pub fn offset(self, delta: i32) -> Option<Square> {
        i32::from(self)
            .checked_add(delta)
            .and_then(|index| index.try_into().ok())
    }

    /// Calculates the offset from a square index without checking for
    /// overflow.
    ///
    /// # Safety
    ///
    /// It is the callers responsibility to ensure that `delta` is a valid
    /// offset for `self`.
    #[must_use]
    #[inline]
    pub unsafe fn offset_unchecked(self, delta: i32) -> Square {
        debug_assert!(-64 < delta && delta < 64);
        unsafe { Square::new_unchecked((i32::from(self) + delta) as u32) }
    }

    /// Return the bitwise XOR of the numeric square representations. For some
    /// operands this is a useful geometric transformation.
    #[must_use]
    #[inline]
    pub fn xor(self, other: Square) -> Square {
        // Safety: 6 bit value XOR 6 bit value -> 6 bit value.
        unsafe { Square::new_unchecked(u32::from(self) ^ u32::from(other)) }
    }

    /// Flip the square horizontally.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::H1.flip_horizontal(), Square::A1);
    /// assert_eq!(Square::D3.flip_horizontal(), Square::E3);
    /// ```
    #[must_use]
    #[inline]
    pub fn flip_horizontal(self) -> Square {
        self.xor(Square::H1)
    }

    /// Flip the square vertically.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::A8.flip_vertical(), Square::A1);
    /// assert_eq!(Square::D3.flip_vertical(), Square::D6);
    /// ```
    #[must_use]
    #[inline]
    pub fn flip_vertical(self) -> Square {
        self.xor(Square::A8)
    }

    /// Flip at the a1-h8 diagonal by swapping file and rank.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::A1.flip_diagonal(), Square::A1);
    /// assert_eq!(Square::A3.flip_diagonal(), Square::C1);
    /// ```
    #[must_use]
    #[inline]
    pub fn flip_diagonal(self) -> Square {
        // See https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#Diagonal.
        // Safety: We are selecting 32 - 26 = 6 bits with the shift, and all
        // 6 bits values are in the range 0..=63.
        unsafe { Square::new_unchecked(u32::from(self).wrapping_mul(0x2080_0000) >> 26) }
    }

    /// Flip at the h1-a8 diagonal.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::A1.flip_anti_diagonal(), Square::H8);
    /// assert_eq!(Square::A3.flip_anti_diagonal(), Square::F8);
    /// ```
    #[must_use]
    #[inline]
    pub fn flip_anti_diagonal(self) -> Square {
        self.flip_diagonal().rotate_180()
    }

    /// Rotate 90 degrees clockwise.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::A1.rotate_90(), Square::A8);
    /// assert_eq!(Square::A3.rotate_90(), Square::C8);
    /// ```
    #[must_use]
    #[inline]
    pub fn rotate_90(self) -> Square {
        self.flip_diagonal().flip_vertical()
    }

    /// Rotate 180 degrees.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::A1.rotate_180(), Square::H8);
    /// assert_eq!(Square::A3.rotate_180(), Square::H6);
    /// ```
    #[must_use]
    #[inline]
    pub fn rotate_180(self) -> Square {
        self.xor(Square::H8)
    }

    /// Rotate 270 degrees clockwise.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::A1.rotate_270(), Square::H1);
    /// assert_eq!(Square::A3.rotate_270(), Square::F1);
    /// ```
    #[must_use]
    #[inline]
    pub fn rotate_270(self) -> Square {
        self.flip_diagonal().flip_horizontal()
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
        (u32::from(self.rank()) + u32::from(self.file())) % 2 == 1
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
        (u32::from(self.rank()) + u32::from(self.file())) % 2 == 0
    }

    /// The distance between the two squares, i.e. the number of king steps
    /// to get from one square to the other.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::A2.distance(Square::B5), 3);
    /// ```
    pub fn distance(self, other: Square) -> u32 {
        max(
            self.file().distance(other.file()),
            self.rank().distance(other.rank()),
        )
    }
}

mod all_squares {
    use super::Square::{self, *};
    impl Square {
        /// `A1`, `B1`, ..., `G8`, `H8`.
        #[rustfmt::skip]
        pub const ALL: [Square; 64] = [
            A1, B1, C1, D1, E1, F1, G1, H1,
            A2, B2, C2, D2, E2, F2, G2, H2,
            A3, B3, C3, D3, E3, F3, G3, H3,
            A4, B4, C4, D4, E4, F4, G4, H4,
            A5, B5, C5, D5, E5, F5, G5, H5,
            A6, B6, C6, D6, E6, F6, G6, H6,
            A7, B7, C7, D7, E7, F7, G7, H7,
            A8, B8, C8, D8, E8, F8, G8, H8,
        ];
    }
}

from_repr_u8_impl! { Square, u8 i8 u16 i16 u32 i32 u64 i64 usize isize }
try_from_int_impl! { Square, 0, 64, u8 i8 u16 i16 u32 i32 u64 i64 usize isize }

impl Sub for Square {
    type Output = i32;

    #[inline]
    fn sub(self, other: Square) -> i32 {
        i32::from(self) - i32::from(other)
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(self.file().char())?;
        f.write_char(self.rank().char())
    }
}

impl fmt::Debug for Square {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char(self.file().upper_char())?;
        f.write_char(self.rank().char())
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

    #[cfg(feature = "nohash-hasher")]
    #[test]
    fn test_nohash_hasher() {
        use core::hash::{Hash, Hasher};

        let mut hasher = nohash_hasher::NoHashHasher::<Square>::default();
        Square::H1.hash(&mut hasher);
        assert_eq!(hasher.finish(), 7);
    }
}
