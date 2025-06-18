use core::{
    error,
    fmt::{self, Write as _},
    mem,
    num::TryFromIntError,
    ops::Sub,
    str,
};

use crate::util::{out_of_range_error, AppendAscii};

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
                    Err(out_of_range_error())
                }
            }
        })+
    }
}

/// A file of the chessboard.
#[allow(missing_docs)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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
    pub const fn from_char(ch: char) -> Option<File> {
        Some(match ch {
            'a' => File::A,
            'b' => File::B,
            'c' => File::C,
            'd' => File::D,
            'e' => File::E,
            'f' => File::F,
            'g' => File::G,
            'h' => File::H,
            _ => return None,
        })
    }

    #[inline]
    pub const fn char(self) -> char {
        match self {
            File::A => 'a',
            File::B => 'b',
            File::C => 'c',
            File::D => 'd',
            File::E => 'e',
            File::F => 'f',
            File::G => 'g',
            File::H => 'h',
        }
    }

    #[inline]
    pub const fn upper_char(self) -> char {
        match self {
            File::A => 'A',
            File::B => 'B',
            File::C => 'C',
            File::D => 'D',
            File::E => 'E',
            File::F => 'F',
            File::G => 'G',
            File::H => 'H',
        }
    }

    #[must_use]
    #[inline]
    pub fn offset(self, delta: i32) -> Option<File> {
        self.to_u32()
            .checked_add_signed(delta)
            .and_then(|index| index.try_into().ok())
    }

    #[inline]
    pub const fn distance(self, other: File) -> u32 {
        self.to_u32().abs_diff(other.to_u32())
    }

    #[must_use]
    #[inline]
    pub const fn flip_horizontal(self) -> File {
        File::new(7 - self.to_u32())
    }

    #[must_use]
    #[inline]
    pub const fn flip_diagonal(self) -> Rank {
        Rank::new(self.to_u32())
    }

    #[must_use]
    #[inline]
    pub const fn flip_anti_diagonal(self) -> Rank {
        Rank::new(7 - self.to_u32())
    }

    #[must_use]
    #[inline]
    pub const fn to_u32(self) -> u32 {
        self as u32
    }

    #[must_use]
    #[inline(always)]
    pub const fn to_usize(self) -> usize {
        self as usize
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

from_enum_as_int_impl! { File, u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize }
try_from_int_impl! { File, 0, 8, u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize }

/// A rank of the chessboard.
#[allow(missing_docs)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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
    pub const fn new(index: u32) -> Rank {
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
    pub const unsafe fn new_unchecked(index: u32) -> Rank {
        debug_assert!(index < 8);
        unsafe { mem::transmute(index as u8) }
    }

    #[inline]
    pub const fn from_char(ch: char) -> Option<Rank> {
        Some(match ch {
            '1' => Rank::First,
            '2' => Rank::Second,
            '3' => Rank::Third,
            '4' => Rank::Fourth,
            '5' => Rank::Fifth,
            '6' => Rank::Sixth,
            '7' => Rank::Seventh,
            '8' => Rank::Eighth,
            _ => return None,
        })
    }

    #[inline]
    pub const fn char(self) -> char {
        match self {
            Rank::First => '1',
            Rank::Second => '2',
            Rank::Third => '3',
            Rank::Fourth => '4',
            Rank::Fifth => '5',
            Rank::Sixth => '6',
            Rank::Seventh => '7',
            Rank::Eighth => '8',
        }
    }

    #[must_use]
    #[inline]
    pub fn offset(self, delta: i32) -> Option<Rank> {
        self.to_u32()
            .checked_add_signed(delta)
            .and_then(|index| index.try_into().ok())
    }

    #[inline]
    pub const fn distance(self, other: Rank) -> u32 {
        self.to_u32().abs_diff(other.to_u32())
    }

    #[must_use]
    #[inline]
    pub const fn flip_vertical(self) -> Rank {
        Rank::new(7 - self.to_u32())
    }

    #[must_use]
    #[inline]
    pub const fn flip_diagonal(self) -> File {
        File::new(self.to_u32())
    }

    #[must_use]
    #[inline]
    pub const fn flip_anti_diagonal(self) -> File {
        File::new(7 - self.to_u32())
    }

    #[must_use]
    #[inline]
    pub const fn to_u32(self) -> u32 {
        self as u32
    }

    #[must_use]
    #[inline]
    pub const fn to_usize(self) -> usize {
        self as usize
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

from_enum_as_int_impl! { Rank, u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize }
try_from_int_impl! { Rank, 0, 8, u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize }

/// Error when parsing an invalid square name.
#[derive(Clone, Debug)]
pub struct ParseSquareError;

impl fmt::Display for ParseSquareError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("invalid square name")
    }
}

impl error::Error for ParseSquareError {}

/// A square of the chessboard.
#[rustfmt::skip]
#[allow(missing_docs)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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
    pub const fn from_coords(file: File, rank: Rank) -> Square {
        // Safety: Files and ranks are represented with 3 bits each, and all
        // 6 bit values are in the range 0..=63.
        unsafe { Square::new_unchecked(file.to_u32() | (rank.to_u32() << 3)) }
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
    pub const fn from_ascii(s: &[u8]) -> Result<Square, ParseSquareError> {
        if s.len() != 2 {
            return Err(ParseSquareError);
        }
        let Some(file) = File::from_char(s[0] as char) else {
            return Err(ParseSquareError);
        };
        let Some(rank) = Rank::from_char(s[1] as char) else {
            return Err(ParseSquareError);
        };
        Ok(Square::from_coords(file, rank))
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
    pub const fn file(self) -> File {
        File::new(self.to_u32() & 7)
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
    pub const fn rank(self) -> Rank {
        Rank::new(self.to_u32() >> 3)
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
    pub const fn coords(self) -> (File, Rank) {
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
        self.to_u32()
            .checked_add_signed(delta)
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
    pub const unsafe fn offset_unchecked(self, delta: i32) -> Square {
        debug_assert!(-64 < delta && delta < 64);
        unsafe { Square::new_unchecked(self.to_u32().wrapping_add_signed(delta)) }
    }

    /// Return the bitwise XOR of the numeric square representations. For some
    /// operands this is a useful geometric transformation.
    #[must_use]
    #[inline]
    pub const fn xor(self, other: Square) -> Square {
        // Safety: 6 bit value XOR 6 bit value -> 6 bit value.
        unsafe { Square::new_unchecked(self.to_u32() ^ other.to_u32()) }
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
    pub const fn flip_horizontal(self) -> Square {
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
    pub const fn flip_vertical(self) -> Square {
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
    pub const fn flip_diagonal(self) -> Square {
        // See https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#Diagonal.
        // Safety: We are selecting 32 - 26 = 6 bits with the shift, and all
        // 6 bits values are in the range 0..=63.
        unsafe { Square::new_unchecked(self.to_u32().wrapping_mul(0x2080_0000) >> 26) }
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
    pub const fn flip_anti_diagonal(self) -> Square {
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
    pub const fn rotate_90(self) -> Square {
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
    pub const fn rotate_180(self) -> Square {
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
    pub const fn rotate_270(self) -> Square {
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
    pub const fn is_light(self) -> bool {
        (self.rank().to_u32() + self.file().to_u32()) % 2 == 1
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
    pub const fn is_dark(self) -> bool {
        (self.rank().to_u32() + self.file().to_u32()) % 2 == 0
    }

    /// The distance between the two squares, i.e. the number of king steps
    /// to get from one square to the other.
    ///
    /// ```
    /// use shakmaty::Square;
    ///
    /// assert_eq!(Square::A2.distance(Square::B5), 3);
    /// ```
    pub const fn distance(self, other: Square) -> u32 {
        let file_distance = self.file().distance(other.file());
        let rank_distance = self.rank().distance(other.rank());

        if file_distance > rank_distance {
            file_distance
        } else {
            rank_distance
        }
    }

    #[must_use]
    #[inline]
    pub const fn to_u32(self) -> u32 {
        self as u32
    }

    #[must_use]
    #[inline]
    pub const fn to_usize(self) -> usize {
        self as usize
    }

    pub(crate) fn append_to<W: AppendAscii>(self, f: &mut W) -> Result<(), W::Error> {
        f.append_ascii(self.file().char())?;
        f.append_ascii(self.rank().char())
    }

    #[cfg(feature = "alloc")]
    pub fn append_to_string(&self, s: &mut alloc::string::String) {
        let _ = self.append_to(s);
    }

    #[cfg(feature = "alloc")]
    pub fn append_ascii_to(&self, buf: &mut alloc::vec::Vec<u8>) {
        let _ = self.append_to(buf);
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

from_enum_as_int_impl! { Square, u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize }
try_from_int_impl! { Square, 0, 64, u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize }

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
        self.append_to(f)
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
