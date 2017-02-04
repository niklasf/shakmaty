use std::ops;
use std::fmt;
use std::fmt::Write;

use square::Square;
use types::Color;

#[derive(PartialEq, Eq, Copy, Clone)]
pub struct Bitboard(pub u64);

impl Bitboard {
    pub fn from_square(Square(sq): Square) -> Bitboard {
        Bitboard(1 << sq)
    }

    pub fn from_all<I>(squares: I) -> Bitboard where I: IntoIterator<Item=Square> {
        let mut result = Bitboard(0);
        for square in squares {
            result.add(square);
        }
        result
    }

    pub fn all() -> Bitboard {
        Bitboard(!0u64)
    }

    pub fn rank(rank: i8) -> Bitboard {
        if 0 <= rank && rank < 8 {
            Bitboard(0xff << (8 * rank))
        } else {
            Bitboard(0)
        }
    }

    pub fn file(file: i8) -> Bitboard {
        if 0 <= file && file < 8 {
            Bitboard(0x101010101010101 << file)
        } else {
            Bitboard(0)
        }
    }

    pub fn relative_rank(color: Color, rank: i8) -> Bitboard {
        match color {
            Color::White => Bitboard(0xff << (8 * rank)),
            Color::Black => Bitboard(0xff00000000000000 >> (8 * rank))
        }
    }

    pub fn relative_shift(self, color: Color, shift: u8) -> Bitboard {
        match color {
            Color::White => Bitboard(self.0 << shift),
            Color::Black => Bitboard(self.0 >> shift)
        }
    }

    pub fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub fn contains(self, sq: Square) -> bool {
        !(self & Bitboard::from_square(sq)).is_empty()
    }

    pub fn add(&mut self, Square(sq): Square) {
        self.0 |= 1 << sq;
    }

    pub fn flip(&mut self, Square(sq): Square) {
        self.0 ^= 1 << sq;
    }

    pub fn remove(&mut self, Square(sq): Square) {
        self.0 &= !(1 << sq);
    }

    pub fn with(self, sq: Square) -> Bitboard {
        self | Bitboard::from_square(sq)
    }

    pub fn without(self, sq: Square) -> Bitboard {
        self & !Bitboard::from_square(sq)
    }

    pub fn first(self) -> Option<Square> {
        if self.is_empty() {
            None
        } else {
            Some(Square(self.0.trailing_zeros() as i8))
        }
    }

    pub fn more_than_one(self) -> bool {
        self.0 & self.0.wrapping_sub(1) != 0
    }

    pub fn single_square(self) -> Option<Square> {
        if self.more_than_one() {
            None
        } else {
            self.first()
        }
    }

    #[cfg(target_feature="bmi2")]
    pub fn pext(self, Bitboard(mask): Bitboard) -> u64 {
        let Bitboard(src) = self;
        let result: u64;
        unsafe {
            asm!("pextq $2, $0, $0"
                 : "=r"(result)
                 : "0"(src), "r"(mask));
        }
        result
    }

    #[cfg(not(target_feature="bmi2"))]
    pub fn pext(self, Bitboard(mut mask): Bitboard) -> u64 {
        let Bitboard(src) = self;
        let mut result = 0;
        let mut bit = 1;

        while mask != 0 {
            if src & mask & 0u64.wrapping_sub(mask) != 0 {
                result |= bit;
            }

            mask &= mask.wrapping_sub(1);
            bit <<= 1;
        }

        result
    }

    pub fn subsets(self) -> CarryRippler {
        CarryRippler::new(self)
    }
}

impl fmt::Debug for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for rank in (0..8).rev() {
            for file in 0..8 {
                let sq = Square::new(file, rank);
                try!(f.write_char(if self.contains(sq) { '1' } else { '.' }));
                try!(f.write_char(if file < 7 { ' ' } else { '\n' }));
            }
        }

        Ok(())
    }
}

impl ops::BitAnd<Bitboard> for Bitboard {
    type Output = Bitboard;

    fn bitand(self, Bitboard(rhs): Bitboard) -> Bitboard {
        let Bitboard(lhs) = self;
        Bitboard(lhs & rhs)
    }
}

impl ops::BitOr<Bitboard> for Bitboard {
    type Output = Bitboard;

    fn bitor(self, Bitboard(rhs): Bitboard) -> Bitboard {
        let Bitboard(lhs) = self;
        Bitboard(lhs | rhs)
    }
}

impl ops::Not for Bitboard {
    type Output = Bitboard;

    fn not(self) -> Bitboard {
        Bitboard(!self.0)
    }
}

impl Iterator for Bitboard {
    type Item = Square;

    fn next(&mut self) -> Option<Square> {
        let square = self.first();
        self.0 &= self.0.wrapping_sub(1);
        square
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(64))
    }

    fn count(self) -> usize {
        self.0.count_ones() as usize
    }

    fn last(self) -> Option<Square> {
        if self.is_empty() {
            None
        } else {
            Some(Square(63 ^ self.0.leading_zeros() as i8))
        }
    }
}

impl DoubleEndedIterator for Bitboard {
    fn next_back(&mut self) -> Option<Square> {
        if self.is_empty() {
            None
        } else {
            let sq = Square(63 ^ self.0.leading_zeros() as i8);
            self.0 ^= 1 << sq.0;
            Some(sq)
        }
    }
}

pub struct CarryRippler {
    bb: u64,
    subset: u64,
    first: bool,
}

impl CarryRippler {
    fn new(Bitboard(bb): Bitboard) -> CarryRippler {
        CarryRippler { bb: bb, subset: 0, first: true }
    }
}

impl Iterator for CarryRippler {
    type Item = Bitboard;

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
    fn test_first() {
        assert_eq!(Bitboard::from_square(square::A1).first(), Some(square::A1));
        assert_eq!(Bitboard::from_square(square::D2).first(), Some(square::D2));
        assert_eq!(Bitboard(0).first(), None);
    }

    #[test]
    fn test_last() {
        assert_eq!(Bitboard::from_square(square::A1).last(), Some(square::A1));
        assert_eq!(Bitboard(0).with(square::A1).with(square::H1).last(), Some(square::H1));
        assert_eq!(Bitboard(0).last(), None);
    }

    #[test]
    fn test_pext() {
        assert_eq!(Bitboard::all().pext(Bitboard(0)), 0);
        assert_eq!(Bitboard::all().pext(Bitboard::all()), !0u64);
        assert_eq!(Bitboard(7).pext(Bitboard(1)), 1);
    }

    #[test]
    fn test_rank() {
        assert_eq!(Bitboard::rank(3), Bitboard(0xff000000));
    }
}
