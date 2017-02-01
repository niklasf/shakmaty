use std::cmp::max;
use std::fmt;

#[derive(PartialOrd, Eq, PartialEq, Copy, Clone)]
pub struct Square(pub i8);

impl Square {
    pub fn new(file: i8, rank: i8) -> Square {
        debug_assert!(0 <= file && file < 8);
        debug_assert!(0 <= rank && rank < 8);
        Square(file | (rank << 3))
    }

    pub fn from_coords(file: i8, rank: i8) -> Option<Square> {
        if 0 <= file && file < 8 && 0 <= rank && rank < 8 {
            Some(Square::new(file, rank))
        } else {
            None
        }
    }

    pub fn from_str(s: &str) -> Option<Square> {
        if s.len() != 2 {
            return None
        }

        let file = s.chars().nth(0).map(|file| file as i8 - 'a' as i8);
        let rank = s.chars().nth(1).map(|rank| rank as i8 - '1' as i8);
        match (file, rank) {
            (Some(file), Some(rank)) => Square::from_coords(file, rank),
            _ => None
        }
    }

    pub fn file(self) -> i8 {
        let Square(sq) = self;
        sq & 7
    }

    pub fn rank(self) -> i8 {
        let Square(sq) = self;
        sq >> 3
    }

    pub fn offset(self, delta: i8) -> Option<Square> {
        let Square(sq) = self;
        if 0 <= sq + delta && sq + delta < 64 {
            Some(Square(sq + delta))
        } else {
            None
        }
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", ('a' as u8 + self.file() as u8) as char,
                          ('1' as u8 + self.rank() as u8) as char)
    }
}

impl fmt::Debug for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string().to_uppercase())
    }
}

pub fn delta(Square(a): Square, Square(b): Square) -> i8 {
    a - b
}

pub fn distance(a: Square, b: Square) -> i8 {
    max((a.file() - b.file()).abs(), (a.rank() - b.rank()).abs())
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
    use square;

    #[test]
    fn test_square() {
        for file in 0..8 {
            for rank in 0..8 {
                let square = Square::new(file, rank);
                assert_eq!(square.file(), file);
                assert_eq!(square.rank(), rank);
            }
        }
    }

    #[test]
    fn test_distance() {
        assert_eq!(distance(square::D2, square::G3), 3);
    }
}
