use std::cmp::max;
use std::fmt;
use std::ops;

#[derive(PartialOrd, Eq, PartialEq, Copy, Clone)]
pub struct Square(pub i8);

impl Square {
    pub fn new(file: i8, rank: i8) -> Square {
        debug_assert!(0 <= file && file < 8);
        debug_assert!(0 <= rank && rank < 8);
        Square(file | (rank << 3))
    }

    pub fn file(self) -> i8 {
        let Square(sq) = self;
        sq & 7
    }

    pub fn rank(self) -> i8 {
        let Square(sq) = self;
        sq >> 3
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", ('a' as u8 + self.file() as u8) as char, ('1' as u8 + self.rank() as u8) as char)
    }
}

impl fmt::Debug for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "square::{}", self.to_string().to_uppercase())
    }
}

pub fn distance(a: Square, b: Square) -> i8 {
    max((a.file() - b.file()).abs(), (a.rank() - b.rank()).abs())
}

pub const A1: Square = Square(0);
pub const D2: Square = Square(11);
pub const G3: Square = Square(22);
pub const D6: Square = Square(43);

mod test {
    use square;
    use square::Square;
    use square::distance;

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
