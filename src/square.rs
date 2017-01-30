use std::cmp::max;

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

pub fn distance(a: Square, b: Square) -> i8 {
    max((a.file() - b.file()).abs(), (a.rank() - b.rank()).abs())
}

mod test {
    use square::Square;

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
}
