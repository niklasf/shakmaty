use square;
use square::Square;
use bitboard::Bitboard;
use board::Color;
use board::Piece;

pub const ROOK_DELTAS: [i8; 4] = [8, 1, -8, -1];
pub const BISHOP_DELTAS: [i8; 4] = [9, 7, -9, -7];
pub const KING_DELTAS: [i8; 8] = [9, 8, 7, 1, -9, -8, -7, -1];
pub const KNIGHT_DELTAS: [i8; 8] = [17, 15, 10, 6, -17, -15, -10, -6];
pub const WHITE_PAWN_DELTAS: [i8; 2] = [7, 9];
pub const BLACK_PAWN_DELTAS: [i8; 2] = [-7, -9];

pub fn sliding_attacks(sq: Square, occupied: Bitboard, deltas: &[i8]) -> Bitboard {
    let mut attack = Bitboard(0);

    for delta in deltas {
        let Square(mut s) = sq;

        loop {
            s += *delta;
            if s < 0 || s >= 64 || square::distance(Square(s), Square(s - delta)) > 2 {
                break;
            }

            attack.add(Square(s));

            if occupied.contains(Square(s)) {
                break;
            }
        }
    }

    attack
}

pub fn step_attacks(sq: Square, deltas: &[i8]) -> Bitboard {
    sliding_attacks(sq, Bitboard::all(), deltas)
}

fn magic_index(indexes: &[usize], masks: &[Bitboard], Square(sq): Square, occupied: Bitboard) -> usize {
    indexes[sq as usize] + occupied.pext(masks[sq as usize]) as usize
}

fn init_magics(indexes: &mut[usize], masks: &mut[Bitboard], attacks: &mut Vec<Bitboard>, deltas: &[i8]) {
    for s in 0..64 {
        let sq = Square(s);

        let edges = ((Bitboard::rank(0) | Bitboard::rank(7)) & !Bitboard::rank(sq.rank())) |
                    ((Bitboard::file(0) | Bitboard::file(7)) & !Bitboard::file(sq.file()));

        let mask = sliding_attacks(sq, Bitboard(0), deltas) & !edges;
        masks[s as usize] = mask;

        let mut size = 0;
        for subset in mask.subsets() {
            let index = magic_index(indexes, masks, sq, subset);
            attacks[index] = sliding_attacks(sq, subset, deltas);
            size += 1;
        }

        if s < 63 {
            indexes[s as usize + 1] = indexes[s as usize] + size;
        }
    }
}

pub struct Precomp {
    knight_attacks: [Bitboard; 64],
    king_attacks: [Bitboard; 64],
    white_pawn_attacks: [Bitboard; 64],
    black_pawn_attacks: [Bitboard; 64],

    rook_indexes: [usize; 64],
    rook_masks: [Bitboard; 64],
    rook_attacks: Vec<Bitboard>,

    bishop_indexes: [usize; 64],
    bishop_masks: [Bitboard; 64],
    bishop_attacks: Vec<Bitboard>,
}

impl Precomp {
    pub fn new() -> Precomp {
        let mut precomp = Precomp {
            knight_attacks: [Bitboard(0); 64],
            king_attacks: [Bitboard(0); 64],
            white_pawn_attacks: [Bitboard(0); 64],
            black_pawn_attacks: [Bitboard(0); 64],

            rook_indexes: [0; 64],
            rook_masks: [Bitboard(0); 64],
            rook_attacks: vec![Bitboard(0); 0x19000],

            bishop_indexes: [0; 64],
            bishop_masks: [Bitboard(0); 64],
            bishop_attacks: vec![Bitboard(0); 0x1480],
        };

        for s in 0..64 {
            precomp.knight_attacks[s] = step_attacks(Square(s as i8), &KNIGHT_DELTAS);
            precomp.king_attacks[s] = step_attacks(Square(s as i8), &KING_DELTAS);
            precomp.white_pawn_attacks[s] = step_attacks(Square(s as i8), &WHITE_PAWN_DELTAS);
            precomp.black_pawn_attacks[s] = step_attacks(Square(s as i8), &BLACK_PAWN_DELTAS);
        }

        init_magics(&mut precomp.rook_indexes,
                    &mut precomp.rook_masks,
                    &mut precomp.rook_attacks,
                    &ROOK_DELTAS);

        init_magics(&mut precomp.bishop_indexes,
                    &mut precomp.bishop_masks,
                    &mut precomp.bishop_attacks,
                    &BISHOP_DELTAS);

        precomp
    }

    pub fn pawn_attacks(&self, color: Color, Square(sq): Square) -> Bitboard {
        let table = match color {
            Color::White => self.white_pawn_attacks,
            Color::Black => self.black_pawn_attacks,
        };

        table[sq as usize]
    }

    pub fn knight_attacks(&self, Square(sq): Square) -> Bitboard {
        self.knight_attacks[sq as usize]
    }

    pub fn king_attacks(&self, Square(sq): Square) -> Bitboard {
        self.king_attacks[sq as usize]
    }

    pub fn rook_attacks(&self, sq: Square, occupied: Bitboard) -> Bitboard {
        self.rook_attacks[magic_index(&self.rook_indexes, &self.rook_masks, sq, occupied)]
    }

    pub fn bishop_attacks(&self, sq: Square, occupied: Bitboard) -> Bitboard {
        self.bishop_attacks[magic_index(&self.bishop_indexes, &self.bishop_masks, sq, occupied)]
    }

    pub fn queen_attacks(&self, sq: Square, occupied: Bitboard) -> Bitboard {
        self.rook_attacks(sq, occupied) | self.bishop_attacks(sq, occupied)
    }

    pub fn attacks(&self, sq: Square, Piece { color, role }: Piece, occupied: Bitboard) -> Bitboard {
        match role {
            Pawn   => self.pawn_attacks(color, sq),
            Knight => self.knight_attacks(sq),
            Bishop => self.bishop_attacks(sq, occupied),
            Rook   => self.rook_attacks(sq, occupied),
            Queen  => self.queen_attacks(sq, occupied),
            King   => self.king_attacks(sq)
        }
    }
}

mod test {
    use attacks::ROOK_DELTAS;
    use attacks::sliding_attacks;
    use attacks::Precomp;
    use bitboard::Bitboard;
    use square;

    #[test]
    fn test_sliding_rook_attacks() {
        let attack = sliding_attacks(square::D6, Bitboard(0x3f7f28802826f5b9), &ROOK_DELTAS);
        assert_eq!(attack, Bitboard(0x8370808000000));
    }

    #[test]
    fn test_rook_attacks() {
        let table = Precomp::new();
        assert_eq!(table.rook_attacks(square::D6, Bitboard(0x3f7f28802826f5b9)), Bitboard(0x8370808000000));
    }
}
