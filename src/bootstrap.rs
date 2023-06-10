// Initialize static lookup tables at compile time.

use crate::magics;

const ROOK_DELTAS: [i32; 4] = [8, 1, -8, -1];
const BISHOP_DELTAS: [i32; 4] = [9, 7, -9, -7];
const KING_DELTAS: [i32; 8] = [9, 8, 7, 1, -9, -8, -7, -1];
const KNIGHT_DELTAS: [i32; 8] = [17, 15, 10, 6, -17, -15, -10, -6];
const WHITE_PAWN_DELTAS: [i32; 2] = [7, 9];
const BLACK_PAWN_DELTAS: [i32; 2] = [-7, -9];

const fn sliding_attacks(square: i32, occupied: u64, deltas: &[i32]) -> u64 {
    let mut attack = 0;

    let mut i = 0;
    let len = deltas.len();
    while i < len {
        let mut previous = square;
        loop {
            let sq = previous + deltas[i];
            let file_diff = (sq & 0x7) - (previous & 0x7);
            if file_diff > 2 || file_diff < -2 || sq < 0 || sq > 63 {
                break;
            }
            let bb = 1 << sq;
            attack |= bb;
            if occupied & bb != 0 {
                break;
            }
            previous = sq;
        }
        i += 1;
    }

    attack
}

const fn init_stepping_attacks(deltas: &[i32]) -> [u64; 64] {
    let mut table = [0; 64];
    let mut sq = 0;
    while sq < 64 {
        table[sq] = sliding_attacks(sq as i32, !0, deltas);
        sq += 1;
    }
    table
}

pub static KNIGHT_ATTACKS: [u64; 64] = init_stepping_attacks(&KNIGHT_DELTAS);
pub static KING_ATTACKS: [u64; 64] = init_stepping_attacks(&KING_DELTAS);
pub static WHITE_PAWN_ATTACKS: [u64; 64] = init_stepping_attacks(&WHITE_PAWN_DELTAS);
pub static BLACK_PAWN_ATTACKS: [u64; 64] = init_stepping_attacks(&BLACK_PAWN_DELTAS);

const fn init_rays() -> [[u64; 64]; 64] {
    let mut table = [[0; 64]; 64];
    let mut a = 0;
    while a < 64 {
        let mut b = 0;
        while b < 64 {
            table[a as usize][b as usize] = if a == b {
                0
            } else if a & 7 == b & 7 {
                0x0101_0101_0101_0101 << (a & 7)
            } else if a >> 3 == b >> 3 {
                0xff << (8 * (a >> 3))
            } else {
                let diag = (a >> 3) - (a & 7);
                let anti_diag = (a >> 3) + (a & 7) - 7;
                if diag == (b >> 3) - (b & 7) {
                    if diag >= 0 {
                        0x8040_2010_0804_0201 << (8 * diag)
                    } else {
                        0x8040_2010_0804_0201 >> (8 * -diag)
                    }
                } else if anti_diag == (b >> 3) + (b & 7) - 7 {
                    if anti_diag >= 0 {
                        0x0102_0408_1020_4080 << (8 * anti_diag)
                    } else {
                        0x0102_0408_1020_4080 >> (8 * -anti_diag)
                    }
                } else {
                    0
                }
            };
            b += 1;
        }
        a += 1;
    }
    table
}

pub static RAYS: [[u64; 64]; 64] = init_rays();

const fn init_magics() -> [u64; 88772] {
    let mut table = [0; 88772];
    let mut square = 0;
    while square < 64 {
        let magic = &magics::BISHOP_MAGICS[square as usize];
        let range = magic.mask;
        let mut subset = 0;
        loop {
            let attack = sliding_attacks(square, subset, &BISHOP_DELTAS);
            let idx = (magic.factor.wrapping_mul(subset) >> (64 - 9)) as usize + magic.offset;
            assert!(table[idx] == 0 || table[idx] == attack);
            table[idx] = attack;
            subset = subset.wrapping_sub(range) & range;
            if subset == 0 {
                break;
            }
        }

        let magic = &magics::ROOK_MAGICS[square as usize];
        let range = magic.mask;
        let mut subset = 0;
        loop {
            let attack = sliding_attacks(square, subset, &ROOK_DELTAS);
            let idx = (magic.factor.wrapping_mul(subset) >> (64 - 12)) as usize + magic.offset;
            assert!(table[idx] == 0 || table[idx] == attack);
            table[idx] = attack;
            subset = subset.wrapping_sub(range) & range;
            if subset == 0 {
                break;
            }
        }

        square += 1;
    }
    table
}

pub static ATTACKS: [u64; 88772] = init_magics();
