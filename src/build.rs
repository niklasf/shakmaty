#![feature(cfg_target_feature)]
#![feature(platform_intrinsics)]
#![feature(const_fn)]

#![allow(dead_code)]

use std::env;
use std::fs::File;
use std::io;
use std::io::Write;
use std::path::Path;
use std::fmt::LowerHex;

mod types;
mod square;
mod bitboard;

use square::Square;
use bitboard::Bitboard;

const ROOK_DELTAS: [i8; 4] = [8, 1, -8, -1];
const BISHOP_DELTAS: [i8; 4] = [9, 7, -9, -7];
const KING_DELTAS: [i8; 8] = [9, 8, 7, 1, -9, -8, -7, -1];
const KNIGHT_DELTAS: [i8; 8] = [17, 15, 10, 6, -17, -15, -10, -6];
const WHITE_PAWN_DELTAS: [i8; 2] = [7, 9];
const BLACK_PAWN_DELTAS: [i8; 2] = [-7, -9];

fn sliding_attacks(sq: Square, occupied: Bitboard, deltas: &[i8]) -> Bitboard {
    let mut attack = Bitboard(0);

    for delta in deltas {
        let mut previous = sq;

        while let Some(s) = previous.offset(*delta) {
            if square::distance(s, previous) > 2 {
                break;
            }

            attack.add(s);

            if occupied.contains(s) {
                break;
            }

            previous = s;
        }
    }

    attack
}

fn sliding_bishop_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    sliding_attacks(sq, occupied, &BISHOP_DELTAS)
}

fn sliding_rook_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    sliding_attacks(sq, occupied, &ROOK_DELTAS)
}

fn step_attacks(sq: Square, deltas: &[i8]) -> Bitboard {
    sliding_attacks(sq, Bitboard::all(), deltas)
}

fn init_magics(indexes: &mut[usize], masks: &mut[Bitboard], ranges: &mut[Bitboard], attacks: &mut[u16], deltas: &[i8]) -> usize {
    for s in 0..64 {
        let sq = Square::from_index_unchecked(s);

        let range = sliding_attacks(sq, Bitboard(0), deltas);
        ranges[s as usize] = range;

        let edges = ((Bitboard::rank(0) | Bitboard::rank(7)) & !Bitboard::rank(sq.rank())) |
                    ((Bitboard::file(0) | Bitboard::file(7)) & !Bitboard::file(sq.file()));

        let mask = range & !edges;
        masks[s as usize] = mask;

        for subset in mask.carry_rippler() {
            let index = indexes[s as usize] + subset.extract(mask) as usize;
            attacks[index] = sliding_attacks(sq, subset, deltas).extract(range) as u16;
        }

        if s < 63 {
            indexes[s as usize + 1] = indexes[s as usize] + (1 << mask.count());
        }
    }

    indexes[63] + (1 << masks[63].count())
}

fn dump_slice<W: Write, T: LowerHex>(w: &mut W, name: &str, tname: &str, slice: &[T]) -> io::Result<()> {
    write!(w, "const {}: [{}; {}] = [", name, tname, slice.len())?;
    for v in slice {
        write!(w, "0x{:x}, ", v)?;
    }
    write!(w, "];\n")
}

fn main() {
    generate().unwrap()
}

fn generate() -> io::Result<()> {
    let out_dir = env::var("OUT_DIR").expect("got OUT_DIR");
    let dest_path = Path::new(&out_dir).join("attacks.rs");
    let mut f = File::create(&dest_path).expect("created attacks.rs");

    let mut knight_attacks = [Bitboard(0); 64];
    let mut king_attacks = [Bitboard(0); 64];
    let mut white_pawn_attacks = [Bitboard(0); 64];
    let mut black_pawn_attacks = [Bitboard(0); 64];

    let mut rook_indexes = [0usize; 64];
    let mut rook_masks = [Bitboard(0); 64];
    let mut rook_ranges = [Bitboard(0); 64];
    let mut rook_attacks = [0u16; 0x19000];

    let mut bishop_indexes = [0usize; 64];
    let mut bishop_masks = [Bitboard(0); 64];
    let mut bishop_ranges = [Bitboard(0); 64];
    let mut bishop_attacks = [0u16; 0x1480];

    let mut bb_rays = [Bitboard(0); 4096];
    let mut bb_between = [Bitboard(0); 4096];

    for s in 0..64 {
        let sq = Square::from_index_unchecked(s as i8);
        knight_attacks[s] = step_attacks(sq, &KNIGHT_DELTAS);
        king_attacks[s] = step_attacks(sq, &KING_DELTAS);
        white_pawn_attacks[s] = step_attacks(sq, &WHITE_PAWN_DELTAS);
        black_pawn_attacks[s] = step_attacks(sq, &BLACK_PAWN_DELTAS);
    }

    write!(f, "// rook table: 0x{:x} entries\n",
             init_magics(&mut rook_indexes, &mut rook_masks, &mut rook_ranges,
                         &mut rook_attacks, &ROOK_DELTAS))?;

    write!(f, "// bishop table: 0x{:x} entries\n",
             init_magics(&mut bishop_indexes, &mut bishop_masks, &mut bishop_ranges,
                         &mut bishop_attacks, &BISHOP_DELTAS))?;

    write!(f, "\n")?;

    for a in 0..64 {
        let sa = Square::from_index_unchecked(a as i8);

        for b in 0..64 {
            let sb = Square::from_index_unchecked(b as i8);
            let idx = a * 64 + b;

            if sliding_bishop_attacks(sa, Bitboard(0)).contains(sb) {
                bb_rays[idx] =
                    (sliding_bishop_attacks(sa, Bitboard(0)) &
                     sliding_bishop_attacks(sb, Bitboard(0))).with(sa).with(sb);
                bb_between[idx] =
                    sliding_bishop_attacks(sa, Bitboard::from_square(sb)) &
                    sliding_bishop_attacks(sb, Bitboard::from_square(sa));
            } else if sliding_rook_attacks(sa, Bitboard(0)).contains(sb) {
                bb_rays[idx] =
                    (sliding_rook_attacks(sa, Bitboard(0)) &
                     sliding_rook_attacks(sb, Bitboard(0))).with(sa).with(sb);
                bb_between[idx] =
                    sliding_rook_attacks(sa, Bitboard::from_square(sb)) &
                    sliding_rook_attacks(sb, Bitboard::from_square(sa));
            }
        }
    }

    dump_slice(&mut f, "KNIGHT_ATTACKS", "u64", &knight_attacks)?;
    dump_slice(&mut f, "KING_ATTACKS", "u64", &king_attacks)?;
    dump_slice(&mut f, "WHITE_PAWN_ATTACKS", "u64", &white_pawn_attacks)?;
    dump_slice(&mut f, "BLACK_PAWN_ATTACKS", "u64", &black_pawn_attacks)?;

    write!(f, "\n")?;

    dump_slice(&mut f, "ROOK_INDEXES", "usize", &rook_indexes)?;
    dump_slice(&mut f, "ROOK_MASKS", "u64", &rook_masks)?;
    dump_slice(&mut f, "ROOK_RANGES", "u64", &rook_ranges)?;
    dump_slice(&mut f, "ROOK_ATTACKS", "u16", &rook_attacks)?;

    write!(f, "\n")?;

    dump_slice(&mut f, "BISHOP_INDEXES", "usize", &bishop_indexes)?;
    dump_slice(&mut f, "BISHOP_MASKS", "u64", &bishop_masks)?;
    dump_slice(&mut f, "BISHOP_RANGES", "u64", &bishop_ranges)?;
    dump_slice(&mut f, "BISHOP_ATTACKS", "u16", &bishop_attacks)?;

    write!(f, "\n")?;

    dump_slice(&mut f, "BB_RAYS", "u64", &bb_rays)?;
    dump_slice(&mut f, "BB_BETWEEN", "u64", &bb_between)?;

    Ok(())
}
