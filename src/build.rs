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
mod magics;

use square::Square;
use bitboard::Bitboard;
use magics::Magic;

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
            if s.distance(previous) > 2 {
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

fn init_magics(sq: Square, magic: &Magic, shift: u8, attacks: &mut [Bitboard], deltas: &[i8]) {
    for subset in Bitboard(magic.mask).carry_rippler() {
        let attack = sliding_attacks(sq, subset, deltas);
        let idx = (magic.factor.wrapping_mul(subset.0) >> (64 - shift)) as usize + magic.offset;
        assert!(attacks[idx].is_empty() || attacks[idx] == attack);
        attacks[idx] = attack;
    }
}

fn dump_slice<W: Write, T: LowerHex>(w: &mut W, name: &str, tname: &str, slice: &[T]) -> io::Result<()> {
    write!(w, "#[cfg_attr(feature = \"cargo-clippy\", allow(unreadable_literal))]")?;
    write!(w, "static {}: [{}; {}] = [", name, tname, slice.len())?;
    for v in slice {
        write!(w, "0x{:x}, ", v)?;
    }
    write!(w, "];\n")
}

fn main() {
    let out_dir = env::var("OUT_DIR").expect("got OUT_DIR");
    let dest_path = Path::new(&out_dir).join("attacks.rs");
    let mut f = File::create(&dest_path).expect("created attacks.rs");

    generate_basics(&mut f).unwrap();
    generate_sliding_attacks(&mut f).unwrap();
}

fn generate_basics<W: Write>(f: &mut W) -> io::Result<()> {
    let mut knight_attacks = [Bitboard(0); 64];
    let mut king_attacks = [Bitboard(0); 64];
    let mut white_pawn_attacks = [Bitboard(0); 64];
    let mut black_pawn_attacks = [Bitboard(0); 64];

    let mut bb_rays = [Bitboard(0); 4096];
    let mut bb_between = [Bitboard(0); 4096];

    for s in 0..64 {
        let sq = Square::from_index(s as i8).expect("square index s in range");
        knight_attacks[s] = step_attacks(sq, &KNIGHT_DELTAS);
        king_attacks[s] = step_attacks(sq, &KING_DELTAS);
        white_pawn_attacks[s] = step_attacks(sq, &WHITE_PAWN_DELTAS);
        black_pawn_attacks[s] = step_attacks(sq, &BLACK_PAWN_DELTAS);
    }

    for a in 0..64 {
        let sa = Square::from_index(a as i8).expect("square index a in range");

        for b in 0..64 {
            let sb = Square::from_index(b as i8).expect("square index b in range");
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

    dump_slice(f, "KNIGHT_ATTACKS", "u64", &knight_attacks)?;
    dump_slice(f, "KING_ATTACKS", "u64", &king_attacks)?;
    dump_slice(f, "WHITE_PAWN_ATTACKS", "u64", &white_pawn_attacks)?;
    dump_slice(f, "BLACK_PAWN_ATTACKS", "u64", &black_pawn_attacks)?;

    write!(f, "\n")?;

    dump_slice(f, "BB_RAYS", "u64", &bb_rays)?;
    dump_slice(f, "BB_BETWEEN", "u64", &bb_between)?;

    write!(f, "\n")?;

    Ok(())
}

fn generate_sliding_attacks<W: Write>(f: &mut W) -> io::Result<()> {
    let mut attacks = [Bitboard(0); 88772];

    for s in 0..64 {
        let sq = Square::from_index(s as i8).expect("square index s in range");
        init_magics(sq, &magics::ROOK_MAGICS[s], 12, &mut attacks, &ROOK_DELTAS);
        init_magics(sq, &magics::BISHOP_MAGICS[s], 9, &mut attacks, &BISHOP_DELTAS);
    }

    dump_slice(f, "ATTACKS", "u64", &attacks)?;

    Ok(())
}
