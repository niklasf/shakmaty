#![allow(dead_code)]

extern crate version_check;

use std::env;
use std::fs::File;
use std::io;
use std::io::Write;
use std::path::Path;

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
    sliding_attacks(sq, Bitboard::ALL, deltas)
}

fn init_magics(sq: Square, magic: &Magic, shift: u32, attacks: &mut [Bitboard], deltas: &[i8]) {
    for subset in Bitboard(magic.mask).carry_rippler() {
        let attack = sliding_attacks(sq, subset, deltas);
        let idx = (magic.factor.wrapping_mul(subset.0) >> (64 - shift)) as usize + magic.offset;
        assert!(attacks[idx].is_empty() || attacks[idx] == attack);
        attacks[idx] = attack;
    }
}

fn write_u64<W: Write>(w: &mut W, x: u64) -> io::Result<()> {
    write!(w, "0x{:04x}_{:04x}_{:04x}_{:04x}", x >> 48, (x >> 32) & 0xffff, (x >> 16) & 0xffff, x & 0xffff)
}

fn dump_slice<W: Write, T: Clone + Into<u64>>(w: &mut W, name: &str, tname: &str, slice: &[T]) -> io::Result<()> {
    write!(w, "static {}: [{}; {}] = [", name, tname, slice.len())?;
    for v in slice.iter().cloned() {
        write_u64(w, v.into())?;
        write!(w, ", ")?;
    }
    write!(w, "];\n")
}

fn dump_table<W: Write, T: Clone + Into<u64>>(w: &mut W, name: &str, tname: &str, table: &[[T; 64]; 64]) -> io::Result<()> {
    write!(w, "static {}: [[{}; 64]; 64] = [", name, tname)?;
    for row in table.iter() {
        write!(w, "[")?;
        for column in row.iter().cloned() {
            write_u64(w, column.into())?;
            write!(w, ", ")?;
        }
        write!(w, "], ")?;
    }
    write!(w, "];\n")
}

fn main() -> io::Result<()> {
    // detect support for nightly features
    if let Some(true) = version_check::supports_features() {
        println!("cargo:rustc-cfg=nightly");
    }

    // generate attacks.rs
    let out_dir = env::var("OUT_DIR").expect("got OUT_DIR");
    let dest_path = Path::new(&out_dir).join("attacks.rs");
    let mut f = File::create(&dest_path).expect("created attacks.rs");
    generate_basics(&mut f)?;
    generate_sliding_attacks(&mut f)
}

fn generate_basics<W: Write>(f: &mut W) -> io::Result<()> {
    let mut knight_attacks = [Bitboard(0); 64];
    let mut king_attacks = [Bitboard(0); 64];
    let mut white_pawn_attacks = [Bitboard(0); 64];
    let mut black_pawn_attacks = [Bitboard(0); 64];

    let mut bb_rays = [[Bitboard(0); 64]; 64];
    let mut bb_between = [[Bitboard(0); 64]; 64];

    for sq in Bitboard::ALL {
        knight_attacks[usize::from(sq)] = step_attacks(sq, &KNIGHT_DELTAS);
        king_attacks[usize::from(sq)] = step_attacks(sq, &KING_DELTAS);
        white_pawn_attacks[usize::from(sq)] = step_attacks(sq, &WHITE_PAWN_DELTAS);
        black_pawn_attacks[usize::from(sq)] = step_attacks(sq, &BLACK_PAWN_DELTAS);
    }

    for a in Bitboard::ALL {
        for b in Bitboard::ALL {
            if sliding_bishop_attacks(a, Bitboard(0)).contains(b) {
                bb_rays[usize::from(a)][usize::from(b)] =
                    (sliding_bishop_attacks(a, Bitboard(0)) &
                     sliding_bishop_attacks(b, Bitboard(0))).with(a).with(b);
                bb_between[usize::from(a)][usize::from(b)] =
                    sliding_bishop_attacks(a, Bitboard::from_square(b)) &
                    sliding_bishop_attacks(b, Bitboard::from_square(a));
            } else if sliding_rook_attacks(a, Bitboard(0)).contains(b) {
                bb_rays[usize::from(a)][usize::from(b)] =
                    (sliding_rook_attacks(a, Bitboard(0)) &
                     sliding_rook_attacks(b, Bitboard(0))).with(a).with(b);
                bb_between[usize::from(a)][usize::from(b)] =
                    sliding_rook_attacks(a, Bitboard::from_square(b)) &
                    sliding_rook_attacks(b, Bitboard::from_square(a));
            }
        }
    }

    dump_slice(f, "KNIGHT_ATTACKS", "u64", &knight_attacks)?;
    dump_slice(f, "KING_ATTACKS", "u64", &king_attacks)?;
    dump_slice(f, "WHITE_PAWN_ATTACKS", "u64", &white_pawn_attacks)?;
    dump_slice(f, "BLACK_PAWN_ATTACKS", "u64", &black_pawn_attacks)?;

    write!(f, "\n")?;

    dump_table(f, "BB_RAYS", "u64", &bb_rays)?;
    dump_table(f, "BB_BETWEEN", "u64", &bb_between)?;

    write!(f, "\n")?;

    Ok(())
}

fn generate_sliding_attacks<W: Write>(f: &mut W) -> io::Result<()> {
    let mut attacks = [Bitboard(0); 88772];

    for sq in Bitboard::ALL {
        init_magics(sq, &magics::ROOK_MAGICS[usize::from(sq)], 12, &mut attacks, &ROOK_DELTAS);
        init_magics(sq, &magics::BISHOP_MAGICS[usize::from(sq)], 9, &mut attacks, &BISHOP_DELTAS);
    }

    dump_slice(f, "ATTACKS", "u64", &attacks)?;

    Ok(())
}
