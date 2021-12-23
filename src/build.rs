// Copyright (C) 2017-2021 Niklas Fiekas <niklas.fiekas@backscattering.de>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.

#![cfg_attr(feature = "step", feature(step_trait))]
#![forbid(unsafe_op_in_unsafe_fn)]
#![allow(dead_code)]

use std::{env, fmt::LowerHex, fs::File, io, io::Write, path::Path};

mod bitboard;
mod color;
mod magics;
mod square;
mod types;
mod util;

use crate::{bitboard::Bitboard, magics::Magic, square::Square};

const ROOK_DELTAS: [i32; 4] = [8, 1, -8, -1];
const BISHOP_DELTAS: [i32; 4] = [9, 7, -9, -7];
const KING_DELTAS: [i32; 8] = [9, 8, 7, 1, -9, -8, -7, -1];
const KNIGHT_DELTAS: [i32; 8] = [17, 15, 10, 6, -17, -15, -10, -6];
const WHITE_PAWN_DELTAS: [i32; 2] = [7, 9];
const BLACK_PAWN_DELTAS: [i32; 2] = [-7, -9];

fn sliding_attacks(sq: Square, occupied: Bitboard, deltas: &[i32]) -> Bitboard {
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

fn step_attacks(sq: Square, deltas: &[i32]) -> Bitboard {
    sliding_attacks(sq, Bitboard::FULL, deltas)
}

fn init_magics(sq: Square, magic: &Magic, shift: u32, attacks: &mut [Bitboard], deltas: &[i32]) {
    for subset in Bitboard(magic.mask).carry_rippler() {
        let attack = sliding_attacks(sq, subset, deltas);
        let idx = (magic.factor.wrapping_mul(subset.0) >> (64 - shift)) as usize + magic.offset;
        assert!(attacks[idx].is_empty() || attacks[idx] == attack);
        attacks[idx] = attack;
    }
}

fn dump_slice<W: Write, T: LowerHex>(
    w: &mut W,
    name: &str,
    tname: &str,
    slice: &[T],
) -> io::Result<()> {
    writeln!(w, "#[allow(clippy::unreadable_literal)]")?;
    write!(w, "static {}: [{}; {}] = [", name, tname, slice.len())?;
    for v in slice {
        write!(w, "0x{:x}, ", v)?;
    }
    writeln!(w, "];")
}

fn dump_table<W: Write, T: LowerHex>(
    w: &mut W,
    name: &str,
    tname: &str,
    table: &[[T; 64]; 64],
) -> io::Result<()> {
    writeln!(w, "#[allow(clippy::unreadable_literal)]")?;
    write!(w, "static {}: [[{}; 64]; 64] = [", name, tname)?;
    for row in table {
        write!(w, "[")?;
        for column in row {
            write!(w, "0x{:x}, ", column)?;
        }
        write!(w, "], ")?;
    }
    writeln!(w, "];")
}

fn main() -> io::Result<()> {
    // generate attacks.rs
    let out_dir = env::var("OUT_DIR").expect("got OUT_DIR");
    let attacks_path = Path::new(&out_dir).join("attacks.rs");
    let mut f = File::create(&attacks_path).expect("created attacks.rs");
    generate_basics(&mut f)?;
    generate_sliding_attacks(&mut f)
}

fn generate_basics<W: Write>(f: &mut W) -> io::Result<()> {
    let mut knight_attacks = [Bitboard(0); 64];
    let mut king_attacks = [Bitboard(0); 64];
    let mut white_pawn_attacks = [Bitboard(0); 64];
    let mut black_pawn_attacks = [Bitboard(0); 64];

    let mut bb_rays = [[Bitboard(0); 64]; 64];

    for sq in Square::ALL {
        knight_attacks[usize::from(sq)] = step_attacks(sq, &KNIGHT_DELTAS);
        king_attacks[usize::from(sq)] = step_attacks(sq, &KING_DELTAS);
        white_pawn_attacks[usize::from(sq)] = step_attacks(sq, &WHITE_PAWN_DELTAS);
        black_pawn_attacks[usize::from(sq)] = step_attacks(sq, &BLACK_PAWN_DELTAS);
    }

    for a in Square::ALL {
        for b in sliding_bishop_attacks(a, Bitboard(0)) {
            bb_rays[usize::from(a)][usize::from(b)] = (sliding_bishop_attacks(a, Bitboard(0))
                & sliding_bishop_attacks(b, Bitboard(0)))
            .with(a)
            .with(b);
        }
        for b in sliding_rook_attacks(a, Bitboard(0)) {
            bb_rays[usize::from(a)][usize::from(b)] = (sliding_rook_attacks(a, Bitboard(0))
                & sliding_rook_attacks(b, Bitboard(0)))
            .with(a)
            .with(b);
        }
    }

    dump_slice(f, "KNIGHT_ATTACKS", "u64", &knight_attacks)?;
    dump_slice(f, "KING_ATTACKS", "u64", &king_attacks)?;
    dump_slice(f, "WHITE_PAWN_ATTACKS", "u64", &white_pawn_attacks)?;
    dump_slice(f, "BLACK_PAWN_ATTACKS", "u64", &black_pawn_attacks)?;

    writeln!(f)?;

    dump_table(f, "BB_RAYS", "u64", &bb_rays)?;

    writeln!(f)
}

fn generate_sliding_attacks<W: Write>(f: &mut W) -> io::Result<()> {
    let mut attacks = vec![Bitboard(0); 88772];

    for sq in Square::ALL {
        init_magics(
            sq,
            &magics::ROOK_MAGICS[usize::from(sq)],
            12,
            &mut attacks,
            &ROOK_DELTAS,
        );
        init_magics(
            sq,
            &magics::BISHOP_MAGICS[usize::from(sq)],
            9,
            &mut attacks,
            &BISHOP_DELTAS,
        );
    }

    dump_slice(f, "ATTACKS", "u64", &attacks)
}
