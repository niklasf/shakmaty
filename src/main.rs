#![feature(asm, cfg_target_feature)]

use std::cmp::max;

fn square_file(sq: i8) -> i8 {
    sq & 7
}

fn square_rank(sq: i8) -> i8 {
    sq >> 3
}

fn square_distance(a: i8, b: i8) -> i8 {
    max((square_file(a) - square_file(b)).abs(),
        (square_rank(a) - square_rank(b)).abs())
}

fn sliding_attack(deltas: &[i8], sq: i8, occupied: u64) -> u64 {
    let mut attack = 0;

    for delta in deltas {
        let mut s = sq;

        loop {
            s += *delta;
            if s < 0 || s >= 64 || square_distance(s, (s - delta)) > 2 {
                break;
            }

            attack |= 1 << s;

            if occupied & (1 << s) != 0 {
                break;
            }
        }
    }

    attack
}

static BB_ALL: u64 = 0xffff_ffff_ffff_ffff;

static BB_RANK: [u64; 8] = [0xff, 0xff00, 0xff0000, 0xff000000, 0xff00000000, 0xff0000000000, 0xff000000000000, 0xff00000000000000];

static BB_FILE: [u64; 8] = [0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808, 0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080];

static mut rook_masks: [u64; 64] = [0; 64];

static rook_indexes : [usize; 64] =
    [0, 4096, 6144, 8192, 10240, 12288, 14336, 16384, 20480, 22528, 23552, 24576, 25600, 26624,
    27648, 28672, 30720, 32768, 33792, 34816, 35840, 36864, 37888, 38912, 40960, 43008, 44032,
    45056, 46080, 47104, 48128, 49152, 51200, 53248, 54272, 55296, 56320, 57344, 58368, 59392,
    61440, 63488, 64512, 65536, 66560, 67584, 68608, 69632, 71680, 73728, 74752, 75776, 76800,
    77824, 78848, 79872, 81920, 86016, 88064, 90112, 92160, 94208, 96256, 98304];

static mut rook_table : [u64; 0x19000] = [0; 0x19000];

#[cfg(target_feature="bmi2")]
#[inline(always)]
fn pext(src: u64, mask: u64) -> u64 {
    let result: u64;
    unsafe {
        asm!("pextq $2, $0, $0"
             : "=r"(result)
             : "0"(src), "r"(mask));
    }
    result
}

#[cfg(not(target_feature="bmi2"))]
#[inline(always)]
fn pext(src: u64, mut mask: u64) -> u64 {
    let mut result = 0;
    let mut bin = 1;
    while mask != 0 {
        if src & (mask & (0 as u64).wrapping_sub(mask)) != 0 {
            result = result | bin;
        }

        mask = mask & mask.wrapping_sub(1);

        bin <<= 1;
    }

    result
}

fn magic_index(indexes: &[usize], masks: &[u64], sq: u8, occupied: u64) -> usize {
    indexes[sq as usize] + pext(occupied, masks[sq as usize]) as usize
}

fn init_rook_tables() {
    let rook_deltas = [8, 1, -8, -1];

    for sq in 0..64 {
        let edges = ((BB_RANK[0] | BB_RANK[7]) & !BB_RANK[square_rank(sq) as usize]) |
                    ((BB_FILE[0] | BB_FILE[7]) & !BB_FILE[square_file(sq) as usize]);

        let mask = sliding_attack(&rook_deltas, sq, 0) & !edges;
        unsafe { rook_masks[sq as usize] = mask; }

        let mut subset = 0;
        loop {
            let attacks = sliding_attack(&rook_deltas, sq, subset);
            let index = magic_index(&rook_indexes, unsafe { &rook_masks }, sq as u8, subset);

            unsafe { rook_table[index] = attacks; }

            subset = subset.wrapping_sub(mask) & mask;
            if subset == 0 {
                break;
            }
        }
    }
}

fn rook_attacks(sq: u8, occupied: u64) -> u64 {
   unsafe {
       rook_table[magic_index(&rook_indexes, &rook_masks, sq, occupied)]
   }
}

fn main() {
    init_rook_tables();
    println!("{}", rook_attacks(1, 16711680));
}

#[test]
fn test_pext() {
    assert_eq!(pext(0, 0), 0);
    assert_eq!(pext(255, 16 | 4096), 1);
}

#[test]
fn test_rook_attacks() {
    init_rook_tables();
    assert_eq!(rook_attacks(43, 4575420277326280121), 2312307447169024);
}
