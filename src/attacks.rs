//! Attack and ray tables.
//!
//! # Example
//!
//! ```
//! use shakmaty::{attacks, Rank, Square, Bitboard};
//!
//! let occupied = Bitboard::from(Rank::Sixth); // blocking pieces
//! let attacks = attacks::bishop_attacks(Square::C2, occupied);
//! // . . . . . . . .
//! // . . . . . . . .
//! // 0 0 0 0 0 0 1 0
//! // . . . . . 1 . .
//! // 1 . . . 1 . . .
//! // . 1 . 1 . . . .
//! // . . . . . . . .
//! // . 1 . 1 . . . .
//!
//! assert!(attacks.contains(Square::G6));
//! assert!(!attacks.contains(Square::H7));
//! ```

use core::hint::assert_unchecked;

use crate::{Bitboard, ByColor, Color, Piece, Role, Square};

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

const fn bootstrap_stepping_attacks(deltas: &[i32]) -> [u64; 64] {
    let mut table = [0; 64];
    let mut sq = 0;
    while sq < 64 {
        table[sq] = sliding_attacks(sq as i32, !0, deltas);
        sq += 1;
    }
    table
}

static KNIGHT_ATTACKS: [u64; 64] = bootstrap_stepping_attacks(&KNIGHT_DELTAS);
static KING_ATTACKS: [u64; 64] = bootstrap_stepping_attacks(&KING_DELTAS);
static PAWN_ATTACKS: ByColor<[u64; 64]> = ByColor {
    white: bootstrap_stepping_attacks(&WHITE_PAWN_DELTAS),
    black: bootstrap_stepping_attacks(&BLACK_PAWN_DELTAS),
};

/// Looks up attacks for a pawn of `color` on `sq`.
#[inline]
pub const fn pawn_attacks(color: Color, sq: Square) -> Bitboard {
    Bitboard(PAWN_ATTACKS.get(color)[sq.to_usize()])
}

/// Looks up attacks for a knight on `sq`.
#[inline]
pub const fn knight_attacks(sq: Square) -> Bitboard {
    Bitboard(KNIGHT_ATTACKS[sq.to_usize()])
}

/// Looks up attacks for a king on `sq`.
#[inline]
pub const fn king_attacks(sq: Square) -> Bitboard {
    Bitboard(KING_ATTACKS[sq.to_usize()])
}

struct Magic {
    pub mask: u64,
    pub factor: u64,
    pub offset: usize,
}

// Fixed shift white magics found by Volker Annuss.
// From: http://www.talkchess.com/forum/viewtopic.php?p=727500&t=64790

#[rustfmt::skip]
static ROOK_MAGICS: [Magic; 64] = [
    Magic { mask: 0x0001_0101_0101_017e, factor: 0x0028_0077_ffeb_fffe, offset: 26304 },
    Magic { mask: 0x0002_0202_0202_027c, factor: 0x2004_0102_0109_7fff, offset: 35520 },
    Magic { mask: 0x0004_0404_0404_047a, factor: 0x0010_0200_1005_3fff, offset: 38592 },
    Magic { mask: 0x0008_0808_0808_0876, factor: 0x0040_0400_0800_4002, offset:  8026 },
    Magic { mask: 0x0010_1010_1010_106e, factor: 0x7fd0_0441_ffff_d003, offset: 22196 },
    Magic { mask: 0x0020_2020_2020_205e, factor: 0x4020_0088_87df_fffe, offset: 80870 },
    Magic { mask: 0x0040_4040_4040_403e, factor: 0x0040_0088_8847_ffff, offset: 76747 },
    Magic { mask: 0x0080_8080_8080_807e, factor: 0x0068_00fb_ff75_fffd, offset: 30400 },
    Magic { mask: 0x0001_0101_0101_7e00, factor: 0x0000_2801_0113_ffff, offset: 11115 },
    Magic { mask: 0x0002_0202_0202_7c00, factor: 0x0020_0402_01fc_ffff, offset: 18205 },
    Magic { mask: 0x0004_0404_0404_7a00, factor: 0x007f_e800_42ff_ffe8, offset: 53577 },
    Magic { mask: 0x0008_0808_0808_7600, factor: 0x0000_1800_217f_ffe8, offset: 62724 },
    Magic { mask: 0x0010_1010_1010_6e00, factor: 0x0000_1800_073f_ffe8, offset: 34282 },
    Magic { mask: 0x0020_2020_2020_5e00, factor: 0x0000_1800_e05f_ffe8, offset: 29196 },
    Magic { mask: 0x0040_4040_4040_3e00, factor: 0x0000_1800_602f_ffe8, offset: 23806 },
    Magic { mask: 0x0080_8080_8080_7e00, factor: 0x0000_3000_2fff_ffa0, offset: 49481 },
    Magic { mask: 0x0001_0101_017e_0100, factor: 0x0030_0018_010b_ffff, offset:  2410 },
    Magic { mask: 0x0002_0202_027c_0200, factor: 0x0003_000c_0085_fffb, offset: 36498 },
    Magic { mask: 0x0004_0404_047a_0400, factor: 0x0004_0008_0201_0008, offset: 24478 },
    Magic { mask: 0x0008_0808_0876_0800, factor: 0x0004_0020_2002_0004, offset: 10074 },
    Magic { mask: 0x0010_1010_106e_1000, factor: 0x0001_0020_0200_2001, offset: 79315 },
    Magic { mask: 0x0020_2020_205e_2000, factor: 0x0001_0010_0080_1040, offset: 51779 },
    Magic { mask: 0x0040_4040_403e_4000, factor: 0x0000_0040_4000_8001, offset: 13586 },
    Magic { mask: 0x0080_8080_807e_8000, factor: 0x0000_0068_00cd_fff4, offset: 19323 },
    Magic { mask: 0x0001_0101_7e01_0100, factor: 0x0040_2000_1008_0010, offset: 70612 },
    Magic { mask: 0x0002_0202_7c02_0200, factor: 0x0000_0800_1004_0010, offset: 83652 },
    Magic { mask: 0x0004_0404_7a04_0400, factor: 0x0004_0100_0802_0008, offset: 63110 },
    Magic { mask: 0x0008_0808_7608_0800, factor: 0x0000_0400_2020_0200, offset: 34496 },
    Magic { mask: 0x0010_1010_6e10_1000, factor: 0x0002_0080_1010_0100, offset: 84966 },
    Magic { mask: 0x0020_2020_5e20_2000, factor: 0x0000_0080_2001_0020, offset: 54341 },
    Magic { mask: 0x0040_4040_3e40_4000, factor: 0x0000_0080_2020_0040, offset: 60421 },
    Magic { mask: 0x0080_8080_7e80_8000, factor: 0x0000_8200_2000_4020, offset: 86402 },
    Magic { mask: 0x0001_017e_0101_0100, factor: 0x00ff_fd18_0030_0030, offset: 50245 },
    Magic { mask: 0x0002_027c_0202_0200, factor: 0x007f_ff7f_bfd4_0020, offset: 76622 },
    Magic { mask: 0x0004_047a_0404_0400, factor: 0x003f_ffbd_0018_0018, offset: 84676 },
    Magic { mask: 0x0008_0876_0808_0800, factor: 0x001f_ffde_8018_0018, offset: 78757 },
    Magic { mask: 0x0010_106e_1010_1000, factor: 0x000f_ffe0_bfe8_0018, offset: 37346 },
    Magic { mask: 0x0020_205e_2020_2000, factor: 0x0001_0000_8020_2001, offset:   370 },
    Magic { mask: 0x0040_403e_4040_4000, factor: 0x0003_fffb_ff98_0180, offset: 42182 },
    Magic { mask: 0x0080_807e_8080_8000, factor: 0x0001_fffd_ff90_00e0, offset: 45385 },
    Magic { mask: 0x0001_7e01_0101_0100, factor: 0x00ff_fefe_ebff_d800, offset: 61659 },
    Magic { mask: 0x0002_7c02_0202_0200, factor: 0x007f_fff7_ffc0_1400, offset: 12790 },
    Magic { mask: 0x0004_7a04_0404_0400, factor: 0x003f_ffbf_e4ff_e800, offset: 16762 },
    Magic { mask: 0x0008_7608_0808_0800, factor: 0x001f_fff0_1fc0_3000, offset:     0 },
    Magic { mask: 0x0010_6e10_1010_1000, factor: 0x000f_ffe7_f8bf_e800, offset: 38380 },
    Magic { mask: 0x0020_5e20_2020_2000, factor: 0x0007_ffdf_df3f_f808, offset: 11098 },
    Magic { mask: 0x0040_3e40_4040_4000, factor: 0x0003_fff8_5fff_a804, offset: 21803 },
    Magic { mask: 0x0080_7e80_8080_8000, factor: 0x0001_fffd_75ff_a802, offset: 39189 },
    Magic { mask: 0x007e_0101_0101_0100, factor: 0x00ff_ffd7_ffeb_ffd8, offset: 58628 },
    Magic { mask: 0x007c_0202_0202_0200, factor: 0x007f_ff75_ff7f_bfd8, offset: 44116 },
    Magic { mask: 0x007a_0404_0404_0400, factor: 0x003f_ff86_3fbf_7fd8, offset: 78357 },
    Magic { mask: 0x0076_0808_0808_0800, factor: 0x001f_ffbf_dfd7_ffd8, offset: 44481 },
    Magic { mask: 0x006e_1010_1010_1000, factor: 0x000f_fff8_1028_0028, offset: 64134 },
    Magic { mask: 0x005e_2020_2020_2000, factor: 0x0007_ffd7_f7fe_ffd8, offset: 41759 },
    Magic { mask: 0x003e_4040_4040_4000, factor: 0x0003_fffc_0c48_0048, offset:  1394 },
    Magic { mask: 0x007e_8080_8080_8000, factor: 0x0001_ffff_afd7_ffd8, offset: 40910 },
    Magic { mask: 0x7e01_0101_0101_0100, factor: 0x00ff_ffe4_ffdf_a3ba, offset: 66516 },
    Magic { mask: 0x7c02_0202_0202_0200, factor: 0x007f_ffef_7ff3_d3da, offset:  3897 },
    Magic { mask: 0x7a04_0404_0404_0400, factor: 0x003f_ffbf_dfef_f7fa, offset:  3930 },
    Magic { mask: 0x7608_0808_0808_0800, factor: 0x001f_ffef_f7fb_fc22, offset: 72934 },
    Magic { mask: 0x6e10_1010_1010_1000, factor: 0x0000_0204_0800_1001, offset: 72662 },
    Magic { mask: 0x5e20_2020_2020_2000, factor: 0x0007_fffe_ffff_77fd, offset: 56325 },
    Magic { mask: 0x3e40_4040_4040_4000, factor: 0x0003_ffff_bf7d_feec, offset: 66501 },
    Magic { mask: 0x7e80_8080_8080_8000, factor: 0x0001_ffff_9dff_a333, offset: 14826 },
];

#[rustfmt::skip]
static BISHOP_MAGICS: [Magic; 64] = [
    Magic { mask: 0x0040_2010_0804_0200, factor: 0x007f_bfbf_bfbf_bfff, offset:  5378 },
    Magic { mask: 0x0000_4020_1008_0400, factor: 0x0000_a060_4010_07fc, offset:  4093 },
    Magic { mask: 0x0000_0040_2010_0a00, factor: 0x0001_0040_0802_0000, offset:  4314 },
    Magic { mask: 0x0000_0000_4022_1400, factor: 0x0000_8060_0400_0000, offset:  6587 },
    Magic { mask: 0x0000_0000_0244_2800, factor: 0x0000_1004_0000_0000, offset:  6491 },
    Magic { mask: 0x0000_0002_0408_5000, factor: 0x0000_21c1_00b2_0000, offset:  6330 },
    Magic { mask: 0x0000_0204_0810_2000, factor: 0x0000_0400_4100_8000, offset:  5609 },
    Magic { mask: 0x0002_0408_1020_4000, factor: 0x0000_0fb0_203f_ff80, offset: 22236 },
    Magic { mask: 0x0020_1008_0402_0000, factor: 0x0000_0401_0040_1004, offset:  6106 },
    Magic { mask: 0x0040_2010_0804_0000, factor: 0x0000_0200_8020_0802, offset:  5625 },
    Magic { mask: 0x0000_4020_100a_0000, factor: 0x0000_0040_1020_2000, offset: 16785 },
    Magic { mask: 0x0000_0040_2214_0000, factor: 0x0000_0080_6004_0000, offset: 16817 },
    Magic { mask: 0x0000_0002_4428_0000, factor: 0x0000_0044_0200_0000, offset:  6842 },
    Magic { mask: 0x0000_0204_0850_0000, factor: 0x0000_0008_0100_8000, offset:  7003 },
    Magic { mask: 0x0002_0408_1020_0000, factor: 0x0000_07ef_e0bf_ff80, offset:  4197 },
    Magic { mask: 0x0004_0810_2040_0000, factor: 0x0000_0008_2082_0020, offset:  7356 },
    Magic { mask: 0x0010_0804_0200_0200, factor: 0x0000_4000_8080_8080, offset:  4602 },
    Magic { mask: 0x0020_1008_0400_0400, factor: 0x0002_1f01_0040_0808, offset:  4538 },
    Magic { mask: 0x0040_2010_0a00_0a00, factor: 0x0001_8000_c06f_3fff, offset: 29531 },
    Magic { mask: 0x0000_4022_1400_1400, factor: 0x0000_2582_0080_1000, offset: 45393 },
    Magic { mask: 0x0000_0244_2800_2800, factor: 0x0000_2400_8084_0000, offset: 12420 },
    Magic { mask: 0x0002_0408_5000_5000, factor: 0x0000_1800_0c03_fff8, offset: 15763 },
    Magic { mask: 0x0004_0810_2000_2000, factor: 0x0000_0a58_4020_8020, offset:  5050 },
    Magic { mask: 0x0008_1020_4000_4000, factor: 0x0000_0200_0820_8020, offset:  4346 },
    Magic { mask: 0x0008_0402_0002_0400, factor: 0x0000_8040_0081_0100, offset:  6074 },
    Magic { mask: 0x0010_0804_0004_0800, factor: 0x0001_0119_0080_2008, offset:  7866 },
    Magic { mask: 0x0020_100a_000a_1000, factor: 0x0000_8040_0081_0100, offset: 32139 },
    Magic { mask: 0x0040_2214_0014_2200, factor: 0x0001_0040_3c04_03ff, offset: 57673 },
    Magic { mask: 0x0002_4428_0028_4400, factor: 0x0007_8402_a880_2000, offset: 55365 },
    Magic { mask: 0x0004_0850_0050_0800, factor: 0x0000_1010_0080_4400, offset: 15818 },
    Magic { mask: 0x0008_1020_0020_1000, factor: 0x0000_0808_0010_4100, offset:  5562 },
    Magic { mask: 0x0010_2040_0040_2000, factor: 0x0000_4004_c008_2008, offset:  6390 },
    Magic { mask: 0x0004_0200_0204_0800, factor: 0x0001_0101_2000_8020, offset:  7930 },
    Magic { mask: 0x0008_0400_0408_1000, factor: 0x0000_8080_9a00_4010, offset: 13329 },
    Magic { mask: 0x0010_0a00_0a10_2000, factor: 0x0007_fefe_0881_0010, offset:  7170 },
    Magic { mask: 0x0022_1400_1422_4000, factor: 0x0003_ff0f_833f_c080, offset: 27267 },
    Magic { mask: 0x0044_2800_2844_0200, factor: 0x007f_e080_1900_3042, offset: 53787 },
    Magic { mask: 0x0008_5000_5008_0400, factor: 0x003f_ffef_ea00_3000, offset:  5097 },
    Magic { mask: 0x0010_2000_2010_0800, factor: 0x0000_1010_1000_2080, offset:  6643 },
    Magic { mask: 0x0020_4000_4020_1000, factor: 0x0000_8020_0508_0804, offset:  6138 },
    Magic { mask: 0x0002_0002_0408_1000, factor: 0x0000_8080_80a8_0040, offset:  7418 },
    Magic { mask: 0x0004_0004_0810_2000, factor: 0x0000_1041_0020_0040, offset:  7898 },
    Magic { mask: 0x000a_000a_1020_4000, factor: 0x0003_ffdf_7f83_3fc0, offset: 42012 },
    Magic { mask: 0x0014_0014_2240_0000, factor: 0x0000_0088_4045_0020, offset: 57350 },
    Magic { mask: 0x0028_0028_4402_0000, factor: 0x0000_7ffc_8018_0030, offset: 22813 },
    Magic { mask: 0x0050_0050_0804_0200, factor: 0x007f_ffdd_8014_0028, offset: 56693 },
    Magic { mask: 0x0020_0020_1008_0400, factor: 0x0002_0080_200a_0004, offset:  5818 },
    Magic { mask: 0x0040_0040_2010_0800, factor: 0x0000_1010_1010_0020, offset:  7098 },
    Magic { mask: 0x0000_0204_0810_2000, factor: 0x0007_ffdf_c180_5000, offset:  4451 },
    Magic { mask: 0x0000_0408_1020_4000, factor: 0x0003_ffef_e0c0_2200, offset:  4709 },
    Magic { mask: 0x0000_0a10_2040_0000, factor: 0x0000_0008_2080_6000, offset:  4794 },
    Magic { mask: 0x0000_1422_4000_0000, factor: 0x0000_0000_0840_3000, offset: 13364 },
    Magic { mask: 0x0000_2844_0200_0000, factor: 0x0000_0001_0020_2000, offset:  4570 },
    Magic { mask: 0x0000_5008_0402_0000, factor: 0x0000_0040_4080_2000, offset:  4282 },
    Magic { mask: 0x0000_2010_0804_0200, factor: 0x0004_0100_4010_0400, offset: 14964 },
    Magic { mask: 0x0000_4020_1008_0400, factor: 0x0000_6020_6018_03f4, offset:  4026 },
    Magic { mask: 0x0002_0408_1020_4000, factor: 0x0003_ffdf_dfc2_8048, offset:  4826 },
    Magic { mask: 0x0004_0810_2040_0000, factor: 0x0000_0008_2082_0020, offset:  7354 },
    Magic { mask: 0x000a_1020_4000_0000, factor: 0x0000_0000_0820_8060, offset:  4848 },
    Magic { mask: 0x0014_2240_0000_0000, factor: 0x0000_0000_0080_8020, offset: 15946 },
    Magic { mask: 0x0028_4402_0000_0000, factor: 0x0000_0000_0100_2020, offset: 14932 },
    Magic { mask: 0x0050_0804_0200_0000, factor: 0x0000_0004_0100_2008, offset: 16588 },
    Magic { mask: 0x0020_1008_0402_0000, factor: 0x0000_0040_4040_4040, offset:  6905 },
    Magic { mask: 0x0040_2010_0804_0200, factor: 0x007f_ff9f_df7f_f813, offset: 16076 },
];

const fn bootstrap_magics() -> [u64; 88772] {
    let mut table = [0; 88772];
    let mut square = 0;
    while square < 64 {
        let magic = &BISHOP_MAGICS[square as usize];
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

        let magic = &ROOK_MAGICS[square as usize];
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

static ATTACKS: [u64; 88772] = bootstrap_magics();

/// Looks up attacks for a rook on `sq` with `occupied` squares.
#[inline]
pub const fn rook_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    let m = &ROOK_MAGICS[sq.to_usize()];

    // Safety: The attack table was generated with sufficient size
    // for all relevant occupancies (all subsets of m.mask). Omitting bounds
    // checks is worth about 2% in move generation and perft.
    let idx = (m.factor.wrapping_mul(occupied.0 & m.mask) >> (64 - 12)) as usize + m.offset;
    unsafe { assert_unchecked(idx < ATTACKS.len()) };
    Bitboard(ATTACKS[idx])
}

/// Looks up attacks for a bishop on `sq` with `occupied` squares.
#[inline]
pub const fn bishop_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    let m = &BISHOP_MAGICS[sq.to_usize()];

    // Safety: The attack table was generated with sufficient size
    // for all relevant occupancies (all subsets of m.mask). Omitting bounds
    // checks is worth about 2% in move generation and perft.
    let idx = (m.factor.wrapping_mul(occupied.0 & m.mask) >> (64 - 9)) as usize + m.offset;
    unsafe { assert_unchecked(idx < ATTACKS.len()) };
    Bitboard(ATTACKS[idx])
}

/// Looks up attacks for a queen on `sq` with `occupied` squares.
#[inline]
pub const fn queen_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    rook_attacks(sq, occupied).toggled_const(bishop_attacks(sq, occupied))
}

/// Looks up attacks for `piece` on `sq` with `occupied` squares.
pub const fn attacks(sq: Square, piece: Piece, occupied: Bitboard) -> Bitboard {
    match piece.role {
        Role::Pawn => pawn_attacks(piece.color, sq),
        Role::Knight => knight_attacks(sq),
        Role::Bishop => bishop_attacks(sq, occupied),
        Role::Rook => rook_attacks(sq, occupied),
        Role::Queen => queen_attacks(sq, occupied),
        Role::King => king_attacks(sq),
    }
}

const fn bootstrap_rays() -> [[u64; 64]; 64] {
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

static RAYS: [[u64; 64]; 64] = bootstrap_rays();

/// The rank, file or diagonal with the two squares (or an empty [`Bitboard`]
/// if they are not aligned).
///
/// # Example
///
/// ```
/// use shakmaty::{attacks, Square};
///
/// let ray = attacks::ray(Square::E2, Square::G4);
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . . . . 1
/// // . . . . . . 1 .
/// // . . . . . 1 . .
/// // . . . . 1 . . .
/// // . . . 1 . . . .
/// ```
#[inline]
pub const fn ray(a: Square, b: Square) -> Bitboard {
    Bitboard(RAYS[a.to_usize()][b.to_usize()])
}

/// The squares between the two squares (bounds not included), or an empty
/// [`Bitboard`] if they are not on the same rank, file or diagonal.
///
/// # Example
///
/// ```
/// use shakmaty::{attacks, Square};
///
/// let between = attacks::between(Square::B1, Square::B7);
/// // . . . . . . . .
/// // . 0 . . . . . .
/// // . 1 . . . . . .
/// // . 1 . . . . . .
/// // . 1 . . . . . .
/// // . 1 . . . . . .
/// // . 1 . . . . . .
/// // . 0 . . . . . .
/// ```
#[inline]
pub const fn between(a: Square, b: Square) -> Bitboard {
    Bitboard(ray(a, b).0 & ((!0 << a.to_u32()) ^ (!0 << b.to_u32()))).without_first()
}

/// Tests if all three squares are aligned on a rank, file or diagonal.
///
/// # Example
///
/// ```
/// use shakmaty::{attacks, Square};
///
/// assert!(attacks::aligned(Square::A1, Square::B2, Square::C3));
/// ```
#[inline]
pub const fn aligned(a: Square, b: Square, c: Square) -> bool {
    ray(a, b).contains(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rook_attacks() {
        assert_eq!(
            rook_attacks(Square::D6, Bitboard(0x3f7f_2880_2826_f5b9)),
            Bitboard(0x0008_3708_0800_0000)
        );
    }
}
