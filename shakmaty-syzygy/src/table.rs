// This file is part of the shakmaty-syzygy library.
// Copyright (C) 2017-2018 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use std::fs::File;
use std::io;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::path::Path;

use arrayvec::ArrayVec;
use bit_vec::BitVec;
use byteorder::{BigEndian, ByteOrder, LittleEndian};
use itertools::Itertools;
use num_integer::binomial;
use positioned_io::ReadAt;

use shakmaty::{Bitboard, Color, Piece, Position, Role, Square};

use material::Material;
use types::{Dtz, Pieces, Syzygy, SyzygyError, SyzygyResult, Wdl, MAX_PIECES};

#[derive(Debug, PartialEq, Eq)]
pub enum Metric {
    Wdl,
    Dtz,
}

pub trait TableTag {
    const METRIC: Metric;
}

#[derive(Debug)]
pub enum WdlTag { }

impl TableTag for WdlTag {
    const METRIC: Metric = Metric::Wdl;
}

#[derive(Debug)]
pub enum DtzTag { }

impl TableTag for DtzTag {
    const METRIC: Metric = Metric::Dtz;
}

bitflags! {
    struct Layout: u8 {
        const SPLIT = 1;
        const HAS_PAWNS = 2;
    }
}

bitflags! {
    struct Flag: u8 {
        const STM = 1;
        const MAPPED = 2;
        const WIN_PLIES = 4;
        const LOSS_PLIES = 8;
        const SINGLE_VALUE = 128;
    }
}

/// Maps squares into the a1-d1-d4 triangle.
const TRIANGLE: [u64; 64] = [
    6, 0, 1, 2, 2, 1, 0, 6,
    0, 7, 3, 4, 4, 3, 7, 0,
    1, 3, 8, 5, 5, 8, 3, 1,
    2, 4, 5, 9, 9, 5, 4, 2,
    2, 4, 5, 9, 9, 5, 4, 2,
    1, 3, 8, 5, 5, 8, 3, 1,
    0, 7, 3, 4, 4, 3, 7, 0,
    6, 0, 1, 2, 2, 1, 0, 6,
];

/// Inverse of `TRIANGLE`.
const INV_TRIANGLE: [usize; 10] = [1, 2, 3, 10, 11, 19, 0, 9, 18, 27];

/// Maps the b1-h1-h7 triangle to `0..=27`.
const LOWER: [u64; 64] = [
    28,  0,  1,  2,  3,  4,  5,  6,
     0, 29,  7,  8,  9, 10, 11, 12,
     1,  7, 30, 13, 14, 15, 16, 17,
     2,  8, 13, 31, 18, 19, 20, 21,
     3,  9, 14, 18, 32, 22, 23, 24,
     4, 10, 15, 19, 22, 33, 25, 26,
     5, 11, 16, 20, 23, 25, 34, 27,
     6, 12, 17, 21, 24, 26, 27, 35,
];

/// Used to initialize `Consts::mult_idx` and `Consts::mult_factor`.
const MULT_TWIST: [u64; 64] = [
    15, 63, 55, 47, 40, 48, 56, 12,
    62, 11, 39, 31, 24, 32,  8, 57,
    54, 38,  7, 23, 16,  4, 33, 49,
    46, 30, 22,  3,  0, 17, 25, 41,
    45, 29, 21,  2,  1, 18, 26, 42,
    53, 37,  6, 20, 19,  5, 34, 50,
    61, 10, 36, 28, 27, 35,  9, 58,
    14, 60, 52, 44, 43, 51, 59, 13,
];

/// Unused.
const Z0: u64 = 0;

/// Encoding of all 461 configurations of two not-connected kings.
const KK_IDX: [[u64; 64]; 10] = [[
     Z0,  Z0,  Z0,   0,   1,   2,   3,   4,
     Z0,  Z0,  Z0,   5,   6,   7,   8,   9,
     10,  11,  12,  13,  14,  15,  16,  17,
     18,  19,  20,  21,  22,  23,  24,  25,
     26,  27,  28,  29,  30,  31,  32,  33,
     34,  35,  36,  37,  38,  39,  40,  41,
     42,  43,  44,  45,  46,  47,  48,  49,
     50,  51,  52,  53,  54,  55,  56,  57,
], [
     58,  Z0,  Z0,  Z0,  59,  60,  61,  62,
     63,  Z0,  Z0,  Z0,  64,  65,  66,  67,
     68,  69,  70,  71,  72,  73,  74,  75,
     76,  77,  78,  79,  80,  81,  82,  83,
     84,  85,  86,  87,  88,  89,  90,  91,
     92,  93,  94,  95,  96,  97,  98,  99,
    100, 101, 102, 103, 104, 105, 106, 107,
    108, 109, 110, 111, 112, 113, 114, 115,
], [
    116, 117,  Z0,  Z0,  Z0, 118, 119, 120,
    121, 122,  Z0,  Z0,  Z0, 123, 124, 125,
    126, 127, 128, 129, 130, 131, 132, 133,
    134, 135, 136, 137, 138, 139, 140, 141,
    142, 143, 144, 145, 146, 147, 148, 149,
    150, 151, 152, 153, 154, 155, 156, 157,
    158, 159, 160, 161, 162, 163, 164, 165,
    166, 167, 168, 169, 170, 171, 172, 173,
], [
    174,  Z0,  Z0,  Z0, 175, 176, 177, 178,
    179,  Z0,  Z0,  Z0, 180, 181, 182, 183,
    184,  Z0,  Z0,  Z0, 185, 186, 187, 188,
    189, 190, 191, 192, 193, 194, 195, 196,
    197, 198, 199, 200, 201, 202, 203, 204,
    205, 206, 207, 208, 209, 210, 211, 212,
    213, 214, 215, 216, 217, 218, 219, 220,
    221, 222, 223, 224, 225, 226, 227, 228,
], [
    229, 230,  Z0,  Z0,  Z0, 231, 232, 233,
    234, 235,  Z0,  Z0,  Z0, 236, 237, 238,
    239, 240,  Z0,  Z0,  Z0, 241, 242, 243,
    244, 245, 246, 247, 248, 249, 250, 251,
    252, 253, 254, 255, 256, 257, 258, 259,
    260, 261, 262, 263, 264, 265, 266, 267,
    268, 269, 270, 271, 272, 273, 274, 275,
    276, 277, 278, 279, 280, 281, 282, 283,
], [
    284, 285, 286, 287, 288, 289, 290, 291,
    292, 293,  Z0,  Z0,  Z0, 294, 295, 296,
    297, 298,  Z0,  Z0,  Z0, 299, 300, 301,
    302, 303,  Z0,  Z0,  Z0, 304, 305, 306,
    307, 308, 309, 310, 311, 312, 313, 314,
    315, 316, 317, 318, 319, 320, 321, 322,
    323, 324, 325, 326, 327, 328, 329, 330,
    331, 332, 333, 334, 335, 336, 337, 338,
], [
     Z0,  Z0, 339, 340, 341, 342, 343, 344,
     Z0,  Z0, 345, 346, 347, 348, 349, 350,
     Z0,  Z0, 441, 351, 352, 353, 354, 355,
     Z0,  Z0,  Z0, 442, 356, 357, 358, 359,
     Z0,  Z0,  Z0,  Z0, 443, 360, 361, 362,
     Z0,  Z0,  Z0,  Z0,  Z0, 444, 363, 364,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 445, 365,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 446,
], [
     Z0,  Z0,  Z0, 366, 367, 368, 369, 370,
     Z0,  Z0,  Z0, 371, 372, 373, 374, 375,
     Z0,  Z0,  Z0, 376, 377, 378, 379, 380,
     Z0,  Z0,  Z0, 447, 381, 382, 383, 384,
     Z0,  Z0,  Z0,  Z0, 448, 385, 386, 387,
     Z0,  Z0,  Z0,  Z0,  Z0, 449, 388, 389,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 450, 390,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 451,
], [
    452, 391, 392, 393, 394, 395, 396, 397,
     Z0,  Z0,  Z0,  Z0, 398, 399, 400, 401,
     Z0,  Z0,  Z0,  Z0, 402, 403, 404, 405,
     Z0,  Z0,  Z0,  Z0, 406, 407, 408, 409,
     Z0,  Z0,  Z0,  Z0, 453, 410, 411, 412,
     Z0,  Z0,  Z0,  Z0,  Z0, 454, 413, 414,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 455, 415,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 456,
], [
    457, 416, 417, 418, 419, 420, 421, 422,
     Z0, 458, 423, 424, 425, 426, 427, 428,
     Z0,  Z0,  Z0,  Z0,  Z0, 429, 430, 431,
     Z0,  Z0,  Z0,  Z0,  Z0, 432, 433, 434,
     Z0,  Z0,  Z0,  Z0,  Z0, 435, 436, 437,
     Z0,  Z0,  Z0,  Z0,  Z0, 459, 438, 439,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 460, 440,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 461,
]];

const PP_IDX: [[u64; 64]; 10] = [[
      0,  Z0,   1,   2,   3,   4,   5,   6,
      7,   8,   9,  10,  11,  12,  13,  14,
     15,  16,  17,  18,  19,  20,  21,  22,
     23,  24,  25,  26,  27,  28,  29,  30,
     31,  32,  33,  34,  35,  36,  37,  38,
     39,  40,  41,  42,  43,  44,  45,  46,
     Z0,  47,  48,  49,  50,  51,  52,  53,
     54,  55,  56,  57,  58,  59,  60,  61,
], [
     62,  Z0,  Z0,  63,  64,  65,  Z0,  66,
     Z0,  67,  68,  69,  70,  71,  72,  Z0,
     73,  74,  75,  76,  77,  78,  79,  80,
     81,  82,  83,  84,  85,  86,  87,  88,
     89,  90,  91,  92,  93,  94,  95,  96,
     Z0,  97,  98,  99, 100, 101, 102, 103,
     Z0, 104, 105, 106, 107, 108, 109,  Z0,
    110,  Z0, 111, 112, 113, 114,  Z0, 115,
], [
    116,  Z0,  Z0,  Z0, 117,  Z0,  Z0, 118,
     Z0, 119, 120, 121, 122, 123, 124,  Z0,
     Z0, 125, 126, 127, 128, 129, 130,  Z0,
    131, 132, 133, 134, 135, 136, 137, 138,
     Z0, 139, 140, 141, 142, 143, 144, 145,
     Z0, 146, 147, 148, 149, 150, 151,  Z0,
     Z0, 152, 153, 154, 155, 156, 157,  Z0,
    158,  Z0,  Z0, 159, 160,  Z0,  Z0, 161,
], [
    162,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 163,
     Z0, 164,  Z0, 165, 166, 167, 168,  Z0,
     Z0, 169, 170, 171, 172, 173, 174,  Z0,
     Z0, 175, 176, 177, 178, 179, 180,  Z0,
     Z0, 181, 182, 183, 184, 185, 186,  Z0,
     Z0,  Z0, 187, 188, 189, 190, 191,  Z0,
     Z0, 192, 193, 194, 195, 196, 197,  Z0,
    198,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 199,
], [
    200,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 201,
     Z0, 202,  Z0,  Z0, 203,  Z0, 204,  Z0,
     Z0,  Z0, 205, 206, 207, 208,  Z0,  Z0,
     Z0, 209, 210, 211, 212, 213, 214,  Z0,
     Z0,  Z0, 215, 216, 217, 218, 219,  Z0,
     Z0,  Z0, 220, 221, 222, 223,  Z0,  Z0,
     Z0, 224,  Z0, 225, 226,  Z0, 227,  Z0,
    228,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 229,
], [
    230,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 231,
     Z0, 232,  Z0,  Z0,  Z0,  Z0, 233,  Z0,
     Z0,  Z0, 234,  Z0, 235, 236,  Z0,  Z0,
     Z0,  Z0, 237, 238, 239, 240,  Z0,  Z0,
     Z0,  Z0,  Z0, 241, 242, 243,  Z0,  Z0,
     Z0,  Z0, 244, 245, 246, 247,  Z0,  Z0,
     Z0, 248,  Z0,  Z0,  Z0,  Z0, 249,  Z0,
    250,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 251,
], [
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 259,
     Z0, 252,  Z0,  Z0,  Z0,  Z0, 260,  Z0,
     Z0,  Z0, 253,  Z0,  Z0, 261,  Z0,  Z0,
     Z0,  Z0,  Z0, 254, 262,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0, 255,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0, 256,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 257,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 258,
], [
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 268,  Z0,
     Z0,  Z0, 263,  Z0,  Z0, 269,  Z0,  Z0,
     Z0,  Z0,  Z0, 264, 270,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0, 265,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0, 266,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0, 267,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,
], [
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0, 274,  Z0,  Z0,
     Z0,  Z0,  Z0, 271, 275,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0, 272,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0, 273,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,
], [
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0, 277,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0, 276,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,
     Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0,  Z0
]];

/// a5, a6, a7, b5, b6, c5.
const TEST45: Bitboard = Bitboard(0x1_0307_0000_0000);

lazy_static! {
    static ref CONSTS: Consts = Consts::new();
}

struct Consts {
    mult_idx: [[u64; 10]; 5],
    mult_factor: [u64; 5],
    map_pawns: [u64; 64],
    lead_pawn_idx: [[u64; 64]; 5],
    lead_pawns_size: [[u64; 4]; 5],
}

impl Consts {
    fn new() -> Consts {
        let mut mult_idx = [[0; 10]; 5];
        let mut mult_factor = [0; 5];

        for i in 0..5 {
            let mut s = 0;
            for j in 0..10 {
                mult_idx[i][j] = s;
                s += if i == 0 { 1 } else { binomial(MULT_TWIST[INV_TRIANGLE[j]], i as u64) }
            }
            mult_factor[i] = s;
        }

        let mut available_squares = 48;

        let mut map_pawns = [0; 64];
        let mut lead_pawn_idx = [[0; 64]; 5];
        let mut lead_pawns_size = [[0; 4]; 5];

        for lead_pawns_cnt in 1..4 + 1 {
            for file in 0..4 {
                let mut idx = 0;

                for rank in 1..7 {
                    let sq = Square::from_coords(file as i8, rank).expect("valid coords");
                    if lead_pawns_cnt == 1 {
                        available_squares -= 1;
                        map_pawns[usize::from(sq)] = available_squares;
                        available_squares -= 1;
                        map_pawns[usize::from(sq.flip_horizontal())] = available_squares;
                    }
                    lead_pawn_idx[lead_pawns_cnt][usize::from(sq)] = idx;
                    idx += binomial(map_pawns[usize::from(sq)], lead_pawns_cnt as u64 - 1);
                }

                lead_pawns_size[lead_pawns_cnt][file] = idx;
            }
        }

        Consts {
            mult_idx,
            mult_factor,
            map_pawns,
            lead_pawn_idx,
            lead_pawns_size,
        }
    }
}

#[derive(Debug)]
struct RandomAccessFile {
    file: File,
}

impl RandomAccessFile {
    fn open<P: AsRef<Path>>(path: P) -> io::Result<RandomAccessFile> {
        File::open(path).map(|file| RandomAccessFile { file })
    }

    fn read_u8(&self, ptr: u64) -> SyzygyResult<u8> {
        let mut buf = [0; 1];
        self.file.read_exact_at(ptr, &mut buf)?;
        Ok(buf[0])
    }

    fn read_u16_le(&self, ptr: u64) -> SyzygyResult<u16> {
        let mut buf = [0; 2];
        self.file.read_exact_at(ptr, &mut buf)?;
        Ok(LittleEndian::read_u16(&buf))
    }

    fn read_u32_le(&self, ptr: u64) -> SyzygyResult<u32> {
        let mut buf = [0; 4];
        self.file.read_exact_at(ptr, &mut buf)?;
        Ok(LittleEndian::read_u32(&buf))
    }

    fn read_u32_be(&self, ptr: u64) -> SyzygyResult<u32> {
        let mut buf = [0; 4];
        self.file.read_exact_at(ptr, &mut buf)?;
        Ok(BigEndian::read_u32(&buf))
    }

    fn read_u64_be(&self, ptr: u64) -> SyzygyResult<u64> {
        let mut buf = [0; 8];
        self.file.read_exact_at(ptr, &mut buf)?;
        Ok(BigEndian::read_u64(&buf))
    }

    fn read_lr(&self, ptr: u64) -> SyzygyResult<(u16, u16)> {
        let mut buf = [0; 3];
        self.file.read_exact_at(ptr, &mut buf)?;
        let left = (u16::from(buf[1] & 0xf) << 8) | u16::from(buf[0]);
        let right = (u16::from(buf[2]) << 4) | (u16::from(buf[1]) >> 4);
        Ok((left, right))
    }

    fn starts_with_magic(&self, magic: &[u8; 4]) -> SyzygyResult<bool> {
        let mut buf = [0; 4];
        if let Err(error) = self.file.read_exact_at(0, &mut buf) {
            match error.kind() {
                io::ErrorKind::UnexpectedEof => Ok(false),
                _ => Err(SyzygyError::Read { error }),
            }
        } else {
            Ok(&buf == magic)
        }
    }
}

/// Header byte to piece.
fn byte_to_piece(p: u8) -> Option<Piece> {
    let color = Color::from_white(p & 8 == 0);
    Some(match p & !8 {
        1 => color.pawn(),
        2 => color.knight(),
        3 => color.bishop(),
        4 => color.rook(),
        5 => color.queen(),
        6 => color.king(),
        _ => return None,
    })
}

/// Checks if a square is on the a1-h8 diagonal.
fn offdiag(sq: Square) -> bool {
    sq.file() != sq.rank()
}

/// Parse a piece list.
fn parse_pieces(raf: &RandomAccessFile, ptr: u64, count: usize, side: Color) -> SyzygyResult<Pieces> {
    let mut buffer = [0; MAX_PIECES];
    let bytes = &mut buffer[..count];
    raf.file.read_exact_at(ptr, bytes)?;

    let mut pieces = Pieces::new();
    for p in bytes {
        pieces.push(u!(byte_to_piece(side.fold(*p & 0xf, *p >> 4))));
    }

    Ok(pieces)
}

/// Group pieces that will be encoded together.
fn group_pieces(pieces: &Pieces) -> ArrayVec<[usize; MAX_PIECES]> {
    let mut result = ArrayVec::new();
    let material = Material::from_iter(pieces.clone());

    // For pawnless positions: If there are at least 3 unique pieces then 3
    // unique pieces wil form the leading group. Otherwise the two kings will
    // form the leading group.
    let first_len = if material.has_pawns() {
        0
    } else if material.unique_pieces() >= 3 {
        3
    } else {
        2
    };

    if first_len > 0 {
        result.push(first_len);
    }

    // The remaining identical pieces are grouped together.
    result.extend(pieces.iter()
        .skip(first_len)
        .group_by(|p| *p)
        .into_iter().map(|(_, g)| g.count()));

    result
}

/// Description of the encoding used for a piece configuration.
#[derive(Debug, Clone)]
struct GroupData {
    pieces: Pieces,
    lens: ArrayVec<[usize; MAX_PIECES]>,
    factors: ArrayVec<[u64; MAX_PIECES + 1]>,
}

impl GroupData {
    pub fn new<S: Syzygy>(pieces: Pieces, order: &[u8; 2], file: usize) -> SyzygyResult<GroupData> {
        ensure!(pieces.len() >= 2);

        let material = Material::from_iter(pieces.clone());

        // Compute group lengths.
        let lens = group_pieces(&pieces);

        // Compute a factor for each group.
        let pp = material.white.has_pawns() && material.black.has_pawns();
        let mut factors = ArrayVec::from([0; MAX_PIECES + 1]);
        factors.truncate(lens.len() + 1);
        let mut free_squares = 64 - lens[0] - if pp { lens[1] } else { 0 };
        let mut next = if pp { 2 } else { 1 };
        let mut idx = 1;
        let mut k = 0;

        while next < lens.len() || k == order[0] || k == order[1] {
            if k == order[0] {
                // Leading pawns or pieces.
                factors[0] = idx;

                if material.has_pawns() {
                    idx *= CONSTS.lead_pawns_size[lens[0]][file];
                } else if material.unique_pieces() >= 3 {
                    idx *= 31_332;
                } else if material.unique_pieces() == 2 {
                    idx *= if S::CONNECTED_KINGS { 518 } else { 462 };
                } else if material.min_like_man() == 2 {
                    idx *= 278;
                } else {
                    idx *= CONSTS.mult_factor[usize::from(material.min_like_man()) - 1];
                }
            } else if k == order[1] {
                // Remaining pawns.
                factors[1] = idx;
                idx *= binomial(48 - lens[0], lens[1]) as u64;
            } else {
                // Remaining pieces.
                factors[next] = idx;
                idx *= binomial(free_squares, lens[next]) as u64;
                free_squares -= lens[next];
                next += 1;
            }
            k += 1;
        }

        factors[lens.len()] = idx;

        Ok(GroupData {
            pieces,
            lens,
            factors,
        })
    }
}

/// Information about DTZ mapping.
#[derive(Debug)]
struct DtzMap {
    /// Offset of the DTZ map.
    ptr: u64,
    /// Indexes into the DTZ map.
    idx: [u16; 4],
}

impl DtzMap {
    fn new(ptr: u64) -> DtzMap {
        DtzMap { ptr, idx: [0; 4] }
    }

    fn ptr(&self, wdl: Wdl) -> u64 {
        self.ptr + u64::from(self.idx[match wdl {
            Wdl::Loss => 1,
            Wdl::BlessedLoss => 3,
            Wdl::Draw => 0,
            Wdl::CursedWin => 2,
            Wdl::Win => 0,
        }])
    }
}

/// Description of encoding and compression.
#[derive(Debug)]
struct PairsData {
    /// Encoding flags.
    flags: Flag,
    /// Piece configuration encoding info.
    groups: GroupData,

    /// Block size in bytes.
    block_size: u32,
    /// About every span values there is a sparse index entry.
    span: u32,
    /// Number of blocks in the table.
    blocks_num: u32,

    /// Offset of the symbol table.
    btree: u64,
    /// Minimum length in bits of the Huffman symbols.
    min_symlen: u8,
    /// Offset of the lowest symbols for each length.
    lowest_sym: u64,
    /// 64-bit padded lowest symbols for each length.
    base: Vec<u64>,
    /// Number of values represented by a given Huffman symbol.
    symlen: Vec<u8>,

    /// Offset of the sparse index.
    sparse_index: u64,
    /// Size of the sparse index.
    sparse_index_size: u32,

    /// Offset of the block length table.
    block_lengths: u64,
    /// Size of the block length table, padded to be bigger than `blocks_num`.
    block_length_size: u32,

    /// Start of compressed data.
    data: u64,

    /// DTZ mapping.
    dtz_map: Option<DtzMap>,
}

impl PairsData {
    pub fn parse<S: Syzygy, T: TableTag>(raf: &RandomAccessFile, mut ptr: u64, groups: GroupData) -> SyzygyResult<(PairsData, u64)> {
        let flags = Flag::from_bits_truncate(raf.read_u8(ptr)?);

        if flags.contains(Flag::SINGLE_VALUE) {
            let single_value = if T::METRIC == Metric::Wdl {
                raf.read_u8(ptr + 1)?
            } else if S::CAPTURES_COMPULSORY {
                1 // http://www.talkchess.com/forum/viewtopic.php?p=698093#698093
            } else {
                0
            };

            return Ok((PairsData {
                flags,
                min_symlen: single_value,
                groups,
                base: Vec::new(),
                block_lengths: 0,
                block_length_size: 0,
                block_size: 0,
                blocks_num: 0,
                btree: 0,
                data: 0,
                lowest_sym: 0,
                span: 0,
                sparse_index: 0,
                sparse_index_size: 0,
                symlen: Vec::new(),
                dtz_map: None,
            }, ptr + 2));
        }

        // Read header.
        let mut header = [0; 10];
        raf.file.read_exact_at(ptr, &mut header)?;

        let tb_size = groups.factors[groups.lens.len()];
        let block_size = u!(1u32.checked_shl(u32::from(header[1])));
        let span = u!(1u32.checked_shl(u32::from(header[2])));
        let sparse_index_size = ((tb_size + u64::from(span) - 1) / u64::from(span)) as u32;
        let padding = header[3];
        let blocks_num = LittleEndian::read_u32(&header[4..]);
        let block_length_size = blocks_num + u32::from(padding);

        let max_symlen = header[8];
        let min_symlen = header[9];
        ensure!(max_symlen >= min_symlen);
        let h = usize::from(max_symlen - min_symlen + 1);

        let lowest_sym = ptr + 10;

        // Initialize base.
        let mut base = vec![0u64; h];
        for i in (0..h - 1).rev() {
            let ptr = lowest_sym + i as u64 * 2;

            base[i] = u!(u!(base[i + 1]
                .checked_add(u64::from(raf.read_u16_le(ptr)?)))
                .checked_sub(u64::from(raf.read_u16_le(ptr + 2)?))) / 2;

            ensure!(base[i] * 2 >= base[i + 1]);
        }

        for i in 0..h {
            base[i] = u!(base[i].checked_shl(64 - (u32::from(min_symlen) + i as u32)));
        }

        // Initialize symlen.
        ptr += 10 + h as u64 * 2;
        let sym = raf.read_u16_le(ptr)?;
        ptr += 2;
        let btree = ptr;
        let mut symlen = vec![0; sym as usize];
        let mut visited = BitVec::from_elem(symlen.len(), false);
        for s in 0..sym {
           read_symlen(raf, btree, &mut symlen, &mut visited, s)?;
        }
        ptr += symlen.len() as u64 * 3 + (symlen.len() as u64 & 1);

        // Result.
        Ok((PairsData {
            flags,
            groups,

            block_size,
            span,
            blocks_num,

            btree,
            min_symlen,
            lowest_sym,
            base,
            symlen,

            sparse_index: 0, // to be initialized later
            sparse_index_size,

            block_lengths: 0, // to be initialized later
            block_length_size,

            data: 0, // to be initialized later

            dtz_map: None, // to be initialized later
        }, ptr))
    }
}

/// Build the symlen table.
fn read_symlen(raf: &RandomAccessFile, btree: u64, symlen: &mut Vec<u8>, visited: &mut BitVec, sym: u16) -> SyzygyResult<()> {
    if u!(visited.get(sym as usize)) {
        return Ok(());
    }

    let ptr = btree + 3 * u64::from(sym);
    let (left, right) = raf.read_lr(ptr)?;

    if right == 0xfff {
        symlen[sym as usize] = 0;
    } else {
        read_symlen(raf, btree, symlen, visited, left)?;
        read_symlen(raf, btree, symlen, visited, right)?;
        symlen[sym as usize] = symlen[left as usize] + symlen[right as usize] + 1;
    }

    visited.set(sym as usize, true);
    Ok(())
}

/// Descripton of encoding and compression for both sides of a table.
#[derive(Debug)]
struct FileData {
    sides: ArrayVec<[PairsData; 2]>,
}

/// A Syzygy table.
#[derive(Debug)]
pub struct Table<T: TableTag, P: Position + Syzygy> {
    is_wdl: PhantomData<T>,
    syzygy: PhantomData<P>,

    raf: RandomAccessFile,

    material: Material,

    num_unique_pieces: u8,
    min_like_man: u8,
    files: ArrayVec<[FileData; 4]>,
}

impl<T: TableTag, S: Position + Syzygy> Table<T, S> {
    pub fn open<P: AsRef<Path>>(path: P, material: &Material) -> SyzygyResult<Table<T, S>> {
        let raf = RandomAccessFile::open(path)?;
        let material = material.clone();

        // Check magic.
        let (magic, pawnless_magic) = match T::METRIC {
            Metric::Wdl => (&S::WDL_MAGIC, &S::PAWNLESS_WDL_MAGIC),
            Metric::Dtz => (&S::DTZ_MAGIC, &S::PAWNLESS_DTZ_MAGIC),
        };

        if !raf.starts_with_magic(magic)? && (material.has_pawns() || !raf.starts_with_magic(pawnless_magic)?) {
            return Err(SyzygyError::Magic);
        }

        // Read layout flags.
        let layout = Layout::from_bits_truncate(raf.read_u8(4)?);
        let has_pawns = layout.contains(Layout::HAS_PAWNS);
        let split = layout.contains(Layout::SPLIT);

        // Check consistency of layout and material key.
        ensure!(has_pawns == material.has_pawns());
        ensure!(split != material.is_symmetric());

        // Read group data.
        let pp = material.white.has_pawns() && material.black.has_pawns();
        let num_files = if has_pawns { 4 } else { 1 };
        let num_sides = if T::METRIC == Metric::Wdl && !material.is_symmetric() { 2 } else { 1 };

        let mut groups: ArrayVec<[ArrayVec<[GroupData; 2]>; 4]> = ArrayVec::new();
        let mut ptr = 5;

        for file in 0..num_files {
            let mut sides = ArrayVec::new();

            let order = [
                [raf.read_u8(ptr)? & 0xf, if pp { raf.read_u8(ptr + 1)? & 0xf } else { 0xf }],
                [raf.read_u8(ptr)? >> 4, if pp { raf.read_u8(ptr + 1)? >> 4 } else { 0xf }],
            ];

            ptr += 1 + if pp { 1 } else { 0 };

            for side in [Color::White, Color::Black].iter().take(num_sides) {
                let pieces = parse_pieces(&raf, ptr, material.count(), *side)?;
                let key = Material::from_iter(pieces.clone());
                ensure!(key == material || key.flip() == material);

                let group = GroupData::new::<S>(pieces, &order[side.fold(0, 1)], file)?;
                sides.push(group);
            }

            ptr += material.count() as u64;

            groups.push(sides);
        }

        ptr += ptr & 1;

        let mut files: ArrayVec<[FileData; 4]> = ArrayVec::new();

        for f in 0..num_files {
            let mut sides = ArrayVec::new();

            for side in [Color::White, Color::Black].iter().take(num_sides) {
                let group = groups[f][side.fold(0, 1)].clone();
                let (mut pairs, next_ptr) = PairsData::parse::<S, T>(&raf, ptr, group)?;

                if T::METRIC == Metric::Dtz && S::CAPTURES_COMPULSORY && pairs.flags.contains(Flag::SINGLE_VALUE) {
                    pairs.min_symlen = 1;
                }

                sides.push(pairs);
                ptr = next_ptr;
            }

            files.push(FileData { sides });
        }

        if T::METRIC == Metric::Dtz {
            let map = ptr;

            for f in 0..num_files {
                if files[f].sides[0].flags.contains(Flag::MAPPED) {
                    let mut dtz_map = DtzMap::new(map);

                    for i in 0..4 {
                        dtz_map.idx[i] = (ptr - map + 1) as u16;
                        ptr += u64::from(raf.read_u8(ptr)?) + 1;
                    }

                    files[f].sides[0].dtz_map = Some(dtz_map);
                }
            }

            ptr += ptr & 1;
        }

        for f in 0..num_files {
            for s in 0..num_sides {
                files[f].sides[s].sparse_index = ptr;
                ptr += u64::from(files[f].sides[s].sparse_index_size) * 6;
            }
        }

        for f in 0..num_files {
            for s in 0..num_sides {
                files[f].sides[s].block_lengths = ptr;
                ptr += u64::from(files[f].sides[s].block_length_size) * 2;
            }
        }

        for f in 0..num_files {
            for s in 0..num_sides {
                ptr = (ptr + 0x3f) & !0x3f; // Check 64 byte alignment
                files[f].sides[s].data = ptr;
                ptr = u!(ptr.checked_add(u64::from(files[f].sides[s].blocks_num) * u64::from(files[f].sides[s].block_size)));
            }
        }

        // Result.
        Ok(Table {
            is_wdl: PhantomData,
            syzygy: PhantomData,
            raf,
            num_unique_pieces: material.unique_pieces(),
            min_like_man: material.min_like_man(),
            files,
            material,
        })
    }

    fn decompress_pairs(&self, d: &PairsData, idx: u64) -> SyzygyResult<u8> {
        if d.flags.contains(Flag::SINGLE_VALUE) {
            return Ok(d.min_symlen);
        }

        let k = idx / u64::from(d.span);

        let mut block = u64::from(self.raf.read_u32_le(d.sparse_index + 6 * k)?);
        let mut offset = i64::from(self.raf.read_u16_le(d.sparse_index + 6 * k + 4)?);

        let diff = idx as i64 % i64::from(d.span) - i64::from(d.span) / 2;
        offset += diff;

        while offset < 0 {
            block -= 1;
            offset += i64::from(self.raf.read_u16_le(d.block_lengths + block * 2)?) + 1;
        }

        while offset > i64::from(self.raf.read_u16_le(d.block_lengths + block * 2)?) {
            offset -= i64::from(self.raf.read_u16_le(d.block_lengths + block * 2)?) + 1;
            block += 1;
        }

        let mut ptr = d.data + block * u64::from(d.block_size);

        let mut buf = self.raf.read_u64_be(ptr)?;
        ptr += 8;
        let mut buf_size = 64;

        let mut sym;

        loop {
            let mut len = 0;

            while buf < *u!(d.base.get(len)) {
                len += 1;
            }

            sym = ((buf - d.base[len]) >> (64 - len - d.min_symlen as usize)) as u16;
            sym += self.raf.read_u16_le(d.lowest_sym + 2 * len as u64)?;

            if offset < i64::from(*u!(d.symlen.get(sym as usize))) + 1 {
                break;
            }

            offset -= i64::from(*u!(d.symlen.get(sym as usize))) + 1;
            len += usize::from(d.min_symlen);
            buf <<= len;
            buf_size -= len;

            // Refill the buffer.
            if buf_size <= 32 {
                buf_size += 32;
                buf |= u64::from(self.raf.read_u32_be(ptr)?) << (64 - buf_size);
                ptr += 4;
            }
        }

        while *u!(d.symlen.get(sym as usize)) != 0 {
            let (left, right) = self.raf.read_lr(d.btree + 3 * u64::from(sym))?;

            if offset < i64::from(*u!(d.symlen.get(left as usize))) + 1 {
                sym = left;
            } else {
                offset -= i64::from(*u!(d.symlen.get(left as usize))) + 1;
                sym = right;
            }
        }

        Ok(self.raf.read_u8(d.btree + 3 * u64::from(sym))?)
    }

    fn encode(&self, pos: &S) -> SyzygyResult<Option<(&PairsData, u64)>> {
        let key = Material::from_board(pos.board());
        let material = Material::from_iter(self.files[0].sides[0].groups.pieces.clone());
        assert!(key == material || key == material.flip());

        let symmetric_btm = material.is_symmetric() && pos.turn().is_black();
        let black_stronger = key != material;
        let flip = symmetric_btm || black_stronger;
        let bside = pos.turn().is_black() ^ flip;

        let mut squares: ArrayVec<[Square; MAX_PIECES]> = ArrayVec::new();
        let mut used = Bitboard(0);

        let file = if material.has_pawns() {
            let reference_pawn = self.files[0].sides[0].groups.pieces[0];
            assert_eq!(reference_pawn.role, Role::Pawn);
            let color = reference_pawn.color ^ flip;

            let lead_pawns = pos.board().pawns() & pos.board().by_color(color);
            used.extend(lead_pawns);
            squares.extend(lead_pawns.map(|sq| if flip { sq.flip_vertical() } else { sq }));

            // Ensure squares[0] is the maximum with regard to map_pawns.
            for i in 1..squares.len() {
                if CONSTS.map_pawns[usize::from(squares[0])] < CONSTS.map_pawns[usize::from(squares[i])] {
                    squares.swap(0, i);
                }
            }
            if squares[0].file() >= 4 {
                squares[0].flip_horizontal().file() as usize
            } else {
                squares[0].file() as usize
            }
        } else {
            0
        };

        let side = &self.files[file].sides[if bside { self.files[file].sides.len() - 1 } else { 0 }];

        if T::METRIC == Metric::Dtz && side.flags.contains(Flag::STM) != bside && (!material.is_symmetric() || material.has_pawns()) {
            // Check other side.
            return Ok(None);
        }

        let lead_pawns_count = squares.len();

        for piece in side.groups.pieces.iter().skip(squares.len()) {
            let color = piece.color ^ flip;
            let square = (pos.board().by_piece(piece.role.of(color)) & !used).first().expect("piece exists");
            squares.push(if flip { square.flip_vertical() } else { square });
            used.add(square);
        }

        assert!(squares.len() >= 2);

        if squares[0].file() >= 4 {
            for square in &mut squares {
                *square = square.flip_horizontal();
            }
        }

        let mut idx = if material.has_pawns() {
            let mut idx = CONSTS.lead_pawn_idx[lead_pawns_count][usize::from(squares[0])];

            squares[1..lead_pawns_count].sort_unstable_by_key(|sq| CONSTS.map_pawns[usize::from(*sq)]);

            for i in 1..lead_pawns_count {
                idx += binomial(CONSTS.map_pawns[usize::from(squares[i])], i as u64);
            }

            idx
        } else {
            if squares[0].rank() >= 4 {
                for square in &mut squares {
                    *square = square.flip_vertical();
                }
            }

            for i in 0..side.groups.lens[0] {
                if squares[i].file() == squares[i].rank() {
                    continue;
                }

                if squares[i].rank() > squares[i].file() {
                    for square in &mut squares[i..] {
                        *square = square.flip_diagonal();
                    }
                }

                break;
            }

            if self.num_unique_pieces > 2 {
                let adjust1 = if squares[1] > squares[0] { 1 } else { 0 };
                let adjust2 = if squares[2] > squares[0] { 1 } else { 0 } +
                              if squares[2] > squares[1] { 1 } else { 0 };

                if offdiag(squares[0]) {
                    TRIANGLE[usize::from(squares[0])] * 63 * 62 +
                    (u64::from(squares[1]) - adjust1) * 62 +
                    (u64::from(squares[2]) - adjust2)
                } else if offdiag(squares[1]) {
                    6 * 63 * 62 +
                    squares[0].rank() as u64 * 28 * 62 +
                    LOWER[usize::from(squares[1])] * 62 +
                    u64::from(squares[2]) - adjust2
                } else if offdiag(squares[2]) {
                    6 * 63 * 62 + 4 * 28 * 62 +
                    squares[0].rank() as u64 * 7 * 28 +
                    (squares[1].rank() as u64 - adjust1) * 28 +
                    LOWER[usize::from(squares[2])]
                } else {
                    6 * 63 * 62 + 4 * 28 * 62 + 4 * 7 * 28 +
                    squares[0].rank() as u64 * 7 * 6 +
                    (squares[1].rank() as u64 - adjust1) * 6 +
                    (squares[2].rank() as u64 - adjust2)
                }
            } else if self.num_unique_pieces == 2 {
                if S::CONNECTED_KINGS {
                    let adjust = if squares[1] > squares[0] { 1 } else { 0 };

                    if offdiag(squares[0]) {
                        TRIANGLE[usize::from(squares[0])] * 63 +
                        (u64::from(squares[1]) - adjust)
                    } else if offdiag(squares[1]) {
                        6 * 63 +
                        squares[0].rank() as u64 * 28 +
                        LOWER[usize::from(squares[1])]
                    } else {
                        6 * 63 + 4 * 28 +
                        squares[0].rank() as u64 * 7 +
                        (squares[1].rank() as u64 - adjust)
                    }
                } else {
                    KK_IDX[TRIANGLE[usize::from(squares[0])] as usize][usize::from(squares[1])]
                }
            } else if self.min_like_man == 2 {
                if TRIANGLE[usize::from(squares[0])] > TRIANGLE[usize::from(squares[1])] {
                    squares.swap(0, 1);
                }

                if squares[0].file() >= 4 {
                    for square in &mut squares {
                        *square = square.flip_horizontal();
                    }
                }

                if squares[0].rank() >= 4 {
                    for square in &mut squares {
                        *square = square.flip_vertical();
                    }
                }

                if squares[0].rank() > squares[0].file() ||
                   (!offdiag(squares[0]) && squares[1].rank() > squares[1].file()) {
                    for square in &mut squares {
                        *square = square.flip_diagonal();
                    }
                }

                if TEST45.contains(squares[1]) && TRIANGLE[usize::from(squares[0])] == TRIANGLE[usize::from(squares[1])] {
                    squares.swap(0, 1);

                    for square in &mut squares {
                        *square = square.flip_vertical().flip_diagonal();
                    }
                }

                PP_IDX[TRIANGLE[usize::from(squares[0])] as usize][usize::from(squares[1])]
            } else {
                for i in 1..side.groups.lens[0] {
                    if TRIANGLE[usize::from(squares[0])] > TRIANGLE[usize::from(squares[i])] {
                        squares.swap(0, i);
                    }
                }

                if squares[0].file() >= 4 {
                    for square in &mut squares {
                        *square = square.flip_horizontal();
                    }
                }

                if squares[0].rank() >= 4 {
                    for square in &mut squares {
                        *square = square.flip_vertical();
                    }
                }

                if squares[0].rank() > squares[0].file() {
                    for square in &mut squares {
                        *square = square.flip_diagonal();
                    }
                }

                for i in 1..side.groups.lens[0] {
                    for j in (i + 1)..side.groups.lens[0] {
                        if MULT_TWIST[usize::from(squares[i])] > MULT_TWIST[usize::from(squares[j])] {
                            squares.swap(i, j);
                        }
                    }
                }

                let mut idx = CONSTS.mult_idx[side.groups.lens[0] - 1][TRIANGLE[usize::from(squares[0])] as usize];
                for i in 1..side.groups.lens[0] {
                    idx += binomial(MULT_TWIST[usize::from(squares[i])], i as u64);
                }

                idx
            }
        };

        idx *= side.groups.factors[0];

        let mut remaining_pawns = material.white.has_pawns() && material.black.has_pawns();
        let mut next = 1;
        let mut group_sq = side.groups.lens[0];
        for lens in side.groups.lens.iter().cloned().skip(1) {
            let (prev_squares, group_squares) = squares.split_at_mut(group_sq);
            let group_squares = &mut group_squares[..lens];
            group_squares.sort_unstable();

            let mut n = 0;

            for i in 0..lens {
                let adjust = prev_squares[..group_sq].iter().filter(|sq| group_squares[i] > **sq).count() as u64;
                n += binomial(u64::from(group_squares[i]) - adjust - if remaining_pawns { 8 } else { 0 }, i as u64 + 1);
            }

            remaining_pawns = false;
            idx += n * side.groups.factors[next];
            group_sq += side.groups.lens[next];
            next += 1;
        }

        Ok(Some((side, idx)))
    }

    pub fn probe_wdl_table(&self, pos: &S) -> SyzygyResult<Wdl> {
        assert_eq!(T::METRIC, Metric::Wdl);

        let (side, idx) = self.encode(pos)?.expect("wdl is two sided");
        let decompressed = self.decompress_pairs(side, idx)?;

        Ok(match decompressed {
            0 => Wdl::Loss,
            1 => Wdl::BlessedLoss,
            2 => Wdl::Draw,
            3 => Wdl::CursedWin,
            4 => Wdl::Win,
            _ => throw!(),
        })
    }

    pub fn probe_dtz_table(&self, pos: &S, wdl: Wdl) -> SyzygyResult<Option<Dtz>> {
        assert_eq!(T::METRIC, Metric::Dtz);

        let (side, idx) = match self.encode(pos)? {
            Some(found) => found,
            None => return Ok(None), // check other side
        };

        let res = self.decompress_pairs(side, idx)?;

        let res = i16::from(if let Some(ref map) = side.dtz_map {
            self.raf.read_u8(map.ptr(wdl) + u64::from(res))?
        } else {
            res
        });

        let stores_moves = match wdl {
            Wdl::Win => !side.flags.contains(Flag::WIN_PLIES),
            Wdl::Loss => !side.flags.contains(Flag::LOSS_PLIES),
            Wdl::CursedWin | Wdl::BlessedLoss => true,
            Wdl::Draw => false,
        };

        Ok(Some(Dtz(if stores_moves { res * 2 } else { res })))
    }
}
