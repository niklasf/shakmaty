// This file is part of the shakmaty-syzygy library.
// Copyright (C) 2017 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

//! Probe Syzygy tablebases.

#![doc(html_root_url = "https://docs.rs/shakmaty-syzygy/0.1.0")]

#![warn(missing_debug_implementations)]

#![feature(try_trait)]
#![feature(inclusive_range_syntax)]

extern crate arrayvec;
#[macro_use]
extern crate bitflags;
extern crate bit_vec;
extern crate byteorder;
extern crate itertools;
#[macro_use]
extern crate lazy_static;
extern crate memmap;
extern crate num_integer;
extern crate shakmaty;

mod material;

use std::fmt;
use std::error::Error;
use std::marker::PhantomData;
use std::option::NoneError;
use std::iter::FromIterator;

use arrayvec::ArrayVec;
use bit_vec::BitVec;
use num_integer::binomial;
use itertools::Itertools;
use shakmaty::{Color, Piece, Square, Bitboard, Position, Chess};
use byteorder::{LittleEndian, BigEndian, ByteOrder};

pub use material::{Material, MaterialSide};

/// A chess variant with Syzygy support.
pub trait Syzygy {
    const WDL_SUFFIX: &'static [u8];
    const DTZ_SUFFIX: &'static [u8];
    const WDL_MAGIC: [u8; 4];
    const DTZ_MAGIC: [u8; 4];
    const PAWNLESS_WDL_MAGIC: [u8; 4];
    const PAWNLESS_DTZ_MAGIC: [u8; 4];
    const ONE_KING: bool;
    const CONNECTED_KINGS: bool;
    const CAPTURES_COMPULSORY: bool;
}

impl Syzygy for Chess {
    const WDL_SUFFIX: &'static [u8] = b".rtbw";
    const DTZ_SUFFIX: &'static [u8] = b".rtbz";
    const WDL_MAGIC: [u8; 4] = [0x71, 0xe8, 0x23, 0x5d];
    const DTZ_MAGIC: [u8; 4] = [0xd7, 0x66, 0x0c, 0xa5];
    const PAWNLESS_WDL_MAGIC: [u8; 4] = [0x71, 0xe8, 0x23, 0x5d];
    const PAWNLESS_DTZ_MAGIC: [u8; 4] = [0xd7, 0x66, 0x0c, 0xa5];
    const ONE_KING: bool = true;
    const CONNECTED_KINGS: bool = false;
    const CAPTURES_COMPULSORY: bool = false;
}

/// Syzygy tables are available for up to 6 pieces.
const MAX_PIECES: usize = 6;

/// Error initializing or probing a table.
///
/// * Unexpected magic header bytes
/// * Corrupted table
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyzygyError {
    kind: ErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ErrorKind {
    Magic,
    CorruptedTable,
}

impl SyzygyError {
    fn desc(&self) -> &str {
        match self.kind {
            ErrorKind::Magic => "invalid magic bytes",
            ErrorKind::CorruptedTable => "corrupted table",
        }
    }
}

impl fmt::Display for SyzygyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.desc().fmt(f)
    }
}

impl Error for SyzygyError {
    fn description(&self) -> &str {
        self.desc()
    }
}

impl From<NoneError> for SyzygyError {
    fn from(_: NoneError) -> SyzygyError {
        SyzygyError { kind: ErrorKind::CorruptedTable }
    }
}

type SyzygyResult<T> = Result<T, SyzygyError>;

type Pieces = ArrayVec<[Piece; MAX_PIECES]>;

#[derive(Debug)]
pub enum WdlTag { }
#[derive(Debug)]
pub enum DtzTag { }

pub trait IsWdl {
    const IS_WDL: bool;
}

impl IsWdl for WdlTag {
    const IS_WDL: bool = true;
}

impl IsWdl for DtzTag {
    const IS_WDL: bool = false;
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

lazy_static! {
    static ref CONSTS: Consts = Consts::new();
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

const INV_TRIANGLE: [usize; 10] = [1, 2, 3, 10, 11, 19, 0, 9, 18, 27];

struct Consts {
    mult_idx: [[u64; 10]; 5],
    mult_factor: [u64; 5],
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

        for lead_pawns_cnt in 1..=4 {
            for file in 0..4 {
                let mut idx = 0;

                for rank in 1..7 {
                    let sq = Square::from_coords(file as i8, rank).expect("valid coords");
                    if lead_pawns_cnt == 1 {
                        available_squares -= 1;
                        map_pawns[usize::from(sq)] = available_squares;
                        available_squares -= 1;
                        map_pawns[usize::from(sq.mirror_horizontal())] = available_squares;
                    }
                    lead_pawn_idx[lead_pawns_cnt][usize::from(sq)] = idx;
                    idx += binomial(map_pawns[usize::from(sq)], lead_pawns_cnt as u64 - 1);
                }

                lead_pawns_size[lead_pawns_cnt][file] = idx;
            }
        }

        Consts { mult_idx, mult_factor, lead_pawn_idx, lead_pawns_size }
    }
}

fn read_u16_le(data: &[u8], ptr: usize) -> Option<u16> {
    Some(LittleEndian::read_u16(&data.get(ptr..ptr+2)?))
}

fn read_u32_le(data: &[u8], ptr: usize) -> Option<u32> {
    Some(LittleEndian::read_u32(&data.get(ptr..ptr+4)?))
}

fn byte_to_piece(p: u8) -> Option<Piece> {
    let color = Color::from_bool(p & 8 != 0);
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
fn parse_pieces(data: &[u8], side: Color) -> SyzygyResult<Pieces> {
    let mut pieces = Pieces::new();
    for p in data.iter().cloned().take(MAX_PIECES).take_while(|p| *p != 0) {
        pieces.push(byte_to_piece(side.fold(p & 0xf, p >> 4))?);
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
    } else {
        if material.unique_pieces() >= 3 { 3 } else { 2 }
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
        if pieces.len() < 2 {
            return Err(SyzygyError { kind: ErrorKind::CorruptedTable });
        }

        let material = Material::from_iter(pieces.clone());

        // Compute group lengths.
        let lens = group_pieces(&pieces);

        // Compute a factor for each group.
        let pp = material.white.has_pawns() && material.black.has_pawns();
        let mut factors = ArrayVec::from([0, 0, 0, 0, 0, 0, 0]);
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
                    idx *= 31332;
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

        Ok(GroupData { pieces, lens, factors })
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
    block_size: usize,
    /// About every span values there is a sparse index entry.
    span: usize,
    /// Number of blocks in the table.
    blocks_num: u32,

    /// Offset of the symbol table.
    btree: usize,
    /// Minimum length in bits of the Huffman symbols.
    min_symlen: u8,
    /// Offset of the lowest symbols for each length.
    lowest_sym: usize,
    /// 64-bit padded lowest symbols for each length.
    base: Vec<u64>,
    /// Number of values represented by a given Huffman symbol.
    symlen: Vec<u8>,

    /// Offset of the sparse index.
    sparse_index: usize,
    /// Size of the sparse index.
    sparse_index_size: usize,

    /// Offset of the block length table.
    block_lengths: usize,
    /// Size of the block length table, padded to be bigger than `blocks_num`.
    block_length_size: u32,

    /// Start of compressed data.
    data: usize,
}

/// Build the symlen table.
fn read_symlen(data: &[u8], btree: usize, symlen: &mut Vec<u8>, visited: &mut BitVec, sym: u16) -> SyzygyResult<()> {
    if visited.get(sym as usize)? {
        return Ok(());
    }

    let ptr = btree + 3 * sym as usize;
    let left = ((u16::from(*data.get(ptr + 1)?) & 0xf) << 8) | u16::from(*data.get(ptr)?);
    let right = (u16::from(*data.get(ptr + 2)?) << 4) | (u16::from(*data.get(ptr + 1)?) >> 4);

    if right == 0xfff {
        symlen[sym as usize] = 0;
    } else {
        read_symlen(data, btree, symlen, visited, left)?;
        read_symlen(data, btree, symlen, visited, right)?;
        symlen[sym as usize] = symlen[left as usize] + symlen[right as usize] + 1;
    }

    visited.set(sym as usize, true);
    Ok(())
}

impl PairsData {
    pub fn parse(data: &[u8], mut ptr: usize, groups: GroupData) -> SyzygyResult<(PairsData, usize)> {
        let flags = Flag::from_bits_truncate(*data.get(ptr)?);

        if flags.contains(Flag::SINGLE_VALUE) {
            panic!("TODO: Implement SINGLE_VALUE PairsData");
        }

        let tb_size = groups.factors[groups.lens.len()];
        let block_size = 1 << *data.get(ptr + 1)?;
        let span = 1 << *data.get(ptr + 2)?;
        let sparse_index_size = ((tb_size + span as u64 - 1) / span as u64) as usize;
        let padding = *data.get(ptr + 3)?;
        let blocks_num = read_u32_le(data, ptr + 4)?;
        let block_length_size = blocks_num + u32::from(padding);

        let max_symlen = *data.get(ptr + 8)?;
        let min_symlen = *data.get(ptr + 9)?;
        let h = usize::from(max_symlen - min_symlen + 1);
        let lowest_sym = ptr + 10;

        // Initialize base.
        let mut base = vec![0u64; h];
        for i in (0..=h - 2).rev() {
            let ptr = lowest_sym + i * 2;

            base[i] = base[i + 1]
                .checked_add(u64::from(read_u16_le(data, ptr)?))?
                .checked_sub(u64::from(read_u16_le(data, ptr + 2)?))? / 2;

            if base[i] * 2 < base[i + 1] {
                return Err(SyzygyError { kind: ErrorKind::CorruptedTable });
            }
        }

        for i in 0..h {
            base[i] <<= 64 - (min_symlen + i as u8);
        }

        // Initialize symlen.
        ptr += 10 + h * 2;
        let sym = read_u16_le(data, ptr)?;
        ptr += 2;
        let btree = ptr;
        let mut symlen = vec![0; sym as usize];
        let mut visited = BitVec::from_elem(symlen.len(), false);
        for s in 0..sym {
           read_symlen(data, btree, &mut symlen, &mut visited, s)?;
        }
        ptr += symlen.len() * 3 + (symlen.len() & 1);

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
        }, ptr))
    }
}

/// Descripton of encoding and compression for both sides of a table.
#[derive(Debug)]
struct FileData {
    sides: ArrayVec<[PairsData; 2]>,
}

/// A Syzygy table.
#[derive(Debug)]
pub struct Table<'a, T: IsWdl, P: Position + Syzygy> {
    is_wdl: PhantomData<T>,
    syzygy: PhantomData<P>,

    data: &'a [u8],

    key: Material,

    num_pieces: u8,
    num_unique_pieces: u8,
    min_like_man: u8,
    files: ArrayVec<[FileData; 4]>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(i8)]
pub enum Wdl {
    Loss = -2,
    BlessedLoss = -1,
    Draw = 0,
    CursedWin = 1,
    Win = 2,
}

impl<'a, T: IsWdl, P: Position + Syzygy> Table<'a, T, P> {
    pub fn new(data: &[u8]) -> SyzygyResult<Table<T, P>> {
        // Check magic.
        let (magic, pawnless_magic) = if T::IS_WDL {
            (&P::WDL_MAGIC, &P::PAWNLESS_WDL_MAGIC)
        } else {
            (&P::DTZ_MAGIC, &P::PAWNLESS_DTZ_MAGIC)
        };

        if !data.starts_with(magic) && !data.starts_with(pawnless_magic) {
            return Err(SyzygyError { kind: ErrorKind::Magic });
        }

        // Read layout flags.
        let layout = Layout::from_bits_truncate(*data.get(4)?);
        let has_pawns = layout.contains(Layout::HAS_PAWNS);
        let split = layout.contains(Layout::SPLIT);

        // Read material key.
        let key = Material::from_iter(parse_pieces(data.get(6..)?, Color::White)?);

        // Check magic again.
        if key.has_pawns() && !data.starts_with(magic) {
            return Err(SyzygyError { kind: ErrorKind::Magic });
        }

        // Check consistency of layout and material key.
        if has_pawns != key.has_pawns() || split == key.is_symmetric() {
            return Err(SyzygyError { kind: ErrorKind::CorruptedTable });
        }

        // Read group data.
        let pp = key.white.has_pawns() && key.black.has_pawns();
        let num_files = if has_pawns { 4 } else { 1 };
        let num_sides = if T::IS_WDL && !key.is_symmetric() { 2 } else { 1 };

        let mut groups: ArrayVec<[ArrayVec<[GroupData; 2]>; 4]> = ArrayVec::new();
        let mut ptr = 5;

        for file in 0..num_files {
            let mut sides = ArrayVec::new();

            let order = [
                [*data.get(ptr)? & 0xf, if pp { *data.get(ptr + 1)? & 0xf } else { 0xf }],
                [*data.get(ptr)? >> 4, if pp { *data.get(ptr + 1)? >> 4 } else { 0xf }],
            ];

            ptr += 1 + if pp { 1 } else { 0 };

            for side in [Color::White, Color::Black].iter().take(num_sides) {
                let pieces = parse_pieces(&data.get(ptr..)?, *side)?;
                let group = GroupData::new::<P>(pieces, &order[side.fold(0, 1)], file)?;
                sides.push(group);
            }

            ptr += key.count() as usize;

            groups.push(sides);
        }

        ptr += ptr & 1;

        let mut files: ArrayVec<[FileData; 4]> = ArrayVec::new();

        for f in 0..num_files {
            let mut sides = ArrayVec::new();

            for side in [Color::White, Color::Black].iter().take(num_sides) {
                let group = groups[f][side.fold(0, 1)].clone();
                let (pairs, next_ptr) = PairsData::parse(&data, ptr, group)?;

                if !T::IS_WDL {
                    panic!("TODO: Antichess");
                }

                sides.push(pairs);
                ptr = next_ptr;
            }

            files.push(FileData { sides });
        }

        if !T::IS_WDL {
            panic!("TODO: set dtz map");
        }

        for f in 0..num_files {
            for s in 0..num_sides {
                files[f].sides[s].sparse_index = ptr;
                ptr += files[f].sides[s].sparse_index_size * 6;
            }
        }

        for f in 0..num_files {
            for s in 0..num_sides {
                files[f].sides[s].block_lengths = ptr;
                ptr += files[f].sides[s].block_length_size as usize * 2;
            }
        }

        for f in 0..num_files {
            for s in 0..num_sides {
                ptr = (ptr + 0x3f) & !0x3f; // Check 64 byte alignment
                files[f].sides[s].data = ptr;
                ptr += files[f].sides[s].blocks_num as usize * files[f].sides[s].block_size;
            }
        }

        // Result.
        Ok(Table {
            is_wdl: PhantomData,
            syzygy: PhantomData,
            data,
            num_pieces: key.count(),
            num_unique_pieces: key.unique_pieces(),
            min_like_man: key.min_like_man(),
            files,
            key,
        })
    }

    fn decompress_pairs(&self, d: &PairsData, idx: u64) -> SyzygyResult<u8> {
        if d.flags.contains(Flag::SINGLE_VALUE) {
            return Ok(d.min_symlen);
        }

        let k = (idx / d.span as u64) as usize;

        let mut block = read_u32_le(self.data, d.sparse_index + 6 * k)? as usize;
        let mut offset = i64::from(read_u16_le(self.data, d.sparse_index + 6 * k + 4)?);

        let diff = idx as i64 % d.span as i64 - d.span as i64 / 2;
        offset += diff;

        while offset < 0 {
            block -= 1;
            offset += i64::from(read_u16_le(self.data, d.block_lengths + block * 2)?) + 1;
        }

        while offset > i64::from(read_u16_le(self.data, d.block_lengths + block * 2)?) {
            offset -= i64::from(read_u16_le(self.data, d.block_lengths + block * 2)?) + 1;
            block += 1;
        }

        let mut ptr = d.data + block * d.block_size;

        let mut buf = BigEndian::read_u64(self.data.get(ptr..ptr + 8)?);
        ptr += 8;
        let mut buf_size = 64;

        let mut sym;

        loop {
            let mut len = 0;

            while buf < d.base[len] {
                len += 1;
            }

            sym = ((buf - d.base[len]) >> (64 - len - d.min_symlen as usize)) as u16;
            sym += read_u16_le(self.data, d.lowest_sym + 2 * len)?;

            if offset < i64::from(*d.symlen.get(sym as usize)?) + 1 {
                break;
            }

            offset -= i64::from(*d.symlen.get(sym as usize)?) + 1;
            len += usize::from(d.min_symlen);
            buf <<= len;
            buf_size -= len;

            // Refill the buffer.
            if buf_size <= 32 {
                buf_size += 32;
                buf |= u64::from(BigEndian::read_u32(self.data.get(ptr..ptr + 4)?)) << (64 - buf_size);
                ptr += 4;
            }
        }

        while *d.symlen.get(sym as usize)? != 0 {
            let w = d.btree + 3 * sym as usize;
            let left = (u16::from(*self.data.get(w + 2)?) << 4) | (u16::from(*self.data.get(w + 1)?) >> 4);

            if offset < i64::from(*d.symlen.get(left as usize)?) + 1 {
                sym = left as u16;
            } else {
                offset -= i64::from(*d.symlen.get(left as usize)?) + 1;
                sym = ((u16::from(*self.data.get(w + 1)?) & 0xf) << 8) | u16::from(*self.data.get(w)?);
            }
        }

        Ok(*self.data.get(d.btree + 3 * sym as usize)?)
    }

    fn encode(&self, pos: &P) -> SyzygyResult<(&PairsData, u64)> {
        let key = Material::from_board(pos.board());

        let symmetric_btm = self.key.is_symmetric() && pos.turn().is_black();
        let black_stronger = key != self.key;
        let stm = Color::from_bool((symmetric_btm || black_stronger) ^ pos.turn().is_black());

        assert!(!key.has_pawns());

        let side = &self.files[0].sides[stm.fold(0, 1)];

        let mut squares: ArrayVec<[Square; MAX_PIECES]> = ArrayVec::new();

        let mut used = Bitboard(0);
        for piece in &side.groups.pieces {
            let color = Color::from_bool(piece.color.is_white() ^ (symmetric_btm || black_stronger));
            let square = (pos.board().by_piece(piece.role.of(color)) & !used).first().expect("piece exists");
            squares.push(square);
            used.add(square);
        }

        if squares[0].file() >= 4 {
            for square in &mut squares {
                *square = square.mirror_horizontal();
            }
        }

        assert!(!key.has_pawns());

        if squares[0].rank() >= 4 {
            for square in &mut squares {
                *square = square.mirror_vertical();
            }
        }

        for i in 0..usize::from(side.groups.lens[0]) {
            if squares[i].file() == squares[i].rank() {
                continue;
            }

            if squares[i].rank() > squares[i].file() {
                for square in &mut squares[i..] {
                    *square = square.mirror_diagonal();
                }
            }

            break;
        }

        let mut idx = if self.num_unique_pieces > 2 {
            let adjust1 = if squares[1] > squares[0] { 1 } else { 0 };
            let adjust2 = if squares[2] > squares[0] { 1 } else { 0 } + if squares[2] > squares[1] { 1 } else { 0 };

            if offdiag(squares[0]) {
                TRIANGLE[usize::from(squares[0])] * 63 * 62 +
                (u64::from(squares[1]) - adjust1) * 62 +
                (u64::from(squares[2]) - adjust2)
            } else {
                panic!("TODO: enc 0 not fully implemented");
            }
        } else {
            panic!("TODO: mapkk not implemented");
        };

        idx *= side.groups.factors[0];

        let mut remaining_pawns = false;
        let mut next = 1;
        let mut group_sq = side.groups.lens[0];
        for lens in side.groups.lens.iter().cloned().skip(1) {
            let (prev_squares, group_squares) = squares.split_at_mut(usize::from(group_sq));
            let group_squares = &mut group_squares[..(usize::from(lens))];
            group_squares.sort();

            let mut n = 0;

            for i in 0..usize::from(lens) {
                let adjust = prev_squares[..usize::from(group_sq)].iter().filter(|sq| group_squares[i] > **sq).count() as u64;
                n += binomial(u64::from(group_squares[i]) - adjust - if remaining_pawns { 8 } else { 0 }, i as u64 + 1);
            }

            remaining_pawns = false;
            idx += n * side.groups.factors[next];
            group_sq += side.groups.lens[next];
            next += 1;
        }

        Ok((side, idx))
    }

    pub fn probe_wdl_table(&self, pos: &P) -> SyzygyResult<Wdl> {
        let (side, idx) = self.encode(pos)?;

        Ok(match self.decompress_pairs(side, idx)? {
            0 => Wdl::Loss,
            1 => Wdl::BlessedLoss,
            2 => Wdl::Draw,
            3 => Wdl::CursedWin,
            4 => Wdl::Win,
            _ => return Err(SyzygyError { kind: ErrorKind::CorruptedTable }),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use memmap::{Mmap, Protection};

    use shakmaty::fen::Fen;
    use shakmaty::Chess;

    #[test]
    fn test_table() {
        let mmap = Mmap::open_path("KQvKR.rtbw", Protection::Read).expect("mmap");
        let bytes = unsafe { mmap.as_slice() };
        let table = Table::<WdlTag, _>::new(bytes).expect("good table");

        let fen: Fen = "4kr2/8/Q7/8/8/8/8/4K3 w - - 0 1".parse().expect("valid fen");
        let pos: Chess = fen.position().expect("legal position");

        let result = table.probe_wdl_table(&pos);
        assert_eq!(result, Ok(Wdl::Win));
    }
}
