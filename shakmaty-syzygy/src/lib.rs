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
extern crate memmap;
extern crate shakmaty;
extern crate num_integer;
extern crate byteorder;

mod material;

use std::fmt;
use std::error::Error;
use std::marker::PhantomData;
use std::option::NoneError;

use arrayvec::ArrayVec;
use num_integer::binomial;
use shakmaty::{Color, Piece, Square, Bitboard, Position, Chess};
use byteorder::{LittleEndian, BigEndian, ByteOrder};

pub use material::{Material, MaterialSide};

pub trait Syzygy {
    const WDL_MAGIC: [u8; 4];
    const ONE_KING: bool;
}

impl Syzygy for Chess {
    const WDL_MAGIC: [u8; 4] = [0x71, 0xe8, 0x23, 0x5d];
    const ONE_KING: bool = true;
}

const MAX_PIECES: usize = 6;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyzygyError {
    kind: ErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ErrorKind {
    CorruptedTable,
}

impl SyzygyError {
    fn desc(&self) -> &str {
        match self.kind {
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

bitflags! {
    struct Flag: u8 {
        const STM = 1;
        const MAPPED = 2;
        const WIN_PLIES = 4;
        const LOSS_PLIES = 8;
        const SINGLE_VALUE = 128;
    }
}

bitflags! {
    struct Layout: u8 {
        const SPLIT = 1;
        const HAS_PAWNS = 2;
    }
}

fn byte_to_piece(p: u8) -> Option<Piece> {
    let color = Color::from_bool(p & 8 == 0);
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

fn offdiag(sq: Square) -> bool {
    sq.file() != sq.rank()
}

#[derive(Debug)]
struct GroupData {
    pieces: ArrayVec<[Piece; MAX_PIECES]>,
    group_len: ArrayVec<[u8; MAX_PIECES]>,
    group_idx: [u64; MAX_PIECES],
}

impl GroupData {
    pub fn parse(data: &[u8], ptr: usize, side: Color) -> SyzygyResult<GroupData> {
        let mut order = [data.get(ptr)? >> 4, data.get(ptr)? & 0xf];
        if side.is_black() {
            order.reverse();
        }

        // Initialize pieces.
        let mut pieces = ArrayVec::new();
        let mut material = Material::new();
        for p in data.get(ptr + 1..)?.iter().cloned().take(MAX_PIECES).take_while(|p| *p != 0) {
            let piece = byte_to_piece(side.fold(p >> 4, p & 0xf))?;
            pieces.push(piece);
            *material.by_piece_mut(piece) += 1;
        }

        // initialize group_len
        let mut group_len: ArrayVec<[u8; MAX_PIECES]> = ArrayVec::new();
        let mut first_len = if material.has_pawns() { 0 } else { if material.unique_pieces() > 2 { 3 } else { 2 } };
        group_len.push(1);
        for window in pieces.windows(2) {
            if first_len > 0 { first_len -= 1 };
            if first_len > 0 || window[0] == window[1] {
                let len = group_len.len();
                group_len[len - 1] += 1;
            } else {
                group_len.push(1);
            }
        }

        // initialize group_idx
        let mut group_idx = [0u64; MAX_PIECES];
        let pp = material.white.has_pawns() && material.black.has_pawns();
        let mut next = if pp { 2 } else { 1 };
        let mut free_squares = 64 - u64::from(group_len[0]) - if pp { u64::from(group_len[1]) } else { 0 };
        let mut idx = 1;

        let mut k = 0;
        while next < group_len.len() || k == order[0] || k == order[1] {
            if k == order[0] {
                group_idx[0] = idx;
                assert!(!material.has_pawns());
                idx *= if material.unique_pieces() > 2 { 31332 } else { 462 };
            } else if k == order[1] {
                group_idx[1] = idx;
                idx *= binomial(48 - u64::from(group_len[0]), u64::from(group_len[1]));
            } else {
                group_idx[next] = idx;
                idx *= binomial(free_squares, u64::from(group_len[next]));
                free_squares -= u64::from(group_len[next]);
                next += 1;
            }
            k += 1;
        }

        group_idx[group_len.len()] = idx;

        Ok(GroupData { pieces, group_len, group_idx })
    }
}

#[derive(Debug)]
struct PairsData {
    flags: Flag,
    groups: GroupData,
    min_sym_len: u8,
    lowest_sym: usize,
    btree: usize,
    base_64: Vec<u64>,
    symlen: Vec<u8>,
    span: u64,
    sparse_index: usize,
    sparse_index_size: u64,
    block_lengths: usize,
    block_length_size: u32,
    block_size: usize,
    data: usize,
    blocks_num: u32,
}

fn calc_symlen(data: &[u8], symlen: &mut Vec<u8>, visited: &mut Vec<bool>, btree: usize, s: usize) {
    let w = btree + 3 * s;
    let sr = ((u16::from(data[w + 2]) << 4) | (u16::from(data[w + 1]) >> 4)) as usize;
    if sr == 0xfff {
        symlen[s] = 0;
    } else {
        let sl = (((u16::from(data[w + 1]) & 0xf) << 8) | u16::from(data[w])) as usize;
        if !visited[sl] {
            calc_symlen(data, symlen, visited, btree, sl);
        }
        if !visited[sr] {
            calc_symlen(data, symlen, visited, btree, sr);
        }
        symlen[s] = symlen[sl] + symlen[sr] + 1;
    }
    visited[s] = true;
}

impl PairsData {
    pub fn new(data: &[u8], mut ptr: usize, groups: GroupData) -> Result<(PairsData, usize), SyzygyError> {
        let flags = Flag::from_bits_truncate(data[ptr]);

        if flags.contains(Flag::SINGLE_VALUE) {
            panic!("single value not yet implemented");
        }

        let tb_size = groups.group_idx[groups.group_len.len()];
        let block_size = 1 << data[ptr + 1];
        let span = 1 << data[ptr + 2];
        let sparse_index_size = (tb_size + span - 1) / span;
        let padding = data[ptr + 3];
        let blocks_num = LittleEndian::read_u32(&data[ptr + 4..]);
        let block_length_size = blocks_num + u32::from(padding);

        let max_sym_len = data[ptr + 8];
        let min_sym_len = data[ptr + 9];
        let h = usize::from(max_sym_len - min_sym_len + 1);
        let lowest_sym = ptr + 10;
        //let num_syms = data[ptr + 10 + 2 * h];
        let mut base_64 = vec![0; h];

        for i in (0..=h - 2).rev() {
            base_64[i] = ((base_64[i + 1] + u64::from(LittleEndian::read_u16(&data[lowest_sym + i * 2..])))
                                          - u64::from(LittleEndian::read_u16(&data[lowest_sym + i * 2 + 2..]))) / 2;
            assert!(base_64[i] * 2 >= base_64[i + 1]);
        }

        for i in 0..h {
            base_64[i] <<= 64 - (min_sym_len + i as u8);
        }

        ptr += 10 + h * 2;
        let mut symlen = vec![0; LittleEndian::read_u16(&data[ptr..]) as usize];
        ptr += 2;
        let btree = ptr;

        let mut visited = vec![false; symlen.len()];
        for s in 0..symlen.len() {
            if !visited[s] {
                calc_symlen(data, &mut symlen, &mut visited, btree, s);
            }
        }

        let next_ptr = ptr + symlen.len() * 3 + (symlen.len() & 1);
        let pairs = PairsData {
            flags,
            groups,
            block_size,
            lowest_sym,
            min_sym_len,
            btree,
            base_64,
            span,
            sparse_index: 0,
            sparse_index_size,
            block_lengths: 0,
            block_length_size,
            symlen,
            data: 0,
            blocks_num,
        };

        Ok((pairs, next_ptr))
    }
}

#[derive(Debug)]
pub struct Table<'a, P: Position + Syzygy> {
    key: Material,
    num_pieces: u8,
    num_unique_pieces: u8,
    min_like_man: u8,
    files: ArrayVec<[FileData; 4]>,
    data: &'a [u8],
    syzygy: PhantomData<P>,
}

#[derive(Debug)]
struct FileData {
    sides: ArrayVec<[PairsData; 2]>,
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

impl<'a, P: Position + Syzygy> Table<'a, P> {
    pub fn new(data: &[u8]) -> Result<Table<P>, SyzygyError> {
        // Check magic.
        assert!(data.starts_with(&P::WDL_MAGIC));

        // Read layout flags.
        let layout = Layout::from_bits_truncate(data[4]);
        let has_pawns = layout.contains(Layout::HAS_PAWNS);
        let split = layout.contains(Layout::SPLIT);

        // Read material key.
        let mut key = Material::new();
        for p in data[6..].iter().cloned().take(MAX_PIECES).take_while(|&p| p != 0) {
            match byte_to_piece(p & 0xf) {
                Some(piece) => *key.by_piece_mut(piece) += 1,
                None => return Err(SyzygyError { kind: ErrorKind::CorruptedTable }),
            }
        }

        // Check consistency of layout and material key.
        assert!(has_pawns == key.has_pawns());
        assert!(split != key.is_symmetric());

        let mut files = ArrayVec::new();

        if has_pawns {
            panic!("not yet implemented");
        } else {
            let group = GroupData::parse(&data, 5, Color::Black)?;

            let mut ptr = 5 + group.pieces.len() + 1;
            ptr += ptr & 0x1;

            let mut sides: ArrayVec<[PairsData; 2]> = ArrayVec::new();
            let (pairs, ptr) = PairsData::new(data, ptr, group)?;
            sides.push(pairs);
            let group = GroupData::parse(&data, 5, Color::White)?;
            let (pairs, mut ptr) = PairsData::new(&data, ptr, group)?;
            sides.push(pairs);

            sides[0].sparse_index = ptr;
            ptr += sides[0].sparse_index_size as usize * 6;
            sides[1].sparse_index = ptr;
            ptr += sides[1].sparse_index_size as usize * 6;

            sides[0].block_lengths = ptr;
            ptr += sides[0].block_length_size as usize * 2;
            sides[1].block_lengths = ptr;
            ptr += sides[1].block_length_size as usize * 2;

            ptr = (ptr + 0x3f) & !0x3f;
            sides[0].data = ptr;
            ptr += sides[0].blocks_num as usize * sides[0].block_size;
            ptr = (ptr + 0x3f) & !0x3f;
            sides[1].data = ptr;

            //let (pairs, _data) = PairsData::new(&next_data[ptr..], GroupData::new(Color::Black, &data[5..])?)?;
            //sides.push(pairs);
            files.push(FileData { sides });
        }

        Ok(Table {
            num_pieces: key.count(),
            num_unique_pieces: key.unique_pieces(),
            min_like_man: key.min_like_man(),
            key,
            files,
            data,
            syzygy: PhantomData
        })
    }

    fn decompress_pairs(&self, d: &PairsData, idx: u64) -> u8 {
        if d.flags.contains(Flag::SINGLE_VALUE) {
            return d.min_sym_len;
        }

        let k = (idx / d.span) as usize;

        let mut block = LittleEndian::read_u32(&self.data[d.sparse_index + 6 * k..]) as usize;
        let mut offset = i64::from(LittleEndian::read_u16(&self.data[d.sparse_index + 6 * k + 4..]));

        let diff = idx as i64 % d.span as i64 - d.span as i64 / 2;
        offset += diff;

        while offset < 0 {
            block -= 1;
            offset += i64::from(LittleEndian::read_u16(&self.data[d.block_lengths + block * 2..])) + 1;
        }

        while offset > i64::from(LittleEndian::read_u16(&self.data[d.block_lengths + block * 2..])) {
            offset -= i64::from(LittleEndian::read_u16(&self.data[d.block_lengths + block * 2..])) + 1;
            block += 1;
        }

        let mut ptr = d.data + block * d.block_size;

        let mut buf_64 = BigEndian::read_u64(&self.data[ptr..]);
        ptr += 8;
        let mut buf_64_size = 64;

        let mut sym;

        loop {
            let mut len = 0;

            while buf_64 < d.base_64[len] {
                len += 1;
            }

            sym = ((buf_64 - d.base_64[len]) >> (64 - len - d.min_sym_len as usize)) as u16;
            sym += LittleEndian::read_u16(&self.data[d.lowest_sym + 2 * len..]);

            if offset < i64::from(d.symlen[sym as usize]) + 1 {
                break;
            }

            offset -= i64::from(d.symlen[sym as usize]) + 1;
            len += usize::from(d.min_sym_len);
            buf_64 <<= len;
            buf_64_size -= len;

            if buf_64_size <= 32 {
                buf_64_size += 32;
                buf_64 |= u64::from(BigEndian::read_u32(&self.data[ptr..])) << (64 - buf_64_size);
                ptr += 4;
            }
        }

        println!("sym: {}", sym);

        while d.symlen[sym as usize] != 0 {
            let w = d.btree + 3 * sym as usize;
            let left = ((u16::from(self.data[w + 2]) << 4) | (u16::from(self.data[w + 1]) >> 4)) as usize;

            if offset < i64::from(d.symlen[left]) + 1 {
                sym = left as u16;
            } else {
                offset -= i64::from(d.symlen[left]) + 1;
                sym = ((u16::from(self.data[w + 1]) & 0xf) << 8) | u16::from(self.data[w]);
            }
        }

        return self.data[d.btree + 3 * sym as usize];
    }

    pub fn probe_wdl_table(self, pos: &P) -> Result<Wdl, SyzygyError> {
        let key = Material::from_board(pos.board());

        let symmetric_btm = self.key.is_symmetric() && pos.turn().is_black();
        let black_stronger = key != self.key;
        let stm = Color::from_bool((symmetric_btm || black_stronger) ^ pos.turn().is_white());

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

        for i in 0..usize::from(side.groups.group_len[0]) {
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
                panic!("enc 0 not fully implemented");
            }
        } else {
            panic!("mapkk not implemented");
        };

        idx *= side.groups.group_idx[0];

        let mut remaining_pawns = false;
        let mut next = 1;
        let mut group_sq = side.groups.group_len[0];
        for group_len in side.groups.group_len.iter().cloned().skip(1) {
            let (prev_squares, group_squares) = squares.split_at_mut(usize::from(group_sq));
            let group_squares = &mut group_squares[..(usize::from(group_len))];
            group_squares.sort();

            let mut n = 0;

            for i in 0..usize::from(group_len) {
                let adjust = prev_squares[..usize::from(group_sq)].iter().filter(|sq| group_squares[i] > **sq).count() as u64;
                n += binomial(u64::from(group_squares[i]) - adjust - if remaining_pawns { 8 } else { 0 }, i as u64 + 1);
            }

            remaining_pawns = false;
            idx += n * side.groups.group_idx[next];
            group_sq += side.groups.group_len[next];
            next += 1;
        }

        println!("idx: {:?}", idx);
        Ok(match self.decompress_pairs(side, idx) {
            0 => Wdl::Loss,
            1 => Wdl::BlessedLoss,
            2 => Wdl::Draw,
            3 => Wdl::CursedWin,
            4 => Wdl::Win,
            _ => unreachable!("invalid decompressed wdl"),
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
        let table = Table::new(bytes).expect("good table");

        let fen: Fen = "4kr2/8/Q7/8/8/8/8/4K3 w - - 0 1".parse().expect("valid fen");
        let pos: Chess = fen.position().expect("legal position");

        let result = table.probe_wdl_table(&pos);
        assert_eq!(result, Ok(Wdl::Win));
    }
}
