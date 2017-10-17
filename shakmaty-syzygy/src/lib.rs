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

extern crate arrayvec;
#[macro_use] extern crate bitflags;
extern crate memmap;
extern crate shakmaty;
extern crate num_integer;

mod material;

use std::fmt;
use std::error::Error;
use std::marker::PhantomData;

use arrayvec::ArrayVec;
use num_integer::binomial;
use shakmaty::{Color, Piece, Square, Bitboard, Position, Chess};

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
    Todo,
}

impl SyzygyError {
    fn desc(&self) -> &str {
        match self.kind {
            ErrorKind::CorruptedTable => "corrupted table",
            ErrorKind::Todo => "not yet implemented",
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
    pub fn new(side: Color, data: &[u8]) -> Result<GroupData, SyzygyError> {
        let mut material = Material::new();
        let mut pieces = ArrayVec::new();

        let order = side.fold(data[0] >> 4, data[0] & 0xf);
        let order2 = side.fold(data[0] & 0xf, data[0] >> 4);

        // initialize pieces
        for p in data[1..].iter().cloned().take(MAX_PIECES).take_while(|p| *p != 0) {
            match byte_to_piece(side.fold(p >> 4, p & 0xf)) {
                Some(piece) => {
                    *material.by_piece_mut(piece) += 1;
                    pieces.push(piece);
                }
                None => return Err(SyzygyError { kind: ErrorKind::CorruptedTable }),
            }
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
        while next < group_len.len() || k == order || k == order2 {
            if k == order {
                println!("leading {}", idx);
                group_idx[0] = idx;
                assert!(!material.has_pawns());
                idx *= if material.unique_pieces() > 2 { 31332 } else { 462 };
            } else if k == order2 {
                group_idx[1] = idx;
                idx *= binomial(48 - u64::from(group_len[0]), u64::from(group_len[1]));
            } else {
                println!("remaining pieces {}", idx);
                group_idx[next] = idx;
                println!("binom({}, {})", group_len[next], free_squares);
                idx *= binomial(free_squares, u64::from(group_len[next]));
                free_squares -= u64::from(group_len[next]);
                next += 1;
            }
            k += 1;
        }

        group_idx[group_len.len()] = idx;

        println!("group idx {:?}", group_idx);

        Ok(GroupData { pieces, group_len, group_idx })
    }
}

#[derive(Debug)]
pub struct Table<P: Position + Syzygy> {
    key: Material,
    num_pieces: u8,
    num_unique_pieces: u8,
    min_like_man: u8,
    files: ArrayVec<[FileData; 4]>,
    syzygy: PhantomData<P>,
}

#[derive(Debug)]
struct FileData {
    sides: ArrayVec<[GroupData; 2]>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Wdl {
    Loss = -2,
    BlessedLoss = -1,
    Draw = 0,
    CursedWin = 1,
    Win = 2,
}

impl<P: Position + Syzygy> Table<P> {
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
            return Err(SyzygyError { kind: ErrorKind::Todo });
        } else {
            let mut sides = ArrayVec::new();
            sides.push(GroupData::new(Color::Black, &data[5..])?);
            sides.push(GroupData::new(Color::White, &data[5..])?);
            files.push(FileData { sides });
        }

        Ok(Table {
            num_pieces: key.count(),
            num_unique_pieces: key.unique_pieces(),
            min_like_man: key.min_like_man(),
            key,
            files,
            syzygy: PhantomData
        })
    }

    pub fn probe_wdl_table(self, pos: &P) -> Result<Wdl, SyzygyError> {
        let key = Material::from_board(pos.board());

        let symmetric_btm = self.key.is_symmetric() && pos.turn().is_black();
        let black_stronger = key != self.key;
        let stm = Color::from_bool((symmetric_btm || black_stronger) ^ pos.turn().is_white());

        assert!(!key.has_pawns());

        let side = &self.files[0].sides[stm.fold(0, 1)];

        println!("{:?}", side);

        let mut squares: ArrayVec<[Square; MAX_PIECES]> = ArrayVec::new();

        let mut used = Bitboard(0);
        for piece in &side.pieces {
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

        for i in 0..usize::from(side.group_len[0]) {
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

        println!("index before remaining: {:?}", idx);
        idx *= side.group_idx[0];

        let mut remaining_pawns = false;
        let mut next = 1;
        let mut group_sq = side.group_len[0];
        for group_len in side.group_len.iter().cloned().skip(1) {
            let (prev_squares, group_squares) = squares.split_at_mut(usize::from(group_sq));
            let group_squares = &mut group_squares[..(usize::from(group_len))];
            group_squares.sort();

            let mut n = 0;

            for i in 0..usize::from(group_len) {
                let adjust = prev_squares[..usize::from(group_sq)].iter().filter(|sq| group_squares[i] > **sq).count() as u64;
                n += binomial(u64::from(group_squares[i]) - adjust - if remaining_pawns { 8 } else { 0 }, i as u64 + 1);
            }

            remaining_pawns = false;
            idx += n * side.group_idx[next];
            group_sq += side.group_len[next];
            next += 1;
        }

        println!("idx: {:?}", idx);

        Ok(Wdl::Draw)
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
        println!("result: {:?}", result);
        panic!("debugging ...");
    }
}
