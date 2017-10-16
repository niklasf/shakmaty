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

mod material;

use std::fmt;
use std::error::Error;
use std::marker::PhantomData;

use arrayvec::ArrayVec;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyzygyError {
    kind: ErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ErrorKind {
    Material,
    CorruptedTable,
    Todo,
}

impl SyzygyError {
    fn desc(&self) -> &str {
        match self.kind {
            ErrorKind::Material => "material signature mismatch",
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

#[derive(Debug)]
struct PairsData {
    pieces: ArrayVec<[Piece; MAX_PIECES]>,
    group_len: ArrayVec<[u8; MAX_PIECES]>,
}

impl PairsData {
    pub fn new(side: Color, data: &[u8]) -> Result<PairsData, SyzygyError> {
        let mut material = Material::new();
        let mut pieces = ArrayVec::new();

        let order = side.fold(data[0] >> 4, data[0] & 0xf);
        let order2 = side.fold(data[0] & 0xf, data[0] >> 4);

        for p in data[1..].iter().cloned().take(MAX_PIECES).take_while(|p| *p != 0) {
            match byte_to_piece(side.fold(p >> 4, p & 0xf)) {
                Some(piece) => {
                    *material.by_piece_mut(piece) += 1;
                    pieces.push(piece);
                }
                None => return Err(SyzygyError { kind: ErrorKind::CorruptedTable }),
            }
        }

        let mut group_len = ArrayVec::new();
        let mut first_len = if material.has_pawns() { 0 } else { if material.unique_pieces() > 0 { 3 } else { 2 } };
        group_len.push(1);
        for window in pieces.windows(2) {
            if first_len > 1 { first_len -= 1 };
            if first_len > 0 || window[0] == window[1] {
                let len = group_len.len();
                group_len[len - 1] += 1;
            } else {
                group_len.push(1);
            }
        }

        Ok(PairsData { pieces, group_len })
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
    sides: ArrayVec<[PairsData; 2]>,
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
            sides.push(PairsData::new(Color::Black, &data[5..])?);
            sides.push(PairsData::new(Color::White, &data[5..])?);
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
