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

#[macro_use]
extern crate bitflags;
extern crate memmap;
extern crate shakmaty;

use std::fmt;
use std::error::Error;

use shakmaty::{Color, Role, Piece};

const MAX_PIECES: usize = 6;

#[derive(Debug, Clone, PartialEq, Eq)]
struct SyzygyError {
    _priv: (),
}

impl SyzygyError {
    fn desc(&self) -> &str {
        "corrupted syzygy table"
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

#[derive(Debug, Default, Eq, PartialEq, Hash)]
pub struct MaterialSide {
    kings: u8,
    queens: u8,
    rooks: u8,
    bishops: u8,
    knights: u8,
    pawns: u8,
}

impl MaterialSide {
    pub fn new() -> MaterialSide {
        MaterialSide::default()
    }

    pub fn by_role(&self, role: Role) -> u8 {
        match role {
            Role::Pawn => self.pawns,
            Role::Knight => self.knights,
            Role::Bishop => self.bishops,
            Role::Rook => self.rooks,
            Role::Queen => self.queens,
            Role::King => self.kings,
        }
    }

    pub fn by_role_mut(&mut self, role: Role) -> &mut u8 {
        match role {
            Role::Pawn => &mut self.pawns,
            Role::Knight => &mut self.knights,
            Role::Bishop => &mut self.bishops,
            Role::Rook => &mut self.rooks,
            Role::Queen => &mut self.queens,
            Role::King => &mut self.kings,
        }
    }
}

impl fmt::Display for MaterialSide {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for &role in &[Role::King, Role::Queen, Role::Rook, Role::Bishop, Role::Knight, Role::Pawn] {
            write!(f, "{}", role.char().to_uppercase().to_string().repeat(self.by_role(role) as usize))?;
        }
        Ok(())
    }
}

#[derive(Debug, Default, Eq, PartialEq, Hash)]
pub struct Material {
    white: MaterialSide,
    black: MaterialSide,
}

impl Material {
    pub fn new() -> Material {
        Material::default()
    }

    pub fn by_color(&self, color: Color) -> &MaterialSide {
        match color {
            Color::Black => &self.black,
            Color::White => &self.white,
        }
    }

    pub fn by_color_mut(&mut self, color: Color) -> &mut MaterialSide {
        match color {
            Color::Black => &mut self.black,
            Color::White => &mut self.white,
        }
    }

    pub fn by_piece(&self, piece: Piece) -> u8 {
        self.by_color(piece.color).by_role(piece.role)
    }

    pub fn by_piece_mut(&mut self, piece: Piece) -> &mut u8 {
        self.by_color_mut(piece.color).by_role_mut(piece.role)
    }
}

impl fmt::Display for Material {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}v{}", self.white, self.black)
    }
}

struct PairsData {
    pieces: [Piece; MAX_PIECES],
}

impl PairsData {
    pub fn new(data: &[u8]) -> Result<(), SyzygyError> {
        let order = data[0] & 0x0f;

        let mut key = Material::new();
        let mut mirrored_key = Material::new();

        for p in data[1..].iter().cloned().take(MAX_PIECES).take_while(|p| *p != 0) {
            match byte_to_piece(p & 0x0f) {
                Some(piece) => *key.by_piece_mut(piece) += 1,
                None => return Err(SyzygyError { _priv: () }),
            }

            match byte_to_piece(p >> 4) {
                Some(piece) => *mirrored_key.by_piece_mut(piece) += 1,
                None => return Err(SyzygyError { _priv: () }),
            }
        }

        println!("{}", key);
        println!("{}", mirrored_key);

        Ok(())
    }
}

struct Table {
    material: Material,
    unique_pieces: u8,
    piece_entry: PairsData,
}

impl Table {
    pub fn new(data: &[u8]) {
        assert!(data.starts_with(b"\x71\xe8\x23\x5d"));
        let layout = Layout::from_bits_truncate(data[4]);
        assert!(!layout.contains(Layout::HAS_PAWNS));
        PairsData::new(&data[5..]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use memmap::{Mmap, Protection};

    #[test]
    fn test_table() {
        let mmap = Mmap::open_path("KQvKR.rtbw", Protection::Read).expect("mmap");
        let bytes = unsafe { mmap.as_slice() };
        Table::new(bytes);
        panic!("debugging ...");
    }
}
