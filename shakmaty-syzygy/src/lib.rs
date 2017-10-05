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

bitflags! {
    struct Flag: u8 {
        const STM = 1;
        const MAPPED = 2;
        const WIN_PLIES = 4;
        const LOSS_PLIES = 8;
        const SINGLE_VALUE = 128;
    }
}

struct MaterialSide {
    kings: u8,
    queens: u8,
    rooks: u8,
    bishops: u8,
    knights: u8,
    pawns: u8,
}

struct Material {
    white: MaterialSide,
    black: MaterialSide,
}

struct PairsData {

}

struct Table {
    material: Material,
    unique_pieces: u8,
}

impl Table {
    pub fn new(data: &[u8]) {
    }
}

#[cfg(test)]
mod tests {
    use memmap::{Mmap, Protection};

    #[test]
    fn test_table() {
        let mmap = Mmap::open_path("KQvK.rtbw", Protection::Read).expect("mmap");
        let bytes = unsafe { mmap.as_slice() };
        Table::new(bytes)
    }
}
