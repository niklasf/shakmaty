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

use std::fmt;

use shakmaty::{Color, Role, Piece};

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
