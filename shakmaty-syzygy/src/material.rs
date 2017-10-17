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
use std::iter::FromIterator;

use shakmaty::{Color, Role, Piece, Board};

const ROLES: [Role; 6] = [
    Role::King,
    Role::Queen,
    Role::Rook,
    Role::Bishop,
    Role::Knight,
    Role::Pawn,
];

#[derive(Debug, Clone, Default, Eq, PartialEq, Hash)]
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

    pub fn count(&self) -> u8 {
        self.pawns + self.knights + self.bishops + self.rooks + self.queens + self.kings
    }

    pub(crate) fn unique_roles(&self) -> u8 {
        ROLES.iter().map(|&r| self.by_role(r)).filter(|&c| c == 1).sum()
    }

    pub(crate) fn has_pawns(&self) -> bool {
        self.pawns > 0
    }
}

impl fmt::Display for MaterialSide {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for &role in &ROLES {
            write!(f, "{}", role.char().to_uppercase().to_string().repeat(self.by_role(role) as usize))?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Default, Eq, PartialEq, Hash)]
pub struct Material {
    pub white: MaterialSide,
    pub black: MaterialSide,
}

impl Material {
    pub fn new() -> Material {
        Material::default()
    }

    pub fn from_board(board: &Board) -> Material {
        Material {
            white: MaterialSide {
                pawns: (board.pawns() & board.white()).len() as u8,
                knights: (board.knights() & board.white()).len() as u8,
                bishops: (board.bishops() & board.white()).len() as u8,
                rooks: (board.rooks() & board.white()).len() as u8,
                queens: (board.queens() & board.white()).len() as u8,
                kings: (board.kings() & board.white()).len() as u8,
            },
            black: MaterialSide {
                pawns: (board.pawns() & board.black()).len() as u8,
                knights: (board.knights() & board.black()).len() as u8,
                bishops: (board.bishops() & board.black()).len() as u8,
                rooks: (board.rooks() & board.black()).len() as u8,
                queens: (board.queens() & board.black()).len() as u8,
                kings: (board.kings() & board.black()).len() as u8,
            }
        }
    }

    pub fn flip(&self) -> Material {
        Material {
            white: self.black.clone(),
            black: self.white.clone(),
        }
    }

    pub fn is_symmetric(&self) -> bool {
        self.white == self.black
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

    pub fn count(&self) -> u8 {
        self.white.count() + self.black.count()
    }

    pub(crate) fn unique_pieces(&self) -> u8 {
        self.white.unique_roles() + self.black.unique_roles()
    }

    pub(crate) fn min_like_man(&self) -> u8 {
        ROLES.iter().map(|&r| self.white.by_role(r))
            .chain(ROLES.iter().map(|&r| self.black.by_role(r)))
            .filter(|&c| 2 <= c)
            .min().unwrap_or(0)
    }

    pub(crate) fn has_pawns(&self) -> bool {
        self.white.has_pawns() || self.black.has_pawns()
    }
}

impl fmt::Display for Material {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}v{}", self.white, self.black)
    }
}

impl Extend<Piece> for Material {
    fn extend<T: IntoIterator<Item = Piece>>(&mut self, iter: T) {
        for piece in iter {
            *self.by_piece_mut(piece) += 1;
        }
    }
}

impl FromIterator<Piece> for Material {
    fn from_iter<T>(iter: T) -> Self
        where T: IntoIterator<Item = Piece>
    {
        let mut result = Material::new();
        result.extend(iter);
        result
    }
}
