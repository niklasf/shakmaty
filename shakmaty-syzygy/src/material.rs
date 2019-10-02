// This file is part of the shakmaty-syzygy library.
// Copyright (C) 2017-2019 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use shakmaty::{Material, MaterialSide, Role};

const ROLES: [Role; 6] = [
    Role::King,
    Role::Queen,
    Role::Rook,
    Role::Bishop,
    Role::Knight,
    Role::Pawn,
];

pub trait MaterialSideExt {
    fn unique_roles(&self) -> u8;
}

impl MaterialSideExt for MaterialSide {
    fn unique_roles(&self) -> u8 {
        ROLES.iter().map(|&r| self.by_role(r)).filter(|&c| c == 1).sum()
    }
}

pub trait MaterialExt {
    fn unique_pieces(&self) -> u8;
    fn min_like_man(&self) -> u8;
}

impl MaterialExt for Material {
    fn unique_pieces(&self) -> u8 {
        self.white.unique_roles() + self.black.unique_roles()
    }

    fn min_like_man(&self) -> u8 {
        ROLES.iter().map(|&r| self.white.by_role(r))
            .chain(ROLES.iter().map(|&r| self.black.by_role(r)))
            .filter(|&c| 2 <= c)
            .min().unwrap_or(0)
    }
}
