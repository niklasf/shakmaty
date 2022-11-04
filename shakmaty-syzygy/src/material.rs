// This file is part of the shakmaty-syzygy library.
// Copyright (C) 2017-2022 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use std::{
    cmp::{Ord, Ordering, PartialOrd},
    fmt,
};

use shakmaty::{Board, ByColor, ByRole, Piece, Role};

#[derive(Clone, Eq, PartialEq, Hash)]
pub(crate) struct MaterialSide {
    by_role: ByRole<u8>,
}

impl MaterialSide {
    fn empty() -> MaterialSide {
        MaterialSide {
            by_role: ByRole::default(),
        }
    }

    fn from_str_part(s: &str) -> Result<MaterialSide, ()> {
        let mut side = MaterialSide::empty();
        for ch in s.as_bytes() {
            let role = Role::from_char(char::from(*ch)).ok_or(())?;
            *side.by_role.get_mut(role) += 1;
        }
        Ok(side)
    }

    pub(crate) fn count(&self) -> usize {
        self.by_role.iter().map(|c| usize::from(*c)).sum()
    }

    pub(crate) fn has_pawns(&self) -> bool {
        self.by_role.pawn > 0
    }

    fn unique_roles(&self) -> u8 {
        self.by_role.iter().filter(|c| **c == 1).sum()
    }
}

impl Ord for MaterialSide {
    fn cmp(&self, other: &MaterialSide) -> Ordering {
        self.count()
            .cmp(&other.count())
            .then(self.by_role.king.cmp(&other.by_role.king))
            .then(self.by_role.queen.cmp(&other.by_role.queen))
            .then(self.by_role.rook.cmp(&other.by_role.rook))
            .then(self.by_role.bishop.cmp(&other.by_role.bishop))
            .then(self.by_role.knight.cmp(&other.by_role.knight))
            .then(self.by_role.pawn.cmp(&other.by_role.pawn))
    }
}

impl PartialOrd for MaterialSide {
    fn partial_cmp(&self, other: &MaterialSide) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for MaterialSide {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (role, count) in self.by_role.as_ref().zip_role().into_iter().rev() {
            f.write_str(&role.upper_char().to_string().repeat(usize::from(*count)))?;
        }
        Ok(())
    }
}

impl fmt::Debug for MaterialSide {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.count() > 0 {
            <Self as fmt::Display>::fmt(self, f)
        } else {
            f.write_str("-")
        }
    }
}

/// A material key.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Material {
    pub(crate) by_color: ByColor<MaterialSide>,
}

impl Material {
    fn empty() -> Material {
        Material {
            by_color: ByColor::new_with(|_| MaterialSide::empty()),
        }
    }

    /// Get the material configuration for a [`Board`].
    pub fn from_board(board: &Board) -> Material {
        Material {
            by_color: ByColor::new_with(|color| MaterialSide {
                by_role: board.material_side(color),
            }),
        }
    }

    pub(crate) fn from_iter<I>(iter: I) -> Material
    where
        I: IntoIterator<Item = Piece>,
    {
        let mut material = Material::empty();
        for piece in iter {
            *material
                .by_color
                .get_mut(piece.color)
                .by_role
                .get_mut(piece.role) += 1;
        }
        material
    }

    pub(crate) fn from_str(s: &str) -> Result<Material, ()> {
        if s.len() > 64 + 1 {
            return Err(());
        }

        let (white, black) = s.split_once('v').ok_or(())?;
        Ok(Material {
            by_color: ByColor {
                white: MaterialSide::from_str_part(white)?,
                black: MaterialSide::from_str_part(black)?,
            },
        })
    }

    pub(crate) fn count(&self) -> usize {
        self.by_color.iter().map(|side| side.count()).sum()
    }

    pub(crate) fn is_symmetric(&self) -> bool {
        self.by_color.white == self.by_color.black
    }

    pub(crate) fn has_pawns(&self) -> bool {
        self.by_color.iter().any(|side| side.has_pawns())
    }

    pub(crate) fn unique_pieces(&self) -> u8 {
        self.by_color.iter().map(|side| side.unique_roles()).sum()
    }

    pub(crate) fn min_like_man(&self) -> u8 {
        self.by_color
            .iter()
            .flat_map(|side| side.by_role.iter())
            .copied()
            .filter(|c| 2 <= *c)
            .min()
            .unwrap_or(0)
    }

    pub(crate) fn into_flipped(self) -> Material {
        Material {
            by_color: self.by_color.into_flipped(),
        }
    }

    pub(crate) fn into_normalized(self) -> Material {
        Material {
            by_color: self.by_color.into_normalized(),
        }
    }
}

impl fmt::Display for Material {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}v{}", self.by_color.white, self.by_color.black)
    }
}
