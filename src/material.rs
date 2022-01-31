// This file is part of the shakmaty library.
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
    error::Error,
    fmt,
    iter::FromIterator,
    ops::{Add, AddAssign, Sub, SubAssign},
    str::FromStr,
};

use crate::{
    color::{ByColor, Color},
    types::{Piece, Role},
};

/// Error when parsing an invalid material key.
#[derive(Clone, Debug)]
pub struct ParseMaterialError;

impl fmt::Display for ParseMaterialError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("invalid material key")
    }
}

impl Error for ParseMaterialError {}

/// The material configuration of one side.
#[derive(Clone, Default, Eq, PartialEq, Hash)]
pub struct MaterialSide {
    pub pawns: u8,
    pub knights: u8,
    pub bishops: u8,
    pub rooks: u8,
    pub queens: u8,
    pub kings: u8,
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

    pub fn is_empty(&self) -> bool {
        self.pawns == 0
            && self.knights == 0
            && self.bishops == 0
            && self.rooks == 0
            && self.queens == 0
            && self.kings == 0
    }

    pub fn count(&self) -> usize {
        usize::from(self.pawns)
            + usize::from(self.knights)
            + usize::from(self.bishops)
            + usize::from(self.rooks)
            + usize::from(self.queens)
            + usize::from(self.kings)
    }

    pub fn has_pawns(&self) -> bool {
        self.pawns > 0
    }

    pub fn from_ascii(s: &[u8]) -> Result<MaterialSide, ParseMaterialError> {
        if s.len() > 64 {
            return Err(ParseMaterialError);
        }

        s.iter()
            .copied()
            .map(|ch| Role::from_char(char::from(ch)).ok_or(ParseMaterialError))
            .collect()
    }
}

impl fmt::Display for MaterialSide {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for role in Role::ALL.into_iter().rev() {
            f.write_str(
                &role
                    .char()
                    .to_uppercase()
                    .to_string()
                    .repeat(usize::from(self.by_role(role))),
            )?;
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

impl Extend<Role> for MaterialSide {
    fn extend<T: IntoIterator<Item = Role>>(&mut self, iter: T) {
        for role in iter.into_iter().take(64 - self.count()) {
            *self.by_role_mut(role) += 1;
        }
    }
}

impl FromIterator<Role> for MaterialSide {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Role>,
    {
        let mut result = MaterialSide::new();
        result.extend(iter);
        result
    }
}

impl Ord for MaterialSide {
    fn cmp(&self, other: &MaterialSide) -> Ordering {
        self.count()
            .cmp(&other.count())
            .then(self.kings.cmp(&other.kings))
            .then(self.queens.cmp(&other.queens))
            .then(self.rooks.cmp(&other.rooks))
            .then(self.bishops.cmp(&other.bishops))
            .then(self.knights.cmp(&other.knights))
            .then(self.pawns.cmp(&other.pawns))
    }
}

impl PartialOrd for MaterialSide {
    fn partial_cmp(&self, other: &MaterialSide) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl FromStr for MaterialSide {
    type Err = ParseMaterialError;

    fn from_str(s: &str) -> Result<MaterialSide, ParseMaterialError> {
        MaterialSide::from_ascii(s.as_bytes())
    }
}

impl<'a> AddAssign<&'a MaterialSide> for MaterialSide {
    fn add_assign(&mut self, other: &'a MaterialSide) {
        self.pawns += other.pawns;
        self.knights += other.knights;
        self.bishops += other.bishops;
        self.rooks += other.rooks;
        self.queens += other.queens;
        self.kings += other.kings;
    }
}

impl AddAssign for MaterialSide {
    fn add_assign(&mut self, other: MaterialSide) {
        *self += &other;
    }
}

impl<'a> Add<&'a MaterialSide> for MaterialSide {
    type Output = MaterialSide;

    fn add(mut self, other: &'a MaterialSide) -> MaterialSide {
        self += other;
        self
    }
}

impl Add for MaterialSide {
    type Output = MaterialSide;

    fn add(mut self, other: MaterialSide) -> MaterialSide {
        self += other;
        self
    }
}

impl<'a> SubAssign<&'a MaterialSide> for MaterialSide {
    fn sub_assign(&mut self, other: &'a MaterialSide) {
        self.pawns -= other.pawns;
        self.knights -= other.knights;
        self.bishops -= other.bishops;
        self.rooks -= other.rooks;
        self.queens -= other.queens;
        self.kings -= other.kings;
    }
}

impl SubAssign for MaterialSide {
    fn sub_assign(&mut self, other: MaterialSide) {
        *self -= &other;
    }
}

impl<'a> Sub<&'a MaterialSide> for MaterialSide {
    type Output = MaterialSide;

    fn sub(mut self, other: &'a MaterialSide) -> MaterialSide {
        self -= other;
        self
    }
}

impl Sub for MaterialSide {
    type Output = MaterialSide;

    fn sub(mut self, other: MaterialSide) -> MaterialSide {
        self -= other;
        self
    }
}

/// The material configuration of both sides.
pub type Material = ByColor<MaterialSide>;

impl Material {
    pub fn new() -> Material {
        Material::default()
    }

    pub fn by_piece(&self, piece: Piece) -> u8 {
        self.by_color(piece.color).by_role(piece.role)
    }

    pub fn by_piece_mut(&mut self, piece: Piece) -> &mut u8 {
        self.by_color_mut(piece.color).by_role_mut(piece.role)
    }

    pub fn is_empty(&self) -> bool {
        self.all(MaterialSide::is_empty)
    }

    pub fn count(&self) -> usize {
        self.white.count() + self.black.count()
    }

    pub fn has_pawns(&self) -> bool {
        self.any(MaterialSide::has_pawns)
    }

    /// Parse material from notation like `KPPvKR`.
    ///
    /// # Errors
    ///
    /// Returns [`ParseMaterialError`] if there are any unrecognized
    /// characters, not exactly one `v` to separate white pieces from black
    /// pieces, or more than 64 pieces.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Material;
    ///
    /// let material = Material::from_ascii(b"KPPvKR")?;
    /// assert_eq!(material.white.kings, 1);
    /// assert_eq!(material.white.pawns, 2);
    /// assert_eq!(material.black.kings, 1);
    /// assert_eq!(material.black.rooks, 1);
    /// # Ok::<_, Box<dyn std::error::Error>>(())
    /// ```
    pub fn from_ascii(s: &[u8]) -> Result<Material, ParseMaterialError> {
        if s.len() > 64 + 1 {
            return Err(ParseMaterialError);
        }

        let mut parts = s.splitn(2, |ch| *ch == b'v');

        Ok(Material {
            white: MaterialSide::from_ascii(parts.next().expect("split non-empty"))?,
            black: MaterialSide::from_ascii(parts.next().ok_or(ParseMaterialError)?)?,
        })
    }

    /// Parse material from notation like `KPPkr`, where white pieces are
    /// uppercase characters and black pieces are lowercase characters.
    ///
    /// # Errors
    ///
    /// Returns [`ParseMaterialError`] if there are any unrecognized
    /// characters, or more than 64 pieces.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Material;
    ///
    /// let material = Material::from_ascii_fen(b"KPPkr")?;
    /// assert_eq!(material.white.kings, 1);
    /// assert_eq!(material.white.pawns, 2);
    /// assert_eq!(material.black.kings, 1);
    /// assert_eq!(material.black.rooks, 1);
    /// # Ok::<_, Box<dyn std::error::Error>>(())
    /// ```
    pub fn from_ascii_fen(s: &[u8]) -> Result<Material, ParseMaterialError> {
        if s.len() > 64 {
            return Err(ParseMaterialError);
        }

        s.iter()
            .copied()
            .map(|ch| Piece::from_char(char::from(ch)).ok_or(ParseMaterialError))
            .collect()
    }

    pub fn fen(&self) -> String {
        let mut fen = String::with_capacity(self.count());

        for color in Color::ALL {
            for role in Role::ALL {
                let piece = Piece { color, role };
                for _ in 0..self.by_piece(piece) {
                    fen.push(piece.char());
                }
            }
        }

        fen
    }
}

impl fmt::Display for Material {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}v{}", self.white, self.black)
    }
}

impl Extend<Piece> for Material {
    fn extend<T: IntoIterator<Item = Piece>>(&mut self, iter: T) {
        for piece in iter.into_iter().take(64_usize.saturating_sub(self.count())) {
            *self.by_piece_mut(piece) += 1;
        }
    }
}

impl FromIterator<Piece> for Material {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Piece>,
    {
        let mut result = Material::new();
        result.extend(iter);
        result
    }
}

impl FromStr for Material {
    type Err = ParseMaterialError;

    fn from_str(s: &str) -> Result<Material, ParseMaterialError> {
        Material::from_ascii(s.as_bytes())
    }
}

impl<'a> AddAssign<&'a Material> for Material {
    fn add_assign(&mut self, other: &'a Material) {
        self.white += &other.white;
        self.black += &other.black;
    }
}

impl AddAssign for Material {
    fn add_assign(&mut self, other: Material) {
        *self += &other;
    }
}

impl<'a> Add<&'a Material> for Material {
    type Output = Material;

    fn add(mut self, other: &'a Material) -> Material {
        self += other;
        self
    }
}

impl Add for Material {
    type Output = Material;

    fn add(mut self, other: Material) -> Material {
        self += other;
        self
    }
}

impl<'a> SubAssign<&'a Material> for Material {
    fn sub_assign(&mut self, other: &'a Material) {
        self.white -= &other.white;
        self.black -= &other.black;
    }
}

impl SubAssign for Material {
    fn sub_assign(&mut self, other: Material) {
        *self -= &other;
    }
}

impl<'a> Sub<&'a Material> for Material {
    type Output = Material;

    fn sub(mut self, other: &'a Material) -> Material {
        self -= other;
        self
    }
}

impl Sub for Material {
    type Output = Material;

    fn sub(mut self, other: Material) -> Material {
        self -= other;
        self
    }
}
