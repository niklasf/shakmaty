use std::{cmp::Ordering, fmt};

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
        self.by_role.into_iter().map(usize::from).sum()
    }

    pub(crate) fn has_pawns(&self) -> bool {
        self.by_role.pawn > 0
    }

    fn unique_roles(&self) -> usize {
        self.by_role.into_iter().filter(|c| *c == 1).count()
    }
}

impl Ord for MaterialSide {
    fn cmp(&self, other: &MaterialSide) -> Ordering {
        self.count()
            .cmp(&other.count())
            .then_with(|| self.by_role.king.cmp(&other.by_role.king))
            .then_with(|| self.by_role.queen.cmp(&other.by_role.queen))
            .then_with(|| self.by_role.rook.cmp(&other.by_role.rook))
            .then_with(|| self.by_role.bishop.cmp(&other.by_role.bishop))
            .then_with(|| self.by_role.knight.cmp(&other.by_role.knight))
            .then_with(|| self.by_role.pawn.cmp(&other.by_role.pawn))
    }
}

impl PartialOrd for MaterialSide {
    fn partial_cmp(&self, other: &MaterialSide) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for MaterialSide {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (role, count) in self.by_role.zip_role().into_iter().rev() {
            f.write_str(&role.upper_char().to_string().repeat(usize::from(count)))?;
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

    pub(crate) fn unique_pieces(&self) -> usize {
        self.by_color.iter().map(|side| side.unique_roles()).sum()
    }

    pub(crate) fn min_like_man(&self) -> usize {
        usize::from(
            self.by_color
                .iter()
                .flat_map(|side| side.by_role)
                .filter(|c| 2 <= *c)
                .min()
                .unwrap_or(0),
        )
    }

    pub(crate) fn into_swapped(self) -> Material {
        Material {
            by_color: self.by_color.into_swapped(),
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
