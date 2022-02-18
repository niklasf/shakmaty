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

use std::fmt::{self, Write as _};

use crate::{
    color::{ByColor, Color},
    role::Role,
    square::{File, Square},
};

/// A piece with [`Color`] and [`Role`].
#[allow(missing_docs)]
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Piece {
    pub color: Color,
    pub role: Role,
}

impl Piece {
    pub fn char(self) -> char {
        self.color.fold_wb(self.role.upper_char(), self.role.char())
    }

    pub fn from_char(ch: char) -> Option<Piece> {
        Role::from_char(ch).map(|role| role.of(Color::from_white(32 & ch as u8 == 0)))
    }
}

/// Information about a move.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
#[repr(align(4))]
pub enum Move {
    Normal {
        role: Role,
        from: Square,
        capture: Option<Role>,
        to: Square,
        promotion: Option<Role>,
    },
    EnPassant {
        from: Square,
        to: Square,
    },
    Castle {
        king: Square,
        rook: Square,
    },
    Put {
        role: Role,
        to: Square,
    },
}

impl Move {
    /// Gets the role of the moved piece.
    pub fn role(&self) -> Role {
        match *self {
            Move::Normal { role, .. } | Move::Put { role, .. } => role,
            Move::EnPassant { .. } => Role::Pawn,
            Move::Castle { .. } => Role::King,
        }
    }

    /// Gets the origin square or `None` for drops.
    pub fn from(&self) -> Option<Square> {
        match *self {
            Move::Normal { from, .. } | Move::EnPassant { from, .. } => Some(from),
            Move::Castle { king, .. } => Some(king),
            Move::Put { .. } => None,
        }
    }

    /// Gets the target square. For castling moves this is the corresponding
    /// rook square.
    pub fn to(&self) -> Square {
        match *self {
            Move::Normal { to, .. } | Move::EnPassant { to, .. } | Move::Put { to, .. } => to,
            Move::Castle { rook, .. } => rook,
        }
    }

    /// Gets the role of the captured piece or `None`.
    pub fn capture(&self) -> Option<Role> {
        match *self {
            Move::Normal { capture, .. } => capture,
            Move::EnPassant { .. } => Some(Role::Pawn),
            _ => None,
        }
    }

    /// Checks if the move is a capture.
    pub fn is_capture(&self) -> bool {
        matches!(
            *self,
            Move::Normal {
                capture: Some(_),
                ..
            } | Move::EnPassant { .. }
        )
    }

    /// Checks if the move is en passant.
    pub fn is_en_passant(&self) -> bool {
        matches!(*self, Move::EnPassant { .. })
    }

    /// Checks if the move zeros the half-move clock.
    pub fn is_zeroing(&self) -> bool {
        matches!(
            *self,
            Move::Normal {
                role: Role::Pawn,
                ..
            } | Move::Normal {
                capture: Some(_),
                ..
            } | Move::EnPassant { .. }
                | Move::Put {
                    role: Role::Pawn,
                    ..
                }
        )
    }

    /// Gets the castling side.
    pub fn castling_side(&self) -> Option<CastlingSide> {
        match *self {
            Move::Castle { king, rook } if king < rook => Some(CastlingSide::KingSide),
            Move::Castle { .. } => Some(CastlingSide::QueenSide),
            _ => None,
        }
    }

    /// Checks if the move is a castling move.
    pub fn is_castle(&self) -> bool {
        matches!(*self, Move::Castle { .. })
    }

    /// Gets the promotion role.
    pub fn promotion(&self) -> Option<Role> {
        match *self {
            Move::Normal { promotion, .. } => promotion,
            _ => None,
        }
    }

    /// Checks if the move is a promotion.
    pub fn is_promotion(&self) -> bool {
        matches!(
            *self,
            Move::Normal {
                promotion: Some(_),
                ..
            }
        )
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Move::Normal {
                role,
                from,
                capture,
                to,
                promotion,
            } => {
                if role != Role::Pawn {
                    f.write_char(role.upper_char())?;
                }

                write!(
                    f,
                    "{}{}{}",
                    from,
                    if capture.is_some() { 'x' } else { '-' },
                    to
                )?;

                if let Some(p) = promotion {
                    write!(f, "={}", p.upper_char())?;
                }

                Ok(())
            }
            Move::EnPassant { from, to, .. } => {
                write!(f, "{}x{}", from, to)
            }
            Move::Castle { king, rook } => f.write_str(if king < rook { "O-O" } else { "O-O-O" }),
            Move::Put { role, to } => {
                if role != Role::Pawn {
                    f.write_char(role.upper_char())?;
                }
                write!(f, "@{}", to)
            }
        }
    }
}

/// `KingSide` (O-O) or `QueenSide` (O-O-O).
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum CastlingSide {
    KingSide = 0,
    QueenSide = 1,
}

impl CastlingSide {
    pub fn is_queen_side(self) -> bool {
        match self {
            CastlingSide::KingSide => false,
            CastlingSide::QueenSide => true,
        }
    }

    pub fn is_king_side(self) -> bool {
        !self.is_queen_side()
    }

    pub fn from_queen_side(queen_side: bool) -> CastlingSide {
        if queen_side {
            CastlingSide::QueenSide
        } else {
            CastlingSide::KingSide
        }
    }

    pub fn from_king_side(king_side: bool) -> CastlingSide {
        if king_side {
            CastlingSide::KingSide
        } else {
            CastlingSide::QueenSide
        }
    }

    pub fn king_to_file(self) -> File {
        match self {
            CastlingSide::KingSide => File::G,
            CastlingSide::QueenSide => File::C,
        }
    }

    pub fn rook_to_file(self) -> File {
        match self {
            CastlingSide::KingSide => File::F,
            CastlingSide::QueenSide => File::D,
        }
    }

    pub fn king_to(self, color: Color) -> Square {
        Square::from_coords(self.king_to_file(), color.backrank())
    }

    pub fn rook_to(self, color: Color) -> Square {
        Square::from_coords(self.rook_to_file(), color.backrank())
    }

    /// `KingSide` and `QueenSide`, in this order.
    pub const ALL: [CastlingSide; 2] = [CastlingSide::KingSide, CastlingSide::QueenSide];
}

/// `Standard` or `Chess960`.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum CastlingMode {
    Standard,
    Chess960,
}

impl CastlingMode {
    pub fn from_standard(standard: bool) -> CastlingMode {
        if standard {
            CastlingMode::Standard
        } else {
            CastlingMode::Chess960
        }
    }

    pub fn from_chess960(chess960: bool) -> CastlingMode {
        if chess960 {
            CastlingMode::Chess960
        } else {
            CastlingMode::Standard
        }
    }

    pub fn is_standard(self) -> bool {
        self == CastlingMode::Standard
    }

    pub fn is_chess960(self) -> bool {
        self == CastlingMode::Chess960
    }
}

#[cfg(test)]
mod tests {
    use std::mem;

    use super::*;

    #[test]
    fn test_role_order() {
        assert!(Role::Pawn < Role::Knight);
        assert!(Role::Knight < Role::Bishop);
        assert!(Role::Bishop < Role::Rook);
        assert!(Role::Rook < Role::Queen);
        assert!(Role::Queen < Role::King);
    }

    #[test]
    fn test_size() {
        assert!(mem::size_of::<Move>() <= 8);
    }
}

/// The number of checks the respective side needs to give in order to win
/// (in a game of Three-Check).
///
/// # Examples
///
/// ```
/// use shakmaty::{ByColor, RemainingChecks};
///
/// let remaining_checks = ByColor::<RemainingChecks>::default();
/// assert_eq!(remaining_checks.white, RemainingChecks(3));
/// assert_eq!(remaining_checks.black, RemainingChecks(3));
///
/// for _ in 0..5 {
///     remaining_checks.white = remaining_checks.white.saturating_sub(1);
/// }
///
/// assert!(remaining_checks.white.is_zero());
/// ```
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub struct RemainingChecks(pub u8);

impl Default for RemainingChecks {
    fn default() -> RemainingChecks {
        RemainingChecks(3)
    }
}

impl RemainingChecks {
    pub fn is_zero(self) -> bool {
        self.0 == 0
    }

    #[must_use]
    pub fn saturating_sub(self, n: u8) -> RemainingChecks {
        RemainingChecks(self.0.saturating_sub(n))
    }
}

macro_rules! int_from_remaining_checks_impl {
    ($($t:ty)+) => {
        $(impl From<RemainingChecks> for $t {
            #[inline]
            fn from(RemainingChecks(checks): RemainingChecks) -> $t {
                checks as $t
            }
        })+
    }
}

int_from_remaining_checks_impl! { u8 i8 u16 i16 u32 i32 u64 i64 usize isize }

impl fmt::Display for ByColor<RemainingChecks> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}+{}", self.white.0, self.black.0)
    }
}
