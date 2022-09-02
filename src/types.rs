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

use core::{
    fmt::{self, Write as _},
    num,
};

use crate::{
    color::{ByColor, Color},
    role::Role,
    square::{File, Square},
    util::overflow_error,
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

    pub fn from_char(ch: char) -> Option<Self> {
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
    pub const fn role(&self) -> Role {
        match *self {
            Self::Normal { role, .. } | Self::Put { role, .. } => role,
            Self::EnPassant { .. } => Role::Pawn,
            Self::Castle { .. } => Role::King,
        }
    }

    /// Gets the origin square or `None` for drops.
    pub const fn from(&self) -> Option<Square> {
        match *self {
            Self::Normal { from, .. } | Self::EnPassant { from, .. } => Some(from),
            Self::Castle { king, .. } => Some(king),
            Self::Put { .. } => None,
        }
    }

    /// Gets the target square. For castling moves this is the corresponding
    /// rook square.
    pub const fn to(&self) -> Square {
        match *self {
            Self::Normal { to, .. } | Self::EnPassant { to, .. } | Self::Put { to, .. } => to,
            Self::Castle { rook, .. } => rook,
        }
    }

    /// Gets the role of the captured piece or `None`.
    pub const fn capture(&self) -> Option<Role> {
        match *self {
            Self::Normal { capture, .. } => capture,
            Self::EnPassant { .. } => Some(Role::Pawn),
            _ => None,
        }
    }

    /// Checks if the move is a capture.
    pub const fn is_capture(&self) -> bool {
        matches!(
            *self,
            Self::Normal {
                capture: Some(_),
                ..
            } | Self::EnPassant { .. }
        )
    }

    /// Checks if the move is en passant.
    pub const fn is_en_passant(&self) -> bool {
        matches!(*self, Self::EnPassant { .. })
    }

    /// Checks if the move zeros the half-move clock.
    pub const fn is_zeroing(&self) -> bool {
        matches!(
            *self,
            Self::Normal {
                role: Role::Pawn,
                ..
            } | Self::Normal {
                capture: Some(_),
                ..
            } | Self::EnPassant { .. }
                | Self::Put {
                    role: Role::Pawn,
                    ..
                }
        )
    }

    /// Gets the castling side.
    pub fn castling_side(&self) -> Option<CastlingSide> {
        match *self {
            Self::Castle { king, rook } if king < rook => Some(CastlingSide::KingSide),
            Self::Castle { .. } => Some(CastlingSide::QueenSide),
            _ => None,
        }
    }

    /// Checks if the move is a castling move.
    pub const fn is_castle(&self) -> bool {
        matches!(*self, Self::Castle { .. })
    }

    /// Gets the promotion role.
    pub const fn promotion(&self) -> Option<Role> {
        match *self {
            Self::Normal { promotion, .. } => promotion,
            _ => None,
        }
    }

    /// Checks if the move is a promotion.
    pub const fn is_promotion(&self) -> bool {
        matches!(
            *self,
            Self::Normal {
                promotion: Some(_),
                ..
            }
        )
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Normal {
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
            Self::EnPassant { from, to, .. } => {
                write!(f, "{}x{}", from, to)
            }
            Self::Castle { king, rook } => f.write_str(if king < rook { "O-O" } else { "O-O-O" }),
            Self::Put { role, to } => {
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
    pub const fn is_queen_side(self) -> bool {
        match self {
            Self::KingSide => false,
            Self::QueenSide => true,
        }
    }

    pub const fn is_king_side(self) -> bool {
        !self.is_queen_side()
    }

    pub const fn from_queen_side(queen_side: bool) -> Self {
        if queen_side {
            Self::QueenSide
        } else {
            Self::KingSide
        }
    }

    pub const fn from_king_side(king_side: bool) -> Self {
        if king_side {
            Self::KingSide
        } else {
            Self::QueenSide
        }
    }

    pub const fn king_to_file(self) -> File {
        match self {
            Self::KingSide => File::G,
            Self::QueenSide => File::C,
        }
    }

    pub const fn rook_to_file(self) -> File {
        match self {
            Self::KingSide => File::F,
            Self::QueenSide => File::D,
        }
    }

    pub fn king_to(self, color: Color) -> Square {
        Square::from_coords(self.king_to_file(), color.backrank())
    }

    pub fn rook_to(self, color: Color) -> Square {
        Square::from_coords(self.rook_to_file(), color.backrank())
    }

    /// `KingSide` and `QueenSide`, in this order.
    pub const ALL: [Self; 2] = [Self::KingSide, Self::QueenSide];
}

/// `Standard` or `Chess960`.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum CastlingMode {
    Standard,
    Chess960,
}

impl CastlingMode {
    pub const fn from_standard(standard: bool) -> Self {
        if standard {
            Self::Standard
        } else {
            Self::Chess960
        }
    }

    pub const fn from_chess960(chess960: bool) -> Self {
        if chess960 {
            Self::Chess960
        } else {
            Self::Standard
        }
    }

    pub const fn is_standard(self) -> bool {
        matches!(self, Self::Standard)
    }

    pub const fn is_chess960(self) -> bool {
        matches!(self, Self::Chess960)
    }
}

/// When to include the en passant square.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum EnPassantMode {
    /// Only if there is a fully legal en passant move.
    Legal,
    /// Only when a pawn has been advanced by two steps and there is an
    /// enemy pawn next to it.
    ///
    /// Follows the X-FEN specification.
    PseudoLegal,
    /// Whenever a pawn has been advanced by two steps.
    ///
    /// Follows the FEN specification.
    Always,
}

#[cfg(test)]
mod tests {
    use core::mem;

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
/// let mut remaining_checks = ByColor::<RemainingChecks>::default();
/// assert_eq!(remaining_checks.white, RemainingChecks::new(3));
/// assert_eq!(remaining_checks.black, RemainingChecks::new(3));
///
/// for _ in 0..5 {
///     remaining_checks.white = remaining_checks.white.saturating_sub(1);
/// }
///
/// assert!(remaining_checks.white.is_zero());
/// ```
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub struct RemainingChecks(u32);

impl Default for RemainingChecks {
    fn default() -> Self {
        Self(3)
    }
}

impl RemainingChecks {
    /// Constructs a new [`RemainingChecks`] value.
    ///
    /// # Panics
    ///
    /// Panics if `n > 3`.
    pub const fn new(n: u32) -> Self {
        assert!(n <= 3);
        Self(n)
    }

    pub const fn is_zero(self) -> bool {
        self.0 == 0
    }

    #[must_use]
    pub const fn saturating_sub(self, n: u32) -> Self {
        Self(self.0.saturating_sub(n))
    }
}

macro_rules! int_from_remaining_checks_impl {
    ($($t:ty)+) => {
        $(impl From<RemainingChecks> for $t {
            #[inline]
            fn from(RemainingChecks(checks): RemainingChecks) -> Self {
                checks as Self
            }
        })+
    }
}

int_from_remaining_checks_impl! { u8 i8 u16 i16 u32 i32 u64 i64 usize isize }

macro_rules! try_remaining_checks_from_int_impl {
    ($($t:ty)+) => {
        $(impl core::convert::TryFrom<$t> for RemainingChecks {
            type Error = num::TryFromIntError;

            #[inline]
            fn try_from(value: $t) -> Result<Self, Self::Error> {
                let n = u32::try_from(value)?;
                if n <= 3 {
                    Ok(Self::new(n))
                } else {
                    Err(overflow_error())
                }
            }
        })+
    }
}

try_remaining_checks_from_int_impl! { u8 i8 u16 i16 u32 i32 u64 i64 usize isize }

impl fmt::Display for ByColor<RemainingChecks> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}+{}", self.white.0, self.black.0)
    }
}
