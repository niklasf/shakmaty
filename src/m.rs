use core::{
    fmt,
    fmt::{Display, Write as _},
};

use arrayvec::ArrayVec;

use crate::{CastlingSide, Role, Square};

/// Information about a move.
///
/// # Display
///
/// `Move` implements [`Display`] using long algebraic notation. If a position
/// is available for context, it is more common to use [SAN](crate::san)
/// (for human interfaces) or [UCI](crate::uci) (for text-based protocols).
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Move {
    /// A normal move, e.g., `Bd3xh7`.
    Normal {
        role: Role,
        from: Square,
        capture: Option<Role>,
        to: Square,
        promotion: Option<Role>,
    },
    /// An en passant capture, e.g., `e5xd6`.
    EnPassant { from: Square, to: Square },
    /// A castling move, `O-O` or `O-O-O`.
    Castle { king: Square, rook: Square },
    /// A piece drop in Crazyhouse, e.g., `Q@g8`.
    Put { role: Role, to: Square },
}

impl Move {
    /// Gets the role of the moved piece.
    pub const fn role(self) -> Role {
        match self {
            Move::Normal { role, .. } | Move::Put { role, .. } => role,
            Move::EnPassant { .. } => Role::Pawn,
            Move::Castle { .. } => Role::King,
        }
    }

    /// Gets the origin square or `None` for drops.
    pub const fn from(self) -> Option<Square> {
        match self {
            Move::Normal { from, .. } | Move::EnPassant { from, .. } => Some(from),
            Move::Castle { king, .. } => Some(king),
            Move::Put { .. } => None,
        }
    }

    /// Gets the target square. For castling moves this is the corresponding
    /// rook square.
    pub const fn to(self) -> Square {
        match self {
            Move::Normal { to, .. } | Move::EnPassant { to, .. } | Move::Put { to, .. } => to,
            Move::Castle { rook, .. } => rook,
        }
    }

    /// Gets the role of the captured piece or `None`.
    pub const fn capture(self) -> Option<Role> {
        match self {
            Move::Normal { capture, .. } => capture,
            Move::EnPassant { .. } => Some(Role::Pawn),
            _ => None,
        }
    }

    /// Checks if the move is a capture.
    pub const fn is_capture(self) -> bool {
        matches!(
            self,
            Move::Normal {
                capture: Some(_),
                ..
            } | Move::EnPassant { .. }
        )
    }

    /// Checks if the move is en passant.
    pub const fn is_en_passant(self) -> bool {
        matches!(self, Move::EnPassant { .. })
    }

    /// Checks if the move zeros the half-move clock.
    pub const fn is_zeroing(self) -> bool {
        matches!(
            self,
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
    pub fn castling_side(self) -> Option<CastlingSide> {
        match self {
            Move::Castle { king, rook } => Some(CastlingSide::from_king_side(king < rook)),
            _ => None,
        }
    }

    /// Checks if the move is a castling move.
    pub const fn is_castle(self) -> bool {
        matches!(self, Move::Castle { .. })
    }

    /// Gets the promotion role.
    pub const fn promotion(self) -> Option<Role> {
        match self {
            Move::Normal { promotion, .. } => promotion,
            _ => None,
        }
    }

    /// Checks if the move is a promotion.
    pub const fn is_promotion(self) -> bool {
        matches!(
            self,
            Move::Normal {
                promotion: Some(_),
                ..
            }
        )
    }

    #[must_use]
    pub const fn to_mirrored(self) -> Move {
        match self {
            Move::Normal {
                role,
                from,
                capture,
                to,
                promotion,
            } => Move::Normal {
                role,
                from: from.flip_vertical(),
                capture,
                to: to.flip_vertical(),
                promotion,
            },
            Move::EnPassant { from, to } => Move::EnPassant {
                from: from.flip_vertical(),
                to: to.flip_vertical(),
            },
            Move::Castle { king, rook } => Move::Castle {
                king: king.flip_vertical(),
                rook: rook.flip_vertical(),
            },
            Move::Put { role, to } => Move::Put {
                role,
                to: to.flip_vertical(),
            },
        }
    }
}

impl Display for Move {
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
            Move::EnPassant { from, to, .. } => write!(f, "{from}x{to}"),
            Move::Castle { king, rook } => f.write_str(if king < rook { "O-O" } else { "O-O-O" }),
            Move::Put { role, to } => {
                if role != Role::Pawn {
                    f.write_char(role.upper_char())?;
                }
                write!(f, "@{to}")
            }
        }
    }
}

/// A container for moves that can be stored inline on the stack.
///
/// The capacity is limited, but there is enough space to hold the legal
/// moves of any chess position, including any of the supported chess variants,
/// if enabled.
///
/// # Example
///
/// ```
/// use shakmaty::{Chess, Position, Role};
///
/// let pos = Chess::default();
/// let mut moves = pos.legal_moves();
/// moves.retain(|m| m.role() == Role::Pawn);
/// assert_eq!(moves.len(), 16);
/// ```
pub type MoveList = ArrayVec<
    Move,
    {
        #[cfg(feature = "variant")]
        {
            512
        }
        #[cfg(not(feature = "variant"))]
        {
            256
        }
    },
>;

#[cfg(test)]
mod tests {
    use core::mem;

    use super::*;

    #[test]
    fn test_move_size() {
        assert!(mem::size_of::<Move>() <= 8);
    }
}
