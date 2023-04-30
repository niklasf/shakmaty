use arrayvec::ArrayVec;

use crate::types::Move;

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
