//! Attack and ray tables.
//!
//! # Example
//!
//! ```
//! use shakmaty::{attacks, Rank, Square, Bitboard};
//!
//! let occupied = Bitboard::from(Rank::Sixth); // blocking pieces
//! let attacks = attacks::bishop_attacks(Square::C2, occupied);
//! // . . . . . . . .
//! // . . . . . . . .
//! // 0 0 0 0 0 0 1 0
//! // . . . . . 1 . .
//! // 1 . . . 1 . . .
//! // . 1 . 1 . . . .
//! // . . . . . . . .
//! // . 1 . 1 . . . .
//!
//! assert!(attacks.contains(Square::G6));
//! assert!(!attacks.contains(Square::H7));
//! ```

use core::hint::assert_unchecked;

use crate::{
    bitboard::Bitboard,
    bootstrap::{ATTACKS, KING_ATTACKS, KNIGHT_ATTACKS, PAWN_ATTACKS, RAYS},
    color::Color,
    magics::{BISHOP_MAGICS, ROOK_MAGICS},
    role::Role,
    square::Square,
    types::Piece,
};

/// Looks up attacks for a pawn of `color` on `sq`.
#[inline]
pub const fn pawn_attacks(color: Color, sq: Square) -> Bitboard {
    Bitboard(PAWN_ATTACKS.get(color)[sq.to_usize()])
}

/// Looks up attacks for a knight on `sq`.
#[inline]
pub const fn knight_attacks(sq: Square) -> Bitboard {
    Bitboard(KNIGHT_ATTACKS[sq.to_usize()])
}

/// Looks up attacks for a king on `sq`.
#[inline]
pub const fn king_attacks(sq: Square) -> Bitboard {
    Bitboard(KING_ATTACKS[sq.to_usize()])
}

/// Looks up attacks for a rook on `sq` with `occupied` squares.
#[inline]
pub const fn rook_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    let m = ROOK_MAGICS[sq.to_usize()];

    // Safety: The attack table was generated with sufficient size
    // for all relevant occupancies (all subsets of m.mask). Omitting bounds
    // checks is worth about 2% in move generation and perft.
    let idx = (m.factor.wrapping_mul(occupied.0 & m.mask) >> (64 - 12)) as usize + m.offset;
    unsafe { assert_unchecked(idx < ATTACKS.len()) };
    Bitboard(ATTACKS[idx])
}

/// Looks up attacks for a bishop on `sq` with `occupied` squares.
#[inline]
pub const fn bishop_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    let m = BISHOP_MAGICS[sq.to_usize()];

    // Safety: The attack table was generated with sufficient size
    // for all relevant occupancies (all subsets of m.mask). Omitting bounds
    // checks is worth about 2% in move generation and perft.
    let idx = (m.factor.wrapping_mul(occupied.0 & m.mask) >> (64 - 9)) as usize + m.offset;
    unsafe { assert_unchecked(idx < ATTACKS.len()) };
    Bitboard(ATTACKS[idx])
}

/// Looks up attacks for a queen on `sq` with `occupied` squares.
#[inline]
pub const fn queen_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    rook_attacks(sq, occupied).toggled_const(bishop_attacks(sq, occupied))
}

/// Looks up attacks for `piece` on `sq` with `occupied` squares.
pub const fn attacks(sq: Square, piece: Piece, occupied: Bitboard) -> Bitboard {
    match piece.role {
        Role::Pawn => pawn_attacks(piece.color, sq),
        Role::Knight => knight_attacks(sq),
        Role::Bishop => bishop_attacks(sq, occupied),
        Role::Rook => rook_attacks(sq, occupied),
        Role::Queen => queen_attacks(sq, occupied),
        Role::King => king_attacks(sq),
    }
}

/// The rank, file or diagonal with the two squares (or an empty [`Bitboard`]
/// if they are not aligned).
///
/// # Example
///
/// ```
/// use shakmaty::{attacks, Square};
///
/// let ray = attacks::ray(Square::E2, Square::G4);
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . . . . 1
/// // . . . . . . 1 .
/// // . . . . . 1 . .
/// // . . . . 1 . . .
/// // . . . 1 . . . .
/// ```
#[inline]
pub const fn ray(a: Square, b: Square) -> Bitboard {
    Bitboard(RAYS[a.to_usize()][b.to_usize()])
}

/// The squares between the two squares (bounds not included), or an empty
/// [`Bitboard`] if they are not on the same rank, file or diagonal.
///
/// # Example
///
/// ```
/// use shakmaty::{attacks, Square};
///
/// let between = attacks::between(Square::B1, Square::B7);
/// // . . . . . . . .
/// // . 0 . . . . . .
/// // . 1 . . . . . .
/// // . 1 . . . . . .
/// // . 1 . . . . . .
/// // . 1 . . . . . .
/// // . 1 . . . . . .
/// // . 0 . . . . . .
/// ```
#[inline]
pub const fn between(a: Square, b: Square) -> Bitboard {
    Bitboard(ray(a, b).0 & ((!0 << a.to_u32()) ^ (!0 << b.to_u32()))).without_first()
}

/// Tests if all three squares are aligned on a rank, file or diagonal.
///
/// # Example
///
/// ```
/// use shakmaty::{attacks, Square};
///
/// assert!(attacks::aligned(Square::A1, Square::B2, Square::C3));
/// ```
#[inline]
pub const fn aligned(a: Square, b: Square, c: Square) -> bool {
    ray(a, b).contains(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rook_attacks() {
        assert_eq!(
            rook_attacks(Square::D6, Bitboard(0x3f7f_2880_2826_f5b9)),
            Bitboard(0x0008_3708_0800_0000)
        );
    }
}
