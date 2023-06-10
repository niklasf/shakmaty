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

use crate::{
    bitboard::Bitboard,
    bootstrap::{
        ATTACKS, BLACK_PAWN_ATTACKS, KING_ATTACKS, KNIGHT_ATTACKS, RAYS, WHITE_PAWN_ATTACKS,
    },
    color::Color,
    magics,
    magics::Magic,
    role::Role,
    square::Square,
    types::Piece,
};

/// Looks up attacks for a pawn of `color` on `sq`.
#[inline]
pub fn pawn_attacks(color: Color, sq: Square) -> Bitboard {
    Bitboard(match color {
        Color::White => WHITE_PAWN_ATTACKS[usize::from(sq)],
        Color::Black => BLACK_PAWN_ATTACKS[usize::from(sq)],
    })
}

/// Looks up attacks for a knight on `sq`.
#[inline]
pub fn knight_attacks(sq: Square) -> Bitboard {
    Bitboard(KNIGHT_ATTACKS[usize::from(sq)])
}

/// Looks up attacks for a king on `sq`.
#[inline]
pub fn king_attacks(sq: Square) -> Bitboard {
    Bitboard(KING_ATTACKS[usize::from(sq)])
}

static ROOK_MAGICS: [Magic; 64] = magics::ROOK_MAGICS;

/// Looks up attacks for a rook on `sq` with `occupied` squares.
#[inline]
pub fn rook_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    let m = ROOK_MAGICS[usize::from(sq)];

    // Safety: The attack table was generated with sufficient size
    // for all relevant occupancies (all subsets of m.mask). Omitting bounds
    // checks is worth about 2% in move generation and perft.
    let idx = (m.factor.wrapping_mul(occupied.0 & m.mask) >> (64 - 12)) as usize + m.offset;
    debug_assert!(idx < ATTACKS.len());
    Bitboard(unsafe { *ATTACKS.get_unchecked(idx) })
}

/// Gets the set of potential blocking squares for a rook on `sq`.
///
/// # Example
///
/// ```
/// use shakmaty::{attacks, Square};
///
/// let mask = attacks::rook_mask(Square::E8);
/// // 0 1 1 1 0 1 1 0
/// // . . . . 1 . . .
/// // . . . . 1 . . .
/// // . . . . 1 . . .
/// // . . . . 1 . . .
/// // . . . . 1 . . .
/// // . . . . 1 . . .
/// // . . . . 0 . . .
///
/// assert_eq!(mask.count(), 11);
#[inline]
pub fn rook_mask(sq: Square) -> Bitboard {
    Bitboard(ROOK_MAGICS[usize::from(sq)].mask)
}

static BISHOP_MAGICS: [Magic; 64] = magics::BISHOP_MAGICS;

/// Looks up attacks for a bishop on `sq` with `occupied` squares.
#[inline]
pub fn bishop_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    let m = BISHOP_MAGICS[usize::from(sq)];

    // Safety: The attack table was generated with sufficient size
    // for all relevant occupancies (all subsets of m.mask). Omitting bounds
    // checks is worth about 2% in move generation and perft.
    let idx = (m.factor.wrapping_mul(occupied.0 & m.mask) >> (64 - 9)) as usize + m.offset;
    debug_assert!(idx < ATTACKS.len());
    Bitboard(unsafe { *ATTACKS.get_unchecked(idx) })
}

/// Gets the set of potential blocking squares for a bishop on `sq`.
///
/// # Example
///
/// ```
/// use shakmaty::{attacks, Square};
///
/// let mask = attacks::bishop_mask(Square::D5);
/// // 0 . . . . . 0 .
/// // . 1 . . . 1 . .
/// // . . 1 . 1 . . .
/// // . . . 0 . . . .
/// // . . 1 . 1 . . .
/// // . 1 . . . 1 . .
/// // 0 . . . . . 1 .
/// // . . . . . . . 0
///
/// assert_eq!(mask.count(), 9);
/// ```
#[inline]
pub fn bishop_mask(sq: Square) -> Bitboard {
    Bitboard(BISHOP_MAGICS[usize::from(sq)].mask)
}

/// Looks up attacks for a queen on `sq` with `occupied` squares.
#[inline]
pub fn queen_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    rook_attacks(sq, occupied) ^ bishop_attacks(sq, occupied)
}

/// Looks up attacks for `piece` on `sq` with `occupied` squares.
pub fn attacks(sq: Square, piece: Piece, occupied: Bitboard) -> Bitboard {
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
pub fn ray(a: Square, b: Square) -> Bitboard {
    Bitboard(RAYS[usize::from(a)][usize::from(b)])
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
pub fn between(a: Square, b: Square) -> Bitboard {
    Bitboard(ray(a, b).0 & ((!0 << u32::from(a)) ^ (!0 << u32::from(b)))).without_first()
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
pub fn aligned(a: Square, b: Square, c: Square) -> bool {
    ray(a, b).contains(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rook_attacks() {
        assert_eq!(
            rook_attacks(Square::D6, Bitboard(0x3f7f28802826f5b9)),
            Bitboard(0x8370808000000)
        );
    }
}
