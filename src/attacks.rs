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
        DIAG_RANGE, ANTI_DIAG_RANGE, FILE_RANGE, RANK_RANGE,
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

fn hyperbola(sq: Square, range: u64, occupied: Bitboard) -> Bitboard {
    let forward = occupied & Bitboard(range);
    let reverse = forward.flip_vertical();
    let forward = Bitboard(forward.0.wrapping_sub(1 << usize::from(sq)));
    let reverse = Bitboard(reverse.0.wrapping_sub(1 << usize::from(sq.flip_vertical())));
    (forward ^ reverse.flip_vertical()) & range
}
fn file_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    hyperbola(sq, FILE_RANGE[usize::from(sq)], occupied)
}
fn rank_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    let range = RANK_RANGE[usize::from(sq)];
    let forward = occupied & range;
    let reverse = forward.rotate_180();
    let forward = Bitboard(forward.0.wrapping_sub(1 << usize::from(sq)));
    let reverse = Bitboard(reverse.0.wrapping_sub(1 << usize::from(sq.rotate_180())));
    (forward ^ reverse.rotate_180()) & range
}
pub fn bishop_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    hyperbola(sq, DIAG_RANGE[usize::from(sq)], occupied) ^ hyperbola(sq, ANTI_DIAG_RANGE[usize::from(sq)], occupied)
}
pub fn rook_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    file_attacks(sq, occupied) ^ rank_attacks(sq, occupied)
}

static ROOK_MAGICS: [Magic; 64] = magics::ROOK_MAGICS;

/// Looks up attacks for a rook on `sq` with `occupied` squares.
/* #[inline]
pub fn rook_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    let m = ROOK_MAGICS[usize::from(sq)];

    // Safety: The attack table was generated with sufficient size
    // for all relevant occupancies (all subsets of m.mask). Omitting bounds
    // checks is worth about 2% in move generation and perft.
    let idx = (m.factor.wrapping_mul(occupied.0 & m.mask) >> (64 - 12)) as usize + m.offset;
    debug_assert!(idx < ATTACKS.len());
    Bitboard(unsafe { *ATTACKS.get_unchecked(idx) })
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
} */

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
    if a == b {
        Bitboard::EMPTY
    } else if a.rank() == b.rank() {
        Bitboard::from_rank(a.rank())
    } else if a.file() == b.file() {
        Bitboard::from_file(a.file())
    } else if Bitboard(DIAG_RANGE[usize::from(a)]).contains(b) {
        Bitboard(DIAG_RANGE[usize::from(a)]).with(a)
    } else if Bitboard(ANTI_DIAG_RANGE[usize::from(a)]).contains(b) {
        Bitboard(ANTI_DIAG_RANGE[usize::from(a)]).with(a)
    } else {
        Bitboard::EMPTY
    }
    //Bitboard(RAYS[usize::from(a)][usize::from(b)])
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
