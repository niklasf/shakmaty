// This file is part of the shakmaty library.
// Copyright (C) 2017 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

//! Attack and ray tables.
//!
//! # Examples
//!
//! ```
//! # use shakmaty::square;
//! # use shakmaty::Bitboard;
//! use shakmaty::attacks;
//!
//! let occupied = Bitboard::rank(5); // blocking pieces
//! let attacks = attacks::bishop_attacks(square::C2, occupied);
//! // . . . . . . . .
//! // . . . . . . . .
//! // 0 0 0 0 0 0 1 0
//! // . . . . . 1 . .
//! // 1 . . . 1 . . .
//! // . 1 . 1 . . . .
//! // . . . . . . . .
//! // . 1 . 1 . . . .
//!
//! assert!(attacks.contains(square::G6));
//! assert!(!attacks.contains(square::H7));
//! ```

use square::Square;
use bitboard::Bitboard;
use types::{Color, Role, Piece};

include!(concat!(env!("OUT_DIR"), "/attacks.rs"));

#[inline]
pub fn pawn_attacks(color: Color, sq: Square) -> Bitboard {
    // This is safe because properly constructed squares are in bounds.
    Bitboard(match color {
        Color::White =>
            unsafe { *WHITE_PAWN_ATTACKS.get_unchecked(sq.index() as usize) },
        Color::Black =>
            unsafe { *BLACK_PAWN_ATTACKS.get_unchecked(sq.index() as usize) },
    })
}

#[inline]
pub fn knight_attacks(sq: Square) -> Bitboard {
    // This is safe because properly constructed squares are in bounds.
    unsafe { Bitboard(*KNIGHT_ATTACKS.get_unchecked(sq.index() as usize)) }
}

#[inline]
pub fn king_attacks(sq: Square) -> Bitboard {
    // This is safe because properly constructed squares are in bounds.
    unsafe { Bitboard(*KING_ATTACKS.get_unchecked(sq.index() as usize)) }
}

#[inline]
pub fn rook_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    unsafe {
        // This is safe because properly constructed squares are in bounds.
        let mask = Bitboard(*ROOK_MASKS.get_unchecked(sq.index() as usize));
        let range = Bitboard(*ROOK_RANGES.get_unchecked(sq.index() as usize));
        let index = *ROOK_INDEXES.get_unchecked(sq.index() as usize) +
                    occupied.extract(mask) as usize;

        // This is safe because a sufficient size for the attack tables was
        // hand-selected.
        Bitboard::deposit(*ROOK_ATTACKS.get_unchecked(index) as u64, range)
    }
}

#[inline]
pub fn bishop_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    unsafe {
        // This is safe because properly constructed squares are in bounds.
        let mask = Bitboard(*BISHOP_MASKS.get_unchecked(sq.index() as usize));
        let range = Bitboard(*BISHOP_RANGES.get_unchecked(sq.index() as usize));
        let index = *BISHOP_INDEXES.get_unchecked(sq.index() as usize) +
                    occupied.extract(mask) as usize;

        // This is safe because a sufficient size for the attack tables was
        // hand-selected.
        Bitboard::deposit(*BISHOP_ATTACKS.get_unchecked(index) as u64, range)
    }
}

#[inline]
pub fn queen_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    rook_attacks(sq, occupied) ^ bishop_attacks(sq, occupied)
}

pub fn attacks(sq: Square, piece: &Piece, occupied: Bitboard) -> Bitboard {
    match piece.role {
        Role::Pawn   => pawn_attacks(piece.color, sq),
        Role::Knight => knight_attacks(sq),
        Role::Bishop => bishop_attacks(sq, occupied),
        Role::Rook   => rook_attacks(sq, occupied),
        Role::Queen  => queen_attacks(sq, occupied),
        Role::King   => king_attacks(sq)
    }
}

/// The rank, file or diagonal with the two squares (or an empty `Bitboard`
/// if they are not aligned).
///
/// ```
/// # use shakmaty::square;
/// # use shakmaty::attacks;
///
/// let ray = attacks::ray(square::E2, square::G4);
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
    // This is safe because properly constructed squares are in bounds.
    let idx = (a.index() as usize) * 64 + b.index() as usize;
    unsafe { Bitboard(*BB_RAYS.get_unchecked(idx)) }
}

/// Like `ray`, but just the squares in-between (exluding the bounds).
#[inline]
pub fn between(a: Square, b: Square) -> Bitboard {
    // This is safe because properly constructed squares are in bounds.
    let idx = (a.index() as usize) * 64 + b.index() as usize;
    unsafe { Bitboard(*BB_BETWEEN.get_unchecked(idx)) }
}

/// Tests if all three squares are aligned on a rank, file or diagonal.
#[inline]
pub fn aligned(a: Square, b: Square, c: Square) -> bool {
    ray(a, b).contains(c)
}

#[cfg(test)]
mod tests {
    use super::*;
    use square;

    #[test]
    fn test_rook_attacks() {
        assert_eq!(rook_attacks(square::D6, Bitboard(0x3f7f28802826f5b9)),
                   Bitboard(0x8370808000000));
    }
}
