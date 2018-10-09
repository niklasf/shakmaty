// This file is part of the shakmaty library.
// Copyright (C) 2017-2018 Niklas Fiekas <niklas.fiekas@backscattering.de>
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

use square::{File, Rank, Square};
use bitboard::Bitboard;
use attacks;
use types::{CastlingSide, Color, Pockets, RemainingChecks, Role};
use board::Board;

/// A not necessarily legal position.
pub trait Setup {
    /// Piece positions on the board.
    fn board(&self) -> &Board;

    /// Pockets in chess variants like Crazyhouse.
    fn pockets(&self) -> Option<&Pockets>;

    /// Side to move.
    fn turn(&self) -> Color;

    /// Castling rights in terms of corresponding rook positions.
    ///
    /// ```
    /// use shakmaty::{Bitboard, Chess, Setup};
    ///
    /// let pos = Chess::default();
    /// let rooks = pos.castling_rights();
    /// // 1 . . . . . . 1
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // 1 . . . . . . 1
    ///
    /// assert_eq!(rooks, Bitboard::CORNERS);
    /// ```
    fn castling_rights(&self) -> Bitboard;

    /// En passant target square on the third or sixth rank.
    fn ep_square(&self) -> Option<Square>;

    /// Remaining checks in chess variants like Three-Check.
    fn remaining_checks(&self) -> Option<&RemainingChecks>;

    /// Number of half-moves since the last
    /// [capture or pawn move](enum.Move.html#method.is_zeroing).
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Chess, Setup};
    ///
    /// let pos = Chess::default();
    /// assert_eq!(pos.halfmoves(), 0);
    /// ```
    fn halfmoves(&self) -> u32 { #[allow(deprecated)] self.halfmove_clock() }

    /// Current move number.
    ///
    /// Starts at 1 and is increased after every black move.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Chess, Setup};
    ///
    /// let pos = Chess::default();
    /// assert_eq!(pos.fullmoves(), 1);
    /// ```
    fn fullmoves(&self) -> u32;

    #[deprecated(since="0.11.3", note="use halfmoves() instead")]
    fn halfmove_clock(&self) -> u32 { self.halfmoves() }

    /// Squares occupied by the side to move.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Bitboard, Chess, Rank, Setup};
    ///
    /// let pos = Chess::default();
    /// let mask = pos.us();
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    ///
    /// assert_eq!(mask, Bitboard::from(Rank::First) | Bitboard::from(Rank::Second));
    fn us(&self) -> Bitboard {
        self.board().by_color(self.turn())
    }

    /// Squares occupied by a given piece type of the side to move.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Bitboard, Chess, Role, Setup, Square};
    ///
    /// let pos = Chess::default();
    /// let mask = pos.our(Role::Queen);
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . 1 . . . .
    ///
    /// assert_eq!(mask, Bitboard::from_square(Square::D1));
    /// ```
    fn our(&self, role: Role) -> Bitboard {
        self.us() & self.board().by_role(role)
    }

    /// Squares occupied by the waiting player.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Bitboard, Chess, Rank, Setup};
    ///
    /// let pos = Chess::default();
    /// let mask = pos.them();
    /// // 1 1 1 1 1 1 1 1
    /// // 1 1 1 1 1 1 1 1
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    ///
    /// assert_eq!(mask, Bitboard::from(Rank::Seventh) | Bitboard::from(Rank::Eighth));
    /// ```
    fn them(&self) -> Bitboard {
        self.board().by_color(!self.turn())
    }

    /// Squares occupied by a given piece type of the waiting player.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Bitboard, Chess, Role, Setup, Square};
    ///
    /// let pos = Chess::default();
    /// let mask = pos.their(Role::Queen);
    /// // . . . 1 . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    ///
    /// assert_eq!(mask, Bitboard::from_square(Square::D8));
    /// ```
    fn their(&self, role: Role) -> Bitboard {
        self.them() & self.board().by_role(role)
    }
}

pub struct SwapTurn<S: Setup>(pub S);

impl<S: Setup> Setup for SwapTurn<S> {
    fn turn(&self) -> Color {
        !self.0.turn()
    }

    fn board(&self) -> &Board { self.0.board() }
    fn pockets(&self) -> Option<&Pockets> { self.0.pockets() }
    fn castling_rights(&self) -> Bitboard { self.0.castling_rights() }
    fn ep_square(&self) -> Option<Square> { self.0.ep_square() }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { self.0.remaining_checks() }
    fn halfmoves(&self) -> u32 { self.0.halfmoves() }
    fn fullmoves(&self) -> u32 { self.0.fullmoves() }
}

/// Castling paths and unmoved rooks.
#[derive(Clone, Debug)]
pub struct Castles {
    mask: Bitboard,
    rook: [[Option<Square>; 2]; 2],
    path: [[Bitboard; 2]; 2],
    chess960: bool,
}

impl Default for Castles {
    fn default() -> Castles {
        Castles {
            chess960: false,
            mask: Bitboard::CORNERS,
            rook: [
                [Some(Square::H8), Some(Square::A8)], // black
                [Some(Square::H1), Some(Square::A1)], // white
            ],
            path: [
                [Bitboard(0x6000_0000_0000_0000), Bitboard(0x0e00_0000_0000_0000)],
                [Bitboard(0x0000_0000_0000_0060), Bitboard(0x0000_0000_0000_000e)],
            ]
        }
    }
}

impl Castles {
    pub fn empty() -> Castles {
        EMPTY_CASTLES.clone()
    }

    pub fn from_setup(setup: &dyn Setup) -> Result<Castles, Castles> {
        let mut castles = Castles::empty();

        let castling_rights = setup.castling_rights();
        let rooks = castling_rights & setup.board().rooks();

        for color in &[Color::Black, Color::White] {
            if let Some(king) = setup.board().king_of(*color) {
                if king.file() == File::A || king.file() == File::H || king.rank() != color.fold(Rank::First, Rank::Eighth) {
                    continue;
                }

                let side = rooks & setup.board().by_color(*color) &
                           Bitboard::relative_rank(*color, Rank::First);

                if let Some(a_side) = side.first().filter(|rook| rook.file() < king.file()) {
                    let rto = CastlingSide::QueenSide.rook_to(*color);
                    let kto = CastlingSide::QueenSide.king_to(*color);
                    castles.chess960 |= king.file() != File::E || a_side.file() != File::A;
                    castles.rook[*color as usize][CastlingSide::QueenSide as usize] = Some(a_side);
                    castles.path[*color as usize][CastlingSide::QueenSide as usize] =
                        attacks::between(a_side, rto).with(rto).without(king).without(a_side) |
                        attacks::between(king, kto).with(kto).without(king).without(a_side);
                }

                if let Some(h_side) = side.last().filter(|rook| king.file() < rook.file()) {
                    let rto = CastlingSide::KingSide.rook_to(*color);
                    let kto = CastlingSide::KingSide.king_to(*color);
                    castles.chess960 |= king.file() != File::E || h_side.file() != File::H;
                    castles.rook[*color as usize][CastlingSide::KingSide as usize] = Some(h_side);
                    castles.path[*color as usize][CastlingSide::KingSide as usize] =
                        attacks::between(h_side, rto).with(rto).without(king).without(h_side) |
                        attacks::between(king, kto).with(kto).without(king).without(h_side);
                }
            }
        }

        castles.mask.extend(castles.rook[0][0]);
        castles.mask.extend(castles.rook[0][1]);
        castles.mask.extend(castles.rook[1][0]);
        castles.mask.extend(castles.rook[1][1]);

        if castles.castling_rights() == castling_rights {
            Ok(castles)
        } else {
            Err(castles)
        }
    }

    pub fn any(&self) -> bool {
        self.mask.any()
    }

    pub fn is_empty(&self) -> bool {
        self.mask.is_empty()
    }

    pub fn has(&self, color: Color, side: CastlingSide) -> bool {
        self.rook(color, side).is_some()
    }

    pub fn has_side(&self, color: Color) -> bool {
        (self.mask & Bitboard::relative_rank(color, Rank::First)).any()
    }

    pub fn discard_rook(&mut self, square: Square) {
        if self.mask.remove(square) {
            self.rook[0][0] = self.rook[0][0].filter(|sq| *sq != square);
            self.rook[0][1] = self.rook[0][1].filter(|sq| *sq != square);
            self.rook[1][0] = self.rook[1][0].filter(|sq| *sq != square);
            self.rook[1][1] = self.rook[1][1].filter(|sq| *sq != square);
        }
    }

    pub fn discard_side(&mut self, color: Color) {
        self.mask.discard(Bitboard::relative_rank(color, Rank::First));
        self.rook[color as usize] = [None, None];
    }

    #[inline]
    pub fn rook(&self, color: Color, side: CastlingSide) -> Option<Square> {
        self.rook[color as usize][side as usize]
    }

    /// Gets the squares that need to be empty so that castling is possible
    /// on the given side.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Castles, CastlingSide, Bitboard, Color, Square};
    ///
    /// let castles = Castles::default();
    /// let path = castles.path(Color::White, CastlingSide::QueenSide);
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // 0 1 1 1 0 . . .
    ///
    /// assert_eq!(path, Bitboard::from(Square::B1) | Bitboard::from(Square::C1) | Bitboard::from(Square::D1));
    /// ```
    #[inline]
    pub fn path(&self, color: Color, side: CastlingSide) -> Bitboard {
        self.path[color as usize][side as usize]
    }

    #[inline]
    pub fn castling_rights(&self) -> Bitboard {
        self.mask
    }

    pub fn is_chess960(&self) -> bool {
        self.chess960
    }
}

pub static EMPTY_CASTLES: Castles = Castles {
    chess960: false,
    mask: Bitboard(0),
    rook: [[None; 2]; 2],
    path: [[Bitboard(0); 2]; 2],
};

#[cfg(test)]
mod tests {
    use super::*;

    struct _AssertObjectSafe(Box<dyn Setup>);
}
