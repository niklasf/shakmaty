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

use square;
use square::Square;
use bitboard::Bitboard;
use attacks;
use types::{Color, Role, Pockets, RemainingChecks};
use board::Board;

use option_filter::OptionFilterExt;

/// A not necessarily legal position.
pub trait Setup {
    fn board(&self) -> &Board;
    fn pockets(&self) -> Option<&Pockets>;
    fn turn(&self) -> Color;
    fn castling_rights(&self) -> Bitboard;
    fn ep_square(&self) -> Option<Square>;
    fn remaining_checks(&self) -> Option<&RemainingChecks>;
    fn halfmove_clock(&self) -> u32;
    fn fullmoves(&self) -> u32;

    fn us(&self) -> Bitboard {
        self.board().by_color(self.turn())
    }

    fn our(&self, role: Role) -> Bitboard {
        self.us() & self.board().by_role(role)
    }

    fn them(&self) -> Bitboard {
        self.board().by_color(!self.turn())
    }

    fn their(&self, role: Role) -> Bitboard {
        self.them() & self.board().by_role(role)
    }
}

/// `Short` or `Long`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum CastlingSide {
    Short = 0,
    Long = 1,
}

impl CastlingSide {
    pub fn king_to(&self, color: Color) -> Square {
        match *self {
            CastlingSide::Short => color.fold(square::G1, square::G8),
            CastlingSide::Long => color.fold(square::C1, square::C8),
        }
    }

    pub fn rook_to(&self, color: Color) -> Square {
        match *self {
            CastlingSide::Short => color.fold(square::F1, square::F8),
            CastlingSide::Long => color.fold(square::D1, square::D8),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Castling {
    rook: [Option<Square>; 4],
    path: [Bitboard; 4],
}

impl Castling {
    pub fn empty() -> Castling {
        Castling {
            rook: [None; 4],
            path: [Bitboard(0); 4],
        }
    }

    pub fn default() -> Castling {
        Castling {
            rook: [
                Some(square::H8), // black short
                Some(square::A8), // black long
                Some(square::H1), // white short
                Some(square::A1), // white long
            ],
            path: [
                Bitboard(0x6000_0000_0000_0000), // black short
                Bitboard(0x0e00_0000_0000_0000), // black long
                Bitboard(0x0000_0000_0000_0060), // white short
                Bitboard(0x0000_0000_0000_000e), // white long
            ]
        }
    }

    pub fn from_setup(setup: &Setup) -> Result<Castling, Castling> {
        let mut castling = Castling::empty();

        let castling_rights = setup.castling_rights();
        let rooks = castling_rights & setup.board().rooks();

        for color in &[Color::Black, Color::White] {
            if let Some(king) = setup.board().king_of(*color) {
                if king.file() == 0 || king.file() == 7 || king.rank() != color.fold(0, 7) {
                    continue;
                }

                let side = rooks & setup.board().by_color(*color) &
                           Bitboard::relative_rank(*color, 0);

                if let Some(a_side) = side.first().filter(|rook| rook.file() < king.file()) {
                    let rto = CastlingSide::Long.rook_to(*color);
                    let kto = CastlingSide::Long.king_to(*color);
                    let idx = *color as usize * 2 + CastlingSide::Long as usize;
                    castling.rook[idx] = Some(a_side);
                    castling.path[idx] = attacks::between(king, a_side)
                                        .with(rto).with(kto).without(king).without(a_side);
                }

                if let Some(h_side) = side.last().filter(|rook| king.file() < rook.file()) {
                    let rto = CastlingSide::Short.rook_to(*color);
                    let kto = CastlingSide::Short.king_to(*color);
                    let idx = *color as usize * 2 + CastlingSide::Short as usize;
                    castling.rook[idx] = Some(h_side);
                    castling.path[idx] = attacks::between(king, h_side)
                                        .with(rto).with(kto).without(king).without(h_side);
                }
            }
        }

        if castling.castling_rights() == castling_rights {
            Ok(castling)
        } else {
            Err(castling)
        }
    }

    pub fn discard_rook(&mut self, square: Square) {
        self.rook[0] = self.rook[0].filter(|sq| *sq != square);
        self.rook[1] = self.rook[1].filter(|sq| *sq != square);
        self.rook[2] = self.rook[2].filter(|sq| *sq != square);
        self.rook[3] = self.rook[3].filter(|sq| *sq != square);
    }

    pub fn discard_side(&mut self, color: Color) {
        let idx = color as usize * 2;
        unsafe {
            *self.rook.get_unchecked_mut(idx) = None;
            *self.rook.get_unchecked_mut(idx + 1) = None;
        }
    }

    #[inline]
    pub fn rook(&self, color: Color, side: CastlingSide) -> Option<Square> {
        unsafe { *self.rook.get_unchecked(2 * color as usize + side as usize) }
    }

    #[inline]
    pub fn path(&self, color: Color, side: CastlingSide) -> Bitboard {
        unsafe { *self.path.get_unchecked(2 * color as usize + side as usize) }
    }

    pub fn castling_rights(&self) -> Bitboard {
        let mut mask = Bitboard(0);
        mask.extend(self.rook[0]);
        mask.extend(self.rook[1]);
        mask.extend(self.rook[2]);
        mask.extend(self.rook[3]);
        mask
    }
}
