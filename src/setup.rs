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
use bitboard;
use bitboard::Bitboard;
use types::{Color, White, Black, Role, Pockets, RemainingChecks};
use board::Board;

use std::iter::FromIterator;
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

/// Returns valid castling rights filtered from a setup.
pub fn clean_castling_rights(setup: &Setup, strict: bool) -> Bitboard {
    let castling = setup.castling_rights() & setup.board().rooks();

    let clean_strict = |color: Color| -> Bitboard {
        let king = color.fold(square::E1, square::E8);
        if setup.board().kings().contains(king) && !setup.board().promoted().contains(king) {
            castling & setup.board().by_color(color)
                     & Bitboard::relative_rank(color, 0)
                     & bitboard::CORNERS
        } else {
            Bitboard(0)
        }
    };

    let clean_loose = |color: Color| -> Bitboard {
        if let Some(king) = setup.board().king_of(color) {
            if king.file() == 0 || king.file() == 7 || king.rank() != color.fold(0, 7) {
                return Bitboard(0)
            }

            let castling = castling & setup.board().by_color(color) &
                           Bitboard::relative_rank(color, 0);

            let a_side = castling.first().filter(|rook| rook.file() < king.file());
            let h_side = castling.last().filter(|rook| king.file() < rook.file());

            Bitboard::from_iter(a_side) | Bitboard::from_iter(h_side)
        } else {
            Bitboard(0)
        }
    };

    if strict {
        clean_strict(Black) | clean_strict(White)
    } else {
        clean_loose(Black) | clean_loose(White)
    }
}
