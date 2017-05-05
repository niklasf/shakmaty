use square;
use square::Square;
use bitboard::Bitboard;
use types::{Color, White, Black, Role, Pockets, RemainingChecks};
use board::Board;

use std::iter::FromIterator;
use option_filter::OptionFilterExt;

/// A (not necessarily legal) position.
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
pub fn clean_castling_rights<S: Setup>(setup: &S, chess960: bool) -> Bitboard {
    let castling = setup.castling_rights() & setup.board().rooks();

    let clean_standard = |color: Color| -> Bitboard {
        let king = color.fold(square::E1, square::E8);
        if (setup.board().kings() & !setup.board().promoted()).contains(king) {
            castling & setup.board().by_color(color) & Bitboard::relative_rank(color, 0)
        } else {
            Bitboard::empty()
        }
    };

    let pre_clean_960 = |color: Color| -> Bitboard {
        if let Some(king) = setup.board().king_of(color) {
            if king.file() == 0 || king.file() == 7 || king.rank() != color.fold(0, 7) {
                return Bitboard::empty()
            }

            let castling = castling & setup.board().by_color(color) &
                           Bitboard::relative_rank(color, 0);

            let a_side = castling.first().filter(|rook| rook.file() < king.file());
            let h_side = castling.last().filter(|rook| king.file() < rook.file());

            Bitboard::from_iter(a_side) | Bitboard::from_iter(h_side)
        } else {
            Bitboard::empty()
        }
    };

    if chess960 {
        pre_clean_960(Black) | pre_clean_960(White)
    } else {
        clean_standard(Black) | clean_standard(White)
    }
}
