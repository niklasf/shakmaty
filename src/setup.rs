use square;
use square::Square;
use bitboard;
use bitboard::Bitboard;
use types::{Color, White, Black, Role, Pockets, RemainingChecks};
use board::Board;

use std::iter::FromIterator;
use option_filter::OptionFilterExt;

/// Reasons for a `Setup` not beeing a legal `Position`.
#[derive(Debug)]
pub enum PositionError {
    Empty,
    NoKing { color: Color },
    TooManyPawns,
    TooManyPieces,
    TooManyKings,
    PawnsOnBackrank,
    BadCastlingRights,
    InvalidEpSquare,
    OppositeCheck,
}

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
pub fn clean_castling_rights<S: Setup>(setup: &S, strict: bool) -> Bitboard {
    let castling = setup.castling_rights() & setup.board().rooks();

    let clean_strict = |color: Color| -> Bitboard {
        let king = color.fold(square::E1, square::E8);
        if (setup.board().kings() & !setup.board().promoted()).contains(king) {
            castling & setup.board().by_color(color)
                     & Bitboard::relative_rank(color, 0)
                     & bitboard::CORNERS
        } else {
            Bitboard::empty()
        }
    };

    let clean_loose = |color: Color| -> Bitboard {
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

    if strict {
        clean_strict(Black) | clean_strict(White)
    } else {
        clean_loose(Black) | clean_loose(White)
    }
}

/// Validates a `Setup` according to standard chess rules.
pub fn validate<S: Setup>(setup: &S) -> Option<PositionError> {
    if setup.board().occupied().is_empty() {
        return Some(PositionError::Empty)
    }

    for color in &[White, Black] {
        if (setup.board().by_piece(&color.king()) & !setup.board().promoted()).is_empty() {
            return Some(PositionError::NoKing { color: *color })
        }
    }

    if let Some(pockets) = setup.pockets() {
        if setup.board().pawns().count() + pockets.white.pawns as usize + pockets.black.pawns as usize > 16 {
            return Some(PositionError::TooManyPawns)
        }
        if setup.board().occupied().count() + pockets.count() as usize > 32 {
            return Some(PositionError::TooManyPieces)
        }
    } else {
        for color in &[White, Black] {
            if setup.board().by_color(*color).count() > 16 {
                return Some(PositionError::TooManyPieces)
            }
            if setup.board().by_piece(&color.pawn()).count() > 8 {
                return Some(PositionError::TooManyPawns)
            }
        }
    }

    if setup.board().kings().count() > 2 {
        return Some(PositionError::TooManyKings)
    }

    if !(setup.board().pawns() & (Bitboard::rank(0) | Bitboard::rank(7))).is_empty() {
        return Some(PositionError::PawnsOnBackrank)
    }

    if clean_castling_rights(setup, false) != setup.castling_rights() {
        return Some(PositionError::BadCastlingRights)
    }

    if let Some(ep_square) = setup.ep_square() {
        if !Bitboard::relative_rank(setup.turn(), 5).contains(ep_square) {
            return Some(PositionError::InvalidEpSquare)
        }

        let fifth_rank_sq = ep_square.offset(setup.turn().fold(-8, 8)).expect("ep square is on sixth rank");
        let seventh_rank_sq  = ep_square.offset(setup.turn().fold(8, -8)).expect("ep square is on sixth rank");

        // The last move must have been a double pawn push. Check for the
        // presence of that pawn.
        if !setup.their(Role::Pawn).contains(fifth_rank_sq) {
            return Some(PositionError::InvalidEpSquare)
        }

        if setup.board().occupied().contains(ep_square) | setup.board().occupied().contains(seventh_rank_sq) {
            return Some(PositionError::InvalidEpSquare)
        }
    }

    if let Some(their_king) = setup.board().king_of(!setup.turn()) {
        if !(setup.board().attacks_to(their_king) & setup.us()).is_empty() {
            return Some(PositionError::OppositeCheck)
        }
    }

    None
}
