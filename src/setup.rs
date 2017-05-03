use square::Square;
use bitboard::Bitboard;
use types::{Color, Role, Pockets, RemainingChecks};
use board::Board;

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
