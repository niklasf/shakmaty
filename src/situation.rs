use std::cmp::max;
use std::ascii::AsciiExt;
use std::fmt;
use std::str::FromStr;

use square;
use square::Square;
use types::{Color, White, Black, Role, Piece, Move, ROLES};
use bitboard::Bitboard;
use board::Board;

#[derive(Clone, Default)]
pub struct Pocket {
    pub pawns: u8,
    pub knights: u8,
    pub bishops: u8,
    pub rooks: u8,
    pub queens: u8,
    pub kings: u8,
}

impl Pocket {
    pub fn by_role(&self, role: Role) -> u8 {
        match role {
            Role::Pawn   => self.pawns,
            Role::Knight => self.knights,
            Role::Bishop => self.bishops,
            Role::Rook   => self.rooks,
            Role::Queen  => self.queens,
            Role::King   => self.kings,
        }
    }

    pub fn mut_by_role(&mut self, role: Role) -> &mut u8 {
        match role {
            Role::Pawn   => &mut self.pawns,
            Role::Knight => &mut self.knights,
            Role::Bishop => &mut self.bishops,
            Role::Rook   => &mut self.rooks,
            Role::Queen  => &mut self.queens,
            Role::King   => &mut self.kings,
        }
    }
}

#[derive(Clone, Default)]
pub struct Pockets {
    pub white: Pocket,
    pub black: Pocket,
}

impl Pockets {
    pub fn by_color(&self, color: Color) -> &Pocket {
        color.fold(&self.white, &self.black)
    }

    pub fn mut_by_color(&mut self, color: Color) -> &mut Pocket {
        color.fold(&mut self.white, &mut self.black)
    }

    pub fn by_piece(&self, piece: Piece) -> u8 {
        self.by_color(piece.color).by_role(piece.role)
    }

    pub fn mut_by_piece(&mut self, piece: Piece) -> &mut u8 {
        self.mut_by_color(piece.color).mut_by_role(piece.role)
    }
}

impl fmt::Display for Pockets {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for color in &[White, Black] {
            for role in &ROLES {
                let piece = Piece { color: *color, role: *role };
                try!(write!(f, "{}", piece.char().to_string().repeat(self.by_piece(piece) as usize)));
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct RemainingChecks {
    pub white: u8,
    pub black: u8,
}

impl Default for RemainingChecks {
    fn default() -> RemainingChecks {
        RemainingChecks { white: 3, black: 3 }
    }
}

impl RemainingChecks {
    pub fn by_color(&self, color: Color) -> u8 {
        color.fold(self.white, self.black)
    }

    pub fn mut_by_color(&mut self, color: Color) -> &mut u8 {
        color.fold(&mut self.white, &mut self.black)
    }
}

impl fmt::Display for RemainingChecks {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}+{}", self.white, self.black)
    }
}


#[derive(Clone)]
pub struct Situation {
    pub board: Board,
    pub turn: Color,
    pub castling_rights: Bitboard,
    pub ep_square: Option<Square>,
    pub halfmove_clock: u32,
    pub fullmoves: u32,
}

impl Default for Situation {
    fn default() -> Self {
        Situation {
            board: Board::default(),
            turn: White,
            castling_rights: Bitboard(0x8100000000000081),
            ep_square: None,
            halfmove_clock: 0,
            fullmoves: 1,
        }
    }
}

impl Situation {
    pub fn do_move(mut self, m: &Move) -> Self {
        let color = self.turn;
        self.ep_square.take();
        self.halfmove_clock += 1;

        match *m {
            Move::Normal { role, from, capture, to, promotion } => {
                if role == Role::Pawn || capture.is_some() {
                    self.halfmove_clock = 0;
                }

                if role == Role::Pawn && square::distance(from, to) == 2 {
                    self.ep_square = from.offset(color.fold(8, -8));
                }

                if role == Role::King {
                    self.castling_rights.discard_all(Bitboard::relative_rank(color, 0));
                } else {
                    self.castling_rights.discard(from);
                    self.castling_rights.discard(to);
                }

                let promoted = self.board.promoted().contains(from) || promotion.is_some();

                self.board.remove_piece_at(from);
                self.board.set_piece_at(to, promotion.map_or(role.of(color), |p| p.of(color)), promoted);
            },
            Move::Castle { king, rook } => {
                let rook_to = Square::from_coords(
                    if square::delta(rook, king) < 0 { 3 } else { 5 },
                    color.fold(0, 7)).unwrap();

                let king_to = Square::from_coords(
                    if square::delta(rook, king) < 0 { 2 } else { 6 },
                    color.fold(0, 7)).unwrap();

                self.board.remove_piece_at(king);
                self.board.remove_piece_at(rook);
                self.board.set_piece_at(rook_to, color.rook(), false);
                self.board.set_piece_at(king_to, color.king(), false);

                self.castling_rights.discard_all(Bitboard::relative_rank(color, 0));
            },
            Move::EnPassant { from, to, pawn } => {
                self.board.remove_piece_at(pawn);
                self.board.remove_piece_at(from).map(|piece| self.board.set_piece_at(to, piece, false));
                self.halfmove_clock = 0;
            },
            Move::Put { to, role } => {
                self.board.set_piece_at(to, Piece { color, role }, false);
            },
            Move::Null => ()
        }

        if color == Black {
            self.fullmoves += 1;
        }

        self.turn = !color;

        self
    }

    pub fn empty() -> Situation {
        Situation {
            board: Board::empty(),
            turn: White,
            castling_rights: Bitboard(0),
            ep_square: None,
            halfmove_clock: 0,
            fullmoves: 1,
        }
    }

    pub fn us(&self) -> Bitboard {
        self.board.by_color(self.turn)
    }

    pub fn our(&self, role: Role) -> Bitboard {
        self.us() & self.board.by_role(role)
    }

    pub fn them(&self) -> Bitboard {
        self.board.by_color(!self.turn)
    }

    pub fn their(&self, role: Role) -> Bitboard {
        self.them() & self.board.by_role(role)
    }
}
