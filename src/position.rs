use std::cmp::max;
use std::char;
use std::ascii::AsciiExt;
use std::str::FromStr;
use std::fmt;

use square;
use square::Square;
use types::{Color, White, Black, Role, Piece, Move, Uci, ROLES};
use bitboard::Bitboard;
use board::Board;
use attacks;

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

pub trait Epd {
    const PROMOTED: bool;

    fn board(&self) -> &Board;
    fn pockets(&self) -> Option<&Pockets>;
    fn turn(&self) -> Color;
    fn castling_rights(&self) -> Bitboard;
    fn ep_square(&self) -> Option<Square>;
    fn remaining_checks(&self) -> Option<&RemainingChecks>;

    fn castling_xfen(&self) -> String {
        let mut fen = String::with_capacity(4);

        for color in &[White, Black] {
            let king = self.board().king_of(*color);

            let candidates = self.board().by_piece(color.rook()) &
                             Bitboard::relative_rank(*color, 0);

            for rook in (candidates & self.castling_rights()).rev() {
                if Some(rook) == candidates.first() && king.map_or(false, |k| rook < k) {
                    fen.push(color.fold('Q', 'q'));
                } else if Some(rook) == candidates.last() && king.map_or(false, |k| k < rook) {
                    fen.push(color.fold('K', 'k'));
                } else {
                    fen.push((rook.file() as u8 + color.fold('A', 'a') as u8) as char);
                }
            }
        }

        if fen.is_empty() {
            fen.push('-');
        }

        fen
    }

    fn epd(&self) -> String {
        let pockets = self.pockets()
                          .map_or("".to_owned(), |p| format!("[{}]", p));

        let checks = self.remaining_checks()
                         .map_or("".to_owned(), |r| format!(" {}", r));

        format!("{}{} {} {} {}{}",
                self.board().board_fen(Self::PROMOTED),
                pockets,
                self.turn().char(),
                self.castling_xfen(),
                self.ep_square().map_or("-".to_owned(), |sq| sq.to_string()),
                checks)
    }
}

pub trait Fen: Epd {
    fn halfmove_clock(&self) -> u32;
    fn fullmoves(&self) -> u32;

    fn fen(&self) -> String {
        format!("{} {} {}", self.epd(), self.halfmove_clock(), self.fullmoves())
    }
}

#[derive(Clone)]
pub struct Position {
    board: Board,
    turn: Color,
    castling_rights: Bitboard,
    ep_square: Option<Square>,
    halfmove_clock: u32,
    fullmoves: u32,
}

impl Default for Position {
    fn default() -> Self {
        Position {
            board: Board::default(),
            turn: White,
            castling_rights: Bitboard(0x8100000000000081),
            ep_square: None,
            halfmove_clock: 0,
            fullmoves: 1,
        }
    }
}

impl Epd for Position {
    const PROMOTED: bool = true;

    fn board(&self) -> &Board { &self.board }
    fn pockets(&self) -> Option<&Pockets> { None }
    fn turn(&self) -> Color { self.turn }
    fn castling_rights(&self) -> Bitboard { self.castling_rights }
    fn ep_square(&self) -> Option<Square> { self.ep_square }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }
}

impl Fen for Position {
    fn halfmove_clock(&self) -> u32 { self.halfmove_clock }
    fn fullmoves(&self) -> u32 { self.fullmoves }
}

impl Position {
    pub fn do_move(mut self, m: &Move) -> Self {
        let color = self.turn();
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

    pub fn empty() -> Position {
        Position {
            board: Board::empty(),
            turn: White,
            castling_rights: Bitboard(0),
            ep_square: None,
            halfmove_clock: 0,
            fullmoves: 1,
        }
    }

    pub fn from_fen(fen: &str) -> Option<Position> {
        let mut pos = Position::empty();
        let mut parts = fen.split(' ');

        if let Some(board) = parts.next().and_then(|board_fen| Board::from_board_fen(board_fen)) {
            pos.board = board
        } else {
            return None
        }

        match parts.next() {
            Some("w") => pos.turn = White,
            Some("b") => pos.turn = Black,
            Some(_)   => return None,
            None      => ()
        }

        if let Some(castling_part) = parts.next() {
            for ch in castling_part.chars() {
                if ch == '-' {
                    continue;
                }

                let color = Color::from_bool(ch.to_ascii_uppercase() == ch);

                let candidates = Bitboard::relative_rank(color, 0) &
                                 pos.board.by_piece(Role::Rook.of(color));

                let flag = match ch.to_ascii_lowercase() {
                    'k'  => candidates.last(),
                    'q'  => candidates.first(),
                    file => (candidates & Bitboard::file(file as i8 - 'a' as i8)).first(),
                };

                match flag {
                    Some(cr) => pos.castling_rights.add(cr),
                    None     => return None
                }
            }
        }

        if let Some(ep_part) = parts.next() {
            if ep_part != "-" {
                match Square::from_str(ep_part) {
                    Ok(sq) => pos.ep_square = Some(sq),
                    _      => return None
                }
            }
        }

        if let Some(halfmoves_part) = parts.next() {
            match halfmoves_part.parse::<u32>() {
                Ok(halfmoves) => pos.halfmove_clock = halfmoves,
                _             => return None
            }
        }

        if let Some(fullmoves_part) = parts.next() {
            match fullmoves_part.parse::<u32>() {
                Ok(fullmoves) => pos.fullmoves = max(1, fullmoves),
                _             => return None
            }
        }

        Some(pos)
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use square;

    /* #[test]
    fn test_castling_moves() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/5NP1/PPPPPPBP/RNBQK2R w KQkq - 0 1";
        let pos = Position::from_fen(fen).unwrap();

        let castle = Move::Castle { king: square::E1, rook: square::H1 };
        let mut moves = Vec::new();
        pos.legal_moves(&mut moves);
        assert!(moves.contains(&castle));

        let pos = pos.do_move(&castle);
        assert_eq!(pos.board().piece_at(square::G1), Some(White.king()));
        assert_eq!(pos.board().piece_at(square::F1), Some(White.rook()));
    }

    #[test]
    fn test_chess960_castling() {
        let fen = "r1k1r2q/p1ppp1pp/8/8/8/8/P1PPP1PP/R1K1R2Q w KQkq - 0 1";
        let pos = Position::from_fen(fen).unwrap();
        assert_eq!(pos.fen(), fen);

        let qs = Move::Castle { king: square::C1, rook: square::A1 };
        let ks = Move::Castle { king: square::C1, rook: square::E1 };

        let mut moves = Vec::new();
        pos.legal_moves(&mut moves);
        assert!(moves.contains(&qs));
        assert!(moves.contains(&ks));
    } */

    #[test]
    fn test_fen() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let pos = Position::from_fen(fen).unwrap();
        assert_eq!(pos.fen(), fen);

        let fen = "4k3/8/8/8/8/8/8/4K2R w K - 0 1";
        let pos = Position::from_fen(fen).unwrap();
        assert_eq!(pos.fen(), fen);
    }

    #[test]
    fn test_do_move() {
        let pos = Position::from_fen("rb6/5b2/1p2r3/p1k1P3/PpP1p3/2R4P/3P4/1N1K2R1 w - -").unwrap();
        let m = Move::Normal { role: Role::Rook, from: square::C3, to: square::C1, capture: None, promotion: None };
        let pos = pos.do_move(&m);
        assert_eq!(pos.fen(), "rb6/5b2/1p2r3/p1k1P3/PpP1p3/7P/3P4/1NRK2R1 b - - 1 1");
    }

    #[test]
    fn test_ep_fen() {
        let pos = Position::default();
        let m = Move::Normal { role: Role::Pawn, from: square::H2, to: square::H4, capture: None, promotion: None };
        let pos = pos.do_move(&m);
        assert_eq!(pos.fen(), "rnbqkbnr/pppppppp/8/8/7P/8/PPPPPPP1/RNBQKBNR b KQkq h3 0 1");

        let m = Move::Normal { role: Role::Knight, from: square::B8, to: square::C6, capture: None, promotion: None };
        let pos = pos.do_move(&m);
        assert_eq!(pos.fen(), "r1bqkbnr/pppppppp/2n5/8/7P/8/PPPPPPP1/RNBQKBNR w KQkq - 1 2");
    }

    /* #[test]
    fn test_san() {
        let pos = Position::default();
        let m = Move::Normal { role: Role::Knight, from: square::G1, capture: None, to: square::F3, promotion: None };
        assert_eq!(pos.san(&m), "Nf3");
    } */
}
