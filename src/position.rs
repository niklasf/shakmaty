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

impl Position {
    pub fn board(&self) -> &Board { &self.board }
    pub fn turn(&self) -> Color { self.turn }
    pub fn castling_rights(&self) -> Bitboard { self.castling_rights }
    pub fn ep_square(&self) -> Option<Square> { self.ep_square }
    pub fn halfmove_clock(&self) -> u32 { self.halfmove_clock }
    pub fn fullmoves(&self) -> u32 { self.fullmoves }

    pub fn castling_xfen(&self) -> String {
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

    pub fn validate(&self, uci: &Uci) -> Option<Move> {
        match *uci {
            Uci::Normal { from, to, promotion } => {
                self.board().role_at(from).map(|role| {
                    if role == Role::King {
                        if self.board().by_piece(self.turn().rook()).contains(to) {
                            return Move::Castle { king: from, rook: to }
                        }
                    }
                    Move::Normal { role, from, capture: self.board().role_at(to), to, promotion }
                })
            },
            Uci::Put { role, to } => Some(Move::Put { role, to }),
            Uci::Null => Some(Move::Null)
        }
    }

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

/* #[cfg(test)]
mod tests {
    use super::*;
    use square;

    #[test]
    fn test_castling_moves() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/5NP1/PPPPPPBP/RNBQK2R w KQkq - 0 1";
        let pos = Position::from_fen(fen).unwrap();

        let castle = pos.validate(&Uci::from_str("e1h1").unwrap()).unwrap();
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
    }

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
        let m = pos.validate(&Uci::from_str("c3c1").unwrap()).unwrap();
        let pos = pos.do_move(&m);
        assert_eq!(pos.fen(), "rb6/5b2/1p2r3/p1k1P3/PpP1p3/7P/3P4/1NRK2R1 b - - 1 1");
    }

    #[test]
    fn test_ep_fen() {
        let pos = Position::default();
        let m = pos.validate(&Uci::from_str("h2h4").unwrap()).unwrap();
        let pos = pos.do_move(&m);
        assert_eq!(pos.fen(), "rnbqkbnr/pppppppp/8/8/7P/8/PPPPPPP1/RNBQKBNR b KQkq h3 0 1");

        let m = pos.validate(&Uci::from_str("b8c6").unwrap()).unwrap();
        let pos = pos.do_move(&m);
        assert_eq!(pos.fen(), "r1bqkbnr/pppppppp/2n5/8/7P/8/PPPPPPP1/RNBQKBNR w KQkq - 1 2");
    }

    #[test]
    fn test_san() {
        let pos = Position::default();
        let m = Move::Normal { role: Role::Knight, from: square::G1, capture: None, to: square::F3, promotion: None };
        assert_eq!(pos.san(&m), "Nf3");
    }
} */
