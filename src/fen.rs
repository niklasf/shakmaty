use std::str::FromStr;
use std::ascii::AsciiExt;
use std::cmp::max;

use square::Square;
use position::{PositionError, Position};
use types::{Color, Black, White, Role, Piece, Pockets, RemainingChecks};
use bitboard::Bitboard;
use situation::Situation;
use board::Board;

pub trait Epd {
    const MARK_PROMOTED_PIECES: bool;

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
                self.board().board_fen(Self::MARK_PROMOTED_PIECES),
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

#[derive(Clone, Default)]
pub struct Setup {
    pub situation: Situation,
    pub pockets: Option<Pockets>,
    pub remaining_checks: Option<RemainingChecks>,
}

impl Setup {
    pub fn new() -> Setup {
        Setup::default()
    }

    pub fn empty() -> Setup {
        Setup {
            situation: Situation::empty(),
            pockets: None,
            remaining_checks: None,
        }
    }

    pub fn from_fen(fen: &str) -> Option<Setup> {
        let mut sit = Situation::empty();
        let mut parts = fen.split(' ');

        let mut pockets: Option<Pockets> = None;

        let maybe_board = parts.next().and_then(|board_part| {
            if board_part.ends_with(']') {
                if let Some(split_point) = board_part.find('[') {
                    let mut tmp_pockets = Pockets::default();
                    for ch in board_part[(split_point + 1)..(board_part.len() - 1)].chars() {
                        if let Some(piece) = Piece::from_char(ch) {
                            *tmp_pockets.mut_by_piece(piece) += 1;
                        } else {
                            return None
                        }
                    }
                    pockets = Some(tmp_pockets);
                    Some(&board_part[..split_point])
                } else {
                    None
                }
            } else {
                Some(&board_part)
            }
        });

        if let Some(board) = maybe_board.and_then(|board_fen| Board::from_board_fen(board_fen)) {
            sit.board = board;
        } else {
            return None;
        }

        match parts.next() {
            Some("w") => sit.turn = White,
            Some("b") => sit.turn = Black,
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
                                 sit.board.by_piece(Role::Rook.of(color));

                let flag = match ch.to_ascii_lowercase() {
                    'k'  => candidates.last(),
                    'q'  => candidates.first(),
                    file => (candidates & Bitboard::file(file as i8 - 'a' as i8)).first(),
                };

                match flag {
                    Some(cr) => sit.castling_rights.add(cr),
                    None     => return None
                }
            }
        }

        if let Some(ep_part) = parts.next() {
            if ep_part != "-" {
                match Square::from_str(ep_part) {
                    Ok(sq) => sit.ep_square = Some(sq),
                    _      => return None
                }
            }
        }

        if let Some(halfmoves_part) = parts.next() {
            match halfmoves_part.parse::<u32>() {
                Ok(halfmoves) => sit.halfmove_clock = halfmoves,
                _             => return None
            }
        }

        if let Some(fullmoves_part) = parts.next() {
            match fullmoves_part.parse::<u32>() {
                Ok(fullmoves) => sit.fullmoves = max(1, fullmoves),
                _             => return None
            }
        }

        Some(Setup { situation: sit, pockets, remaining_checks: None })
    }

    pub fn position<P: Position>(&self) -> Result<P, PositionError> {
        P::from_setup(self)
    }
}
