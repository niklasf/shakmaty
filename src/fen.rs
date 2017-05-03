use std::str::FromStr;
use std::ascii::AsciiExt;
use std::cmp::max;

use square::Square;
use position::{PositionError, Position};
use types::{Color, Black, White, Role, Piece, Pockets, RemainingChecks};
use bitboard::Bitboard;
use situation::Situation;
use board::Board;

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

