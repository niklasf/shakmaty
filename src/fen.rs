//! Parse and write Forsyth-Edwards-Notation.

use std::str::FromStr;
use std::ascii::AsciiExt;
use std::cmp::max;
use std::fmt;

use square::Square;
use types::{Color, Black, White, Role, Piece, Pockets, RemainingChecks};
use bitboard::Bitboard;
use board::Board;
use setup::Setup;
use position::{Position, PositionError};

/// A parsed FEN.
#[derive(Clone)]
pub struct Fen {
    pub board: Board,
    pub pockets: Option<Pockets>,
    pub turn: Color,
    pub castling_rights: Bitboard,
    pub ep_square: Option<Square>,
    pub remaining_checks: Option<RemainingChecks>,
    pub halfmove_clock: u32,
    pub fullmoves: u32,
}

impl Setup for Fen {
    fn board(&self) -> &Board { &self.board }
    fn pockets(&self) -> Option<&Pockets> { self.pockets.as_ref() }
    fn turn(&self) -> Color { self.turn }
    fn castling_rights(&self) -> Bitboard { self.castling_rights }
    fn ep_square(&self) -> Option<Square> { self.ep_square }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { self.remaining_checks.as_ref() }
    fn halfmove_clock(&self) -> u32 { self.halfmove_clock }
    fn fullmoves(&self) -> u32 { self.fullmoves }
}

/// Errors that can occur when parsing FENs.
#[derive(Debug)]
pub enum FenError {
    InvalidBoard,
    InvalidTurn,
    InvalidCastling,
    InvalidEpSquare,
    InvalidHalfmoveClock,
    InvalidFullmoves,
}

impl Default for Fen {
    fn default() -> Fen {
        Fen {
            board: Board::default(),
            pockets: None,
            turn: White,
            castling_rights: Bitboard(0x8100000000000081),
            ep_square: None,
            remaining_checks: None,
            halfmove_clock: 0,
            fullmoves: 1,
        }
    }
}

impl Fen {
    pub fn empty() -> Fen {
        Fen {
            board: Board::empty(),
            castling_rights: Bitboard::empty(),
            ..Fen::default()
        }
    }

    pub fn position<P: Position>(&self) -> Result<P, PositionError> {
        P::from_setup(self)
    }
}

impl FromStr for Fen {
    type Err = FenError;

    fn from_str(fen: &str) -> Result<Fen, FenError> {
        let mut result = Fen::empty();

        //let mut result = Fen:sit = Situation::empty();
        let mut parts = fen.split(' ');

        let board_part = parts.next().expect("splits have at least one part");

        result.board = Board::from_board_fen(board_part).ok_or(FenError::InvalidBoard)?;

        /* let mut pockets: Option<Pockets> = None;

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
        }); */

        result.turn = match parts.next() {
            Some("w") => White,
            Some("b") => Black,
            Some(_)   => return Err(FenError::InvalidTurn),
            None      => White,
        };

        if let Some(castling_part) = parts.next() {
            for ch in castling_part.chars() {
                if ch == '-' {
                    continue;
                }

                let color = Color::from_bool(ch.to_ascii_uppercase() == ch);

                let candidates = Bitboard::relative_rank(color, 0) &
                                 result.board.by_piece(Role::Rook.of(color));

                let flag = match ch.to_ascii_lowercase() {
                    'k'  => candidates.last(),
                    'q'  => candidates.first(),
                    file => (candidates & Bitboard::file(file as i8 - 'a' as i8)).first(),
                };

                result.castling_rights.add(flag.ok_or(FenError::InvalidCastling)?);
            }
        }

        match parts.next() {
            Some("-") | None => (),
            Some(ep_part) =>
                result.ep_square = Some(Square::from_str(ep_part).map_err(|_| FenError::InvalidEpSquare)?)
        }

        if let Some(halfmoves_part) = parts.next() {
            result.halfmove_clock = halfmoves_part.parse().map_err(|_| FenError::InvalidHalfmoveClock)?;
        }

        if let Some(fullmoves_part) = parts.next() {
            result.fullmoves = fullmoves_part.parse().map_err(|_| FenError::InvalidFullmoves)?;
        }

        Ok(result)
    }
}

fn castling_xfen(board: &Board, castling_rights: Bitboard) -> String {
    let mut fen = String::with_capacity(4);

    for color in &[White, Black] {
        let king = board.king_of(*color);

        let candidates = board.by_piece(color.rook()) &
                         Bitboard::relative_rank(*color, 0);

        for rook in (candidates & castling_rights).rev() {
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

/// Create an EPD such as `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -`.
pub fn epd<S: Setup>(setup: &S, promoted: bool) -> String {
    let pockets = setup.pockets()
                       .map_or("".to_owned(), |p| format!("[{}]", p));

    let checks = setup.remaining_checks()
                      .map_or("".to_owned(), |r| format!(" {}", r));

    format!("{}{} {} {} {}{}",
            setup.board().board_fen(promoted),
            pockets,
            setup.turn().char(),
            castling_xfen(setup.board(), setup.castling_rights()),
            setup.ep_square().map_or("-".to_owned(), |sq| sq.to_string()),
            checks)
}

/// Create a FEN such as `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`.
pub fn fen<S: Setup>(setup: &S, promoted: bool) -> String {
    format!("{} {} {}", epd(setup, promoted), setup.halfmove_clock(), setup.fullmoves())
}

impl fmt::Display for Fen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", fen(self, true))
    }
}
