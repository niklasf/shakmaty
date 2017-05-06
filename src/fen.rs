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

//! Parse and write Forsyth-Edwards-Notation.
//!
//! # Examples
//!
//! `fen::fen()` and `fen::epd()` can produce a FEN for any `Setup`.
//!
//! ```
//! use shakmaty::fen;
//! use shakmaty::Chess;
//!
//! let pos = Chess::default();
//! assert_eq!(fen::epd(&pos, false), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -");
//! ```
//!
//! `Fen` also implements `Display`:
//!
//! ```
//! use shakmaty::fen::Fen;
//!
//! let empty_fen = Fen::empty();
//! assert_eq!(empty_fen.to_string(), "8/8/8/8/8/8/8/8 w - - 0 1");
//! ```
//!
//! Parsing FENs:
//!
//! ```
//! # use shakmaty::fen::Fen;
//! # use shakmaty::Chess;
//! use shakmaty::Position;
//!
//! let input = "r1bqkbnr/ppp2Qpp/2np4/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4";
//!
//! let setup: Fen = input.parse().expect("invalid fen");
//! let position: Chess = setup.position().expect("illegal position");
//! assert!(position.is_checkmate());
//! ```

use std::str::FromStr;
use std::ascii::AsciiExt;
use std::fmt;

use square::Square;
use types::{Color, Black, White, Piece, Pockets, RemainingChecks};
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
    InvalidPocket,
    InvalidTurn,
    InvalidCastling,
    InvalidEpSquare,
    InvalidRemainingChecks,
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

        let mut parts = fen.split(' ');

        let board_part = parts.next().expect("splits have at least one part");

        let (board_part, pockets) = if board_part.ends_with(']') {
            let split_point = board_part.find('[').ok_or(FenError::InvalidBoard)?;
            let mut pockets = Pockets::default();
            for ch in board_part[(split_point + 1)..(board_part.len() - 1)].chars() {
                let piece = Piece::from_char(ch).ok_or(FenError::InvalidPocket)?;
                pockets.add(piece);
            }
            (&board_part[..split_point], Some(pockets))
        } else {
            (board_part, None)
        };

        result.board = Board::from_board_fen(board_part).ok_or(FenError::InvalidBoard)?;
        result.pockets = pockets;

        result.turn = match parts.next() {
            Some("w") | None => White,
            Some("b") => Black,
            Some(_) => return Err(FenError::InvalidTurn),
        };

        if let Some(castling_part) = parts.next() {
            for ch in castling_part.chars() {
                if ch == '-' {
                    continue;
                }

                let color = Color::from_bool(ch.to_ascii_uppercase() == ch);

                let candidates = Bitboard::relative_rank(color, 0) &
                                 result.board.by_piece(&color.rook());

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

        let halfmoves_part = if let Some(checks_part) = parts.next() {
            let mut checks = checks_part.splitn(2, '+');
            if let (Some(w), Some(b)) = (checks.next(), checks.next()) {
                result.remaining_checks = Some(RemainingChecks {
                    white: w.parse().map_err(|_| FenError::InvalidRemainingChecks)?,
                    black: b.parse().map_err(|_| FenError::InvalidRemainingChecks)?,
                });
                parts.next()
            } else {
                Some(checks_part)
            }
        } else {
            None
        };

        if let Some(halfmoves_part) = halfmoves_part {
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

        let candidates = board.by_piece(&color.rook()) &
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
pub fn epd(setup: &Setup, promoted: bool) -> String {
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
pub fn fen(setup: &Setup, promoted: bool) -> String {
    format!("{} {} {}", epd(setup, promoted), setup.halfmove_clock(), setup.fullmoves())
}

impl fmt::Display for Fen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", fen(self, true))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use position::Chess;

    #[test]
    fn test_pockets() {
        let fen: Fen = "8/8/8/8/8/8/8/8[Q]".parse().expect("valid fen");
        assert_eq!(fen.pockets().map_or(0, |p| p.by_piece(&White.queen())), 1);
    }

    #[test]
    fn test_remaining_checks() {
        let fen: Fen = "8/8/8/8/8/8/8/8 w - - 1+2 12 42".parse().expect("valid fen");
        let expected = RemainingChecks { white: 1, black: 2 };
        assert_eq!(fen.remaining_checks, Some(expected));
        assert_eq!(fen.halfmove_clock, 12);
        assert_eq!(fen.fullmoves, 42);
    }

    #[test]
    fn test_legal_ep_square() {
        let original_epd = "4k3/8/8/8/3Pp3/8/8/3KR3 b - d3";
        let fen: Fen = original_epd.parse().expect("valid fen");
        assert_eq!(epd(&fen, false), original_epd);

        // The en passant square is not actually legal.
        let pos: Chess = fen.position().expect("legal position");
        assert_eq!(epd(&pos, false), "4k3/8/8/8/3Pp3/8/8/3KR3 b - -");
    }
}
