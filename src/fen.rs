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
//! [`fen::fen()`](fn.fen.html) and [`fen::epd()`](fn.epd.html) can produce a
//! FEN for any [`Setup`].
//!
//! ```
//! use shakmaty::fen;
//! use shakmaty::fen::FenOpts;
//! use shakmaty::Chess;
//!
//! let pos = Chess::default();
//!
//! assert_eq!(fen::epd(&pos, &FenOpts::default()),
//!            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -");
//! ```
//!
//! [`Fen`] and [`Board`] also implement [`Display`]:
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
//! # use std::error::Error;
//! #
//! # use shakmaty::fen::Fen;
//! # use shakmaty::Chess;
//! #
//! # fn try_main() -> Result<(), Box<Error>> {
//! use shakmaty::Position;
//!
//! let input = "r1bqkbnr/ppp2Qpp/2np4/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4";
//!
//! let setup: Fen = input.parse()?;
//! let position: Chess = setup.position()?;
//! assert!(position.is_checkmate());
//! #
//! #     Ok(())
//! # }
//! #
//! # fn main() {
//! #     try_main().unwrap();
//! # }
//! ```
//!
//! [`Setup`]: ../trait.Setup.html
//! [`Board`]: ../struct.Board.html
//! [`Fen`]: struct.Fen.html
//! [`Display`]: https://doc.rust-lang.org/std/fmt/trait.Display.html

use std::str::FromStr;
use std::fmt;
use std::char;
use std::error::Error;

use btoi;

use square::Square;
use types::{Color, Black, White, Piece, Pockets, RemainingChecks};
use bitboard::Bitboard;
use board::Board;
use setup::Setup;
use position::{Position, PositionError};

/// FEN formatting options.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct FenOpts {
    promoted: bool,
    shredder: bool,
}

impl FenOpts {
    /// Standard X-FEN.
    pub fn new() -> FenOpts {
        FenOpts {
            promoted: false,
            shredder: false,
        }
    }

    /// Decide if promoted pieces should be tracked, e.g. `Q~`.
    pub fn promoted(&mut self, promoted: bool) -> &mut FenOpts {
        self.promoted = promoted;
        self
    }

    /// Decide if castling rights should be displayed in Shredder format,
    /// e.g. `HAha` instead of `KQkq`.
    pub fn shredder(&mut self, shredder: bool) -> &mut FenOpts {
        self.shredder = shredder;
        self
    }

    /// See [`board_fen()`](fn.board_fen.html).
    pub fn board_fen(&self, board: &Board) -> String {
        board_fen(board, self)
    }

    /// See [`epd()`](fn.epd.html).
    pub fn epd(&self, setup: &Setup) -> String {
        epd(setup, self)
    }

    /// See [`fen()`](fn.fen.html)
    pub fn fen(&self, setup: &Setup) -> String {
        fen(setup, self)
    }
}

impl Default for FenOpts {
    fn default() -> FenOpts {
        FenOpts::new()
    }
}

/// Errors that can occur when parsing a FEN.
#[derive(Eq, PartialEq, Debug)]
pub enum FenError {
    InvalidFen,
    InvalidBoard,
    InvalidPocket,
    InvalidTurn,
    InvalidCastling,
    InvalidEpSquare,
    InvalidRemainingChecks,
    InvalidHalfmoveClock,
    InvalidFullmoves,
}

impl FenError {
    fn desc(&self) -> &str {
        match *self {
            FenError::InvalidFen => "invalid fen",
            FenError::InvalidBoard => "invalid board part in fen",
            FenError::InvalidPocket => "invalid pocket in fen",
            FenError::InvalidTurn => "invalid turn part in fen",
            FenError::InvalidCastling => "invalid castling part in fen",
            FenError::InvalidEpSquare => "invalid ep square in fen",
            FenError::InvalidRemainingChecks => "invalid remaining checks in fen",
            FenError::InvalidHalfmoveClock => "invalid halfmove clock in fen",
            FenError::InvalidFullmoves => "invalid fullmove part in fen",
        }
    }
}

impl fmt::Display for FenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.desc().fmt(f)
    }
}

impl Error for FenError {
    fn description(&self) -> &str {
        self.desc()
    }
}

impl Board {
    fn from_board_fen(board_fen: &[u8]) -> Result<Board, FenError> {
        let mut board = Board::empty();

        let mut rank = 7i8;
        let mut file = 0i8;
        let mut promoted = false;

        for &ch in board_fen {
            if ch == b'/' && file == 8 {
                file = 0;
                rank -= 1;
            } else if ch == b'~' {
                promoted = true;
                continue;
            } else if b'1' <= ch && ch <= b'8' {
                file += (ch - b'0') as i8;
                if file > 8 {
                    return Err(FenError::InvalidBoard);
                }
            } else if let Some(piece) = Piece::from_char(ch as char) {
                match Square::from_coords(file, rank) {
                    Some(sq) => {
                        board.set_piece_at(sq, piece, promoted);
                        promoted = false;
                    }
                    None => return Err(FenError::InvalidBoard),
                }
                file += 1;
            } else {
                return Err(FenError::InvalidBoard);
            }

            if promoted {
                return Err(FenError::InvalidBoard);
            }
        }

        if rank == 0 && file == 8 {
            Ok(board)
        } else {
            Err(FenError::InvalidBoard)
        }
    }
}

impl FromStr for Board {
    type Err = FenError;

    fn from_str(board_fen: &str) -> Result<Board, FenError> {
        Board::from_board_fen(board_fen.as_bytes())
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", board_fen(self, FenOpts::new().promoted(true)))
    }
}

/// A parsed FEN.
#[derive(Clone, Eq, PartialEq, Debug)]
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

impl Default for Fen {
    fn default() -> Fen {
        Fen {
            board: Board::default(),
            pockets: None,
            turn: White,
            castling_rights: Bitboard::CORNERS,
            ep_square: None,
            remaining_checks: None,
            halfmove_clock: 0,
            fullmoves: 1,
        }
    }
}

impl Fen {
    /// The FEN of the empty position `8/8/8/8/8/8/8/8 w - - 0 1`.
    pub fn empty() -> Fen {
        Fen {
            board: Board::empty(),
            castling_rights: Bitboard(0),
            ..Fen::default()
        }
    }

    /// Set up a [`Position`].
    ///
    /// # Errors
    ///
    /// Returns [`PositionError`] if the setup is not a legal position.
    ///
    /// [`Position`]: ../trait.Position.html
    /// [`PositionError`]: ../enum.PositionError.html
    pub fn position<P: Position>(&self) -> Result<P, PositionError> {
        P::from_setup(self)
    }

    /// Parses a FEN.
    ///
    /// # Errors
    ///
    /// Returns [`FenError`] if the input is not a valid FEN.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::error::Error;
    /// #
    /// # fn try_main() -> Result<(), Box<Error>> {
    /// use shakmaty::fen::Fen;
    ///
    /// let fen = Fen::from_bytes(b"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")?;
    /// assert_eq!(fen, Fen::default());
    /// #
    /// #     Ok(())
    /// # }
    /// #
    /// # fn main() {
    /// #     try_main().unwrap();
    /// # }
    /// ```
    ///
    /// [`FenError`]: enum.FenError.html
    pub fn from_bytes(fen: &[u8]) -> Result<Fen, FenError> {
        let mut parts = fen.split(|ch| *ch == b' ');
        let mut result = Fen::empty();

        let board_part = parts.next().expect("splits have at least one part");

        let (board_part, pockets) = if board_part.ends_with(b"]") {
            let split_point = board_part
                .iter().position(|ch| *ch == b'[')
                .ok_or(FenError::InvalidBoard)?;
            let mut pockets = Pockets::default();
            for &ch in &board_part[(split_point + 1)..(board_part.len() - 1)] {
                pockets.add(Piece::from_char(ch as char).ok_or(FenError::InvalidPocket)?);
            }
            (&board_part[..split_point], Some(pockets))
        } else {
            (board_part, None)
        };

        result.board = Board::from_board_fen(board_part)?;
        result.pockets = pockets;

        result.turn = match parts.next() {
            Some(b"w") | None => White,
            Some(b"b") => Black,
            Some(_) => return Err(FenError::InvalidTurn),
        };

        match parts.next() {
            Some(b"-") | None => (),
            Some(castling_part) => {
                for &ch in castling_part {
                    let color = Color::from_bool(ch < b'a'); // uppercase

                    let candidates = Bitboard::relative_rank(color, 0) &
                                     result.board.by_piece(color.rook());

                    let flag = match ch | 32 {
                        b'k' => candidates.last(),
                        b'q' => candidates.first(),
                        file @ b'a' ... b'h' => {
                            (candidates & Bitboard::file((file as u8 - b'a') as i8)).first()
                        },
                        _ => return Err(FenError::InvalidCastling),
                    };

                    result.castling_rights.add(flag.ok_or(FenError::InvalidCastling)?);
                }
            }
        }

        match parts.next() {
            Some(b"-") | None => (),
            Some(ep_part) => {
                result.ep_square =
                    Some(Square::from_bytes(ep_part)
                            .map_err(|_| FenError::InvalidEpSquare)?);
            },
        }

        let halfmoves_part = if let Some(checks_part) = parts.next() {
            let mut checks = checks_part.splitn(2, |ch| *ch == b'+');
            if let (Some(w), Some(b)) = (checks.next(), checks.next()) {
                result.remaining_checks = Some(RemainingChecks {
                    white: btoi::btou(w).map_err(|_| FenError::InvalidRemainingChecks)?,
                    black: btoi::btou(b).map_err(|_| FenError::InvalidRemainingChecks)?,
                });
                parts.next()
            } else {
                Some(checks_part)
            }
        } else {
            None
        };

        if let Some(halfmoves_part) = halfmoves_part {
            result.halfmove_clock = btoi::btou_saturating(halfmoves_part)
                .map_err(|_| FenError::InvalidHalfmoveClock)?;
        }

        if let Some(fullmoves_part) = parts.next() {
            result.fullmoves = btoi::btou_saturating(fullmoves_part)
                .map_err(|_| FenError::InvalidFullmoves)?;
        }

        if parts.next().is_some() {
            Err(FenError::InvalidFen)
        } else {
            Ok(result)
        }
    }
}

impl FromStr for Fen {
    type Err = FenError;

    fn from_str(fen: &str) -> Result<Fen, FenError> {
        Fen::from_bytes(fen.as_bytes())
    }
}

impl fmt::Display for Fen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", fen(self, FenOpts::new().promoted(true)))
    }
}

fn castling_fen(board: &Board, castling_rights: Bitboard, opts: &FenOpts) -> String {
    let mut fen = String::with_capacity(4);

    for color in &[White, Black] {
        let king = board.king_of(*color);

        let candidates = board.by_piece(color.rook()) &
                         Bitboard::relative_rank(*color, 0);

        for rook in (candidates & castling_rights).rev() {
            if !opts.shredder && Some(rook) == candidates.first() && king.map_or(false, |k| rook < k) {
                fen.push(color.fold('Q', 'q'));
            } else if !opts.shredder && Some(rook) == candidates.last() && king.map_or(false, |k| k < rook) {
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

/// Create a board FEN such as `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR`.
pub fn board_fen(board: &Board, opts: &FenOpts) -> String {
    let mut fen = String::with_capacity(15);

    for rank in (0..8).rev() {
        let mut empty = 0;

        for file in 0..8 {
            let square = Square::from_coords(file, rank).unwrap();

            empty = board.piece_at(square).map_or_else(|| empty + 1, |piece| {
                if empty > 0 {
                    fen.push(char::from_digit(empty, 10).expect("at most 8 empty squares on a rank"));
                }
                fen.push(piece.char());
                if opts.promoted && board.promoted().contains(square) {
                    fen.push('~');
                }
                0
            });

            if file == 7 && empty > 0 {
                fen.push(char::from_digit(empty, 10).expect("at most 8 empty squares on a rank"));
            }

            if file == 7 && rank > 0 {
                fen.push('/')
            }
        }
    }

    fen
}

/// Create an EPD such as `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -`.
pub fn epd(setup: &Setup, opts: &FenOpts) -> String {
    let pockets = setup.pockets()
                       .map_or("".to_owned(), |p| format!("[{}]", p));

    let checks = setup.remaining_checks()
                      .map_or("".to_owned(), |r| format!(" {}", r));

    format!("{}{} {} {} {}{}",
            board_fen(setup.board(), opts),
            pockets,
            setup.turn().char(),
            castling_fen(setup.board(), setup.castling_rights(), opts),
            setup.ep_square().map_or("-".to_owned(), |sq| sq.to_string()),
            checks)
}

/// Create a FEN such as `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`.
pub fn fen(setup: &Setup, opts: &FenOpts) -> String {
    format!("{} {} {}", epd(setup, opts), setup.halfmove_clock(), setup.fullmoves())
}

#[cfg(test)]
mod tests {
    use super::*;
    use position::Chess;

    #[test]
    fn test_legal_ep_square() {
        let original_epd = "4k3/8/8/8/3Pp3/8/8/3KR3 b - d3";
        let fen: Fen = original_epd.parse().expect("valid fen");
        assert_eq!(epd(&fen, &FenOpts::default()), original_epd);

        // The en passant square is not actually legal.
        let pos: Chess = fen.position().expect("legal position");
        assert_eq!(epd(&pos, &FenOpts::default()),
                   "4k3/8/8/8/3Pp3/8/8/3KR3 b - -");
    }

    #[test]
    fn test_invalid_fen() {
        assert!("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQQKBNR w cq - 0P1".parse::<Fen>().is_err());
    }

    #[test]
    fn test_pockets() {
        let fen: Fen = "8/8/8/8/8/8/8/8[Q]".parse().expect("valid fen");
        assert_eq!(fen.pockets().map_or(0, |p| p.by_piece(White.queen())), 1);
    }

    #[test]
    fn test_shredder_fen() {
        let pos = Chess::default();
        assert_eq!(fen(&pos, FenOpts::default().shredder(true)),
                   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w HAha - 0 1");
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
    fn test_non_ascii() {
        // mind the dot in the castling part
        let input = "8/8/8/8/8/8/8/8 w · - 0 1";
        let error = input.parse::<Fen>().expect_err("invalid fen");
        assert_eq!(error, FenError::InvalidCastling);
    }
}
