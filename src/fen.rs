// This file is part of the shakmaty library.
// Copyright (C) 2017-2021 Niklas Fiekas <niklas.fiekas@backscattering.de>
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
//! # En passant only if fully legal
//!
//! This implementation intentionally deviates from the specification in
//! a backwards compatible way: [`Setup::ep_square()`]
//! implemented for each [`Position`](crate::Position) omits en passant
//! squares, unless there is a fully legal en passant capture.
//!
//! # Examples
//!
//! [`fen::fen()`](fen()) and [`fen::epd()`](epd()) can produce a
//! FEN for any [`Setup`].
//!
//! ```
//! use shakmaty::fen;
//! use shakmaty::Chess;
//!
//! let pos = Chess::default();
//!
//! assert_eq!(fen::epd(&pos),
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
//! use shakmaty::{CastlingMode, Position};
//!
//! let input = "r1bqkbnr/ppp2Qpp/2np4/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4";
//!
//! let setup: Fen = input.parse()?;
//! let position: Chess = setup.position(CastlingMode::Standard)?;
//! assert!(position.is_checkmate());
//! #
//! # Ok::<_, Box<dyn Error>>(())
//! ```
//!
//! [`Setup`]: super::Setup
//! [`Board`]: super::Board
//! [`Display`]: std::fmt::Display

use std::convert::TryFrom;
use std::cmp::max;
use std::num::NonZeroU32;
use std::str::FromStr;
use std::fmt;
use std::char;
use std::error::Error;

use crate::square::{File, Rank, Square};
use crate::color::{ByColor, Color};
use crate::types::{Piece, CastlingMode, RemainingChecks};
use crate::material::Material;
use crate::bitboard::Bitboard;
use crate::board::Board;
use crate::setup::Setup;
use crate::position::{FromSetup, PositionError};

/// FEN formatting options.
#[derive(Default, Clone, Debug)]
pub struct FenOpts {
    shredder: bool,
    scid: bool,
}

impl FenOpts {
    /// Default X-FEN formatting.
    pub fn new() -> FenOpts {
        FenOpts::default()
    }

    /// Decide if castling rights should be displayed in Shredder format,
    /// e.g. `HAha` instead of `KQkq`.
    pub fn shredder(&mut self, shredder: bool) -> &mut FenOpts {
        self.shredder = shredder;
        self
    }

    /// Decide if Crazyhouse pockets and remaining check counters should use
    /// Scid-style, e.g. `/q` instead of `[q]` and `+0+0` instead of `3+3`.
    pub fn scid(&mut self, scid: bool) -> &mut FenOpts {
        self.scid = scid;
        self
    }

    fn castling_fen(&self, board: &Board, castling_rights: Bitboard) -> String {
        let mut fen = String::with_capacity(4);

        for color in Color::ALL {
            let king = board.king_of(color);

            let candidates = board.by_piece(color.rook()) & Bitboard::relative_rank(color, Rank::First);

            for rook in (candidates & castling_rights).into_iter().rev() {
                if !self.shredder && Some(rook) == candidates.first() && king.map_or(false, |k| rook < k) {
                    fen.push(color.fold('Q', 'q'));
                } else if !self.shredder && Some(rook) == candidates.last() && king.map_or(false, |k| k < rook) {
                    fen.push(color.fold('K', 'k'));
                } else {
                    let file = rook.file();
                    fen.push(color.fold(file.char().to_ascii_uppercase(), file.char()));
                }
            }
        }

        if fen.is_empty() {
            fen.push('-');
        }

        fen
    }

    /// Create an EPD such as
    /// `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -`.
    pub fn epd(&self, setup: &dyn Setup) -> String {
        let pockets = setup.pockets().map_or("".to_owned(), |p| {
            if self.scid {
                format!("/{}", p.fen())
            } else {
                format!("[{}]", p.fen())
            }
        });

        let checks = setup.remaining_checks().map_or("".to_owned(), |r| {
            if self.scid {
                format!(" +{}+{}", 3u8.saturating_sub(u8::from(r.white)), 3u8.saturating_sub(u8::from(r.black)))
            } else {
                format!(" {}", r)
            }
        });

        let board = setup.board();

        format!("{}{} {} {} {}{}",
                board.board_fen(setup.promoted()),
                pockets,
                setup.turn().char(),
                self.castling_fen(board, setup.castling_rights()),
                setup.ep_square().map_or("-".to_owned(), |sq| sq.to_string()),
                checks)
    }

    /// Create a FEN such as
    /// `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`.
    pub fn fen(&self, setup: &dyn Setup) -> String {
        match setup.remaining_checks() {
            Some(checks) if self.scid => {
                let board = setup.board();

                format!("{}{} {} {} {} {} {} +{}+{}",
                    board.board_fen(setup.promoted()),
                    setup.pockets().map_or("".to_owned(), |p| format!("/{}", p.fen())),
                    setup.turn().char(),
                    self.castling_fen(board, setup.castling_rights()),
                    setup.ep_square().map_or("-".to_owned(), |sq| sq.to_string()),
                    setup.halfmoves(),
                    setup.fullmoves(),
                    3u8.saturating_sub(u8::from(checks.white)),
                    3u8.saturating_sub(u8::from(checks.black)))
            }
            _ => format!("{} {} {}", self.epd(setup), setup.halfmoves(), setup.fullmoves())
        }
    }
}

/// Errors that can occur when parsing a FEN.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ParseFenError {
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

impl ParseFenError {
    fn desc(&self) -> &str {
        match *self {
            ParseFenError::InvalidFen => "invalid fen",
            ParseFenError::InvalidBoard => "invalid board part in fen",
            ParseFenError::InvalidPocket => "invalid pocket in fen",
            ParseFenError::InvalidTurn => "invalid turn part in fen",
            ParseFenError::InvalidCastling => "invalid castling part in fen",
            ParseFenError::InvalidEpSquare => "invalid ep square in fen",
            ParseFenError::InvalidRemainingChecks => "invalid remaining checks in fen",
            ParseFenError::InvalidHalfmoveClock => "invalid halfmove clock in fen",
            ParseFenError::InvalidFullmoves => "invalid fullmove part in fen",
        }
    }
}

impl fmt::Display for ParseFenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.desc().fmt(f)
    }
}

impl Error for ParseFenError {}

fn parse_board_fen(board_fen: &[u8]) -> Result<(Board, Bitboard), ParseFenError> {
    let mut promoted = Bitboard(0);
    let mut board = Board::empty();

    let mut rank = 7i8;
    let mut file = 0i8;

    let mut iter = board_fen.iter().cloned().peekable();

    while let Some(ch) = iter.next() {
        if ch == b'/' && file == 8 {
            file = 0;
            rank -= 1;
        } else if (b'1'..=b'8').contains(&ch) {
            file += (ch - b'0') as i8;
            if file > 8 {
                return Err(ParseFenError::InvalidBoard);
            }
        } else if let Some(piece) = Piece::from_char(char::from(ch)) {
            match (File::try_from(file), Rank::try_from(rank)) {
                (Ok(f), Ok(r)) => {
                    let sq = Square::from_coords(f, r);
                    let is_promoted = iter.peek() == Some(&b'~');
                    if is_promoted {
                        promoted.add(sq);
                        iter.next();
                    }
                    board.set_piece_at(sq, piece);
                }
                _ => return Err(ParseFenError::InvalidBoard),
            }
            file += 1;
        } else {
            return Err(ParseFenError::InvalidBoard);
        }
    }

    if rank == 0 && file == 8 {
        Ok((board, promoted))
    } else {
        Err(ParseFenError::InvalidBoard)
    }
}

fn parse_remaining_checks(s: &[u8]) -> Option<ByColor<RemainingChecks>> {
    let mut split = s.splitn(3, |ch| *ch == b'+');
    Some(match (split.next(), split.next(), split.next()) {
        (Some(b""), Some(white_given), Some(black_given)) => {
            // format: +0+0
            ByColor {
                white: RemainingChecks(3u8.checked_sub(btoi::btou(white_given).ok()?)?),
                black: RemainingChecks(3u8.checked_sub(btoi::btoi(black_given).ok()?)?),
            }
        }
        (Some(white), Some(black), None) => {
            // format: 3+3
            ByColor {
                white: RemainingChecks(btoi::btou(white).ok()?),
                black: RemainingChecks(btoi::btou(black).ok()?),
            }
        }
        _ => return None
    })
}

impl Board {
    pub fn from_board_fen(board_fen: &[u8]) -> Result<Board, ParseFenError> {
        Ok(parse_board_fen(board_fen)?.0)
    }

    /// Create a board FEN such as
    /// `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR`.
    ///
    /// Promoted pieces are marked like `Q~`.
    pub fn board_fen(&self, promoted: Bitboard) -> String {
        let mut fen = String::with_capacity(15);

        for rank in IntoIterator::into_iter(Rank::ALL).rev() {
            let mut empty = 0;

            for file in File::ALL {
                let square = Square::from_coords(file, rank);

                empty = self.piece_at(square).map_or_else(|| empty + 1, |piece| {
                    if empty > 0 {
                        fen.push(char::from_digit(empty, 10).expect("at most 8 empty squares on a rank"));
                    }
                    fen.push(piece.char());
                    if promoted.contains(square) {
                        fen.push('~');
                    }
                    0
                });

                if file == File::H && empty > 0 {
                    fen.push(char::from_digit(empty, 10).expect("at most 8 empty squares on a rank"));
                }

                if file == File::H && rank > Rank::First {
                    fen.push('/')
                }
            }
        }

        fen
    }
}

impl FromStr for Board {
    type Err = ParseFenError;

    fn from_str(board_fen: &str) -> Result<Board, ParseFenError> {
        Board::from_board_fen(board_fen.as_bytes())
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.board_fen(Bitboard(0)))
    }
}

/// A parsed FEN.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Fen {
    pub board: Board,
    pub promoted: Bitboard,
    pub pockets: Option<Material>,
    pub turn: Color,
    pub castling_rights: Bitboard,
    pub ep_square: Option<Square>,
    pub remaining_checks: Option<ByColor<RemainingChecks>>,
    pub halfmoves: u32,
    pub fullmoves: NonZeroU32,
}

impl Setup for Fen {
    fn board(&self) -> &Board { &self.board }
    fn promoted(&self) -> Bitboard { self.promoted }
    fn pockets(&self) -> Option<&Material> { self.pockets.as_ref() }
    fn turn(&self) -> Color { self.turn }
    fn castling_rights(&self) -> Bitboard { self.castling_rights }
    fn ep_square(&self) -> Option<Square> { self.ep_square }
    fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> { self.remaining_checks.as_ref() }
    fn halfmoves(&self) -> u32 { self.halfmoves }
    fn fullmoves(&self) -> NonZeroU32 { self.fullmoves }
}

impl Default for Fen {
    fn default() -> Fen {
        Fen {
            board: Board::default(),
            promoted: Bitboard(0),
            pockets: None,
            turn: Color::White,
            castling_rights: Bitboard::CORNERS,
            ep_square: None,
            remaining_checks: None,
            halfmoves: 0,
            fullmoves: NonZeroU32::new(1).unwrap(),
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
    /// [`FromSetup`]: super::FromSetup
    /// [`Position`]: super::Position
    /// [`PositionError`]: super::PositionError
    pub fn position<P: FromSetup>(&self, mode: CastlingMode) -> Result<P, PositionError<P>> {
        P::from_setup(self, mode)
    }

    /// Parses a FEN.
    ///
    /// # Errors
    ///
    /// Returns [`ParseFenError`] if the input is not a valid FEN.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::error::Error;
    /// #
    /// use shakmaty::fen::Fen;
    ///
    /// let fen = Fen::from_ascii(b"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")?;
    /// assert_eq!(fen, Fen::default());
    /// #
    /// # Ok::<_, Box<dyn Error>>(())
    /// ```
    pub fn from_ascii(fen: &[u8]) -> Result<Fen, ParseFenError> {
        let mut parts = fen.split(|ch| *ch == b' ');
        let mut result = Fen::empty();

        let board_part = parts.next().expect("splits have at least one part");

        let (board_part, pocket_part) = if board_part.ends_with(b"]") {
            // format: ...[pocket]
            let split_point = board_part
                .iter()
                .position(|ch| *ch == b'[')
                .ok_or(ParseFenError::InvalidBoard)?;
            let pocket_part = &board_part[(split_point + 1)..(board_part.len() - 1)];
            (&board_part[..split_point], Some(pocket_part))
        } else if let Some(split_point) = board_part.iter().enumerate().filter_map(|(idx, ch)| (*ch == b'/').then(|| idx)).nth(7) {
            // format: .../pocket
            (&board_part[..split_point], Some(&board_part[(split_point + 1)..]))
        } else {
            (board_part, None)
        };

        let (board, promoted) = parse_board_fen(board_part)?;
        result.board = board;
        result.promoted = promoted;

        if let Some(pocket_part) = pocket_part {
            result.pockets = Some(Material::from_ascii_fen(pocket_part).map_err(|_| ParseFenError::InvalidPocket)?);
        }

        result.turn = match parts.next() {
            Some(b"w") | None => Color::White,
            Some(b"b") => Color::Black,
            Some(_) => return Err(ParseFenError::InvalidTurn),
        };

        match parts.next() {
            Some(b"-") | None => (),
            Some(castling_part) => {
                for &ch in castling_part {
                    let color = Color::from_white(ch < b'a'); // uppercase

                    let candidates = Bitboard::relative_rank(color, Rank::First) &
                                     result.board.by_piece(color.rook());

                    let flag = match ch | 32 {
                        b'k' => candidates.last(),
                        b'q' => candidates.first(),
                        file @ b'a'..=b'h' => {
                            (candidates & File::new(u32::from(file as u8 - b'a'))).first()
                        }
                        _ => return Err(ParseFenError::InvalidCastling),
                    };

                    result.castling_rights.add(flag.ok_or(ParseFenError::InvalidCastling)?);
                }
            }
        }

        match parts.next() {
            Some(b"-") | None => (),
            Some(ep_part) => {
                result.ep_square =
                    Some(Square::from_ascii(ep_part).map_err(|_| ParseFenError::InvalidEpSquare)?);
            }
        }

        let halfmoves_part = if let Some(checks_part) = parts.next() {
            if let Some(remaining_checks) = parse_remaining_checks(checks_part) {
                result.remaining_checks = Some(remaining_checks);
                parts.next()
            } else {
                Some(checks_part)
            }
        } else {
            None
        };

        if let Some(halfmoves_part) = halfmoves_part {
            result.halfmoves = btoi::btou_saturating(halfmoves_part)
                .map_err(|_| ParseFenError::InvalidHalfmoveClock)?;
        }

        if let Some(fullmoves_part) = parts.next() {
            let fullmoves = btoi::btou_saturating(fullmoves_part).map_err(|_| ParseFenError::InvalidFullmoves)?;
            result.fullmoves = NonZeroU32::new(max(fullmoves, 1)).unwrap();
        }

        let last_part = if let Some(checks_part) = parts.next() {
            if result.remaining_checks.is_some() {
                Some(checks_part) // got checks earlier
            } else if let Some(remaining_checks) = parse_remaining_checks(checks_part) {
                result.remaining_checks = Some(remaining_checks);
                parts.next()
            } else {
                Some(checks_part)
            }
        } else {
            None
        };

        if last_part.is_some() {
            Err(ParseFenError::InvalidFen)
        } else {
            Ok(result)
        }
    }

    pub fn from_setup<S: Setup>(setup: &S) -> Fen {
        Fen {
            board: setup.board().clone(),
            promoted: setup.promoted(),
            pockets: setup.pockets().cloned(),
            turn: setup.turn(),
            castling_rights: setup.castling_rights(),
            ep_square: setup.ep_square(),
            remaining_checks: setup.remaining_checks().cloned(),
            halfmoves: setup.halfmoves(),
            fullmoves: setup.fullmoves(),
        }
    }
}

impl FromStr for Fen {
    type Err = ParseFenError;

    fn from_str(fen: &str) -> Result<Fen, ParseFenError> {
        Fen::from_ascii(fen.as_bytes())
    }
}

impl fmt::Display for Fen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", FenOpts::default().fen(self))
    }
}

/// Create an EPD such as
/// `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -` with default
/// [`FenOpts`].
pub fn epd(setup: &dyn Setup) -> String {
    FenOpts::default().epd(setup)
}

/// Create a FEN such as
/// `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1` with default
/// [`FenOpts`].
pub fn fen(setup: &dyn Setup) -> String {
    FenOpts::default().fen(setup)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::position::Chess;

    #[test]
    fn test_legal_ep_square() {
        let original_epd = "4k3/8/8/8/3Pp3/8/8/3KR3 b - d3";
        let fen: Fen = original_epd.parse().expect("valid fen");
        assert_eq!(epd(&fen), original_epd);

        // The en passant square is not actually legal.
        let pos: Chess = fen.position(CastlingMode::Standard).expect("legal position");
        assert_eq!(epd(&pos), "4k3/8/8/8/3Pp3/8/8/3KR3 b - -");
    }

    #[test]
    fn test_invalid_fen() {
        assert!("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQQKBNR w cq - 0P1".parse::<Fen>().is_err());
    }

    #[test]
    fn test_pockets() {
        let fen: Fen = "8/8/8/8/8/8/8/8[Q]".parse().expect("valid fen");
        assert_eq!(fen.pockets().map_or(0, |p| p.by_piece(Color::White.queen())), 1);
    }

    #[test]
    fn test_lichess_promoted() {
        let input = "rnbqk1nQ~/ppppp3/8/5p2/8/5N2/PPPPPPP1/RNBQKB1R/PPBR b KQq - 0 6";
        let fen: Fen = input.parse().expect("valid fen");
        assert_eq!(fen.promoted, Bitboard::from(Square::H8));
        assert_eq!(FenOpts::default().scid(true).fen(&fen), input);
    }

    #[test]
    fn test_lichess_pockets() {
        let input = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1";
        let fen: Fen = input.parse().expect("valid fen");
        assert_eq!(fen.pockets().map(|p| p.is_empty()), Some(true));
        assert_eq!(FenOpts::default().scid(true).fen(&fen), input);
    }

    #[test]
    fn test_shredder_fen() {
        let pos = Chess::default();
        assert_eq!(FenOpts::default().shredder(true).fen(&pos),
                   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w HAha - 0 1");
    }

    #[test]
    fn test_remaining_checks() {
        let fen: Fen = "8/8/8/8/8/8/8/8 w - - 1+2 12 42".parse().expect("valid fen");
        let expected = ByColor { white: RemainingChecks(1), black: RemainingChecks(2) };
        assert_eq!(fen.remaining_checks, Some(expected));
        assert_eq!(fen.halfmoves, 12);
        assert_eq!(fen.fullmoves.get(), 42);
    }

    #[test]
    fn test_lichess_remaining_checks() {
        let input = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 1 2 +0+0";
        let fen: Fen = input.parse().expect("valid fen");
        let expected = ByColor { white: RemainingChecks(3), black: RemainingChecks(3) };
        assert_eq!(fen.remaining_checks, Some(expected));
        assert_eq!(fen.halfmoves, 1);
        assert_eq!(fen.fullmoves.get(), 2);
        assert_eq!(FenOpts::default().scid(true).fen(&fen), input);
    }

    #[test]
    fn test_non_ascii() {
        // mind the dot in the castling part
        let input = "8/8/8/8/8/8/8/8 w · - 0 1";
        let error = input.parse::<Fen>().expect_err("invalid fen");
        assert_eq!(error, ParseFenError::InvalidCastling);
    }
}
