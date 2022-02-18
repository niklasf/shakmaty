// This file is part of the shakmaty library.
// Copyright (C) 2017-2022 Niklas Fiekas <niklas.fiekas@backscattering.de>
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
//! # Parsing
//!
//! The parser is relaxed:
//!
//! * Supports X-FEN and Shredder-FEN for castling right notation.
//!   Allows repeated castling rights and castling rights in any order.
//! * Supports `[q]` and `/q` styles for Crazyhouse pockets.
//! * Supports `3+3` and `+0+0` for remaining checks in Three-Check.
//! * Accepts partial FENs and fills missing fields with the default values
//!   of `8/8/8/8/8/8/8/8 w - - 0 1`.
//! * Accepts multiple spaces and underscores (`_`) as separators between
//!   FEN fields.
//! * Accepts `0` as fulllmove number and uses `1` instead.
//!
//! # Writing
//!
//! Writes X-FEN with `[q]` style for Crazyhouse pockets and `3+3` style
//! for remainig checks in Three-Check.
//!
//! The writer intentionally deviates from the specification when formatting
//! [`Position`] in the following backwards compatible way: En passant squares
//! are included only if there is a fully legal en passant capture.
//! [`Position::into_setup()`] also omits en passant squares, unless there
//! is a fully legal en passant capture.
//!
//! # Examples
//!
//! [`Fen`] and [`Epd`] implement [`Display`]:
//!
//! ```
//! use shakmaty::{fen::Epd, Chess};
//!
//! let pos = Chess::default();
//!
//! assert_eq!(Epd::from(pos).to_string(),
//!            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -");
//! ```
//!
//! They also implemnet [`FromStr`]:
//!
//! ```
//! # use shakmaty::Chess;
//! use shakmaty::{fen::Fen, CastlingMode, Position};
//!
//! let fen: Fen = "r1bqkbnr/ppp2Qpp/2np4/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4".parse()?;
//!
//! let pos: Chess = fen.position(CastlingMode::Standard)?;
//! assert!(pos.is_checkmate());
//! # Ok::<_, Box<dyn std::error::Error>>(())
//! ```

use std::{char, cmp::max, convert::TryFrom, error::Error, fmt, num::NonZeroU32, str::FromStr};

use crate::{
    Bitboard, Board, ByColor, ByRole, CastlingMode, Color, File, FromSetup, Piece, Position,
    PositionError, Rank, RemainingChecks, Role, Setup, Square,
};

fn castling_fen(board: &Board, castling_rights: Bitboard) -> String {
    let mut fen = String::with_capacity(4);

    for color in Color::ALL {
        let king = board.king_of(color);

        let candidates = board.by_piece(color.rook()) & color.backrank();

        for rook in (candidates & castling_rights).into_iter().rev() {
            if Some(rook) == candidates.first() && king.map_or(false, |k| rook < k) {
                fen.push(color.fold_wb('Q', 'q'));
            } else if Some(rook) == candidates.last() && king.map_or(false, |k| k < rook) {
                fen.push(color.fold_wb('K', 'k'));
            } else {
                let file = rook.file();
                fen.push(color.fold_wb(file.char().to_ascii_uppercase(), file.char()));
            }
        }
    }

    if fen.is_empty() {
        fen.push('-');
    }

    fen
}

fn epd(setup: &Setup) -> String {
    format!(
        "{}{} {} {} {}{}",
        setup.board.board_fen(setup.promoted),
        setup
            .pockets
            .as_ref()
            .map_or("".to_owned(), |p| format!("[{}]", pocket_fen(p))),
        setup.turn.char(),
        castling_fen(&setup.board, setup.castling_rights),
        setup.ep_square.map_or("-".to_owned(), |sq| sq.to_string()),
        setup
            .remaining_checks
            .as_ref()
            .map_or("".to_owned(), |r| format!(" {}", r)),
    )
}

fn fen(setup: &Setup) -> String {
    format!("{} {} {}", epd(setup), setup.halfmoves, setup.fullmoves)
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

impl fmt::Display for ParseFenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            ParseFenError::InvalidFen => "invalid fen",
            ParseFenError::InvalidBoard => "invalid board part in fen",
            ParseFenError::InvalidPocket => "invalid pocket in fen",
            ParseFenError::InvalidTurn => "invalid turn part in fen",
            ParseFenError::InvalidCastling => "invalid castling part in fen",
            ParseFenError::InvalidEpSquare => "invalid ep square in fen",
            ParseFenError::InvalidRemainingChecks => "invalid remaining checks in fen",
            ParseFenError::InvalidHalfmoveClock => "invalid halfmove clock in fen",
            ParseFenError::InvalidFullmoves => "invalid fullmove part in fen",
        })
    }
}

impl Error for ParseFenError {}

fn parse_board_fen(board_fen: &[u8]) -> Result<(Board, Bitboard), ParseFenError> {
    let mut promoted = Bitboard(0);
    let mut board = Board::empty();

    let mut rank = 7i8;
    let mut file = 0i8;

    let mut iter = board_fen.iter().copied().peekable();

    while let Some(ch) = iter.next() {
        if ch == b'/' && file == 8 {
            file = 0;
            rank -= 1;
            if rank < 0 {
                return Err(ParseFenError::InvalidBoard);
            }
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
                white: RemainingChecks::new(3_u32.checked_sub(btoi::btou(white_given).ok()?)?),
                black: RemainingChecks::new(3_u32.checked_sub(btoi::btoi(black_given).ok()?)?),
            }
        }
        (Some(white), Some(black), None) => {
            // format: 3+3
            ByColor {
                white: RemainingChecks::try_from(btoi::btou::<u32>(white).ok()?).ok()?,
                black: RemainingChecks::try_from(btoi::btou::<u32>(black).ok()?).ok()?,
            }
        }
        _ => return None,
    })
}

fn parse_pockets(s: &[u8]) -> Option<ByColor<ByRole<u8>>> {
    if s.len() > 64 {
        return None;
    }
    let mut result = ByColor::<ByRole<u8>>::default();
    for ch in s {
        *result.piece_mut(Piece::from_char(char::from(*ch))?) += 1;
    }
    Some(result)
}

fn pocket_fen(pockets: &ByColor<ByRole<u8>>) -> String {
    let mut fen = String::new();
    for color in Color::ALL {
        for role in Role::ALL {
            let piece = Piece { color, role };
            for _ in 0..*pockets.piece(piece) {
                fen.push(piece.char());
            }
        }
    }
    fen
}

impl Board {
    pub fn from_ascii_board_fen(board_fen: &[u8]) -> Result<Board, ParseFenError> {
        Ok(parse_board_fen(board_fen)?.0)
    }

    /// Create a board FEN such as
    /// `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR`.
    ///
    /// Promoted pieces are marked like `Q~`.
    pub fn board_fen(&self, promoted: Bitboard) -> String {
        let mut fen = String::with_capacity(15);

        for rank in Rank::ALL.into_iter().rev() {
            let mut empty = 0;

            for file in File::ALL {
                let square = Square::from_coords(file, rank);

                empty = self.piece_at(square).map_or_else(
                    || empty + 1,
                    |piece| {
                        if empty > 0 {
                            fen.push(
                                char::from_digit(empty, 10)
                                    .expect("at most 8 empty squares on a rank"),
                            );
                        }
                        fen.push(piece.char());
                        if promoted.contains(square) {
                            fen.push('~');
                        }
                        0
                    },
                );

                if file == File::H && empty > 0 {
                    fen.push(
                        char::from_digit(empty, 10).expect("at most 8 empty squares on a rank"),
                    );
                }

                if file == File::H && rank > Rank::First {
                    fen.push('/');
                }
            }
        }

        fen
    }
}

impl FromStr for Board {
    type Err = ParseFenError;

    fn from_str(board_fen: &str) -> Result<Board, ParseFenError> {
        Board::from_ascii_board_fen(board_fen.as_bytes())
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.board_fen(Bitboard(0)))
    }
}

/// A FEN like `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct Fen(pub Setup);

impl Fen {
    /// The FEN of the empty position `8/8/8/8/8/8/8/8 w - - 0 1`.
    pub fn empty() -> Fen {
        Fen(Setup::empty())
    }

    pub fn into_inner(self) -> Setup {
        self.0
    }

    /// Set up a [`Position`].
    ///
    /// # Errors
    ///
    /// Returns [`PositionError`] if the setup does not meet basic validity
    /// requirements.
    ///
    /// [`FromSetup`]: super::FromSetup
    /// [`Position`]: super::Position
    /// [`PositionError`]: super::PositionError
    pub fn position<P: FromSetup>(self, mode: CastlingMode) -> Result<P, PositionError<P>> {
        P::from_setup(self.0, mode)
    }

    /// Parses a FEN or EPD.
    ///
    /// FENs consist of parts separated by spaces. This parser also accepts
    /// parts separated by underscores. Missing parts are filled with defaults.
    ///
    /// # Errors
    ///
    /// Returns [`ParseFenError`] if any part is syntactically invalid.
    ///
    /// # Example
    ///
    /// ```
    /// use shakmaty::fen::Fen;
    ///
    /// let fen = Fen::from_ascii(b"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")?;
    /// assert_eq!(fen, Fen::default());
    /// # Ok::<_, Box<dyn std::error::Error>>(())
    /// ```
    pub fn from_ascii(fen: &[u8]) -> Result<Fen, ParseFenError> {
        let mut result = Setup::empty();
        let mut parts = fen
            .split(|ch| *ch == b' ' || *ch == b'_')
            .filter(|s| !s.is_empty());

        let board_part = parts.next().ok_or(ParseFenError::InvalidFen)?;

        let (board_part, pocket_part) = if board_part.ends_with(b"]") {
            // format: ...[pocket]
            let split_point = board_part
                .iter()
                .position(|ch| *ch == b'[')
                .ok_or(ParseFenError::InvalidBoard)?;
            let pocket_part = &board_part[(split_point + 1)..(board_part.len() - 1)];
            (&board_part[..split_point], Some(pocket_part))
        } else if let Some(split_point) = board_part
            .iter()
            .enumerate()
            .filter_map(|(idx, ch)| (*ch == b'/').then(|| idx))
            .nth(7)
        {
            // format: .../pocket
            (
                &board_part[..split_point],
                Some(&board_part[(split_point + 1)..]),
            )
        } else {
            (board_part, None)
        };

        let (board, promoted) = parse_board_fen(board_part)?;
        result.board = board;
        result.promoted = promoted;

        if let Some(pocket_part) = pocket_part {
            result.pockets = Some(parse_pockets(pocket_part).ok_or(ParseFenError::InvalidPocket)?);
        }

        result.turn = match parts.next() {
            Some(b"w") | None => Color::White,
            Some(b"b") => Color::Black,
            Some(_) => return Err(ParseFenError::InvalidTurn),
        };

        match parts.next() {
            Some(b"-") | None => (),
            Some(castling_part) => {
                result.castling_rights = castling_part
                    .iter()
                    .map(|ch| {
                        let color = Color::from_white(ch.is_ascii_uppercase());
                        let rooks_and_kings = result.board.by_color(color)
                            & (result.board.rooks() | result.board.kings())
                            & color.backrank();
                        Ok(match ch.to_ascii_lowercase() {
                            b'k' => rooks_and_kings
                                .last()
                                .filter(|sq| result.board.rooks().contains(*sq))
                                .unwrap_or_else(|| Square::from_coords(File::H, color.backrank())),
                            b'q' => rooks_and_kings
                                .first()
                                .filter(|sq| result.board.rooks().contains(*sq))
                                .unwrap_or_else(|| Square::from_coords(File::A, color.backrank())),
                            file => Square::from_coords(
                                File::from_char(char::from(file))
                                    .ok_or(ParseFenError::InvalidCastling)?,
                                color.backrank(),
                            ),
                        })
                    })
                    .collect::<Result<_, ParseFenError>>()?;

                for color in Color::ALL {
                    if (result.castling_rights & color.backrank()).count() > 2 {
                        return Err(ParseFenError::InvalidCastling);
                    }
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
            let fullmoves = btoi::btou_saturating(fullmoves_part)
                .map_err(|_| ParseFenError::InvalidFullmoves)?;
            result.fullmoves = NonZeroU32::new(max(fullmoves, 1)).expect("non-zero fullmoves");
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
            Ok(Fen(result))
        }
    }
}

impl From<Setup> for Fen {
    fn from(setup: Setup) -> Fen {
        Fen(setup)
    }
}

impl<P: Position> From<P> for Fen {
    fn from(pos: P) -> Fen {
        Fen::from(pos.into_setup())
    }
}

impl From<Fen> for Setup {
    fn from(fen: Fen) -> Setup {
        fen.0
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
        f.write_str(&fen(&self.0))
    }
}

/// An EPD like `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -`.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct Epd {
    inner: Setup,
}

impl Epd {
    pub fn empty() -> Epd {
        Epd {
            inner: Setup::empty(),
        }
    }

    pub fn into_inner(self) -> Setup {
        self.inner
    }

    pub fn position<P: FromSetup>(self, mode: CastlingMode) -> Result<P, PositionError<P>> {
        P::from_setup(self.inner, mode)
    }

    pub fn from_ascii(epd: &[u8]) -> Result<Epd, ParseFenError> {
        Ok(Epd::from(Fen::from_ascii(epd)?.into_inner()))
    }
}

impl From<Setup> for Epd {
    fn from(mut setup: Setup) -> Epd {
        setup.halfmoves = 0;
        setup.fullmoves = NonZeroU32::new(1).unwrap();
        Epd { inner: setup }
    }
}

impl<P: Position> From<P> for Epd {
    fn from(pos: P) -> Epd {
        Epd::from(pos.into_setup())
    }
}

impl From<Epd> for Setup {
    fn from(epd: Epd) -> Setup {
        epd.inner
    }
}

impl FromStr for Epd {
    type Err = ParseFenError;

    fn from_str(epd: &str) -> Result<Epd, ParseFenError> {
        Epd::from_ascii(epd.as_bytes())
    }
}

impl fmt::Display for Epd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&epd(&self.inner))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::position::Chess;

    #[test]
    fn test_legal_ep_square() {
        let original_epd = "4k3/8/8/8/3Pp3/8/8/3KR3 b - d3";
        let fen: Fen = original_epd.parse().expect("valid fen");
        assert_eq!(
            Epd::from(fen.clone().into_inner()).to_string(),
            original_epd
        );

        // The en passant square is not actually legal.
        let pos: Chess = fen
            .position(CastlingMode::Standard)
            .expect("legal position");
        assert_eq!(Epd::from(pos).to_string(), "4k3/8/8/8/3Pp3/8/8/3KR3 b - -");
    }

    #[test]
    fn test_invalid_fen() {
        assert_eq!("".parse::<Fen>().unwrap_err(), ParseFenError::InvalidFen);

        assert_eq!(
            "8/8/8/8/8/8/8/8 w · - 0 1" // not ascii
                .parse::<Fen>()
                .unwrap_err(),
            ParseFenError::InvalidCastling
        );

        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQQKBNR w cq - 0P1" // syntax
                .parse::<Fen>()
                .unwrap_err(),
            ParseFenError::InvalidCastling
        );

        assert_eq!(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w  - 0 1" // double space
                .parse::<Fen>()
                .unwrap_err(),
            ParseFenError::InvalidEpSquare
        );

        assert_eq!(
            "4k2r/8/8/8/8/8/8/RR2K2R w KBQk - 0 1" // triple castling rights
                .parse::<Fen>()
                .unwrap_err(),
            ParseFenError::InvalidCastling
        );
    }

    #[test]
    fn test_pockets() {
        let setup = "8/8/8/8/8/8/8/8[Q]"
            .parse::<Fen>()
            .expect("valid fen")
            .into_inner();
        assert_eq!(
            setup.pockets.map(|p| *p.piece(Color::White.queen())),
            Some(1)
        );
    }

    #[test]
    fn test_lichess_promoted() {
        let setup = "rnbqk1nQ~/ppppp3/8/5p2/8/5N2/PPPPPPP1/RNBQKB1R/PPBR b KQq - 0 6"
            .parse::<Fen>()
            .expect("valid fen")
            .into_inner();
        assert_eq!(setup.promoted, Bitboard::from(Square::H8));
    }

    #[test]
    fn test_lichess_pockets() {
        let setup = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .into_inner();
        assert_eq!(setup.pockets, Some(Default::default()));
    }

    #[test]
    fn test_remaining_checks() {
        let setup = "8/8/8/8/8/8/8/8 w - - 1+2 12 42"
            .parse::<Fen>()
            .expect("valid fen")
            .into_inner();
        assert_eq!(
            setup.remaining_checks,
            Some(ByColor {
                white: RemainingChecks::new(1),
                black: RemainingChecks::new(2),
            })
        );
        assert_eq!(setup.halfmoves, 12);
        assert_eq!(setup.fullmoves.get(), 42);
    }

    #[test]
    fn test_lichess_remaining_checks() {
        let setup = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 1 2 +0+0"
            .parse::<Fen>()
            .expect("valid fen")
            .into_inner();
        assert_eq!(
            setup.remaining_checks,
            Some(ByColor {
                white: RemainingChecks::new(3),
                black: RemainingChecks::new(3),
            })
        );
        assert_eq!(setup.halfmoves, 1);
        assert_eq!(setup.fullmoves.get(), 2);
    }
}
