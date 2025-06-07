//! Parse and write Forsyth-Edwards-Notation.
//!
//! # Parsing
//!
//! The parser is relaxed:
//!
//! * Supports X-FEN and Shredder-FEN for castling right notation.
//!   - Ignores repeated castling rights.
//!   - Allows castling rights in any order.
//!   - Allows castling rights without matching rooks (treating `Q` and `K` as
//!     `A` and `H` respectively).
//! * Supports `[q]` and `/q` styles for Crazyhouse pockets.
//! * Supports `3+3` and `+0+0` for remaining checks in Three-Check.
//! * Accepts missing FEN fields (except the board) and fills them with
//!   default values of `8/8/8/8/8/8/8/8 w - - 0 1`.
//! * Accepts multiple spaces and underscores (`_`) as separators between
//!   FEN fields.
//! * Accepts `0` as fullmove number and uses `1` instead.
//!
//! [`Fen`] and [`Epd`] implement [`FromStr`]:
//!
//! ```
//! use shakmaty::{fen::Fen, CastlingMode, Chess, Position};
//!
//! let fen: Fen = "r1bqkbnr/ppp2Qpp/2np4/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4".parse()?;
//! let pos: Chess = fen.into_position(CastlingMode::Standard)?;
//! assert!(pos.is_checkmate());
//!
//! # use shakmaty::{fen::ParseFenError, PositionError};
//! # #[derive(Debug)] struct CommonError;
//! # impl From<ParseFenError> for CommonError { fn from(_: ParseFenError) -> Self { Self } }
//! # impl<P> From<PositionError<P>> for CommonError { fn from(_: PositionError<P>) -> Self { Self } }
//! # Ok::<_, CommonError>(())
//! ```
//!
//! # Writing
//!
//! Writes X-FEN with `[q]` style for Crazyhouse pockets and `3+3` style
//! for remaining checks in Three-Check.
//!
//! [`Fen`] and [`Epd`] implement [`Display`]:
//!
//! ```
//! use shakmaty::{fen::Epd, EnPassantMode, Chess, Position};
//!
//! let pos = Chess::default();
//!
//! assert_eq!(Epd::from_position(&pos, EnPassantMode::Legal).to_string(),
//!            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -");
//! ```

use core::{
    char, error,
    fmt::{self, Display},
    num::NonZeroU32,
    str::FromStr,
};

use bitflags::bitflags;

use crate::{
    util::AppendAscii, Bitboard, Board, ByColor, ByRole, CastlingMode, Color, EnPassantMode, File,
    FromSetup, Piece, Position, PositionError, Rank, RemainingChecks, Role, Setup, Square,
};

fn append_castling<W: AppendAscii>(
    f: &mut W,
    board: &Board,
    castling_rights: Bitboard,
) -> Result<(), W::Error> {
    let mut empty = true;

    for color in Color::ALL {
        let king = board
            .king_of(color)
            .filter(|k| k.rank() == color.backrank());

        let candidates = board.by_piece(color.rook()) & color.backrank();

        for rook in (castling_rights & color.backrank()).into_iter().rev() {
            f.append_ascii(
                if Some(rook) == candidates.first() && king.is_some_and(|k| rook < k) {
                    color.fold_wb('Q', 'q')
                } else if Some(rook) == candidates.last() && king.is_some_and(|k| k < rook) {
                    color.fold_wb('K', 'k')
                } else {
                    let file = rook.file();
                    color.fold_wb(file.char().to_ascii_uppercase(), file.char())
                },
            )?;
            empty = false;
        }
    }

    if empty {
        f.append_ascii('-')?;
    }

    Ok(())
}

fn append_pockets<W: AppendAscii>(
    f: &mut W,
    pockets: &ByColor<ByRole<u8>>,
) -> Result<(), W::Error> {
    f.append_ascii('[')?;
    for color in Color::ALL {
        for role in Role::ALL {
            let piece = Piece { color, role };
            for _ in 0..*pockets.piece(piece) {
                f.append_ascii(piece.char())?;
            }
        }
    }
    f.append_ascii(']')
}

fn append_epd<W: AppendAscii>(
    f: &mut W,
    board: &Board,
    promoted: Bitboard,
    pockets: &Option<ByColor<ByRole<u8>>>,
    turn: Color,
    castling_rights: Bitboard,
    ep_square: Option<Square>,
    remaining_checks: &Option<ByColor<RemainingChecks>>,
) -> Result<(), W::Error> {
    f.reserve(21);
    BoardFen { board, promoted }.append_to(f)?;
    if let Some(ref pockets) = pockets {
        append_pockets(f, pockets)?;
    }
    f.append_ascii(' ')?;
    f.append_ascii(turn.char())?;
    f.append_ascii(' ')?;
    append_castling(f, board, castling_rights)?;
    f.append_ascii(' ')?;
    match ep_square {
        Some(ep_square) => ep_square.append_to(f)?,
        None => f.append_ascii('-')?,
    }
    if let Some(remaining_checks) = remaining_checks {
        f.append_ascii(' ')?;
        remaining_checks.append_to(f)?;
    }
    Ok(())
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

impl Display for ParseFenError {
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

impl error::Error for ParseFenError {}

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
                    board.set_new_piece_at(sq, piece);
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

bitflags! {
    /// Reasons for a [`Setup`] not being representable as a [`Fen`].
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct LossyFenErrorKinds: u32 {
        /// Set of squares with promoted pieces does not match pieces
        /// on the board.
        const PROMOTED = 1 << 0;

        /// Too many castling rights or castling rights not on the backrank.
        const CASTLING_RIGHTS = 1 << 1;
    }
}

/// Error when trying to create a [`Fen`] from a [`Setup`] that cannot be
/// losslessly represented.
///
/// See [`LossyFenErrorKinds`] for possible reasons.
#[derive(Debug, Clone)]
pub struct LossyFenError<F> {
    fen: F,
    errors: LossyFenErrorKinds,
}

impl<F> LossyFenError<F> {
    /// Returns the reasons for this error.
    pub fn kinds(&self) -> LossyFenErrorKinds {
        self.errors
    }

    /// Ignores all information that cannot be losslessly represented.
    pub fn ignore(self) -> F {
        self.fen
    }
}

impl Board {
    pub fn from_ascii_board_fen(board_fen: &[u8]) -> Result<Board, ParseFenError> {
        let (board, _promoted) = parse_board_fen(board_fen)?;
        Ok(board)
    }

    /// Create a board FEN such as
    /// `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR`.
    ///
    /// Returns a [`BoardFen`] which implements [`Display`].
    pub const fn board_fen(&self) -> BoardFen<'_> {
        BoardFen {
            board: self,
            promoted: Bitboard::EMPTY,
        }
    }

    /// Create a board FEN such as
    /// `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQ~KBNR`, marking
    /// the given promoted pieces like `Q~`.
    ///
    /// Returns a [`BoardFen`] which implements [`Display`].
    ///
    /// # Errors
    ///
    /// Errors if `promoted` is not a subset of the occupied squares of the
    /// board.
    pub const fn board_fen_with_promoted(
        &self,
        promoted: Bitboard,
    ) -> Result<BoardFen<'_>, LossyFenError<BoardFen<'_>>> {
        let fen = BoardFen {
            board: self,
            promoted,
        };

        if promoted.is_subset_const(self.occupied()) {
            Ok(fen)
        } else {
            Err(LossyFenError {
                fen,
                errors: LossyFenErrorKinds::PROMOTED,
            })
        }
    }
}

impl FromStr for Board {
    type Err = ParseFenError;

    fn from_str(board_fen: &str) -> Result<Board, ParseFenError> {
        Board::from_ascii_board_fen(board_fen.as_bytes())
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.board_fen().append_to(f)
    }
}

/// Displays a board with notation like
/// `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR`.
///
/// See [`Board::board_fen`].
#[derive(Debug)]
pub struct BoardFen<'b> {
    board: &'b Board,
    promoted: Bitboard,
}

impl BoardFen<'_> {
    fn append_to<W: AppendAscii>(&self, f: &mut W) -> Result<(), W::Error> {
        f.reserve(15);

        for rank in Rank::ALL.into_iter().rev() {
            let mut prev_file = -1;

            for square in self.board.occupied() & rank {
                let empty = i32::from(square.file()) - prev_file - 1;
                if empty > 0 {
                    f.append_ascii(char::from(b'0' + empty as u8))?;
                }
                prev_file = i32::from(square.file());

                f.append_ascii(self.board.piece_at(square).expect("piece").char())?;
                if self.promoted.contains(square) {
                    f.append_ascii('~')?;
                }
            }

            let empty = i32::from(File::H) - prev_file;
            if empty > 0 {
                f.append_ascii(char::from(b'0' + empty as u8))?;
            }

            if rank > Rank::First {
                f.append_ascii('/')?;
            }
        }

        Ok(())
    }

    #[cfg(feature = "alloc")]
    pub fn append_to_string(&self, s: &mut alloc::string::String) {
        let _ = self.append_to(s);
    }

    #[cfg(feature = "alloc")]
    pub fn append_ascii_to(&self, buf: &mut alloc::vec::Vec<u8>) {
        let _ = self.append_to(buf);
    }
}

impl Display for BoardFen<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.append_to(f)
    }
}

/// A FEN like `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1`.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct Fen {
    setup: Setup,
}

impl Fen {
    /// The FEN of the empty position `8/8/8/8/8/8/8/8 w - - 0 1`.
    pub const fn empty() -> Fen {
        Fen {
            setup: Setup::empty(),
        }
    }

    #[allow(clippy::result_large_err)] // Ok variant is also large
    pub fn try_from_setup(setup: Setup) -> Result<Fen, LossyFenError<Fen>> {
        let mut errors = LossyFenErrorKinds::empty();
        if !setup.promoted.is_subset(setup.board.occupied()) {
            errors |= LossyFenErrorKinds::PROMOTED;
        }
        if !setup.castling_rights.is_subset(Bitboard::BACKRANKS)
            || (setup.castling_rights & Rank::First).count() > 2
            || (setup.castling_rights & Rank::Eighth).count() > 2
        {
            errors |= LossyFenErrorKinds::CASTLING_RIGHTS;
        }

        let fen = Fen::from_setup_unchecked(setup);
        if errors.is_empty() {
            Ok(fen)
        } else {
            Err(LossyFenError { fen, errors })
        }
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
    /// # Ok::<_, shakmaty::fen::ParseFenError>(())
    /// ```
    pub fn from_ascii(fen: &[u8]) -> Result<Fen, ParseFenError> {
        let mut setup = Setup::empty();
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
            .filter_map(|(idx, ch)| (*ch == b'/').then_some(idx))
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
        setup.board = board;
        setup.promoted = promoted;

        if let Some(pocket_part) = pocket_part {
            setup.pockets = Some(parse_pockets(pocket_part).ok_or(ParseFenError::InvalidPocket)?);
        }

        setup.turn = match parts.next() {
            Some(b"w") | None => Color::White,
            Some(b"b") => Color::Black,
            Some(_) => return Err(ParseFenError::InvalidTurn),
        };

        match parts.next() {
            Some(b"-") | None => (),
            Some(castling_part) => {
                setup.castling_rights = castling_part
                    .iter()
                    .map(|ch| {
                        let color = Color::from_white(ch.is_ascii_uppercase());
                        let rooks_and_kings = setup.board.by_color(color)
                            & (setup.board.rooks() | setup.board.kings())
                            & color.backrank();
                        Ok(match ch.to_ascii_lowercase() {
                            b'k' => rooks_and_kings
                                .last()
                                .filter(|sq| setup.board.rooks().contains(*sq))
                                .unwrap_or_else(|| Square::from_coords(File::H, color.backrank())),
                            b'q' => rooks_and_kings
                                .first()
                                .filter(|sq| setup.board.rooks().contains(*sq))
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
                    if (setup.castling_rights & color.backrank()).count() > 2 {
                        return Err(ParseFenError::InvalidCastling);
                    }
                }
            }
        }

        match parts.next() {
            Some(b"-") | None => (),
            Some(ep_part) => {
                setup.ep_square =
                    Some(Square::from_ascii(ep_part).map_err(|_| ParseFenError::InvalidEpSquare)?);
            }
        }

        let halfmoves_part = if let Some(checks_part) = parts.next() {
            if let Some(remaining_checks) = parse_remaining_checks(checks_part) {
                setup.remaining_checks = Some(remaining_checks);
                parts.next()
            } else {
                Some(checks_part)
            }
        } else {
            None
        };

        if let Some(halfmoves_part) = halfmoves_part {
            setup.halfmoves = btoi::btou_saturating(halfmoves_part)
                .map_err(|_| ParseFenError::InvalidHalfmoveClock)?;
        }

        if let Some(fullmoves_part) = parts.next() {
            let fullmoves = btoi::btou_saturating(fullmoves_part)
                .map_err(|_| ParseFenError::InvalidFullmoves)?;
            setup.fullmoves = NonZeroU32::new(fullmoves).unwrap_or(NonZeroU32::MIN);
        }

        let last_part = if let Some(checks_part) = parts.next() {
            if setup.remaining_checks.is_some() {
                Some(checks_part) // got checks earlier
            } else if let Some(remaining_checks) = parse_remaining_checks(checks_part) {
                setup.remaining_checks = Some(remaining_checks);
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
            Ok(Fen { setup })
        }
    }

    const fn from_setup_unchecked(setup: Setup) -> Fen {
        Fen { setup }
    }

    pub fn from_position<P: Position>(pos: &P, mode: EnPassantMode) -> Fen {
        Fen::from_setup_unchecked(pos.to_setup(mode))
    }

    pub const fn as_setup(&self) -> &Setup {
        &self.setup
    }

    pub const fn into_setup(self) -> Setup {
        self.setup
    }

    /// Set up a [`Position`]. See [`FromSetup`].
    ///
    /// # Errors
    ///
    /// Returns [`PositionError`] if the setup does not meet basic validity
    /// requirements.
    pub fn into_position<P: FromSetup>(self, mode: CastlingMode) -> Result<P, PositionError<P>> {
        P::from_setup(self.setup, mode)
    }

    fn append_to<W: AppendAscii>(&self, f: &mut W) -> Result<(), W::Error> {
        append_epd(
            f,
            &self.setup.board,
            self.setup.promoted,
            &self.setup.pockets,
            self.setup.turn,
            self.setup.castling_rights,
            self.setup.ep_square,
            &self.setup.remaining_checks,
        )?;
        f.append_ascii(' ')?;
        f.append_u32(self.setup.halfmoves)?;
        f.append_ascii(' ')?;
        f.append_u32(u32::from(self.setup.fullmoves))
    }

    #[cfg(feature = "alloc")]
    pub fn append_to_string(&self, s: &mut alloc::string::String) {
        let _ = self.append_to(s);
    }

    #[cfg(feature = "alloc")]
    pub fn append_ascii_to(&self, buf: &mut alloc::vec::Vec<u8>) {
        let _ = self.append_to(buf);
    }
}

impl TryFrom<Setup> for Fen {
    type Error = LossyFenError<Fen>;

    fn try_from(setup: Setup) -> Result<Fen, LossyFenError<Fen>> {
        Fen::try_from_setup(setup)
    }
}

impl From<Epd> for Fen {
    fn from(epd: Epd) -> Fen {
        Fen::from_setup_unchecked(epd.into_setup())
    }
}

impl From<Fen> for Setup {
    fn from(fen: Fen) -> Setup {
        fen.into_setup()
    }
}

impl FromStr for Fen {
    type Err = ParseFenError;

    fn from_str(fen: &str) -> Result<Fen, ParseFenError> {
        Fen::from_ascii(fen.as_bytes())
    }
}

impl Display for Fen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.append_to(f)
    }
}

#[cfg(all(feature = "serde", feature = "alloc"))]
impl serde::Serialize for Fen {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut s = alloc::string::String::new();
        self.append_to_string(&mut s);
        serializer.serialize_str(&s)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Fen {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct FenVisitor;

        impl serde::de::Visitor<'_> for FenVisitor {
            type Value = Fen;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("FEN string")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Fen::from_str(value).map_err(serde::de::Error::custom)
            }
        }

        deserializer.deserialize_str(FenVisitor)
    }
}

/// An EPD like `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -`.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Epd {
    board: Board,
    promoted: Bitboard,
    pockets: Option<ByColor<ByRole<u8>>>,
    turn: Color,
    castling_rights: Bitboard,
    ep_square: Option<Square>,
    remaining_checks: Option<ByColor<RemainingChecks>>,
}

impl Epd {
    pub const fn empty() -> Epd {
        Epd {
            board: Board::empty(),
            promoted: Bitboard::EMPTY,
            pockets: None,
            turn: Color::White,
            castling_rights: Bitboard::EMPTY,
            ep_square: None,
            remaining_checks: None,
        }
    }

    const fn from_setup_unchecked(setup: Setup) -> Epd {
        Epd {
            board: setup.board,
            promoted: setup.promoted,
            pockets: setup.pockets,
            turn: setup.turn,
            castling_rights: setup.castling_rights,
            ep_square: setup.ep_square,
            remaining_checks: setup.remaining_checks,
        }
    }

    pub const fn from_fen(fen: Fen) -> Epd {
        Epd::from_setup_unchecked(fen.setup)
    }

    #[allow(clippy::result_large_err)] // Ok variant also large
    pub fn try_from_setup(setup: Setup) -> Result<Epd, LossyFenError<Epd>> {
        match Fen::try_from_setup(setup) {
            Ok(fen) => Ok(Epd::from_fen(fen)),
            Err(LossyFenError { fen, errors }) => Err(LossyFenError {
                fen: Epd::from_fen(fen),
                errors,
            }),
        }
    }

    pub fn from_ascii(epd: &[u8]) -> Result<Epd, ParseFenError> {
        Ok(Epd::from_fen(Fen::from_ascii(epd)?))
    }

    pub fn from_position<P: Position>(pos: &P, mode: EnPassantMode) -> Epd {
        Epd::from_setup_unchecked(pos.to_setup(mode))
    }

    pub const fn into_setup(self) -> Setup {
        Setup {
            board: self.board,
            promoted: self.promoted,
            pockets: self.pockets,
            turn: self.turn,
            castling_rights: self.castling_rights,
            ep_square: self.ep_square,
            remaining_checks: self.remaining_checks,
            halfmoves: 0,
            fullmoves: NonZeroU32::MIN,
        }
    }

    pub fn into_position<P: FromSetup>(self, mode: CastlingMode) -> Result<P, PositionError<P>> {
        P::from_setup(self.into_setup(), mode)
    }

    fn append_to<W: AppendAscii>(&self, f: &mut W) -> Result<(), W::Error> {
        append_epd(
            f,
            &self.board,
            self.promoted,
            &self.pockets,
            self.turn,
            self.castling_rights,
            self.ep_square,
            &self.remaining_checks,
        )
    }

    #[cfg(feature = "alloc")]
    pub fn append_to_string(&self, s: &mut alloc::string::String) {
        let _ = self.append_to(s);
    }

    #[cfg(feature = "alloc")]
    pub fn append_ascii_to(&self, buf: &mut alloc::vec::Vec<u8>) {
        let _ = self.append_to(buf);
    }
}

impl Default for Epd {
    fn default() -> Epd {
        Epd {
            board: Board::default(),
            promoted: Bitboard::EMPTY,
            pockets: None,
            turn: Color::White,
            castling_rights: Bitboard::CORNERS,
            ep_square: None,
            remaining_checks: None,
        }
    }
}

impl TryFrom<Setup> for Epd {
    type Error = LossyFenError<Epd>;

    fn try_from(setup: Setup) -> Result<Epd, LossyFenError<Epd>> {
        Epd::try_from_setup(setup)
    }
}

impl From<Fen> for Epd {
    fn from(fen: Fen) -> Epd {
        Epd::from_fen(fen)
    }
}

impl From<Epd> for Setup {
    fn from(epd: Epd) -> Setup {
        epd.into_setup()
    }
}

impl FromStr for Epd {
    type Err = ParseFenError;

    fn from_str(epd: &str) -> Result<Epd, ParseFenError> {
        Epd::from_ascii(epd.as_bytes())
    }
}

impl Display for Epd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.append_to(f)
    }
}

#[cfg(all(feature = "serde", feature = "alloc"))]
impl serde::Serialize for Epd {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut s = alloc::string::String::new();
        self.append_to_string(&mut s);
        serializer.serialize_str(&s)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Epd {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct EpdVisitor;

        impl serde::de::Visitor<'_> for EpdVisitor {
            type Value = Epd;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("EPD string")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Epd::from_str(value).map_err(serde::de::Error::custom)
            }
        }

        deserializer.deserialize_str(EpdVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(feature = "alloc")]
    #[test]
    fn test_legal_ep_square() {
        use alloc::string::ToString as _;

        let original_epd = "4k3/8/8/8/3Pp3/8/8/3KR3 b - d3";
        let fen: Fen = original_epd.parse().expect("valid fen");
        assert_eq!(Epd::from(fen.clone()).to_string(), original_epd);

        // The en passant square is not actually legal.
        let pos: crate::Chess = fen
            .into_position(CastlingMode::Standard)
            .expect("legal position");
        assert_eq!(pos.maybe_ep_square(), Some(Square::D3));
        assert_eq!(pos.pseudo_legal_ep_square(), Some(Square::D3));
        assert_eq!(pos.legal_ep_square(), None);

        assert_eq!(
            Epd::from_position(&pos, EnPassantMode::Legal).to_string(),
            "4k3/8/8/8/3Pp3/8/8/3KR3 b - -"
        );
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
            .into_setup();
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
            .into_setup();
        assert_eq!(setup.promoted, Bitboard::from(Square::H8));
    }

    #[test]
    fn test_lichess_pockets() {
        let setup = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .into_setup();
        assert_eq!(setup.pockets, Some(ByColor::default()));
    }

    #[test]
    fn test_remaining_checks() {
        let setup = "8/8/8/8/8/8/8/8 w - - 1+2 12 42"
            .parse::<Fen>()
            .expect("valid fen")
            .into_setup();
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
            .into_setup();
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

    #[cfg(feature = "alloc")]
    #[test]
    fn test_castling_right_without_rook() {
        use alloc::string::ToString as _;

        let fen = "rRpppppp/8/8/8/8/8/PPPPPPBN/PPRQKBNR w KA"
            .parse::<Fen>()
            .expect("valid fen");
        assert_eq!(
            fen.as_setup().castling_rights,
            Bitboard::from_iter([Square::A1, Square::H1])
        );

        assert_eq!(
            fen.to_string(),
            "rRpppppp/8/8/8/8/8/PPPPPPBN/PPRQKBNR w KA - 0 1"
        );
    }
}
