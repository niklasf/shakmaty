use core::{convert::identity, num::NonZeroU32};

#[cfg(feature = "proptest")]
use proptest::prelude::*;

use crate::{
    attacks, Bitboard, Board, ByCastlingSide, ByColor, ByRole, CastlingMode, CastlingSide, Color,
    File, FromSetup, PositionError, Rank, RemainingChecks, Square,
};

/// A not necessarily legal position.
///
/// # Equality
///
/// [`Hash`](core::hash::Hash), [`PartialEq`], and
/// [`Eq`] are implemented in terms of structural equality.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(feature = "proptest", derive(proptest_derive::Arbitrary))]
pub struct Setup {
    /// Piece positions on the board.
    pub board: Board,

    /// Positions of tracked promoted pieces. Used only for Crazyhouse.
    pub promoted: Bitboard,

    /// Pockets in chess variants like Crazyhouse.
    pub pockets: Option<ByColor<ByRole<u8>>>,

    /// Side to move.
    pub turn: Color,

    /// Castling rights in terms of corresponding rook positions.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Bitboard, Setup};
    ///
    /// let setup = Setup::default();
    /// let rooks = setup.castling_rights;
    /// // 1 . . . . . . 1
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // 1 . . . . . . 1
    ///
    /// assert_eq!(rooks, Bitboard::CORNERS);
    /// ```
    pub castling_rights: Bitboard,

    /// En passant target square. Valid target squares are on the third or
    /// sixth rank.
    pub ep_square: Option<Square>,

    /// Remaining checks in chess variants like Three-Check.
    pub remaining_checks: Option<ByColor<RemainingChecks>>,

    /// Number of half-moves since the last
    /// [capture or pawn move](super::Move::is_zeroing()).
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Setup;
    ///
    /// let setup = Setup::default();
    /// assert_eq!(setup.halfmoves, 0);
    /// ```
    pub halfmoves: u32,

    /// Current move number.
    ///
    /// Starts at 1 and is increased after every black move.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::Setup;
    ///
    /// let setup = Setup::default();
    /// assert_eq!(setup.fullmoves.get(), 1);
    /// ```
    pub fullmoves: NonZeroU32,
}

impl Setup {
    /// Plain, empty board. No pieces. White to play.
    pub const fn empty() -> Setup {
        Setup {
            board: Board::empty(),
            pockets: None,
            promoted: Bitboard::EMPTY,
            turn: Color::White,
            castling_rights: Bitboard::EMPTY,
            ep_square: None,
            remaining_checks: None,
            halfmoves: 0,
            fullmoves: match NonZeroU32::new(1) {
                Some(num) => num,
                _ => unreachable!(),
            },
        }
    }

    /// Default board setup.
    pub const fn initial() -> Setup {
        Setup {
            board: Board::new(),
            castling_rights: Bitboard::CORNERS,
            ..Setup::empty()
        }
    }

    /// Swap turns and discard en passant rights. This is sometimes called
    /// "playing a null move".
    pub const fn swap_turn(&mut self) {
        self.turn = self.turn.other();
        self.ep_square = None;
    }

    #[must_use]
    pub const fn into_swapped_turn(mut self) -> Setup {
        self.swap_turn();
        self
    }

    /// Mirror vertically and swap turns and all piece colors, so that the
    /// resulting setup would be equivalent in standard chess.
    ///
    /// Move counters remain unchanged.
    pub fn mirror(&mut self) {
        self.board.mirror();
        self.promoted = self.promoted.flip_vertical();
        if let Some(pockets) = &mut self.pockets {
            pockets.swap();
        }
        self.turn = !self.turn;
        self.castling_rights = self.castling_rights.flip_vertical();
        self.ep_square = self.ep_square.map(Square::flip_vertical);
        if let Some(remaining_checks) = &mut self.remaining_checks {
            remaining_checks.swap();
        }
    }

    #[must_use]
    pub fn into_mirrored(mut self) -> Setup {
        self.mirror();
        self
    }

    pub fn position<P: FromSetup>(self, mode: CastlingMode) -> Result<P, PositionError<P>> {
        P::from_setup(self, mode)
    }
}

impl Default for Setup {
    fn default() -> Setup {
        Setup::initial()
    }
}

/// Castling paths and unmoved rooks.
#[derive(Clone, Debug)]
pub struct Castles {
    mask: Bitboard,
    rook: ByColor<ByCastlingSide<Option<Square>>>,
    path: ByColor<ByCastlingSide<Bitboard>>,
    mode: CastlingMode,
}

impl Castles {
    pub const fn new() -> Castles {
        Castles {
            mode: CastlingMode::Standard,
            mask: Bitboard::CORNERS,
            rook: ByColor {
                black: ByCastlingSide {
                    king_side: Some(Square::H8),
                    queen_side: Some(Square::A8),
                },
                white: ByCastlingSide {
                    king_side: Some(Square::H1),
                    queen_side: Some(Square::A1),
                },
            },
            path: ByColor {
                black: ByCastlingSide {
                    king_side: Bitboard(0x6000_0000_0000_0000),
                    queen_side: Bitboard(0x0e00_0000_0000_0000),
                },
                white: ByCastlingSide {
                    king_side: Bitboard(0x0000_0000_0000_0060),
                    queen_side: Bitboard(0x0000_0000_0000_000e),
                },
            },
        }
    }
}

impl Default for Castles {
    fn default() -> Castles {
        Castles::new()
    }
}

impl CastlingMode {
    pub fn detect(setup: &Setup) -> CastlingMode {
        let standard = Castles::from_setup(setup, CastlingMode::Standard).unwrap_or_else(identity);
        let chess960 = Castles::from_setup(setup, CastlingMode::Chess960).unwrap_or_else(identity);
        CastlingMode::from_standard(standard.mask == chess960.mask)
    }
}

impl Castles {
    pub const fn empty(mode: CastlingMode) -> Castles {
        Castles {
            mode,
            mask: Bitboard(0),
            rook: ByColor {
                black: ByCastlingSide {
                    king_side: None,
                    queen_side: None,
                },
                white: ByCastlingSide {
                    king_side: None,
                    queen_side: None,
                },
            },
            path: ByColor {
                black: ByCastlingSide {
                    king_side: Bitboard::EMPTY,
                    queen_side: Bitboard::EMPTY,
                },
                white: ByCastlingSide {
                    king_side: Bitboard::EMPTY,
                    queen_side: Bitboard::EMPTY,
                },
            },
        }
    }

    pub fn from_setup(setup: &Setup, mode: CastlingMode) -> Result<Castles, Castles> {
        let mut castles = Castles::empty(mode);
        let rooks = setup.castling_rights & setup.board.rooks();

        for color in Color::ALL {
            if let Some(king) = setup.board.king_of(color) {
                if king.rank() != color.fold_wb(Rank::First, Rank::Eighth) {
                    continue;
                }

                let side = rooks & setup.board.by_color(color) & color.backrank();

                if let Some(a_side) = side.first().filter(|rook| rook.file() < king.file()) {
                    let rook_to = CastlingSide::QueenSide.rook_to(color);
                    let king_to = CastlingSide::QueenSide.king_to(color);
                    let chess960 = king.file() != File::E || a_side.file() != File::A;
                    if !chess960 || mode.is_chess960() {
                        castles.mask.add(a_side);
                        castles.rook.get_mut(color).queen_side = Some(a_side);
                        castles.path.get_mut(color).queen_side =
                            (attacks::between(a_side, rook_to).with(rook_to)
                                | attacks::between(king, king_to).with(king_to))
                            .without(king)
                            .without(a_side);
                    }
                }

                if let Some(h_side) = side.last().filter(|rook| king.file() < rook.file()) {
                    let rook_to = CastlingSide::KingSide.rook_to(color);
                    let king_to = CastlingSide::KingSide.king_to(color);
                    let chess960 = king.file() != File::E || h_side.file() != File::H;
                    if !chess960 || mode.is_chess960() {
                        castles.mask.add(h_side);
                        castles.rook.get_mut(color).king_side = Some(h_side);
                        castles.path.get_mut(color).king_side = (attacks::between(h_side, rook_to)
                            .with(rook_to)
                            | attacks::between(king, king_to).with(king_to))
                        .without(king)
                        .without(h_side);
                    }
                }
            }
        }

        if castles.castling_rights() == setup.castling_rights {
            Ok(castles)
        } else {
            Err(castles)
        }
    }

    pub const fn any(&self) -> bool {
        self.mask.any()
    }

    pub const fn is_empty(&self) -> bool {
        self.mask.is_empty()
    }

    pub const fn has(&self, color: Color, side: CastlingSide) -> bool {
        self.rook(color, side).is_some()
    }

    pub const fn has_color(&self, color: Color) -> bool {
        self.mask
            .intersect_const(Bitboard::from_rank(color.backrank()))
            .any()
    }

    pub fn discard_rook(&mut self, square: Square) {
        if square <= Square::H1 {
            self.mask.discard(square);
            if self.rook.white.king_side == Some(square) {
                self.rook.white.king_side = None;
            } else if self.rook.white.queen_side == Some(square) {
                self.rook.white.queen_side = None;
            }
        } else if square >= Square::A8 {
            self.mask.discard(square);
            if self.rook.black.king_side == Some(square) {
                self.rook.black.king_side = None;
            } else if self.rook.black.queen_side == Some(square) {
                self.rook.black.queen_side = None;
            }
        }
    }

    pub fn discard_color(&mut self, color: Color) {
        self.mask.discard(color.backrank());
        *self.rook.get_mut(color) = ByCastlingSide {
            king_side: None,
            queen_side: None,
        };
    }

    #[inline]
    pub const fn rook(&self, color: Color, side: CastlingSide) -> Option<Square> {
        *self.rook.get(color).get(side)
    }

    /// Gets the squares that need to be empty so that castling is possible
    /// on the given side, assuming the player still has the required
    /// castling rigths.
    ///
    /// # Examples
    ///
    /// ```
    /// use shakmaty::{Castles, CastlingSide, Bitboard, Color, Square};
    ///
    /// let castles = Castles::default();
    /// let path = castles.path(Color::White, CastlingSide::QueenSide);
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // . . . . . . . .
    /// // 0 1 1 1 0 . . .
    ///
    /// assert_eq!(path, Bitboard::from(Square::B1) | Bitboard::from(Square::C1) | Bitboard::from(Square::D1));
    /// ```
    #[inline]
    pub const fn path(&self, color: Color, side: CastlingSide) -> Bitboard {
        *self.path.get(color).get(side)
    }

    /// Castling rigths in terms of corresponding rook positions.
    #[inline]
    pub const fn castling_rights(&self) -> Bitboard {
        self.mask
    }

    pub const fn mode(&self) -> CastlingMode {
        self.mode
    }
}

/// En passant square on the third or sixth rank.
#[derive(Debug, Copy, Clone)]
pub(crate) struct EnPassant(pub Square);

impl From<EnPassant> for Square {
    fn from(ep: EnPassant) -> Square {
        ep.square()
    }
}

impl EnPassant {
    pub fn from_setup(setup: &Setup) -> Result<Option<EnPassant>, ()> {
        let Some(ep_square) = setup.ep_square else {
            return Ok(None);
        };

        if ep_square.rank() != setup.turn.relative_rank(Rank::Sixth) {
            return Err(());
        }

        let maybe = EnPassant(ep_square);

        // The last move must have been a double pawn push. Check for the
        // presence of that pawn.
        if !((setup.board.pawns() & setup.board.by_color(!setup.turn))
            .contains(maybe.pawn_pushed_to()))
        {
            return Err(());
        }

        if setup.board.occupied().contains(ep_square)
            || setup.board.occupied().contains(maybe.pawn_pushed_from())
        {
            return Err(());
        }

        Ok(Some(EnPassant(ep_square)))
    }

    #[inline]
    pub const fn square(self) -> Square {
        self.0
    }

    pub const fn pawn_pushed_from(self) -> Square {
        self.0.xor(Square::A4)
    }

    pub const fn pawn_pushed_to(self) -> Square {
        self.0.xor(Square::A2)
    }
}
