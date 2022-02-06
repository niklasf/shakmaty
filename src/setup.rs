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

use std::num::NonZeroU32;

use crate::{
    attacks,
    bitboard::Bitboard,
    board::Board,
    color::{ByColor, Color},
    material::Material,
    square::{File, Rank, Square},
    types::{CastlingMode, CastlingSide, RemainingChecks},
};

/// A not necessarily legal position.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Setup {
    /// Piece positions on the board.
    pub board: Board,

    /// Positions of tracked promoted pieces. Used only for Crazyhouse.
    pub promoted: Bitboard,

    /// Pockets in chess variants like Crazyhouse.
    pub pockets: Option<Material>,

    /// Side to move.
    pub turn: Color,

    /// Castling rights in terms of corresponding rook positions.
    ///
    /// ```
    /// use shakmaty::{Bitboard, Setup};
    ///
    /// let pos = Setup::default();
    /// let rooks = pos.castling_rights();
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
    /// let pos = Setup::default();
    /// assert_eq!(pos.halfmoves(), 0);
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
    /// let pos = Setup::default();
    /// assert_eq!(pos.fullmoves().get(), 1);
    /// ```
    pub fullmoves: NonZeroU32,
}

impl Setup {
    pub fn empty() -> Setup {
        Setup {
            board: Board::empty(),
            pockets: None,
            promoted: Bitboard::EMPTY,
            turn: Color::White,
            castling_rights: Bitboard::EMPTY,
            ep_square: None,
            remaining_checks: None,
            halfmoves: 0,
            fullmoves: NonZeroU32::new(1).unwrap(),
        }
    }

    pub fn swap_turn(&mut self) {
        self.turn = !self.turn;
        self.ep_square = None;
    }
}

impl Default for Setup {
    fn default() -> Setup {
        Setup {
            board: Board::default(),
            castling_rights: Bitboard::CORNERS,
            ..Setup::empty()
        }
    }
}

/// Castling paths and unmoved rooks.
#[derive(Clone, Debug)]
pub struct Castles {
    mask: Bitboard,
    rook: [[Option<Square>; 2]; 2],
    path: [[Bitboard; 2]; 2],
    mode: CastlingMode,
}

impl Default for Castles {
    fn default() -> Castles {
        Castles {
            mode: CastlingMode::Standard,
            mask: Bitboard::CORNERS,
            rook: [
                [Some(Square::H8), Some(Square::A8)], // black
                [Some(Square::H1), Some(Square::A1)], // white
            ],
            path: [
                [
                    Bitboard(0x6000_0000_0000_0000),
                    Bitboard(0x0e00_0000_0000_0000),
                ],
                [
                    Bitboard(0x0000_0000_0000_0060),
                    Bitboard(0x0000_0000_0000_000e),
                ],
            ],
        }
    }
}

impl CastlingMode {
    pub fn detect(setup: &Setup) -> CastlingMode {
        let standard = Castles::from_setup(setup, CastlingMode::Standard).unwrap_or_else(|c| c);
        let chess960 = Castles::from_setup(setup, CastlingMode::Chess960).unwrap_or_else(|c| c);
        CastlingMode::from_standard(standard.mask == chess960.mask)
    }
}

impl Castles {
    pub fn empty(mode: CastlingMode) -> Castles {
        Castles {
            mode,
            mask: Bitboard(0),
            rook: [[None; 2]; 2],
            path: [[Bitboard(0); 2]; 2],
        }
    }

    pub fn from_setup(setup: &Setup, mode: CastlingMode) -> Result<Castles, Castles> {
        let mut castles = Castles::empty(mode);

        let rooks = setup.castling_rights & setup.board.rooks();

        for color in Color::ALL {
            if let Some(king) = setup.board.king_of(color) {
                if king.file() == File::A
                    || king.file() == File::H
                    || king.rank() != color.fold_wb(Rank::First, Rank::Eighth)
                {
                    continue;
                }

                let side = rooks & setup.board.by_color(color) & color.backrank();

                if let Some(a_side) = side.first().filter(|rook| rook.file() < king.file()) {
                    let rook_to = CastlingSide::QueenSide.rook_to(color);
                    let king_to = CastlingSide::QueenSide.king_to(color);
                    let chess960 = king.file() != File::E || a_side.file() != File::A;
                    if !chess960 || mode.is_chess960() {
                        castles.mask.add(a_side);
                        castles.rook[color as usize][CastlingSide::QueenSide as usize] =
                            Some(a_side);
                        castles.path[color as usize][CastlingSide::QueenSide as usize] =
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
                        castles.rook[color as usize][CastlingSide::KingSide as usize] =
                            Some(h_side);
                        castles.path[color as usize][CastlingSide::KingSide as usize] =
                            (attacks::between(h_side, rook_to).with(rook_to)
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

    pub fn any(&self) -> bool {
        self.mask.any()
    }

    pub fn is_empty(&self) -> bool {
        self.mask.is_empty()
    }

    pub fn has(&self, color: Color, side: CastlingSide) -> bool {
        self.rook(color, side).is_some()
    }

    pub fn has_side(&self, color: Color) -> bool {
        (self.mask & color.backrank()).any()
    }

    pub fn discard_rook(&mut self, square: Square) {
        if self.mask.remove(square) {
            self.rook[0][0] = self.rook[0][0].filter(|sq| *sq != square);
            self.rook[0][1] = self.rook[0][1].filter(|sq| *sq != square);
            self.rook[1][0] = self.rook[1][0].filter(|sq| *sq != square);
            self.rook[1][1] = self.rook[1][1].filter(|sq| *sq != square);
        }
    }

    pub fn discard_side(&mut self, color: Color) {
        self.mask.discard(color.backrank());
        self.rook[color as usize] = [None, None];
    }

    #[inline]
    pub fn rook(&self, color: Color, side: CastlingSide) -> Option<Square> {
        self.rook[color as usize][side as usize]
    }

    /// Gets the squares that need to be empty so that castling is possible
    /// on the given side.
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
    pub fn path(&self, color: Color, side: CastlingSide) -> Bitboard {
        self.path[color as usize][side as usize]
    }

    #[inline]
    pub fn castling_rights(&self) -> Bitboard {
        self.mask
    }

    pub fn mode(&self) -> CastlingMode {
        self.mode
    }
}

/// En passant square on the third or sixth rank.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct EnPassant(pub Square);

impl From<EnPassant> for Square {
    fn from(ep: EnPassant) -> Square {
        ep.square()
    }
}

impl EnPassant {
    pub fn from_setup(setup: &Setup) -> Result<Option<EnPassant>, ()> {
        let ep_square = match setup.ep_square {
            Some(ep_square) => ep_square,
            None => return Ok(None),
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
    pub fn square(self) -> Square {
        self.0
    }

    pub fn pawn_pushed_from(self) -> Square {
        self.0.xor(Square::A4)
    }

    pub fn pawn_pushed_to(self) -> Square {
        self.0.xor(Square::A2)
    }
}
