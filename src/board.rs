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

use std::fmt;
use std::fmt::Write;
use std::char;
use std::error::Error;

use square::Square;
use types::{ Color, Role, Piece };
use bitboard::Bitboard;
use attacks;

/// Error when attempting to parse an invalid board.
pub struct BoardFenError { _priv: () }

impl fmt::Debug for BoardFenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
         f.debug_struct("BoardFenError").finish()
    }
}

impl fmt::Display for BoardFenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "invalid board fen".fmt(f)
    }
}

impl Error for BoardFenError {
    fn description(&self) -> &str { "invalid board fen" }
}

/// Piece positions on a board.
///
/// # Examples
///
/// ```
/// # use shakmaty::{Board, square};
/// # use shakmaty::Color::Black;
/// let board = Board::new();
/// // r n b q k b n r
/// // p p p p p p p p
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . . . . .
/// // P P P P P P P P
/// // R N B Q K B N R
///
/// assert_eq!(board.piece_at(square::E8), Some(Black.king()));
///
/// let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR";
/// assert_eq!(board.board_fen(), fen);
/// ```
#[derive(Clone, Eq, PartialEq)]
pub struct Board {
    occupied: Bitboard,

    white: Bitboard,
    black: Bitboard,

    pawns: Bitboard,
    knights: Bitboard,
    bishops: Bitboard,
    rooks: Bitboard,
    queens: Bitboard,
    kings: Bitboard,
}

impl Board {
    pub fn new() -> Board {
        Board {
            occupied: Bitboard(0xffff00000000ffff),

            black: Bitboard(0xffff000000000000),
            white: Bitboard(0xffff),

            pawns: Bitboard(0xff00000000ff00),
            knights: Bitboard(0x4200000000000042),
            bishops: Bitboard(0x2400000000000024),
            rooks: Bitboard(0x8100000000000081),
            queens: Bitboard(0x800000000000008),
            kings: Bitboard(0x1000000000000010),
        }
    }

    pub fn empty() -> Board {
        Board {
            occupied: Bitboard(0),

            black: Bitboard(0),
            white: Bitboard(0),

            pawns: Bitboard(0),
            knights: Bitboard(0),
            bishops: Bitboard(0),
            rooks: Bitboard(0),
            queens: Bitboard(0),
            kings: Bitboard(0),
        }
    }

    pub fn horde() -> Board {
        Board {
            occupied: Bitboard(0xffff0066ffffffff),

            black: Bitboard(0xffff000000000000),
            white: Bitboard(0x66ffffffff),

            pawns: Bitboard(0xff0066ffffffff),
            knights: Bitboard(0x4200000000000000),
            bishops: Bitboard(0x2400000000000000),
            rooks: Bitboard(0x8100000000000000),
            queens: Bitboard(0x800000000000000),
            kings: Bitboard(0x1000000000000000),
        }
    }

    pub fn racing_kings() -> Board {
        Board {
            occupied: Bitboard(0xffff),

            black: Bitboard(0xf0f),
            white: Bitboard(0xf0f0),

            pawns: Bitboard(0),
            knights: Bitboard(0x1818),
            bishops: Bitboard(0x2424),
            rooks: Bitboard(0x4242),
            queens: Bitboard(0x81),
            kings: Bitboard(0x8100),
        }
    }

    pub fn from_board_fen(board_fen: &str) -> Result<Board, BoardFenError> {
        let mut board = Board::empty();

        let mut rank = 7i8;
        let mut file = 0i8;

        for ch in board_fen.chars() {
            if ch == '/' {
                file = 0;
                rank = rank.saturating_sub(1);
            } else if let Some(empty) = ch.to_digit(10) {
                file = file.saturating_add(empty as i8);
            } else if let Some(piece) = Piece::from_char(ch) {
                match Square::from_coords(file as i8, rank) {
                    Some(sq) => {
                        board.set_piece_at(sq, piece);
                    },
                    None => return Err(BoardFenError { _priv: () })
                }
                file += 1;
            } else {
                return Err(BoardFenError { _priv: () })
            }
        }

        Ok(board)
    }

    pub fn board_fen(&self) -> String {
        let mut fen = String::with_capacity(15);

        for rank in (0..8).rev() {
            let mut empty = 0;

            for file in 0..8 {
                let square = Square::from_coords(file, rank).unwrap();

                empty = self.piece_at(square).map_or_else(|| empty + 1, |piece| {
                    if empty > 0 {
                        fen.push(char::from_digit(empty, 10).expect("at most 8 empty squares on a rank"));
                    }
                    fen.push(piece.char());
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

    pub fn occupied(&self) -> Bitboard { self.occupied }

    pub fn pawns(&self)   -> Bitboard { self.pawns }
    pub fn knights(&self) -> Bitboard { self.knights }
    pub fn bishops(&self) -> Bitboard { self.bishops }
    pub fn rooks(&self)   -> Bitboard { self.rooks }
    pub fn queens(&self)  -> Bitboard { self.queens }
    pub fn kings(&self)   -> Bitboard { self.kings }

    pub fn white(&self) -> Bitboard { self.white }
    pub fn black(&self) -> Bitboard { self.black }

    /// Bishops, rooks and queens.
    pub fn sliders(&self) -> Bitboard { self.bishops | self.rooks | self.queens }

    pub fn rooks_and_queens(&self) -> Bitboard { self.rooks | self.queens }
    pub fn bishops_and_queens(&self) -> Bitboard { self.bishops | self.queens }

    /// The (unique) king of the given side.
    pub fn king_of(&self, color: Color) -> Option<Square> {
        self.by_piece(&color.king()).single_square()
    }

    pub fn color_at(&self, sq: Square) -> Option<Color> {
        if self.white.contains(sq) {
            Some(Color::White)
        } else if self.black.contains(sq) {
            Some(Color::Black)
        } else {
            None
        }
    }

    pub fn role_at(&self, sq: Square) -> Option<Role> {
        if !self.occupied.contains(sq) {
            None // catch early
        } else if self.pawns.contains(sq) {
            Some(Role::Pawn)
        } else if self.knights.contains(sq) {
            Some(Role::Knight)
        } else if self.bishops.contains(sq) {
            Some(Role::Bishop)
        } else if self.rooks.contains(sq) {
            Some(Role::Rook)
        } else if self.queens.contains(sq) {
            Some(Role::Queen)
        } else {
            Some(Role::King)
        }
    }

    pub fn piece_at(&self, sq: Square) -> Option<Piece> {
        self.role_at(sq).map(|role| {
            Piece { color: Color::from_bool(self.white.contains(sq)), role }
        })
    }

    pub fn remove_piece_at(&mut self, sq: Square) -> Option<Piece> {
        self.piece_at(sq).map(|piece| {
            self.occupied.flip(sq);
            self.by_color_mut(piece.color).flip(sq);
            self.by_role_mut(piece.role).flip(sq);
            piece
        })
    }

    pub fn set_piece_at(&mut self, sq: Square, Piece { color, role }: Piece) {
        self.remove_piece_at(sq);
        self.occupied.flip(sq);
        self.by_color_mut(color).flip(sq);
        self.by_role_mut(role).flip(sq);
    }

    pub fn by_color(&self, color: Color) -> Bitboard {
        color.fold(self.white, self.black)
    }

    fn by_color_mut(&mut self, color: Color) -> &mut Bitboard {
        color.fold(&mut self.white, &mut self.black)
    }

    pub fn by_role(&self, role: Role) -> Bitboard {
        match role {
            Role::Pawn   => self.pawns,
            Role::Knight => self.knights,
            Role::Bishop => self.bishops,
            Role::Rook   => self.rooks,
            Role::Queen  => self.queens,
            Role::King   => self.kings
        }
    }

    fn by_role_mut(&mut self, role: Role) -> &mut Bitboard {
        match role {
            Role::Pawn   => &mut self.pawns,
            Role::Knight => &mut self.knights,
            Role::Bishop => &mut self.bishops,
            Role::Rook   => &mut self.rooks,
            Role::Queen  => &mut self.queens,
            Role::King   => &mut self.kings
        }
    }

    pub fn by_piece(&self, piece: &Piece) -> Bitboard {
        self.by_color(piece.color) & self.by_role(piece.role)
    }

    pub fn attacks_from(&self, sq: Square) -> Bitboard {
        self.piece_at(sq)
            .map_or(Bitboard(0), |ref piece| attacks::attacks(sq, piece, self.occupied))
    }

    pub fn attacks_to(&self, sq: Square, attacker: Color, occupied: Bitboard) -> Bitboard {
        self.by_color(attacker) & (
            (attacks::rook_attacks(sq, occupied) & (self.rooks | self.queens)) |
            (attacks::bishop_attacks(sq, occupied) & (self.bishops | self.queens)) |
            (attacks::knight_attacks(sq) & self.knights) |
            (attacks::king_attacks(sq) & self.kings) |
            (attacks::pawn_attacks(!attacker, sq) & self.pawns))
    }
}

impl Default for Board {
    fn default() -> Self {
        Board::new()
    }
}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for rank in (0..8).rev() {
            for file in 0..8 {
                f.write_char(self.piece_at(Square::from_coords(file, rank).unwrap())
                                 .map_or('.', |piece| piece.char()))?;

                if file < 7 {
                    f.write_char(' ')?;
                } else {
                    f.write_char('\n')?;
                }

            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use square;
    use types::White;

    #[test]
    fn test_piece_at() {
        let board = Board::new();
        assert_eq!(board.piece_at(square::A2), Some(White.pawn()));
        assert_eq!(board.piece_at(square::B1), Some(White.knight()));
    }

    #[test]
    fn test_set_piece_at() {
        let mut board = Board::new();
        board.set_piece_at(square::A3, White.pawn());
        assert_eq!(board.piece_at(square::A3), Some(White.pawn()));
    }
}
