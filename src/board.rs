// This file is part of the shakmaty library.
// Copyright (C) 2017-2019 Niklas Fiekas <niklas.fiekas@backscattering.de>
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
use std::iter::FromIterator;

use crate::attacks;
use crate::bitboard::Bitboard;
use crate::square::{File, Rank, Square};
use crate::types::{Color, Piece, Role};
use crate::material::{Material, MaterialSide};

/// [`Piece`] positions on a board.
///
/// # Examples
///
/// ```
/// # use shakmaty::{Square, Board};
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
/// assert_eq!(board.piece_at(Square::E8), Some(Black.king()));
/// ```
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Board {
    occupied_co: [Bitboard; 2], // indexed by Color
    occupied: [Bitboard; 7], // all and pieces indexed by Role
    promoted: Bitboard,
}

impl Board {
    pub fn new() -> Board {
        Board {
            occupied_co: [Bitboard(0xffff_0000_0000_0000), Bitboard(0xffff)],
            occupied: [
                Bitboard(0xffff_0000_0000_ffff),
                Bitboard(0x00ff_0000_0000_ff00), // pawns
                Bitboard(0x4200_0000_0000_0042), // knights
                Bitboard(0x2400_0000_0000_0024), // bishops
                Bitboard(0x8100_0000_0000_0081), // rooks
                Bitboard(0x0800_0000_0000_0008), // queens
                Bitboard(0x1000_0000_0000_0010), // kings
            ],
            promoted: Bitboard(0),
        }
    }

    pub fn empty() -> Board {
        Board {
            occupied_co: [Bitboard(0), Bitboard(0)],
            occupied: [Bitboard(0); 7],
            promoted: Bitboard(0),
        }
    }

    pub fn racing_kings() -> Board {
        Board {
            occupied_co: [Bitboard(0x0f0f), Bitboard(0xf0f0)],
            occupied: [
                Bitboard(0xffff),
                Bitboard(0x0000), // pawns
                Bitboard(0x1818), // knights
                Bitboard(0x2424), // bishops
                Bitboard(0x4242), // rooks
                Bitboard(0x0081), // queens
                Bitboard(0x8100), // kings
            ],
            promoted: Bitboard(0),
        }
    }

    pub fn horde() -> Board {
        Board {
            occupied_co: [
                Bitboard(0xffff_0000_0000_0000), // black
                Bitboard(0x0000_0066_ffff_ffff), // white
            ],
            occupied: [
                Bitboard(0xffff_0066_ffff_ffff),
                Bitboard(0x00ff_0066_ffff_ffff), // pawns
                Bitboard(0x4200_0000_0000_0000), // knights
                Bitboard(0x2400_0000_0000_0000), // bishops
                Bitboard(0x8100_0000_0000_0000), // rooks
                Bitboard(0x0800_0000_0000_0000), // queens
                Bitboard(0x1000_0000_0000_0000), // kings
            ],
            promoted: Bitboard(0),
        }
    }

    #[inline]
    pub fn occupied(&self) -> Bitboard { self.occupied[0] }

    #[inline]
    pub fn pawns(&self)   -> Bitboard { self.occupied[Role::Pawn as usize] }
    #[inline]
    pub fn knights(&self) -> Bitboard { self.occupied[Role::Knight as usize] }
    #[inline]
    pub fn bishops(&self) -> Bitboard { self.occupied[Role::Bishop as usize] }
    #[inline]
    pub fn rooks(&self)   -> Bitboard { self.occupied[Role::Rook as usize] }
    #[inline]
    pub fn queens(&self)  -> Bitboard { self.occupied[Role::Queen as usize] }
    #[inline]
    pub fn kings(&self)   -> Bitboard { self.occupied[Role::King as usize] }

    #[inline]
    pub fn white(&self) -> Bitboard { self.occupied_co[Color::White as usize] }
    #[inline]
    pub fn black(&self) -> Bitboard { self.occupied_co[Color::Black as usize] }

    #[inline]
    pub fn promoted(&self) -> Bitboard { self.promoted }

    /// Bishops, rooks and queens.
    #[inline]
    pub fn sliders(&self) -> Bitboard { self.bishops() ^ self.rooks() ^ self.queens() }
    /// Pawns, knights and kings.
    #[inline]
    pub fn steppers(&self) -> Bitboard { self.pawns() ^ self.knights() ^ self.kings() }

    #[inline]
    pub fn rooks_and_queens(&self) -> Bitboard { self.rooks() ^ self.queens() }
    #[inline]
    pub fn bishops_and_queens(&self) -> Bitboard { self.bishops() ^ self.queens() }

    /// The (unique, unpromoted) king of the given side.
    #[inline]
    pub fn king_of(&self, color: Color) -> Option<Square> {
        (self.kings() & self.by_color(color) & !self.promoted).single_square()
    }

    #[inline]
    pub fn color_at(&self, sq: Square) -> Option<Color> {
        if self.white().contains(sq) {
            Some(Color::White)
        } else if self.black().contains(sq) {
            Some(Color::Black)
        } else {
            None
        }
    }

    #[inline]
    pub fn role_at(&self, sq: Square) -> Option<Role> {
        if !self.occupied[0].contains(sq) {
            None // catch early
        } else if self.pawns().contains(sq) {
            Some(Role::Pawn)
        } else if self.knights().contains(sq) {
            Some(Role::Knight)
        } else if self.bishops().contains(sq) {
            Some(Role::Bishop)
        } else if self.rooks().contains(sq) {
            Some(Role::Rook)
        } else if self.queens().contains(sq) {
            Some(Role::Queen)
        } else {
            Some(Role::King)
        }
    }

    #[inline]
    pub fn piece_at(&self, sq: Square) -> Option<Piece> {
        self.role_at(sq).map(|role| Piece {
            color: Color::from_white(self.white().contains(sq)),
            role,
        })
    }

    #[inline]
    pub fn remove_piece_at(&mut self, sq: Square) -> Option<Piece> {
        let piece = self.piece_at(sq);
        if let Some(p) = piece {
            self.occupied[0].toggle(sq);
            self.by_color_mut(p.color).toggle(sq);
            self.by_role_mut(p.role).toggle(sq);
            self.promoted.discard(sq);
        }
        piece
    }

    #[inline]
    pub fn discard_piece_at(&mut self, sq: Square) {
        self.occupied_co[0].discard(sq);
        self.occupied_co[1].discard(sq);
        self.occupied[0].discard(sq);
        self.occupied[1].discard(sq);
        self.occupied[2].discard(sq);
        self.occupied[3].discard(sq);
        self.occupied[4].discard(sq);
        self.occupied[5].discard(sq);
        self.occupied[6].discard(sq);
        self.promoted.discard(sq);
    }

    #[inline]
    pub fn set_piece_at(&mut self, sq: Square, Piece { color, role }: Piece, promoted: bool) {
        self.discard_piece_at(sq);
        self.occupied[0].toggle(sq);
        self.by_color_mut(color).toggle(sq);
        self.by_role_mut(role).toggle(sq);
        if promoted {
            self.promoted.toggle(sq);
        }
    }

    #[inline]
    pub fn by_color(&self, color: Color) -> Bitboard {
        self.occupied_co[color as usize]
    }

    #[inline]
    fn by_color_mut(&mut self, color: Color) -> &mut Bitboard {
        &mut self.occupied_co[color as usize]
    }

    #[inline]
    pub fn by_role(&self, role: Role) -> Bitboard {
        self.occupied[role as usize]
    }

    #[inline]
    fn by_role_mut(&mut self, role: Role) -> &mut Bitboard {
        &mut self.occupied[role as usize]
    }

    #[inline]
    pub fn by_piece(&self, piece: Piece) -> Bitboard {
        self.by_color(piece.color) & self.by_role(piece.role)
    }

    pub fn attacks_from(&self, sq: Square) -> Bitboard {
        self.piece_at(sq).map_or(Bitboard(0), |piece| {
            attacks::attacks(sq, piece, self.occupied[0])
        })
    }

    #[inline]
    pub fn attacks_to(&self, sq: Square, attacker: Color, occupied: Bitboard) -> Bitboard {
        self.by_color(attacker) & (
            (attacks::rook_attacks(sq, occupied) & self.rooks_and_queens()) |
            (attacks::bishop_attacks(sq, occupied) & self.bishops_and_queens()) |
            (attacks::knight_attacks(sq) & self.knights()) |
            (attacks::king_attacks(sq) & self.kings()) |
            (attacks::pawn_attacks(!attacker, sq) & self.pawns()))
    }

    pub fn pieces(&self) -> Pieces {
        Pieces {
            pawns: self.pawns(),
            knights: self.knights(),
            bishops: self.bishops(),
            rooks: self.rooks(),
            queens: self.queens(),
            kings: self.kings(),
            white: self.white(),
        }
    }

    pub fn material_side(&self, color: Color) -> MaterialSide {
        let side = self.by_color(color);

        MaterialSide {
            pawns: (self.pawns() & side).count() as u8,
            knights: (self.knights() & side).count() as u8,
            bishops: (self.bishops() & side).count() as u8,
            rooks: (self.rooks() & side).count() as u8,
            queens: (self.queens() & side).count() as u8,
            kings: (self.kings() & side).count() as u8,
        }
    }

    pub fn material(&self) -> Material {
        Material {
            black: self.material_side(Color::Black),
            white: self.material_side(Color::White),
        }
    }
}

impl Default for Board {
    fn default() -> Self {
        Board::new()
    }
}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for rank in (0..8).map(Rank::new).rev() {
            for file in (0..8).map(File::new) {
                let square = Square::from_coords(file, rank);
                f.write_char(self.piece_at(square).map_or('.', Piece::char))?;
                f.write_char(if file < File::H { ' ' } else { '\n' })?;
            }
        }

        Ok(())
    }
}

impl Extend<(Square, Piece)> for Board {
    fn extend<T: IntoIterator<Item = (Square, Piece)>>(&mut self, iter: T) {
        for (sq, piece) in iter {
            self.set_piece_at(sq, piece, false);
        }
    }
}

impl FromIterator<(Square, Piece)> for Board {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (Square, Piece)>,
    {
        let mut board = Board::empty();
        board.extend(iter);
        board
    }
}

/// Iterator over the pieces of a [`Board`].
#[derive(Clone)]
pub struct Pieces {
    pawns: Bitboard,
    knights: Bitboard,
    bishops: Bitboard,
    rooks: Bitboard,
    queens: Bitboard,
    kings: Bitboard,
    white: Bitboard,
}

impl fmt::Debug for Pieces {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Pieces").finish()
    }
}

impl Iterator for Pieces {
    type Item = (Square, Piece);

    fn next(&mut self) -> Option<(Square, Piece)> {
        if let Some(sq) = self.pawns.pop_front() {
            return Some((sq, (Color::from_white(self.white.contains(sq)).pawn())));
        }
        if let Some(sq) = self.knights.pop_front() {
            return Some((sq, (Color::from_white(self.white.contains(sq)).knight())));
        }
        if let Some(sq) = self.bishops.pop_front() {
            return Some((sq, (Color::from_white(self.white.contains(sq)).bishop())));
        }
        if let Some(sq) = self.rooks.pop_front() {
            return Some((sq, (Color::from_white(self.white.contains(sq)).rook())));
        }
        if let Some(sq) = self.queens.pop_front() {
            return Some((sq, (Color::from_white(self.white.contains(sq)).queen())));
        }
        if let Some(sq) = self.kings.pop_front() {
            return Some((sq, (Color::from_white(self.white.contains(sq)).king())));
        }
        None
    }

    fn count(self) -> usize {
        self.len()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl ExactSizeIterator for Pieces {
    fn len(&self) -> usize {
        self.pawns.count() + self.knights.count() + self.bishops.count()
            + self.rooks.count() + self.queens.count() + self.kings.count()
    }
}

impl ::std::iter::FusedIterator for Pieces {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{Black, White};

    #[test]
    fn test_piece_at() {
        let board = Board::new();
        assert_eq!(board.piece_at(Square::A2), Some(White.pawn()));
        assert_eq!(board.piece_at(Square::B1), Some(White.knight()));
    }

    #[test]
    fn test_set_piece_at() {
        let mut board = Board::new();
        board.set_piece_at(Square::A3, White.pawn(), false);
        assert_eq!(board.piece_at(Square::A3), Some(White.pawn()));
    }

    #[test]
    fn test_promoted() {
        let board: Board = "4k3/8/8/8/8/8/8/2q~1K3".parse().expect("valid fen");
        assert_eq!(board.piece_at(Square::C1), Some(Black.queen()));
        assert!(board.promoted().contains(Square::C1));
    }
}
