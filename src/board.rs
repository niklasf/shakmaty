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

use std::{
    fmt,
    fmt::Write,
    iter::{FromIterator, FusedIterator},
};

use crate::{
    attacks,
    bitboard::Bitboard,
    color::{ByColor, Color},
    material::{Material, MaterialSide},
    square::{File, Rank, Square},
    types::{Piece, Role},
};

/// [`Piece`] positions on a board.
///
/// # Examples
///
/// ```
/// use shakmaty::{Square, Board, Color::Black};
///
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
    occupied_co: ByColor<Bitboard>,
    occupied: [Bitboard; 7], // all and pieces indexed by Role
}

impl Board {
    pub fn new() -> Board {
        Board {
            occupied_co: ByColor {
                black: Bitboard(0xffff_0000_0000_0000),
                white: Bitboard(0xffff),
            },
            occupied: [
                Bitboard(0xffff_0000_0000_ffff),
                Bitboard(0x00ff_0000_0000_ff00), // pawns
                Bitboard(0x4200_0000_0000_0042), // knights
                Bitboard(0x2400_0000_0000_0024), // bishops
                Bitboard(0x8100_0000_0000_0081), // rooks
                Bitboard(0x0800_0000_0000_0008), // queens
                Bitboard(0x1000_0000_0000_0010), // kings
            ],
        }
    }

    pub fn empty() -> Board {
        Board {
            occupied_co: ByColor::default(),
            occupied: [Bitboard(0); 7],
        }
    }

    pub fn racing_kings() -> Board {
        Board {
            occupied_co: ByColor {
                black: Bitboard(0x0f0f),
                white: Bitboard(0xf0f0),
            },
            occupied: [
                Bitboard(0xffff),
                Bitboard(0x0000), // pawns
                Bitboard(0x1818), // knights
                Bitboard(0x2424), // bishops
                Bitboard(0x4242), // rooks
                Bitboard(0x0081), // queens
                Bitboard(0x8100), // kings
            ],
        }
    }

    pub fn horde() -> Board {
        Board {
            occupied_co: ByColor {
                black: Bitboard(0xffff_0000_0000_0000),
                white: Bitboard(0x0000_0066_ffff_ffff),
            },
            occupied: [
                Bitboard(0xffff_0066_ffff_ffff),
                Bitboard(0x00ff_0066_ffff_ffff), // pawns
                Bitboard(0x4200_0000_0000_0000), // knights
                Bitboard(0x2400_0000_0000_0000), // bishops
                Bitboard(0x8100_0000_0000_0000), // rooks
                Bitboard(0x0800_0000_0000_0000), // queens
                Bitboard(0x1000_0000_0000_0000), // kings
            ],
        }
    }

    #[inline]
    pub fn occupied(&self) -> Bitboard {
        self.occupied[0]
    }

    #[inline]
    pub fn pawns(&self) -> Bitboard {
        self.occupied[Role::Pawn as usize]
    }
    #[inline]
    pub fn knights(&self) -> Bitboard {
        self.occupied[Role::Knight as usize]
    }
    #[inline]
    pub fn bishops(&self) -> Bitboard {
        self.occupied[Role::Bishop as usize]
    }
    #[inline]
    pub fn rooks(&self) -> Bitboard {
        self.occupied[Role::Rook as usize]
    }
    #[inline]
    pub fn queens(&self) -> Bitboard {
        self.occupied[Role::Queen as usize]
    }
    #[inline]
    pub fn kings(&self) -> Bitboard {
        self.occupied[Role::King as usize]
    }

    #[inline]
    pub fn white(&self) -> Bitboard {
        self.occupied_co.white
    }
    #[inline]
    pub fn black(&self) -> Bitboard {
        self.occupied_co.black
    }

    /// Bishops, rooks and queens.
    #[inline]
    pub fn sliders(&self) -> Bitboard {
        self.bishops() ^ self.rooks() ^ self.queens()
    }
    /// Pawns, knights and kings.
    #[inline]
    pub fn steppers(&self) -> Bitboard {
        self.pawns() ^ self.knights() ^ self.kings()
    }

    #[inline]
    pub fn rooks_and_queens(&self) -> Bitboard {
        self.rooks() ^ self.queens()
    }
    #[inline]
    pub fn bishops_and_queens(&self) -> Bitboard {
        self.bishops() ^ self.queens()
    }

    /// The (unique) king of the given side.
    #[inline]
    pub fn king_of(&self, color: Color) -> Option<Square> {
        (self.kings() & self.by_color(color)).single_square()
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
        }
        piece
    }

    #[inline]
    pub fn discard_piece_at(&mut self, sq: Square) {
        self.occupied_co.black.discard(sq);
        self.occupied_co.white.discard(sq);
        self.occupied[0].discard(sq);
        self.occupied[1].discard(sq);
        self.occupied[2].discard(sq);
        self.occupied[3].discard(sq);
        self.occupied[4].discard(sq);
        self.occupied[5].discard(sq);
        self.occupied[6].discard(sq);
    }

    #[inline]
    pub fn set_piece_at(&mut self, sq: Square, Piece { color, role }: Piece) {
        self.discard_piece_at(sq);
        self.occupied[0].toggle(sq);
        self.by_color_mut(color).toggle(sq);
        self.by_role_mut(role).toggle(sq);
    }

    #[inline]
    pub fn by_color(&self, color: Color) -> Bitboard {
        *self.occupied_co.by_color(color)
    }

    #[inline]
    fn by_color_mut(&mut self, color: Color) -> &mut Bitboard {
        self.occupied_co.by_color_mut(color)
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
        self.by_color(attacker)
            & ((attacks::rook_attacks(sq, occupied) & self.rooks_and_queens())
                | (attacks::bishop_attacks(sq, occupied) & self.bishops_and_queens())
                | (attacks::knight_attacks(sq) & self.knights())
                | (attacks::king_attacks(sq) & self.kings())
                | (attacks::pawn_attacks(!attacker, sq) & self.pawns()))
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

    #[inline]
    fn apply_transform(&mut self, f: &dyn Fn(Bitboard) -> Bitboard) {
        self.occupied_co.black = f(self.occupied_co.black);
        self.occupied_co.white = f(self.occupied_co.white);
        self.occupied[0] = f(self.occupied[0]);
        self.occupied[1] = f(self.occupied[1]);
        self.occupied[2] = f(self.occupied[2]);
        self.occupied[3] = f(self.occupied[3]);
        self.occupied[4] = f(self.occupied[4]);
        self.occupied[5] = f(self.occupied[5]);
        self.occupied[6] = f(self.occupied[6]);
    }

    /// Flip the board vertically, see [`Bitboard::flip_vertical`].
    #[inline]
    pub fn flip_vertical(&mut self) {
        self.apply_transform(&Bitboard::flip_vertical)
    }

    /// Flip the board horizontally, see [`Bitboard::flip_horizontal`].
    #[inline]
    pub fn flip_horizontal(&mut self) {
        self.apply_transform(&Bitboard::flip_horizontal)
    }

    /// Flip the board diagonally, see [`Bitboard::flip_diagonal`].
    #[inline]
    pub fn flip_diagonal(&mut self) {
        self.apply_transform(&Bitboard::flip_diagonal)
    }

    /// Flip the board anti-diagonally, see [`Bitboard::flip_anti_diagonal`].
    #[inline]
    pub fn flip_anti_diagonal(&mut self) {
        self.apply_transform(&Bitboard::flip_anti_diagonal)
    }

    /// Rotate the board 90° clockwise, see [`Bitboard::rotate_90`].
    #[inline]
    pub fn rotate_90(&mut self) {
        self.apply_transform(&Bitboard::rotate_90)
    }

    /// Rotate the board at 180°, see [`Bitboard::rotate_180`].
    #[inline]
    pub fn rotate_180(&mut self) {
        self.apply_transform(&Bitboard::rotate_180)
    }

    /// Rotate the board at 270° clockwise, see [`Bitboard::rotate_270`].
    #[inline]
    pub fn rotate_270(&mut self) {
        self.apply_transform(&Bitboard::rotate_270)
    }
}

impl Default for Board {
    fn default() -> Self {
        Board::new()
    }
}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for rank in Rank::ALL.into_iter().rev() {
            for file in File::ALL {
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
            self.set_piece_at(sq, piece);
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
#[derive(Clone, Eq, PartialEq, Hash)]
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
        let (sq, role) = if let Some(sq) = self.pawns.pop_front() {
            (sq, Role::Pawn)
        } else if let Some(sq) = self.knights.pop_front() {
            (sq, Role::Knight)
        } else if let Some(sq) = self.bishops.pop_front() {
            (sq, Role::Bishop)
        } else if let Some(sq) = self.rooks.pop_front() {
            (sq, Role::Rook)
        } else if let Some(sq) = self.queens.pop_front() {
            (sq, Role::Queen)
        } else if let Some(sq) = self.kings.pop_front() {
            (sq, Role::King)
        } else {
            return None;
        };

        Some((sq, role.of(Color::from_white(self.white.contains(sq)))))
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
        self.pawns.count()
            + self.knights.count()
            + self.bishops.count()
            + self.rooks.count()
            + self.queens.count()
            + self.kings.count()
    }
}

impl FusedIterator for Pieces {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::color::Color::{Black, White};

    #[test]
    fn test_piece_at() {
        let board = Board::new();
        assert_eq!(board.piece_at(Square::A2), Some(White.pawn()));
        assert_eq!(board.piece_at(Square::B1), Some(White.knight()));
    }

    #[test]
    fn test_set_piece_at() {
        let mut board = Board::new();
        board.set_piece_at(Square::A3, White.pawn());
        assert_eq!(board.piece_at(Square::A3), Some(White.pawn()));
    }

    #[test]
    fn test_promoted() {
        let board: Board = "4k3/8/8/8/8/8/8/2q~1K3".parse().expect("valid fen");
        assert_eq!(board.piece_at(Square::C1), Some(Black.queen()));
    }

    #[test]
    fn test_board_transformation() {
        let board: Board = "1qrb4/1k2n3/1P2p3/1N1K4/1BQ5/1R1R4/1Q2B3/1K3N2"
            .parse()
            .expect("valid fen");
        let compare_trans = |trans: &dyn Fn(&mut Board), fen: &str| {
            let mut board_trans = board.clone();
            trans(&mut board_trans);
            assert_eq!(
                board_trans,
                Board::from_board_fen(fen.as_bytes()).expect("valid fen")
            );
        };
        compare_trans(
            &Board::flip_vertical,
            "1K3N2/1Q2B3/1R1R4/1BQ5/1N1K4/1P2p3/1k2n3/1qrb4",
        );
        compare_trans(
            &Board::flip_horizontal,
            "4brq1/3n2k1/3p2P1/4K1N1/5QB1/4R1R1/3B2Q1/2N3K1",
        );
        compare_trans(
            &Board::flip_diagonal,
            "8/8/N7/1B3pn1/2R1K2b/3Q3r/KQRBNPkq/8",
        );
        compare_trans(
            &Board::flip_anti_diagonal,
            "8/qkPNBRQK/r3Q3/b2K1R2/1np3B1/7N/8/8",
        );
        compare_trans(&Board::rotate_90, "8/KQRBNPkq/3Q3r/2R1K2b/1B3pn1/N7/8/8");
        compare_trans(
            &Board::rotate_180,
            "2N3K1/3B2Q1/4R1R1/5QB1/4K1N1/3p2P1/3n2k1/4brq1",
        );
        compare_trans(&Board::rotate_270, "8/8/7N/1np3B1/b2K1R2/r3Q3/qkPNBRQK/8");
    }
}
