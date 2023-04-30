//! Piece positions on a board.

use core::{
    fmt,
    fmt::Write,
    iter::{FromIterator, FusedIterator},
};

use crate::{attacks, Bitboard, ByColor, ByRole, Color, File, Piece, Rank, Role, Square};

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
    by_role: ByRole<Bitboard>,
    by_color: ByColor<Bitboard>,
    occupied: Bitboard,
}

impl Board {
    pub const fn new() -> Board {
        Board {
            by_role: ByRole {
                pawn: Bitboard(0x00ff_0000_0000_ff00),
                knight: Bitboard(0x4200_0000_0000_0042),
                bishop: Bitboard(0x2400_0000_0000_0024),
                rook: Bitboard(0x8100_0000_0000_0081),
                queen: Bitboard(0x0800_0000_0000_0008),
                king: Bitboard(0x1000_0000_0000_0010),
            },
            by_color: ByColor {
                black: Bitboard(0xffff_0000_0000_0000),
                white: Bitboard(0xffff),
            },
            occupied: Bitboard(0xffff_0000_0000_ffff),
        }
    }

    pub const fn empty() -> Board {
        Board {
            by_role: ByRole {
                pawn: Bitboard::EMPTY,
                knight: Bitboard::EMPTY,
                bishop: Bitboard::EMPTY,
                rook: Bitboard::EMPTY,
                queen: Bitboard::EMPTY,
                king: Bitboard::EMPTY,
            },
            by_color: ByColor {
                white: Bitboard::EMPTY,
                black: Bitboard::EMPTY,
            },
            occupied: Bitboard::EMPTY,
        }
    }

    /// Creates a board from bitboard constituents.
    ///
    /// # Panics
    ///
    /// Panics if the bitboards are inconsistent.
    #[track_caller]
    pub fn from_bitboards(by_role: ByRole<Bitboard>, by_color: ByColor<Bitboard>) -> Board {
        let mut occupied = Bitboard::EMPTY;
        by_role.for_each(|role| {
            assert!(occupied.is_disjoint(role), "by_role not disjoint");
            occupied |= role;
        });
        assert!(
            by_color.black.is_disjoint(by_color.white),
            "by_color not disjoint"
        );
        assert_eq!(
            occupied,
            by_color.black | by_color.white,
            "by_role does not match by_color"
        );
        Board {
            by_role,
            by_color,
            occupied,
        }
    }

    pub const fn into_bitboards(self) -> (ByRole<Bitboard>, ByColor<Bitboard>) {
        (self.by_role, self.by_color)
    }

    #[cfg(feature = "variant")]
    pub const fn racing_kings() -> Board {
        Board {
            by_role: ByRole {
                pawn: Bitboard(0x0000),
                knight: Bitboard(0x1818),
                bishop: Bitboard(0x2424),
                rook: Bitboard(0x4242),
                queen: Bitboard(0x0081),
                king: Bitboard(0x8100),
            },
            by_color: ByColor {
                black: Bitboard(0x0f0f),
                white: Bitboard(0xf0f0),
            },
            occupied: Bitboard(0xffff),
        }
    }

    #[cfg(feature = "variant")]
    pub const fn horde() -> Board {
        Board {
            by_role: ByRole {
                pawn: Bitboard(0x00ff_0066_ffff_ffff),
                knight: Bitboard(0x4200_0000_0000_0000),
                bishop: Bitboard(0x2400_0000_0000_0000),
                rook: Bitboard(0x8100_0000_0000_0000),
                queen: Bitboard(0x0800_0000_0000_0000),
                king: Bitboard(0x1000_0000_0000_0000),
            },
            by_color: ByColor {
                black: Bitboard(0xffff_0000_0000_0000),
                white: Bitboard(0x0000_0066_ffff_ffff),
            },
            occupied: Bitboard(0xffff_0066_ffff_ffff),
        }
    }

    #[inline]
    pub const fn occupied(&self) -> Bitboard {
        self.occupied
    }

    #[inline]
    pub const fn pawns(&self) -> Bitboard {
        self.by_role.pawn
    }

    #[inline]
    pub const fn knights(&self) -> Bitboard {
        self.by_role.knight
    }

    #[inline]
    pub const fn bishops(&self) -> Bitboard {
        self.by_role.bishop
    }

    #[inline]
    pub const fn rooks(&self) -> Bitboard {
        self.by_role.rook
    }

    #[inline]
    pub const fn queens(&self) -> Bitboard {
        self.by_role.queen
    }

    #[inline]
    pub const fn kings(&self) -> Bitboard {
        self.by_role.king
    }

    #[inline]
    pub const fn white(&self) -> Bitboard {
        self.by_color.white
    }

    #[inline]
    pub const fn black(&self) -> Bitboard {
        self.by_color.black
    }

    /// Bishops, rooks and queens.
    #[inline]
    pub const fn sliders(&self) -> Bitboard {
        let ByRole {
            bishop,
            rook,
            queen,
            ..
        } = self.by_role;
        bishop.toggled_const(rook).toggled_const(queen)
    }

    /// Pawns, knights and kings.
    #[inline]
    pub const fn steppers(&self) -> Bitboard {
        let ByRole {
            pawn, knight, king, ..
        } = self.by_role;
        pawn.toggled_const(knight).toggled_const(king)
    }

    #[inline]
    pub const fn rooks_and_queens(&self) -> Bitboard {
        let ByRole { rook, queen, .. } = self.by_role;
        rook.toggled_const(queen)
    }
    #[inline]
    pub const fn bishops_and_queens(&self) -> Bitboard {
        let ByRole { bishop, queen, .. } = self.by_role;
        bishop.toggled_const(queen)
    }

    /// The (unique!) king of the given side, if any.
    #[inline]
    pub const fn king_of(&self, color: Color) -> Option<Square> {
        self.by_role
            .king
            .intersect(self.by_color(color))
            .single_square()
    }

    #[inline]
    pub fn color_at(&self, sq: Square) -> Option<Color> {
        self.by_color.find(|c| c.contains(sq))
    }

    #[inline]
    pub fn role_at(&self, sq: Square) -> Option<Role> {
        if self.occupied.contains(sq) {
            self.by_role.find(|r| r.contains(sq))
        } else {
            None // catch early
        }
    }

    #[inline]
    pub fn piece_at(&self, sq: Square) -> Option<Piece> {
        self.role_at(sq).map(|role| Piece {
            color: Color::from_white(self.by_color.white.contains(sq)),
            role,
        })
    }

    #[must_use = "use Board::discard_piece_at() if return value is not needed"]
    #[inline]
    pub fn remove_piece_at(&mut self, sq: Square) -> Option<Piece> {
        let piece = self.piece_at(sq);
        if let Some(p) = piece {
            self.by_role.get_mut(p.role).toggle(sq);
            self.by_color.get_mut(p.color).toggle(sq);
            self.occupied.toggle(sq);
        }
        piece
    }

    #[inline]
    pub fn discard_piece_at(&mut self, sq: Square) {
        self.by_role.as_mut().for_each(|r| r.discard(sq));
        self.by_color.as_mut().for_each(|c| c.discard(sq));
        self.occupied.discard(sq);
    }

    #[inline]
    pub fn set_piece_at(&mut self, sq: Square, Piece { color, role }: Piece) {
        self.discard_piece_at(sq);
        self.by_role.get_mut(role).toggle(sq);
        self.by_color.get_mut(color).toggle(sq);
        self.occupied.toggle(sq);
    }

    #[inline]
    pub const fn by_color(&self, color: Color) -> Bitboard {
        *self.by_color.get(color)
    }

    #[inline]
    pub const fn by_role(&self, role: Role) -> Bitboard {
        *self.by_role.get(role)
    }

    #[inline]
    pub const fn by_piece(&self, piece: Piece) -> Bitboard {
        self.by_color(piece.color)
            .intersect(self.by_role(piece.role))
    }

    pub fn attacks_from(&self, sq: Square) -> Bitboard {
        self.piece_at(sq).map_or(Bitboard(0), |piece| {
            attacks::attacks(sq, piece, self.occupied)
        })
    }

    #[inline]
    pub fn attacks_to(&self, sq: Square, attacker: Color, occupied: Bitboard) -> Bitboard {
        self.by_color(attacker)
            & ((attacks::rook_attacks(sq, occupied) & self.rooks_and_queens())
                | (attacks::bishop_attacks(sq, occupied) & self.bishops_and_queens())
                | (attacks::knight_attacks(sq) & self.by_role.knight)
                | (attacks::king_attacks(sq) & self.by_role.king)
                | (attacks::pawn_attacks(attacker.other(), sq) & self.by_role.pawn))
    }

    pub fn material_side(&self, color: Color) -> ByRole<u8> {
        let side = self.by_color(color);
        self.by_role
            .as_ref()
            .map(|pieces| (*pieces & side).count() as u8)
    }

    pub fn material(&self) -> ByColor<ByRole<u8>> {
        ByColor::new_with(|color| self.material_side(color))
    }

    fn transform<F>(&mut self, f: F)
    where
        F: Fn(Bitboard) -> Bitboard,
    {
        // In order to guarantee consistency, this method cannot be public
        // for use with custom transformations.
        self.by_role.as_mut().for_each(|r| *r = f(*r));
        self.by_color.as_mut().for_each(|c| *c = f(*c));
        self.occupied = self.by_color.white | self.by_color.black;
    }

    /// Mirror the board vertically. See [`Bitboard::flip_vertical`].
    pub fn flip_vertical(&mut self) {
        self.transform(Bitboard::flip_vertical);
    }

    /// Mirror the board horizontally. See [`Bitboard::flip_horizontal`].
    pub fn flip_horizontal(&mut self) {
        self.transform(Bitboard::flip_horizontal);
    }

    /// Mirror the board at the a1-h8 diagonal.
    /// See [`Bitboard::flip_diagonal`].
    pub fn flip_diagonal(&mut self) {
        self.transform(Bitboard::flip_diagonal);
    }

    /// Mirror the board at the h1-a8 diagonal.
    /// See [`Bitboard::flip_anti_diagonal`].
    pub fn flip_anti_diagonal(&mut self) {
        self.transform(Bitboard::flip_anti_diagonal);
    }

    /// Rotate the board 90 degrees clockwise. See [`Bitboard::rotate_90`].
    pub fn rotate_90(&mut self) {
        self.transform(Bitboard::rotate_90);
    }

    /// Rotate the board 180 degrees. See [`Bitboard::rotate_180`].
    pub fn rotate_180(&mut self) {
        self.transform(Bitboard::rotate_180);
    }

    /// Rotate the board 270 degrees clockwise. See [`Bitboard::rotate_270`].
    pub fn rotate_270(&mut self) {
        self.transform(Bitboard::rotate_270);
    }

    pub fn pop_front(&mut self) -> Option<(Square, Piece)> {
        self.occupied
            .first()
            .and_then(|sq| self.remove_piece_at(sq).map(|piece| (sq, piece)))
    }

    pub fn pop_back(&mut self) -> Option<(Square, Piece)> {
        self.occupied
            .last()
            .and_then(|sq| self.remove_piece_at(sq).map(|piece| (sq, piece)))
    }
}

impl Default for Board {
    fn default() -> Board {
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
    fn from_iter<T>(iter: T) -> Board
    where
        T: IntoIterator<Item = (Square, Piece)>,
    {
        let mut board = Board::empty();
        board.extend(iter);
        board
    }
}

impl IntoIterator for Board {
    type IntoIter = IntoIter;
    type Item = (Square, Piece);

    fn into_iter(self) -> IntoIter {
        IntoIter { inner: self }
    }
}

/// Iterator over the pieces of a [`Board`].
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct IntoIter {
    inner: Board,
}

impl fmt::Debug for IntoIter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IntoIter").finish_non_exhaustive()
    }
}

impl Iterator for IntoIter {
    type Item = (Square, Piece);

    fn next(&mut self) -> Option<(Square, Piece)> {
        self.inner.pop_front()
    }

    fn count(self) -> usize {
        self.len()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl ExactSizeIterator for IntoIter {
    fn len(&self) -> usize {
        self.inner.occupied.count()
    }
}

impl DoubleEndedIterator for IntoIter {
    fn next_back(&mut self) -> Option<(Square, Piece)> {
        self.inner.pop_back()
    }
}

impl FusedIterator for IntoIter {}

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
                Board::from_ascii_board_fen(fen.as_bytes()).expect("valid fen")
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

    #[test]
    fn test_from_bitboards() {
        let (by_role, by_color) = Board::default().into_bitboards();
        assert_eq!(Board::default(), Board::from_bitboards(by_role, by_color));
    }
}
