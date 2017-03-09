use std::fmt;
use std::fmt::Write;
use std::char;

use square::Square;
use types::{ Color, Role, Piece };
use bitboard::Bitboard;
use attacks::Precomp;

#[derive(Clone)]
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

    promoted: Bitboard,
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

            promoted: Bitboard(0),
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

            promoted: Bitboard(0),
        }
    }

    pub fn from_board_fen(board_fen: &str) -> Option<Board> {
        let mut board = Board::empty();

        let mut rank = 7;
        let mut file = 0;
        let mut promoted = false;

        for ch in board_fen.chars() {
            if ch == '/' {
                file = 0;
                rank -= 1;
            } else if ch == '~' {
                promoted = true;
                continue;
            } else if let Some(empty) = ch.to_digit(10) {
                file += empty;
            } else if let Some(piece) = Piece::from_char(ch) {
                match Square::from_coords(file as i8, rank as i8) {
                    Some(sq) => {
                        board.set_piece_at(sq, piece);
                        if promoted {
                            board.promoted.add(sq);
                            promoted = false;
                        }
                    },
                    None => return None
                }
                file += 1;
            } else {
                return None
            }

            if promoted {
                return None
            }
        }

        Some(board)
    }

    pub fn board_fen(&self, promoted: bool) -> String {
        let mut fen = String::with_capacity(15);

        for rank in (0..8).rev() {
            let mut empty = 0;

            for file in 0..8 {
                let square = Square::from_coords(file, rank).unwrap();

                empty = self.piece_at(square).map_or_else(|| empty + 1, |piece| {
                    if empty > 0 {
                        fen.push(char::from_digit(empty, 10).unwrap());
                    }
                    fen.push(piece.char());
                    if promoted && self.promoted.contains(square) {
                        fen.push('~');
                    }
                    0
                });

                if file == 7 && empty > 0 {
                    fen.push(char::from_digit(empty, 10).unwrap());
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

    pub fn promoted(&self) -> Bitboard { self.promoted }

    pub fn sliders(&self) -> Bitboard { self.bishops | self.rooks | self.queens }
    pub fn rooks_and_queens(&self) -> Bitboard { self.rooks | self.queens }
    pub fn bishops_and_queens(&self) -> Bitboard { self.bishops | self.queens }

    pub fn king_of(&self, color: Color) -> Option<Square> {
        (self.by_piece(color.king()) & !self.promoted).single_square()
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
        if self.pawns.contains(sq) {
            Some(Role::Pawn)
        } else if self.knights.contains(sq) {
            Some(Role::Knight)
        } else if self.bishops.contains(sq) {
            Some(Role::Bishop)
        } else if self.rooks.contains(sq) {
            Some(Role::Rook)
        } else if self.queens.contains(sq) {
            Some(Role::Queen)
        } else if self.kings.contains(sq) {
            Some(Role::King)
        } else {
            None
        }
    }

    pub fn piece_at(&self, sq: Square) -> Option<Piece> {
        self.color_at(sq).and_then(|color| {
            self.role_at(sq).map(|role| Piece { color: color, role: role })
        })
    }

    pub fn remove_piece_at(&mut self, sq: Square) -> Option<Piece> {
        self.piece_at(sq).map(|piece| {
            self.occupied.flip(sq);
            self.mut_by_color(piece.color).flip(sq);
            self.mut_by_role(piece.role).flip(sq);
            piece
        })
    }

    pub fn set_piece_at(&mut self, sq: Square, Piece { color, role }: Piece) {
        self.remove_piece_at(sq);
        self.occupied.flip(sq);
        self.mut_by_color(color).flip(sq);
        self.mut_by_role(role).flip(sq);
    }

    pub fn by_color(&self, color: Color) -> Bitboard {
        color.fold(self.white, self.black)
    }

    fn mut_by_color(&mut self, color: Color) -> &mut Bitboard {
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

    fn mut_by_role(&mut self, role: Role) -> &mut Bitboard {
        match role {
            Role::Pawn   => &mut self.pawns,
            Role::Knight => &mut self.knights,
            Role::Bishop => &mut self.bishops,
            Role::Rook   => &mut self.rooks,
            Role::Queen  => &mut self.queens,
            Role::King   => &mut self.kings
        }
    }

    pub fn by_piece(&self, Piece { color, role }: Piece) -> Bitboard {
        self.by_color(color) & self.by_role(role)
    }

    pub fn attacks_from(&self, sq: Square, precomp: &Precomp) -> Bitboard {
        self.piece_at(sq)
            .map(|piece| precomp.attacks(sq, piece, self.occupied))
            .unwrap_or(Bitboard(0))
    }

    pub fn attacks_to(&self, sq: Square, precomp: &Precomp) -> Bitboard {
        (precomp.rook_attacks(sq, self.occupied) & (self.rooks | self.queens)) |
        (precomp.bishop_attacks(sq, self.occupied) & (self.bishops | self.queens)) |
        (precomp.knight_attacks(sq) & self.knights) |
        (precomp.king_attacks(sq) & self.kings) |
        (precomp.pawn_attacks(Color::White, sq) & self.pawns & self.black) |
        (precomp.pawn_attacks(Color::Black, sq) & self.pawns & self.white)
    }
}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for rank in (0..8).rev() {
            for file in 0..8 {
                try!(f.write_char(self.piece_at(Square::from_coords(file, rank).unwrap())
                                      .map(|piece| piece.char())
                                      .unwrap_or('.')));

                if file < 7 {
                    try!(f.write_char(' '));
                } else {
                    try!(f.write_char('\n'));
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
    use types::{ White, Black };

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

    #[test]
    fn test_promoted() {
        let board = Board::from_board_fen("4k3/8/8/8/8/8/8/2~q1K3").unwrap();
        assert_eq!(board.piece_at(square::C1), Some(Black.queen()));
        assert!(board.promoted.contains(square::C1));
    }
}
