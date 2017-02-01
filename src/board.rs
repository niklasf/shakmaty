use std::ops;
use square::Square;
use bitboard::Bitboard;
use attacks::Precomp;

#[derive(Copy, Clone)]
pub enum Color {
    Black,
    White,
}

impl Color {
    fn fold<T>(self, white: T, black: T) -> T {
        match self {
            Color::Black => black,
            Color::White => white
        }
    }
}

impl ops::Not for Color {
    type Output = Color;

    fn not(self) -> Color {
        self.fold(Color::Black, Color::White)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Role {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl Role {
    pub fn of(self, color: Color) -> Piece {
        Piece { color, role: self }
    }
}

pub struct Piece {
    pub color: Color,
    pub role: Role,
}

#[derive(Debug, Clone)]
pub enum Move {
    Normal { from: Square, to: Square, promotion: Option<Role> },
    Put { to: Square, role: Role },
}

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

    turn: Color,
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

            turn: Color::White,
        }
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
        match color {
            Color::Black => self.black,
            Color::White => self.white,
        }
    }

    fn mut_by_color(&mut self, color: Color) -> &mut Bitboard {
        match color {
            Color::Black => &mut self.black,
            Color::White => &mut self.white
        }
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
        self.role_at(sq).map(|role| match role {
            Role::Pawn   => precomp.pawn_attacks(self.turn, sq),
            Role::Knight => precomp.knight_attacks(sq),
            Role::Bishop => precomp.bishop_attacks(sq, self.occupied),
            Role::Rook   => precomp.rook_attacks(sq, self.occupied),
            Role::Queen  => precomp.queen_attacks(sq, self.occupied),
            Role::King   => precomp.king_attacks(sq)
        }).unwrap_or(Bitboard(0))
    }

    fn push_pawn_moves(&self, moves: &mut Vec<Move>, from: Square, to: Square) {
        if to.rank() == self.turn.fold(7, 0) {
            moves.push(Move::Normal { from, to, promotion: Some(Role::Queen) } );
            moves.push(Move::Normal { from, to, promotion: Some(Role::Rook) } );
            moves.push(Move::Normal { from, to, promotion: Some(Role::Bishop) } );
            moves.push(Move::Normal { from, to, promotion: Some(Role::Knight) } );
        } else {
            moves.push(Move::Normal { from, to, promotion: None } );
        }
    }

    pub fn pseudo_legal_moves(&self, moves: &mut Vec<Move>, precomp: &Precomp) {
        let us = self.by_color(self.turn);
        let them = self.by_color(!self.turn);

        for from in us & !self.pawns {
            for to in self.attacks_from(from, precomp) & !us {
                moves.push(Move::Normal { from, to, promotion: None } );
            }
        }

        for from in us & self.pawns {
            for to in self.attacks_from(from, precomp) & them {
                self.push_pawn_moves(moves, from, to);
            }
        }

        let single_moves = (us & self.pawns).relative_shift(self.turn, 8) & !self.occupied;
        let double_moves = single_moves.relative_shift(self.turn, 8) &
                           Bitboard::relative_rank(self.turn, 3) &
                           !self.occupied;

        for to in single_moves {
            let from = Square(to.0 + self.turn.fold(-8, 8));
            self.push_pawn_moves(moves, from, to);
        }

        for to in double_moves {
            let from = Square(to.0 + self.turn.fold(-16, 16));
            self.push_pawn_moves(moves, from, to);
        }

        // TODO: Castling
        // TODO: En-passant
    }

    pub fn do_move(&mut self, m: &Move) {
        let color = self.turn;

        match m {
            &Move::Normal { from, to, promotion } =>
                if let Some(moved) = self.remove_piece_at(from) {
                    self.set_piece_at(to, promotion.map(|role| role.of(color))
                                                   .unwrap_or(moved))
                },
            &Move::Put { to, role } =>
                self.set_piece_at(to, Piece { color, role })
        }

        self.turn = !self.turn;
    }
}
