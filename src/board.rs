use square;
use square::Square;
use bitboard::Bitboard;
use attacks::Precomp;
use std::ascii::AsciiExt;
use std::char;
use std::ops;
use std::fmt;
use std::fmt::Write;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Color {
    Black,
    White,
}

impl Color {
    pub fn fold<T>(self, white: T, black: T) -> T {
        match self {
            Color::Black => black,
            Color::White => white
        }
    }

    pub fn knight(self) -> Piece { Role::Knight.of(self) }
}

impl ops::Not for Color {
    type Output = Color;

    fn not(self) -> Color {
        self.fold(Color::Black, Color::White)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

    pub fn chr(self) -> char {
        match self {
            Role::Pawn =>   'p',
            Role::Knight => 'n',
            Role::Bishop => 'b',
            Role::Rook =>   'r',
            Role::Queen =>  'q',
            Role::King =>   'k'
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Piece {
    pub color: Color,
    pub role: Role,
}

impl Piece {
    pub fn chr(self) -> char {
        self.color.fold(self.role.chr().to_ascii_uppercase(), self.role.chr())
    }
}

#[derive(Debug, Clone)]
pub enum Move {
    Normal { from: Square, to: Square, promotion: Option<Role> },
    Put { to: Square, role: Role },
}

impl Move {
    pub fn from_uci(uci: &str) -> Option<Move> {
        if uci.len() < 4 || uci.len() > 5 {
            return None
        }

        Square::from_str(&uci[2..4]).and_then(|to| {
            Square::from_str(&uci[0..2]).map(|from| {
                // TODO: Promotions
                Move::Normal { from, to, promotion: None }
            })
            // TODO: Drops
       })
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Move::Normal { from, to, promotion: None } =>
                write!(f, "{}{}", from, to),
            &Move::Normal { from, to, promotion: Some(promotion) } =>
                write!(f, "{}{}{}", from, to, promotion.chr()),
            &Move::Put { to, role } =>
                write!(f, "{}@{}", role.chr().to_ascii_uppercase(), to)
        }
    }
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

    promoted: Bitboard,

    turn: Color,
    ep_square: Option<Square>,
}

struct Pins {
    blockers: Bitboard,
    pinners: Bitboard,
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

            turn: Color::White,
            ep_square: None,
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
        color.fold(self.black, self.white)
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

    pub fn us(&self) -> Bitboard {
        self.turn.fold(self.white, self.black)
    }

    pub fn our(&self, role: Role) -> Bitboard {
        self.us() & self.by_role(role)
    }

    pub fn them(&self) -> Bitboard {
        self.turn.fold(self.black, self.white)
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

    pub fn checkers(&self, precomp: &Precomp) -> Bitboard {
        self.our(Role::King).first()
            .map(|king| self.them() & self.attacks_to(king, precomp))
            .unwrap_or(Bitboard(0))
    }

    pub fn board_fen(&self) -> String {
        let mut fen = String::with_capacity(15);

        for rank in (0..8).rev() {
            let mut empty = 0;

            for file in 0..8 {
                empty = self.piece_at(Square::new(file, rank))
                    .map_or_else(|| empty + 1, |piece| {
                        if empty > 0 {
                            fen.push(char::from_digit(empty, 10).unwrap());
                        }
                        fen.push(piece.chr());
                        return 0
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

    fn push_moves(&self, moves: &mut Vec<Move>, from: Square, to: Bitboard) {
        for square in to {
            moves.push(Move::Normal { from, to: square, promotion: None });
        }
    }

    fn pseudo_legal_moves(&self, target: Bitboard, moves: &mut Vec<Move>, precomp: &Precomp) {
        for from in self.our(Role::King) {
            self.push_moves(moves, from,
                            precomp.king_attacks(from) & !self.us() & target);
        }

        for from in self.our(Role::Knight) {
            self.push_moves(moves, from,
                            precomp.knight_attacks(from) & !self.us() & target);
        }

        for from in self.our(Role::Rook) | self.our(Role::Queen) {
            self.push_moves(moves, from,
                            precomp.rook_attacks(from, self.occupied) & !self.us() & target);
        }

        for from in self.our(Role::Bishop) | self.our(Role::Queen) {
            self.push_moves(moves, from,
                            precomp.bishop_attacks(from, self.occupied) & !self.us() & target);
        }

        for from in self.our(Role::Pawn) {
            for to in precomp.pawn_attacks(self.turn, from) & self.them() & target {
                self.push_pawn_moves(moves, from, to);
            }
        }

        let single_moves = self.our(Role::Pawn).relative_shift(self.turn, 8) & !self.occupied;
        let double_moves = single_moves.relative_shift(self.turn, 8) &
                           Bitboard::relative_rank(self.turn, 3) &
                           !self.occupied;

        for to in single_moves & target {
            if let Some(from) = to.offset(self.turn.fold(-8, 8)) {
                self.push_pawn_moves(moves, from, to);
            }
        }

        for to in double_moves & target {
            if let Some(from) = to.offset(self.turn.fold(-16, 16)) {
                self.push_pawn_moves(moves, from, to);
            }
        }

        if let Some(to) = self.ep_square {
            if target.contains(to) {
                for from in self.our(Role::Pawn) & precomp.pawn_attacks(!self.turn, to) {
                    moves.push(Move::Normal { from, to, promotion: None });
                }
            }
        }

        // TODO: Castling
    }

    fn slider_blockers(&self, sliders: Bitboard, sq: Square, precomp: &Precomp) -> Pins {
        let snipers = (precomp.rook_attacks(sq, Bitboard(0)) & (self.queens | self.rooks)) |
                      (precomp.bishop_attacks(sq, Bitboard(0)) & (self.queens | self.bishops));

        let mut result = Pins { pinners: Bitboard(0), blockers: Bitboard(0) };

        for sniper in snipers & sliders {
            let b = precomp.between(sq, sniper) & self.occupied;

            if !b.more_than_one() {
                result.blockers = result.blockers | b;

                let team = self.color_at(sq)
                    .map(|color| self.by_color(color))
                    .unwrap_or(Bitboard(0));

                if !(b & team).is_empty() {
                    result.pinners.add(sniper);
                }
            }
        }

        result
    }

    fn is_legal(&self, m: &Move, pins: &Pins, precomp: &Precomp) -> bool {
        match m {
            &Move::Normal { from, to, promotion: _ } =>
                if let Some(ep_capture) = self.ep_capture(m) {
                    let mut occupied = self.occupied;
                    occupied.flip(from);
                    occupied.flip(ep_capture);
                    occupied.add(to);

                    self.our(Role::King).first().map(|king| {
                        (precomp.rook_attacks(king, occupied) & self.them() & (self.queens | self.rooks)).is_empty() &&
                        (precomp.bishop_attacks(king, occupied) & self.them() & (self.queens | self.bishops)).is_empty()
                    }).unwrap_or(true)
                } else if self.kings.contains(from) {
                    // TODO: Castling
                    (self.attacks_to(to, precomp) & self.them()).is_empty()
                } else {
                    !(self.us() & pins.blockers).contains(from) ||
                    precomp.aligned(from, to, self.our(Role::King).first().unwrap())
                },
            _ =>
                false // TODO
        }
    }

    fn evasions(&self, moves: &mut Vec<Move>, precomp: &Precomp) {
        let checkers = self.checkers(precomp);
        let king = self.our(Role::King).first().unwrap();
        let sliders = checkers & (self.rooks | self.bishops | self.queens);

        let mut attacked = Bitboard(0);
        for checker in sliders {
            attacked = attacked | precomp.ray(checker, king).without(checker);
        }

        for to in precomp.king_attacks(king) & !self.us() & !attacked {
            moves.push(Move::Normal { from: king, to, promotion: None });
        }

        if let Some(checker) = checkers.single_square() {
            let target = precomp.between(king, checker).with(checker);
            self.pseudo_legal_moves(target, moves, precomp);
        }
    }

    pub fn legal_moves(&self, moves: &mut Vec<Move>, precomp: &Precomp) {
        if self.checkers(precomp).is_empty() {
            self.pseudo_legal_moves(Bitboard::all(), moves, precomp);
        } else {
            self.evasions(moves, precomp);
        }

        let pins = self.slider_blockers(self.them(),
                                        self.our(Role::King).first().unwrap(),
                                        precomp);

        moves.retain(|m| self.is_legal(m, &pins, precomp));
    }

    fn ep_capture(&self, m: &Move) -> Option<Square> {
        match m {
            &Move::Normal { from, to, promotion: _ } =>
                if square::delta(from, to) & 1 != 0 &&
                        self.pawns.contains(from) &&
                        !self.occupied.contains(to) {
                    self.ep_square.and_then(|sq| sq.offset(self.turn.fold(-8, 8)))
                } else {
                    None
                },
            _ => return None
        }
    }

    pub fn do_move(&mut self, m: &Move) {
        let color = self.turn;

        match m {
            &Move::Normal { from, to, promotion } =>
                if let Some(moved) = self.remove_piece_at(from) {
                    // Execute en passant capture.
                    self.ep_capture(m).map(|sq| self.remove_piece_at(sq));

                    // Update en passant square.
                    if moved.role == Role::Pawn && square::distance(from, to) == 2 {
                        self.ep_square = from.offset(color.fold(8, -8))
                    } else {
                        self.ep_square = None;
                    }

                    // Move piece to new square.
                    self.set_piece_at(to, promotion.map(|role| role.of(color))
                                                   .unwrap_or(moved));
                },
            &Move::Put { to, role } => {
                self.ep_square = None;
                self.set_piece_at(to, Piece { color, role });
            }
        }

        self.turn = !self.turn;
    }
}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for rank in (0..8).rev() {
            for file in 0..8 {
                try!(f.write_char(self.piece_at(Square::new(file, rank))
                                      .map(|piece| piece.chr())
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

    #[test]
    fn test_piece_at() {
        let board = Board::new();
        assert_eq!(board.piece_at(square::A2), Some(Role::Pawn.of(Color::White)));
        assert_eq!(board.piece_at(square::B1), Some(Role::Knight.of(Color::White)));
    }

    #[test]
    fn test_set_piece_at() {
        let mut board = Board::new();
        board.set_piece_at(square::A3, Role::Pawn.of(Color::White));
        assert_eq!(board.piece_at(square::A3), Some(Role::Pawn.of(Color::White)));
    }
}
