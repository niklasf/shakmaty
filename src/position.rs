use attacks;
use situation::{Situation, Pockets, RemainingChecks};
use board::Board;
use bitboard::Bitboard;
use square;
use square::Square;
use types::{Color, White, Black, Role, Move, Uci};
use std::ascii::AsciiExt;
use std::str::FromStr;
use std::cmp::max;

#[derive(Debug)]
pub enum PositionError { }

#[derive(Clone, Default)]
pub struct PositionBuilder {
    pub situation: Situation,
    pub pockets: Option<Pockets>,
    pub remaining_checks: Option<RemainingChecks>,
}

impl PositionBuilder {
    pub fn new() -> PositionBuilder {
        PositionBuilder::default()
    }

    pub fn empty() -> PositionBuilder {
        PositionBuilder {
            situation: Situation::empty(),
            pockets: None,
            remaining_checks: None,
        }
    }

    pub fn from_fen(fen: &str) -> Option<PositionBuilder> {
        let mut sit = Situation::empty();
        let mut parts = fen.split(' ');

        if let Some(board) = parts.next().and_then(|board_fen| Board::from_board_fen(board_fen)) {
            sit.board = board
        } else {
            return None
        }

        match parts.next() {
            Some("w") => sit.turn = White,
            Some("b") => sit.turn = Black,
            Some(_)   => return None,
            None      => ()
        }

        if let Some(castling_part) = parts.next() {
            for ch in castling_part.chars() {
                if ch == '-' {
                    continue;
                }

                let color = Color::from_bool(ch.to_ascii_uppercase() == ch);

                let candidates = Bitboard::relative_rank(color, 0) &
                                 sit.board.by_piece(Role::Rook.of(color));

                let flag = match ch.to_ascii_lowercase() {
                    'k'  => candidates.last(),
                    'q'  => candidates.first(),
                    file => (candidates & Bitboard::file(file as i8 - 'a' as i8)).first(),
                };

                match flag {
                    Some(cr) => sit.castling_rights.add(cr),
                    None     => return None
                }
            }
        }

        if let Some(ep_part) = parts.next() {
            if ep_part != "-" {
                match Square::from_str(ep_part) {
                    Ok(sq) => sit.ep_square = Some(sq),
                    _      => return None
                }
            }
        }

        if let Some(halfmoves_part) = parts.next() {
            match halfmoves_part.parse::<u32>() {
                Ok(halfmoves) => sit.halfmove_clock = halfmoves,
                _             => return None
            }
        }

        if let Some(fullmoves_part) = parts.next() {
            match fullmoves_part.parse::<u32>() {
                Ok(fullmoves) => sit.fullmoves = max(1, fullmoves),
                _             => return None
            }
        }

        Some(PositionBuilder {
            situation: sit,
            pockets: None,
            remaining_checks: None
        })
    }

    pub fn build<P: Position>(&self) -> Result<P, PositionError> {
        P::from_builder(self)
    }
}

pub trait Position : Default + Clone {
    const MAX_LEGAL_MOVES: usize;
    const FEN_PROMOTED: bool;

    fn from_builder(builder: &PositionBuilder) -> Result<Self, PositionError>;

    fn situation(&self) -> &Situation;
    fn board(&self) -> &Board { &self.situation().board }
    fn turn(&self) -> Color { self.situation().turn }
    fn castling_rights(&self) -> Bitboard { self.situation().castling_rights }
    fn ep_square(&self) -> Option<Square> { self.situation().ep_square }
    fn halfmove_clock(&self) -> u32 { self.situation().halfmove_clock }
    fn fullmoves(&self) -> u32 { self.situation().fullmoves }

    fn pockets(&self) -> Option<&Pockets> { None }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }

    fn checkers(&self) -> Bitboard {
        self.board().king_of(self.turn())
           .map_or(Bitboard(0), |king| self.board().by_color(!self.turn()) & self.board().attacks_to(king))
    }

    fn validate(&self, uci: &Uci) -> Option<Move> {
        match *uci {
            Uci::Normal { from, to, promotion } => {
                self.board().role_at(from).map(|role| {
                    if role == Role::King {
                        if self.board().by_piece(self.turn().rook()).contains(to) {
                            return Move::Castle { king: from, rook: to}
                        }
                    }
                    Move::Normal { role, from, capture: self.board().role_at(to), to, promotion }
                })
            },
            Uci::Put { role, to } => Some(Move::Put { role, to }),
            Uci::Null => Some(Move::Null)
        }
    }

    /* fn san_candidates(&self, moves: &mut Vec<Move>, role: Role, target: Square) {
        let pos = self.position();
        self.legal_moves(moves);
        moves.retain(|m| match *m {
            Move::Normal { from, to, .. } =>
                to == target && pos.board().by_piece(role.of(pos.turn())).contains(from),
            _ => false
        })
    }

    fn san(self, m: &Move) -> String {
        fn suffix(pos: Situation, m: &Move) -> &'static str {
            let after = pos.do_move(m);

            if after.checkers().is_empty() {
                ""
            } else {
                let mut legals = Vec::new();
                after.legal_moves(&mut legals);
                if legals.is_empty() { "#" } else { "+" }
            }
        }

        match *m {
            Move::Normal { role, from, capture, to, promotion } => {
                let mut san = String::new();

                if role != Role::Pawn {
                    san.push(role.char().to_ascii_uppercase());

                    // Disambiguate.
                    let mut legals = Vec::new();
                    self.san_candidates(&mut legals, role, to);

                    let (rank, file) = legals.iter().fold((false, false), |(rank, file), c| match *c {
                        Move::Normal { from: candidate, .. } =>
                            if from == candidate {
                                (rank, file)
                            } else if from.rank() == candidate.rank() || from.file() != candidate.file() {
                                (rank, true)
                            } else {
                                (true, file)
                            },
                        _ => (rank, file)
                    });

                    if file {
                        san.push(from.file_char());
                    }
                    if rank {
                        san.push(from.rank_char());
                    }
                }

                if capture.is_some() {
                    if role == Role::Pawn {
                        san.push(from.file_char())
                    }
                    san.push('x');
                }

                san.push_str(&to.to_string());

                promotion.map(|r| {
                    san.push('=');
                    san.push(r.char().to_ascii_uppercase());
                });

                san.push_str(suffix(self, m));

                san
            },
            Move::EnPassant { from, to, .. } => format!("{}x{}{}", from.file_char(), to, suffix(self, m)),
            Move::Castle { .. } => format!("{}{}", m, suffix(self, m)),
            Move::Put { to, role } => format!("{}@{}{}", role.char().to_ascii_uppercase(), to, suffix(self, m)),
            Move::Null => "--".to_owned()
        }
    } */

    fn castling_xfen(&self) -> String {
        let mut fen = String::with_capacity(4);

        for color in &[White, Black] {
            let king = self.board().king_of(*color);

            let candidates = self.board().by_piece(color.rook()) &
                             Bitboard::relative_rank(*color, 0);

            for rook in (candidates & self.castling_rights()).rev() {
                if Some(rook) == candidates.first() && king.map_or(false, |k| rook < k) {
                    fen.push(color.fold('Q', 'q'));
                } else if Some(rook) == candidates.last() && king.map_or(false, |k| k < rook) {
                    fen.push(color.fold('K', 'k'));
                } else {
                    fen.push((rook.file() as u8 + color.fold('A', 'a') as u8) as char);
                }
            }
        }

        if fen.is_empty() {
            fen.push('-');
        }

        fen
    }

    fn epd(&self) -> String {
        let pockets = self.pockets()
                          .map_or("".to_owned(), |p| format!("[{}]", p));

        let checks = self.remaining_checks()
                         .map_or("".to_owned(), |r| format!(" {}", r));

        format!("{}{} {} {} {}{}",
                self.board().board_fen(Self::FEN_PROMOTED),
                pockets,
                self.turn().char(),
                self.castling_xfen(),
                self.ep_square().map_or("-".to_owned(), |sq| sq.to_string()),
                checks)
    }

    fn fen(&self) -> String {
        format!("{} {} {}", self.epd(), self.halfmove_clock(), self.fullmoves())
    }

    fn legal_moves(&self, moves: &mut Vec<Move>) {
        let pos = self.situation();
        let checkers = self.checkers();

        if checkers.is_empty() {
            gen_pseudo_legal(pos, Bitboard::all(), Bitboard::all(), moves);
            gen_en_passant(pos, moves);
            gen_castling_moves(pos, moves);
        } else {
            evasions(pos, checkers, moves);
        }

        let blockers = slider_blockers(pos, pos.them(),
                                       pos.board.king_of(pos.turn).unwrap());

        moves.retain(|m| is_safe(self.situation(), m, blockers));
    }

    fn do_move(self, m: &Move) -> Self;
}

#[derive(Default, Clone)]
pub struct Standard {
    situation: Situation
}

impl Position for Standard {
    const MAX_LEGAL_MOVES: usize = 255;
    const FEN_PROMOTED: bool = true;

    fn from_builder(builder: &PositionBuilder) -> Result<Standard, PositionError> {
        Ok(Standard { situation: builder.situation.clone() })
    }

    fn situation(&self) -> &Situation {
        &self.situation
    }

    fn do_move(mut self, m: &Move) -> Standard {
        self.situation = self.situation.do_move(m);
        self
    }
}

/* #[derive(Default, Clone)]
pub struct Crazyhouse {
    pos: Situation,
    pockets: Pockets,
}

impl Variant for Crazyhouse {
    fn from_fen(fen: &str) -> Option<Crazyhouse> {
        Situation::from_fen(fen).map(|pos| Crazyhouse { pos, ..Crazyhouse::default() })
    }

    fn position(&self) -> &Situation {
        &self.pos
    }

    fn do_move(mut self, m: &Move) -> Crazyhouse {
        let color = self.pos.turn();

        match *m {
            Move::Normal { capture: Some(role), to, .. } =>
                if self.pos.board().promoted().contains(to) {
                    *self.pockets.mut_by_color(color).mut_by_role(Role::Pawn) += 1;
                } else {
                    *self.pockets.mut_by_color(color).mut_by_role(role) += 1;
                },
            Move::Put { role, .. } =>
                *self.pockets.mut_by_color(color).mut_by_role(role) -= 1,
            _ => ()
        }

        self.pos = self.pos.do_move(m);

        self
    }
} */

/* #[derive(Default, Clone)]
pub struct ThreeCheck {
    pos: Situation,
    remaining_checks: RemainingChecks,
}

impl Variant for ThreeCheck {
    fn from_fen(fen: &str) -> Option<ThreeCheck> {
        Situation::from_fen(fen).map(|pos| ThreeCheck { pos, ..ThreeCheck::default() })
    }

    fn position(&self) -> &Situation {
        &self.pos
    }

    fn do_move(mut self, m: &Move) -> ThreeCheck {
        self.pos = self.pos.do_move(m);

        if !self.checkers().is_empty() {
            *self.remaining_checks.mut_by_color(self.pos.turn()) -= 1;
        }

        self
    }
} */

fn evasions(pos: &Situation, checkers: Bitboard, moves: &mut Vec<Move>) {
    let king = pos.our(Role::King).first().unwrap();
    let sliders = checkers & pos.board.sliders();

    let mut attacked = Bitboard(0);
    for checker in sliders {
        attacked = attacked | attacks::ray(checker, king).without(checker);
    }

    for to in attacks::king_attacks(king) & !pos.us() & !attacked {
        moves.push(Move::Normal { role: Role::King, from: king, capture: pos.board.role_at(to), to, promotion: None });
    }

    if let Some(checker) = checkers.single_square() {
        let target = attacks::between(king, checker).with(checker);
        gen_pseudo_legal(pos, !pos.board.kings(), target, moves);
        gen_en_passant(pos, moves);
    }
}

fn gen_castling_moves(pos: &Situation, moves: &mut Vec<Move>) {
    let backrank = Bitboard::relative_rank(pos.turn, 0);

    for king in pos.our(Role::King) & backrank {
        'next_rook: for rook in pos.castling_rights & backrank {
            let (king_to, rook_to) = if king < rook {
                (pos.turn.fold(square::G1, square::G8),
                 pos.turn.fold(square::F1, square::F8))
            } else {
                (pos.turn.fold(square::C1, square::C8),
                 pos.turn.fold(square::D1, square::D8))
            };

            let empty_for_king = attacks::between(king, king_to).with(king_to)
                                        .without(rook).without(king);

            let empty_for_rook = attacks::between(rook, rook_to).with(rook_to)
                                        .without(rook).without(king);

            if !(pos.board.occupied() & empty_for_king).is_empty() {
                continue;
            }

            if !(pos.board.occupied() & empty_for_rook).is_empty() {
                continue;
            }

            for sq in attacks::between(king, king_to).with(king).with(king_to) {
                if !(pos.board.attacks_to(sq) & pos.them()).is_empty() {
                    continue 'next_rook;
                }
            }

            if !(attacks::rook_attacks(king_to, pos.board.occupied().without(rook)) &
                 pos.them() & pos.board.rooks_and_queens()).is_empty() {
                continue;
            }

            moves.push(Move::Castle { king, rook });
        }
    }
}

fn push_pawn_moves(pos: &Situation, moves: &mut Vec<Move>, from: Square, to: Square) {
    let capture = pos.board.role_at(to); // XXX

    if to.rank() == pos.turn.fold(7, 0) {
        moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Queen) } );
        moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Rook) } );
        moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Bishop) } );
        moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Knight) } );
    } else {
        moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: None } );
    }
}

fn push_moves(pos: &Situation, moves: &mut Vec<Move>, role: Role, from: Square, to: Bitboard) {
    for square in to {
        moves.push(Move::Normal { role, from, capture: pos.board.role_at(square), to: square, promotion: None });
    }
}

fn gen_pseudo_legal(pos: &Situation, selection: Bitboard, target: Bitboard, moves: &mut Vec<Move>) {
    for from in pos.our(Role::King) & selection {
        push_moves(pos, moves, Role::King, from,
                   attacks::king_attacks(from) & !pos.us() & target);
    }

    for from in pos.our(Role::Knight) & selection {
        push_moves(pos, moves, Role::Knight, from,
                   attacks::knight_attacks(from) & !pos.us() & target);
    }

    for from in pos.our(Role::Rook) & selection {
        push_moves(pos, moves, Role::Rook, from,
                   attacks::rook_attacks(from, pos.board.occupied()) & !pos.us() & target);
    }

    for from in pos.our(Role::Queen) & selection {
        push_moves(pos, moves, Role::Queen, from,
                   attacks::rook_attacks(from, pos.board.occupied()) & !pos.us() & target);
    }

    for from in pos.our(Role::Bishop) & selection {
        push_moves(pos, moves, Role::Bishop, from,
                   attacks::bishop_attacks(from, pos.board.occupied()) & !pos.us() & target);
    }

    for from in pos.our(Role::Queen) & selection {
        push_moves(pos, moves, Role::Queen, from,
                   attacks::bishop_attacks(from, pos.board.occupied()) & !pos.us() & target);
    }

    for from in pos.our(Role::Pawn) {
        for to in attacks::pawn_attacks(pos.turn, from) & pos.them() & target {
            push_pawn_moves(pos, moves, from, to);
        }
    }

    let single_moves = (pos.our(Role::Pawn) & selection).relative_shift(pos.turn, 8) &
                       !pos.board.occupied();

    let double_moves = single_moves.relative_shift(pos.turn, 8) &
                       Bitboard::relative_rank(pos.turn, 3) &
                       !pos.board.occupied();

    for to in single_moves & target {
        if let Some(from) = to.offset(pos.turn.fold(-8, 8)) {
            push_pawn_moves(pos, moves, from, to);
        }
    }

    for to in double_moves & target {
        if let Some(from) = to.offset(pos.turn.fold(-16, 16)) {
            push_pawn_moves(pos, moves, from, to);
        }
    }
}

fn gen_en_passant(pos: &Situation, moves: &mut Vec<Move>) {
    if let Some(to) = pos.ep_square {
        for from in pos.our(Role::Pawn) & attacks::pawn_attacks(!pos.turn, to) {
            moves.push(Move::EnPassant { from, to, pawn: to.offset(pos.turn.fold(-8, 8)).unwrap() }); // XXX
        }
    }
}

fn slider_blockers(pos: &Situation, sliders: Bitboard, sq: Square) -> Bitboard {
    let snipers = (attacks::rook_attacks(sq, Bitboard(0)) & pos.board.rooks_and_queens()) |
                  (attacks::bishop_attacks(sq, Bitboard(0)) & pos.board.bishops_and_queens());

    let mut blockers = Bitboard(0);

    for sniper in snipers & sliders {
        let b = attacks::between(sq, sniper) & pos.board.occupied();

        if !b.more_than_one() {
            blockers = blockers | b;
        }
    }

    blockers
}

fn is_safe(pos: &Situation, m: &Move, blockers: Bitboard) -> bool {
    match *m {
        Move::Normal { role, from, to, .. } =>
            if role == Role::King {
                (pos.board.attacks_to(to) & pos.them()).is_empty()
            } else {
                !(pos.us() & blockers).contains(from) ||
                attacks::aligned(from, to, pos.our(Role::King).first().unwrap())
            },
        Move::EnPassant { from, to, pawn } => {
            let mut occupied = pos.board.occupied();
            occupied.flip(from);
            occupied.flip(pawn);
            occupied.add(to);

            pos.our(Role::King).first().map(|king| {
                (attacks::rook_attacks(king, occupied) & pos.them() & pos.board.rooks_and_queens()).is_empty() &&
                (attacks::bishop_attacks(king, occupied) & pos.them() & pos.board.bishops_and_queens()).is_empty()
            }).unwrap_or(true)
        },
        Move::Castle { .. } => {
            true
        },
        _ => false
    }
}
