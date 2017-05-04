use attacks;
use board::Board;
use bitboard::Bitboard;
use square;
use square::Square;
use types::{Color, White, Black, Role, Piece, Move, Pockets, RemainingChecks, Uci};
use setup::Setup;

use arrayvec::ArrayVec;

#[derive(Debug)]
pub enum PositionError {
    Empty,
    NoKing { color: Color },
    TooManyPawns { color: Color },
    TooManyPieces { color: Color },
    TooManyKings,
    PawnsOnBackrank,
    BadCastlingRights,
    InvalidEpSquare,
    OppositeCheck,
}

pub type MoveError = ();

pub type MoveList = ArrayVec<[Move; 512]>;

/// A chess or chess variant position.
pub trait Position : Setup + Default + Clone {
    /// Whether or not promoted pieces are special in the respective chess
    /// variant. For example in Crazyhouse a promoted queen should be marked
    /// as `Q~` in FENs and will become a pawn when captured.
    const TRACK_PROMOTED: bool;

    /// Validate a `Setup` and construct a position.
    fn from_setup<S: Setup>(setup: &S) -> Result<Self, PositionError>;

    /// Bitboard of pieces giving check.
    fn checkers(&self) -> Bitboard {
        self.our(Role::King).first()
            .map_or(Bitboard(0), |king| self.board().by_color(!self.turn()) & self.board().attacks_to(king))
    }

    /// Generates legal moves.
    fn legal_moves(&self, moves: &mut MoveList);

    /// Tests a move for legality.
    fn is_legal(&self, m: &Move) -> bool {
        let mut legals = MoveList::new();
        self.legal_moves(&mut legals);
        legals.contains(m)
    }

    /// Validates and plays a move.
    fn play(self, m: &Move) -> Result<Self, MoveError> {
        if self.is_legal(m) {
            Ok(self.play_unchecked(m))
        } else {
            Err(())
        }
    }

    fn play_unchecked(self, m: &Move) -> Self;

    /// Tries to convert an `Uci` to a legal move.
    fn uci_to_move(&self, uci: &Uci) -> Result<Move, MoveError> {
        let candidate = match *uci {
            Uci::Normal { from, to, promotion } => {
                let role = self.board().role_at(from).ok_or(())?;

                if role == Role::King && self.castling_rights().contains(to) {
                    Move::Castle { king: from, rook: to }
                } else if role == Role::King &&
                          from == self.turn().fold(square::E1, square::E8) &&
                          to.rank() == self.turn().fold(0, 7) &&
                          square::distance(from, to) == 2 {
                    if from.file() < to.file() {
                        Move::Castle { king: from, rook: self.turn().fold(square::H1, square::H8) }
                    } else {
                        Move::Castle { king: from, rook: self.turn().fold(square::A1, square::A8) }
                    }
                } else {
                    Move::Normal { role, from, capture: self.board().role_at(to), to, promotion }
                }
            },
            Uci::Put { role, to } => Move::Put { role, to },
            Uci::Null => return Ok(Move::Null)
        };

        if self.is_legal(&candidate) {
            Ok(candidate)
        } else {
            Err(())
        }
    }
}

#[derive(Clone)]
struct Situation {
    board: Board,
    turn: Color,
    castling_rights: Bitboard,
    ep_square: Option<Square>,
    halfmove_clock: u32,
    fullmoves: u32,
}

impl Default for Situation {
    fn default() -> Self {
        Situation {
            board: Board::default(),
            turn: White,
            castling_rights: Bitboard(0x8100000000000081),
            ep_square: None,
            halfmove_clock: 0,
            fullmoves: 1,
        }
    }
}

impl Situation {
    pub fn do_move(&mut self, m: &Move) {
        let color = self.turn;
        self.ep_square.take();
        self.halfmove_clock += 1;

        match *m {
            Move::Normal { role, from, capture, to, promotion } => {
                if role == Role::Pawn || capture.is_some() {
                    self.halfmove_clock = 0;
                }

                if role == Role::Pawn && square::distance(from, to) == 2 {
                    self.ep_square = from.checked_offset(color.fold(8, -8));
                }

                if role == Role::King {
                    self.castling_rights.discard_all(Bitboard::relative_rank(color, 0));
                } else {
                    self.castling_rights.discard(from);
                    self.castling_rights.discard(to);
                }

                let promoted = self.board.promoted().contains(from) || promotion.is_some();

                self.board.remove_piece_at(from);
                self.board.set_piece_at(to, promotion.map_or(role.of(color), |p| p.of(color)), promoted);
            },
            Move::Castle { king, rook } => {
                let rook_to = square::combine(
                    if square::delta(rook, king) < 0 { square::D1 } else { square::F1 },
                    rook);

                let king_to = square::combine(
                    if square::delta(rook, king) < 0 { square::C1 } else { square::G1 },
                    king);

                self.board.remove_piece_at(king);
                self.board.remove_piece_at(rook);
                self.board.set_piece_at(rook_to, color.rook(), false);
                self.board.set_piece_at(king_to, color.king(), false);

                self.castling_rights.discard_all(Bitboard::relative_rank(color, 0));
            },
            Move::EnPassant { from, to } => {
                self.board.remove_piece_at(square::combine(to, from)); // captured pawn
                self.board.remove_piece_at(from).map(|piece| self.board.set_piece_at(to, piece, false));
                self.halfmove_clock = 0;
            },
            Move::Put { to, role } => {
                self.board.set_piece_at(to, Piece { color, role }, false);
            },
            Move::Null => ()
        }

        if color == Black {
            self.fullmoves += 1;
        }

        self.turn = !color;
    }

    pub fn us(&self) -> Bitboard {
        self.board.by_color(self.turn)
    }

    pub fn our(&self, role: Role) -> Bitboard {
        self.us() & self.board.by_role(role)
    }

    pub fn them(&self) -> Bitboard {
        self.board.by_color(!self.turn)
    }

    pub fn their(&self, role: Role) -> Bitboard {
        self.them() & self.board.by_role(role)
    }
}

#[derive(Default, Clone)]
pub struct Chess {
    situation: Situation
}

impl Setup for Chess {
    fn board(&self) -> &Board { &self.situation.board }
    fn pockets(&self) -> Option<&Pockets> { None }
    fn turn(&self) -> Color { self.situation.turn }
    fn castling_rights(&self) -> Bitboard { self.situation.castling_rights }
    fn ep_square(&self) -> Option<Square> { self.situation.ep_square }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }
    fn halfmove_clock(&self) -> u32 { self.situation.halfmove_clock }
    fn fullmoves(&self) -> u32 { self.situation.fullmoves }
}

impl Chess {
    fn ensure_valid(self) -> Result<Chess, PositionError> {
        let pos = self;

        if pos.board().occupied().is_empty() {
            return Err(PositionError::Empty)
        }

        for color in &[White, Black] {
            if (pos.board().by_piece(color.king()) & !pos.board().promoted()).is_empty() {
                return Err(PositionError::NoKing { color: *color })
            }
            if pos.board().by_color(*color).count() > 16 {
                return Err(PositionError::TooManyPieces { color: *color })
            }
            if pos.board().by_piece(color.pawn()).count() > 8 {
                return Err(PositionError::TooManyPawns { color: *color })
            }
        }

        if pos.board().kings().count() > 2 {
            return Err(PositionError::TooManyKings)
        }

        if !(pos.board().pawns() & (Bitboard::rank(0) | Bitboard::rank(7))).is_empty() {
            return Err(PositionError::PawnsOnBackrank)
        }

        // TODO: Validate castling rights.

        if let Some(ep_square) = pos.ep_square() {
            if !Bitboard::relative_rank(pos.turn(), 5).contains(ep_square) {
                return Err(PositionError::InvalidEpSquare)
            }

            let fifth_rank_sq = ep_square.saturating_offset(pos.turn().fold(-8, 8));
            let seventh_rank_sq  = ep_square.saturating_offset(pos.turn().fold(8, -8));

            // The last move must have been a double pawn push. Check for the
            // presence of that pawn.
            if !pos.situation.their(Role::Pawn).contains(fifth_rank_sq) {
                return Err(PositionError::InvalidEpSquare)
            }

            if pos.board().occupied().contains(ep_square) | pos.board().occupied().contains(seventh_rank_sq) {
                return Err(PositionError::InvalidEpSquare)
            }
        }

        if let Some(their_king) = pos.board().king_of(!pos.turn()) {
            if !(pos.board().attacks_to(their_king) & pos.situation.us()).is_empty() {
                return Err(PositionError::OppositeCheck)
            }
        }

        Ok(pos)
    }
}

impl Position for Chess {
    const TRACK_PROMOTED: bool = false;

    fn play_unchecked(mut self, m: &Move) -> Chess {
        self.situation.do_move(m);
        self
    }

    fn play(self, m: &Move) -> Result<Chess, MoveError> {
        self.play_unchecked(m).ensure_valid().map_err(|_| ())
    }

    fn from_setup<S: Setup>(setup: &S) -> Result<Chess, PositionError> {
        Chess {
            situation: Situation {
                board: setup.board().clone(),
                turn: setup.turn(),
                castling_rights: setup.castling_rights(),
                ep_square: setup.ep_square(),
                halfmove_clock: setup.halfmove_clock(),
                fullmoves: setup.fullmoves(),
            }
        }.ensure_valid()
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        let king = self.our(Role::King).first().expect("has a king");
        let checkers = self.checkers();

        let pos = &self.situation;
        gen_en_passant(pos, moves);

        if checkers.is_empty() {
            gen_non_king(pos, Bitboard::all(), moves);
            KingTag::gen_moves(pos, Bitboard::all(), moves);
            gen_castling_moves(pos, moves);
        } else {
            evasions(pos, king, checkers, moves);
        }

        let blockers = slider_blockers(self.board(), self.them(), king);
        moves.retain(|m| is_safe(self, king, m, blockers));
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

fn evasions(pos: &Situation, king: Square, checkers: Bitboard, moves: &mut MoveList) {
    let sliders = checkers & pos.board.sliders();

    let mut attacked = Bitboard(0);
    for checker in sliders {
        attacked = attacked | attacks::ray(checker, king).without(checker);
    }

    moves.extend((attacks::king_attacks(king) & !pos.us() & !attacked).map(|to| {
        Move::Normal { role: Role::King, from: king, capture: pos.board.role_at(to), to, promotion: None }
    }));

    if let Some(checker) = checkers.single_square() {
        let target = attacks::between(king, checker).with(checker);

        gen_non_king(pos, target, moves);
    }
}

fn gen_castling_moves(pos: &Situation, moves: &mut MoveList) {
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

fn push_pawn_moves(moves: &mut MoveList, from: Square, to: Square, capture: Option<Role>) {
    if to.rank() != 0 && to.rank() < 7 {
        moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: None } );
    } else {
        moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Queen) } );
        moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Rook) } );
        moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Bishop) } );
        moves.push(Move::Normal { role: Role::Pawn, from, capture, to, promotion: Some(Role::Knight) } );
    }
}

trait Stepper {
    const ROLE: Role;

    fn attacks(from: Square) -> Bitboard;

    fn gen_moves(pos: &Situation, target: Bitboard, moves: &mut MoveList) {
        for from in pos.our(Self::ROLE) {
            moves.extend((Self::attacks(from) & !pos.us() & target).map(|to| {
                Move::Normal { role: Self::ROLE, from, capture: pos.board.role_at(to), to, promotion: None }
            }));
        }
    }
}

trait Slider {
    const ROLE: Role;

    fn attacks(from: Square, occupied: Bitboard) -> Bitboard;

    fn gen_moves(pos: &Situation, target: Bitboard, moves: &mut MoveList) {
        for from in pos.our(Self::ROLE) {
            moves.extend((Self::attacks(from, pos.board.occupied()) & !pos.us() & target).map(|to| {
                Move::Normal { role: Self::ROLE, from, capture: pos.board.role_at(to), to, promotion: None }
            }));
        }
    }
}

struct KingTag { }
struct KnightTag { }
struct BishopTag { }
struct RookTag { }
struct QueenTag { }

impl Stepper for KingTag {
    const ROLE: Role = Role::King;
    fn attacks(from: Square) -> Bitboard { attacks::king_attacks(from) }
}

impl Stepper for KnightTag {
    const ROLE: Role = Role::Knight;
    fn attacks(from: Square) -> Bitboard { attacks::knight_attacks(from) }
}

impl Slider for BishopTag {
    const ROLE: Role = Role::Bishop;
    fn attacks(from: Square, occupied: Bitboard) -> Bitboard { attacks::bishop_attacks(from, occupied) }
}

impl Slider for RookTag {
    const ROLE: Role = Role::Rook;
    fn attacks(from: Square, occupied: Bitboard) -> Bitboard { attacks::rook_attacks(from, occupied) }
}

impl Slider for QueenTag {
    const ROLE: Role = Role::Queen;
    fn attacks(from: Square, occupied: Bitboard) -> Bitboard { attacks::queen_attacks(from, occupied) }
}

fn gen_non_king(pos: &Situation, target: Bitboard, moves: &mut MoveList) {
    KnightTag::gen_moves(pos, target, moves);
    QueenTag::gen_moves(pos, target, moves);
    RookTag::gen_moves(pos, target, moves);
    BishopTag::gen_moves(pos, target, moves);
    gen_pawn_moves(pos, target, moves);
}

fn gen_pawn_moves(pos: &Situation, target: Bitboard, moves: &mut MoveList) {
    for from in pos.our(Role::Pawn) {
        for to in attacks::pawn_attacks(pos.turn, from) & pos.them() & target {
            push_pawn_moves(moves, from, to, pos.board.role_at(to));
        }
    }

    let single_moves = pos.our(Role::Pawn).relative_shift(pos.turn, 8) &
                       !pos.board.occupied();

    let double_moves = single_moves.relative_shift(pos.turn, 8) &
                       Bitboard::relative_rank(pos.turn, 3) &
                       !pos.board.occupied();

    for to in single_moves & target {
        let from = to.saturating_offset(pos.turn.fold(-8, 8));
        push_pawn_moves(moves, from, to, None);
    }

    for to in double_moves & target {
        let from = to.saturating_offset(pos.turn.fold(-16, 16));
        moves.push(Move::Normal { role: Role::Pawn, from, capture: None, to, promotion: None });
    }
}

fn gen_en_passant(pos: &Situation, moves: &mut MoveList) {
    if let Some(to) = pos.ep_square {
        for from in pos.our(Role::Pawn) & attacks::pawn_attacks(!pos.turn, to) {
            moves.push(Move::EnPassant { from, to });
        }
    }
}

fn slider_blockers(board: &Board, enemy: Bitboard, king: Square) -> Bitboard {
    let snipers = (attacks::rook_attacks(king, Bitboard(0)) & board.rooks_and_queens()) |
                  (attacks::bishop_attacks(king, Bitboard(0)) & board.bishops_and_queens());

    let mut blockers = Bitboard(0);

    for sniper in snipers & enemy {
        let b = attacks::between(king, sniper) & board.occupied();

        if !b.more_than_one() {
            blockers.add_all(b);
        }
    }

    blockers
}

fn is_safe<P: Position>(pos: &P, king: Square, m: &Move, blockers: Bitboard) -> bool {
    match *m {
        Move::Normal { role, from, to, .. } =>
            if role == Role::King {
                (pos.board().attacks_to(to) & pos.them()).is_empty()
            } else {
                !(pos.us() & blockers).contains(from) ||
                attacks::aligned(from, to, king)
            },
        Move::EnPassant { from, to } => {
            let mut occupied = pos.board().occupied();
            occupied.flip(from);
            occupied.flip(square::combine(to, from)); // captured pawn
            occupied.add(to);

            (attacks::rook_attacks(king, occupied) & pos.them() & pos.board().rooks_and_queens()).is_empty() &&
            (attacks::bishop_attacks(king, occupied) & pos.them() & pos.board().bishops_and_queens()).is_empty()
        },
        Move::Castle { .. } => {
            true
        },
        _ => false
    }
}
