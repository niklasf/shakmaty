use attacks;
use board::Board;
use bitboard;
use bitboard::Bitboard;
use square;
use square::Square;
use types::{Color, White, Role, Piece, Move, Pockets, RemainingChecks};
use setup;
use setup::{Setup, PositionError};

use arrayvec::ArrayVec;

/// Outcome of a game.
pub enum Outcome {
    Decisive { winner: Color },
    Draw
}

pub type MoveError = ();

/// A stack-allocated container to hold legal moves.
pub type MoveList = ArrayVec<[Move; 512]>;

/// A legal chess or chess variant position. See `Chess` and
/// `shakmaty::variants` for concrete implementations.
pub trait Position : Setup + Default + Clone {
    /// Whether or not promoted pieces are special in the respective chess
    /// variant. For example in Crazyhouse a promoted queen should be marked
    /// as `Q~` in FENs and will become a pawn when captured.
    const TRACK_PROMOTED: bool;

    /// Wether or not pawns can be promoted to kings in this variant.
    const KING_PROMOTIONS: bool;

    /// Validates a `Setup` and construct a position.
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

    /// Tests if a move zeros the halfmove clock.
    fn is_zeroing(&self, m: &Move) -> bool {
        match *m {
            Move::Normal { capture: Some(_), .. } => true,
            Move::Normal { role: Role::Pawn, .. } => true,
            Move::EnPassant { .. } => true,
            _ => false
        }
    }

    /// Checks if the game is over due to a special variant end condition.
    ///
    /// Note that for example stalemate is not considered a variant-specific
    /// end condition (`is_variant_end()` will return `false`), but it can have
    /// a special `variant_outcome()` in suicide chess.
    fn is_variant_end(&self) -> bool;

    /// Tests for checkmate.
    fn is_checkmate(&self) -> bool {
        if self.checkers().is_empty() {
            return false;
        }

        let mut legals = MoveList::new();
        self.legal_moves(&mut legals);
        legals.is_empty()
    }

    /// Tests for stalemate.
    fn is_stalemate(&self) -> bool {
        if !self.checkers().is_empty() || self.is_variant_end() {
            false
        } else {
            let mut legals = MoveList::new();
            self.legal_moves(&mut legals);
            legals.is_empty()
        }
    }

    /// Tests for insufficient winning material.
    fn is_insufficient_material(&self) -> bool;

    /// Tests if the game is over due to checkmate, stalemate, insufficient
    /// material or variant end.
    fn is_game_over(&self) -> bool {
        let mut legals = MoveList::new();
        self.legal_moves(&mut legals);
        legals.is_empty() || self.is_insufficient_material()
    }

    /// Tests special variant winning, losing and drawing conditions.
    fn variant_outcome(&self) -> Option<Outcome>;

    /// The outcome of the game, or `None` if the game is not over.
    fn outcome(&self) -> Option<Outcome> {
        self.variant_outcome().or_else(|| {
            if self.is_checkmate() {
                Some(Outcome::Decisive { winner: !self.turn() }) // checkmate
            } else if self.is_stalemate() || self.is_insufficient_material() {
                Some(Outcome::Draw)
            } else {
                None
            }
        })
    }

    /// Validates and plays a move.
    fn play(self, m: &Move) -> Result<Self, MoveError> {
        if self.is_legal(m) {
            Ok(self.play_unchecked(m))
        } else {
            Err(())
        }
    }

    /// Plays a move. It is the callers responsibility to ensure the move is
    /// legal.
    ///
    /// # Panics
    ///
    /// Illegal moves can corrupt the state of the position and may
    /// (or may not) panic or cause panics on future calls.
    fn play_unchecked(self, m: &Move) -> Self;
}

/// A standard Chess position.
#[derive(Clone)]
pub struct Chess {
    board: Board,
    turn: Color,
    castling_rights: Bitboard,
    ep_square: Option<Square>,
    halfmove_clock: u32,
    fullmoves: u32,
}

impl Default for Chess {
    fn default() -> Chess {
        Chess {
            board: Board::default(),
            turn: White,
            castling_rights: bitboard::CORNERS,
            ep_square: None,
            halfmove_clock: 0,
            fullmoves: 1,
        }
    }
}

impl Setup for Chess {
    fn board(&self) -> &Board { &self.board }
    fn pockets(&self) -> Option<&Pockets> { None }
    fn turn(&self) -> Color { self.turn }
    fn castling_rights(&self) -> Bitboard { self.castling_rights }
    fn ep_square(&self) -> Option<Square> { self.ep_square }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }
    fn halfmove_clock(&self) -> u32 { self.halfmove_clock }
    fn fullmoves(&self) -> u32 { self.fullmoves }
}

impl Position for Chess {
    const TRACK_PROMOTED: bool = false;
    const KING_PROMOTIONS: bool = false;

    fn play_unchecked(mut self, m: &Move) -> Chess {
        do_move(&mut self.board, &mut self.turn, &mut self.castling_rights,
                &mut self.ep_square, &mut self.halfmove_clock,
                &mut self.fullmoves, m);
        self
    }

    fn from_setup<S: Setup>(setup: &S) -> Result<Chess, PositionError> {
        let pos = Chess {
            board: setup.board().clone(),
            turn: setup.turn(),
            castling_rights: setup.castling_rights(),
            ep_square: setup.ep_square(),
            halfmove_clock: setup.halfmove_clock(),
            fullmoves: setup.fullmoves(),
        };

        setup::validate(&pos).map_or(Ok(pos), |err| Err(err))
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        gen_standard(self, moves);
    }

    fn is_insufficient_material(&self) -> bool {
        if self.board().pawns().any() || self.board().rooks_and_queens().any() {
            return false;
        }

        if self.board().occupied().count() < 3 {
            return true; // single knight or bishop
        }

        if self.board().knights().any() {
            return false; // more than a single knight
        }

        // all bishops on the same color
        if (self.board().bishops() & bitboard::DARK_SQUARES).is_empty() {
            return true;
        }
        if (self.board().bishops() & bitboard::LIGHT_SQUARES).is_empty() {
            return true;
        }

        false
    }

    fn is_variant_end(&self) -> bool { false }
    fn variant_outcome(&self) -> Option<Outcome> { None }
}

/// A Crazyhouse position.
#[derive(Clone)]
pub struct Crazyhouse {
    board: Board,
    pockets: Pockets,
    turn: Color,
    castling_rights: Bitboard,
    ep_square: Option<Square>,
    halfmove_clock: u32,
    fullmoves: u32,
}

impl Setup for Crazyhouse {
    fn board(&self) -> &Board { &self.board }
    fn pockets(&self) -> Option<&Pockets> { Some(&self.pockets) }
    fn turn(&self) -> Color { self.turn }
    fn castling_rights(&self) -> Bitboard { self.castling_rights }
    fn ep_square(&self) -> Option<Square> { self.ep_square }
    fn remaining_checks(&self) -> Option<&RemainingChecks> { None }
    fn halfmove_clock(&self) -> u32 { self.halfmove_clock }
    fn fullmoves(&self) -> u32 { self.fullmoves }
}

impl Default for Crazyhouse {
    fn default() -> Crazyhouse {
        Crazyhouse {
            board: Board::default(),
            pockets: Pockets::default(),
            turn: White,
            castling_rights: bitboard::CORNERS,
            ep_square: None,
            halfmove_clock: 0,
            fullmoves: 1,
        }
    }
}

impl Crazyhouse {
    fn legal_put_squares(&self) -> Bitboard {
        let checkers = self.checkers();

        if checkers.is_empty() {
            !self.board().occupied()
        } else if let Some(checker) = checkers.single_square() {
            let king = self.our(Role::King).first().expect("has a king");
            return attacks::between(checker, king)
        } else {
            Bitboard::empty()
        }
    }
}

impl Position for Crazyhouse {
    const TRACK_PROMOTED: bool = true;
    const KING_PROMOTIONS: bool = false;

    fn is_zeroing(&self, _: &Move) -> bool {
        false
    }

    fn play_unchecked(mut self, m: &Move) -> Crazyhouse {
        let turn = self.turn();
        let mut fake_halfmove_clock = 0;

        do_move(&mut self.board, &mut self.turn, &mut self.castling_rights,
                &mut self.ep_square, &mut fake_halfmove_clock,
                &mut self.fullmoves, m);

        match *m {
            Move::Normal { capture: Some(role), .. } =>
                self.pockets.add(role.of(turn)),
            Move::EnPassant { .. } =>
                self.pockets.add(turn.pawn()),
            Move::Put { role, .. } =>
                self.pockets.remove(&role.of(turn)),
            _ => ()
        }

        self.halfmove_clock += 1;
        self
    }

    fn from_setup<S: Setup>(setup: &S) -> Result<Self, PositionError> {
        // TODO: Validate
        Ok(Crazyhouse {
            board: setup.board().clone(),
            pockets: setup.pockets().map_or(Pockets::default(), |p| p.clone()),
            turn: setup.turn(),
            castling_rights: setup.castling_rights(),
            ep_square: setup.ep_square(),
            halfmove_clock: setup.halfmove_clock(),
            fullmoves: setup.fullmoves(),
        })
    }

    fn legal_moves(&self, moves: &mut MoveList) {
        gen_standard(self, moves);

        for to in self.legal_put_squares() {
            for role in &[Role::Knight, Role::Bishop, Role::Rook, Role::Queen] {
                if self.pockets.by_piece(&role.of(self.turn())) > 0 {
                    moves.push(Move::Put { role: *role, to });
                }
            }

            if 0 < to.rank() && to.rank() < 7 && self.pockets.by_color(self.turn()).pawns > 0 {
                moves.push(Move::Put { role: Role::Pawn, to });
            }
        }
    }

    fn is_insufficient_material(&self) -> bool {
        false
    }

    fn is_variant_end(&self) -> bool { false }
    fn variant_outcome(&self) -> Option<Outcome> { None }
}

fn do_move(board: &mut Board,
           turn: &mut Color,
           castling_rights: &mut Bitboard,
           ep_square: &mut Option<Square>,
           halfmove_clock: &mut u32,
           fullmoves: &mut u32,
           m: &Move) {
    let color = *turn;
    ep_square.take();
    *halfmove_clock += 1;

    match *m {
        Move::Normal { role, from, capture, to, promotion } => {
            if role == Role::Pawn || capture.is_some() {
                *halfmove_clock = 0;
            }

            if role == Role::Pawn && square::distance(from, to) == 2 {
                *ep_square = from.offset(color.fold(8, -8));
            }

            if role == Role::King {
                castling_rights.discard_all(Bitboard::relative_rank(color, 0));
            } else {
                castling_rights.discard(from);
                castling_rights.discard(to);
            }

            let promoted = board.promoted().contains(from) || promotion.is_some();

            board.remove_piece_at(from);
            board.set_piece_at(to, promotion.map_or(role.of(color), |p| p.of(color)), promoted);
        },
        Move::Castle { king, rook } => {
            let rook_to = square::combine(
                if square::delta(rook, king) < 0 { square::D1 } else { square::F1 },
                rook);

            let king_to = square::combine(
                if square::delta(rook, king) < 0 { square::C1 } else { square::G1 },
                king);

            board.remove_piece_at(king);
            board.remove_piece_at(rook);
            board.set_piece_at(rook_to, color.rook(), false);
            board.set_piece_at(king_to, color.king(), false);

            castling_rights.discard_all(Bitboard::relative_rank(color, 0));
        },
        Move::EnPassant { from, to } => {
            board.remove_piece_at(square::combine(to, from)); // captured pawn
            board.remove_piece_at(from).map(|piece| board.set_piece_at(to, piece, false));
            *halfmove_clock = 0;
        },
        Move::Put { to, role } => {
            board.set_piece_at(to, Piece { color, role }, false);
        },
        Move::Null => ()
    }

    if color.is_black() {
        *fullmoves += 1;
    }

    *turn = !color;
}

fn gen_standard<P: Position>(pos: &P, moves: &mut MoveList) {
    let king = pos.our(Role::King).first().expect("has a king");
    let checkers = pos.checkers();

    gen_en_passant(pos.board(), pos.turn(), pos.ep_square(), moves);

    if checkers.is_empty() {
        gen_non_king(pos, Bitboard::all(), moves);
        KingTag::gen_moves(pos, Bitboard::all(), moves);
        gen_castling_moves(pos, moves);
    } else {
        evasions(pos, king, checkers, moves);
    }

    let blockers = slider_blockers(pos.board(), pos.them(), king);
    moves.retain(|m| is_safe(pos, king, m, blockers));
}

fn gen_non_king<P: Position>(pos: &P, target: Bitboard, moves: &mut MoveList) {
    KnightTag::gen_moves(pos, target, moves);
    QueenTag::gen_moves(pos, target, moves);
    RookTag::gen_moves(pos, target, moves);
    BishopTag::gen_moves(pos, target, moves);
    gen_pawn_moves(pos, target, moves);
}

fn evasions<P: Position>(pos: &P, king: Square, checkers: Bitboard, moves: &mut MoveList) {
    let sliders = checkers & pos.board().sliders();

    let mut attacked = Bitboard(0);
    for checker in sliders {
        attacked = attacked | attacks::ray(checker, king).without(checker);
    }

    moves.extend((attacks::king_attacks(king) & !pos.us() & !attacked).map(|to| {
        Move::Normal { role: Role::King, from: king, capture: pos.board().role_at(to), to, promotion: None }
    }));

    if let Some(checker) = checkers.single_square() {
        let target = attacks::between(king, checker).with(checker);

        gen_non_king(pos, target, moves);
    }
}

fn gen_castling_moves<P: Position>(pos: &P, moves: &mut MoveList) {
    let backrank = Bitboard::relative_rank(pos.turn(), 0);

    for king in pos.our(Role::King) & backrank {
        'next_rook: for rook in pos.castling_rights() & backrank {
            let (king_to, rook_to) = if king < rook {
                (pos.turn().fold(square::G1, square::G8),
                 pos.turn().fold(square::F1, square::F8))
            } else {
                (pos.turn().fold(square::C1, square::C8),
                 pos.turn().fold(square::D1, square::D8))
            };

            let empty_for_king = attacks::between(king, king_to).with(king_to)
                                        .without(rook).without(king);

            let empty_for_rook = attacks::between(rook, rook_to).with(rook_to)
                                        .without(rook).without(king);

            if !(pos.board().occupied() & empty_for_king).is_empty() {
                continue;
            }

            if !(pos.board().occupied() & empty_for_rook).is_empty() {
                continue;
            }

            for sq in attacks::between(king, king_to).with(king).with(king_to) {
                if !(pos.board().attacks_to(sq) & pos.them()).is_empty() {
                    continue 'next_rook;
                }
            }

            if !(attacks::rook_attacks(king_to, pos.board().occupied().without(rook)) &
                 pos.them() & pos.board().rooks_and_queens()).is_empty() {
                continue;
            }

            moves.push(Move::Castle { king, rook });
        }
    }
}

trait Stepper {
    const ROLE: Role;

    fn attacks(from: Square) -> Bitboard;

    fn gen_moves<P: Position>(pos: &P, target: Bitboard, moves: &mut MoveList) {
        for from in pos.our(Self::ROLE) {
            moves.extend((Self::attacks(from) & !pos.us() & target).map(|to| {
                Move::Normal { role: Self::ROLE, from, capture: pos.board().role_at(to), to, promotion: None }
            }));
        }
    }
}

trait Slider {
    const ROLE: Role;

    fn attacks(from: Square, occupied: Bitboard) -> Bitboard;

    fn gen_moves<P: Position>(pos: &P, target: Bitboard, moves: &mut MoveList) {
        for from in pos.our(Self::ROLE) {
            moves.extend((Self::attacks(from, pos.board().occupied()) & !pos.us() & target).map(|to| {
                Move::Normal { role: Self::ROLE, from, capture: pos.board().role_at(to), to, promotion: None }
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

fn gen_pawn_moves<P: Position>(pos: &P, target: Bitboard, moves: &mut MoveList) {
    for from in pos.our(Role::Pawn) {
        for to in attacks::pawn_attacks(pos.turn(), from) & pos.them() & target {
            push_pawn_moves(moves, from, to, pos.board().role_at(to));
        }
    }

    let single_moves = pos.our(Role::Pawn).relative_shift(pos.turn(), 8) &
                       !pos.board().occupied();

    let double_moves = single_moves.relative_shift(pos.turn(), 8) &
                       Bitboard::relative_rank(pos.turn(), 3) &
                       !pos.board().occupied();

    for to in single_moves & target {
        if let Some(from) = to.offset(pos.turn().fold(-8, 8)) {
            push_pawn_moves(moves, from, to, None);
        }
    }

    for to in double_moves & target {
        if let Some(from) = to.offset(pos.turn().fold(-16, 16)) {
            moves.push(Move::Normal { role: Role::Pawn, from, capture: None, to, promotion: None });
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

fn gen_en_passant(board: &Board, turn: Color, ep_square: Option<Square>, moves: &mut MoveList) {
    if let Some(to) = ep_square {
        for from in board.pawns() & board.by_color(turn) & attacks::pawn_attacks(!turn, to) {
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
        Move::Castle { .. } => true,
        _ => false
    }
}
