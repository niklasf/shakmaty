//! Chess variants.
//!
//! These are games played with normal chess pieces but special rules.
//! Every chess variant implements [`FromSetup`] and [`Position`].

use core::{error, fmt, num::NonZeroU32, str, str::FromStr};

pub use crate::position::{
    Chess,
    variant::{Antichess, Atomic, Crazyhouse, Horde, KingOfTheHill, RacingKings, ThreeCheck},
};
use crate::{
    Bitboard, Board, ByColor, ByRole, Castles, CastlingMode, CastlingSide, Color, EnPassantMode,
    FromSetup, Move, MoveList, Outcome, Position, PositionError, RemainingChecks, Role, Setup,
    Square, zobrist::ZobristValue,
};

/// Discriminant of [`VariantPosition`].
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "bincode", derive(bincode::Encode, bincode::Decode))]
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy, Default)]
pub enum Variant {
    /// See [`Chess`].
    #[default]
    Chess,
    /// See [`Atomic`].
    Atomic,
    /// See [`Antichess`].
    Antichess,
    /// See [`KingOfTheHill`].
    KingOfTheHill,
    /// See [`ThreeCheck`].
    ThreeCheck,
    /// See [`Crazyhouse`].
    Crazyhouse,
    /// See [`RacingKings`].
    RacingKings,
    /// See [`Horde`].
    Horde,
}

impl Variant {
    /// Gets the name of the variant, as expected by the `UCI_Variant` option
    /// of chess engines.
    pub const fn uci(self) -> &'static str {
        match self {
            Variant::Chess => "chess",
            Variant::Atomic => "atomic",
            Variant::Antichess => "antichess",
            Variant::KingOfTheHill => "kingofthehill",
            Variant::ThreeCheck => "3check",
            Variant::Crazyhouse => "crazyhouse",
            Variant::RacingKings => "racingkings",
            Variant::Horde => "horde",
        }
    }

    /// Selects a variant based on the name used by the `UCI_Variant` option
    /// of chess engines.
    pub fn from_uci(s: &str) -> Result<Variant, ParseVariantError> {
        Ok(match s {
            "chess" => Variant::Chess,
            "atomic" => Variant::Atomic,
            "antichess" => Variant::Antichess,
            "kingofthehill" => Variant::KingOfTheHill,
            "3check" => Variant::ThreeCheck,
            "crazyhouse" => Variant::Crazyhouse,
            "racingkings" => Variant::RacingKings,
            "horde" => Variant::Horde,
            _ => return Err(ParseVariantError),
        })
    }

    /// Selects a variant based on its name or known alias.
    pub fn from_ascii(s: &[u8]) -> Result<Variant, ParseVariantError> {
        Ok(match s {
            b"chess" | b"standard" | b"chess960" | b"fromPosition" | b"Standard" | b"Chess960"
            | b"From Position" => Variant::Chess,
            b"atomic" | b"Atomic" => Variant::Atomic,
            b"antichess" | b"Antichess" => Variant::Antichess,
            b"kingofthehill" | b"kingOfTheHill" | b"King of the Hill" => Variant::KingOfTheHill,
            b"3check" | b"threeCheck" | b"Three-check" => Variant::ThreeCheck,
            b"crazyhouse" | b"Crazyhouse" => Variant::Crazyhouse,
            b"racingkings" | b"racingKings" | b"Racing Kings" => Variant::RacingKings,
            b"horde" | b"Horde" => Variant::Horde,
            _ => return Err(ParseVariantError),
        })
    }

    pub const fn distinguishes_promoted(self) -> bool {
        matches!(self, Variant::Crazyhouse)
    }

    pub const ALL: [Variant; 8] = [
        Variant::Chess,
        Variant::Atomic,
        Variant::Antichess,
        Variant::KingOfTheHill,
        Variant::ThreeCheck,
        Variant::Crazyhouse,
        Variant::RacingKings,
        Variant::Horde,
    ];
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.uci())
    }
}

/// Error when parsing an unknown variant name.
#[derive(Clone, Debug)]
pub struct ParseVariantError;

impl fmt::Display for ParseVariantError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("unknown variant")
    }
}

impl error::Error for ParseVariantError {}

impl FromStr for Variant {
    type Err = ParseVariantError;

    fn from_str(s: &str) -> Result<Variant, ParseVariantError> {
        Variant::from_ascii(s.as_bytes())
    }
}

#[cfg(feature = "nohash-hasher")]
impl nohash_hasher::IsEnabled for Variant {}

/// Dynamically dispatched chess variant [`Position`].
#[allow(missing_docs)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum VariantPosition {
    Chess(Chess),
    Atomic(Atomic),
    Antichess(Antichess),
    KingOfTheHill(KingOfTheHill),
    ThreeCheck(ThreeCheck),
    Crazyhouse(Crazyhouse),
    RacingKings(RacingKings),
    Horde(Horde),
}

impl Default for VariantPosition {
    fn default() -> VariantPosition {
        VariantPosition::new(Variant::default())
    }
}

impl From<Chess> for VariantPosition {
    fn from(pos: Chess) -> VariantPosition {
        VariantPosition::Chess(pos)
    }
}

impl From<Atomic> for VariantPosition {
    fn from(pos: Atomic) -> VariantPosition {
        VariantPosition::Atomic(pos)
    }
}

impl From<Antichess> for VariantPosition {
    fn from(pos: Antichess) -> VariantPosition {
        VariantPosition::Antichess(pos)
    }
}

impl From<KingOfTheHill> for VariantPosition {
    fn from(pos: KingOfTheHill) -> VariantPosition {
        VariantPosition::KingOfTheHill(pos)
    }
}

impl From<ThreeCheck> for VariantPosition {
    fn from(pos: ThreeCheck) -> VariantPosition {
        VariantPosition::ThreeCheck(pos)
    }
}

impl From<Crazyhouse> for VariantPosition {
    fn from(pos: Crazyhouse) -> VariantPosition {
        VariantPosition::Crazyhouse(pos)
    }
}

impl From<RacingKings> for VariantPosition {
    fn from(pos: RacingKings) -> VariantPosition {
        VariantPosition::RacingKings(pos)
    }
}

impl From<Horde> for VariantPosition {
    fn from(pos: Horde) -> VariantPosition {
        VariantPosition::Horde(pos)
    }
}

impl VariantPosition {
    pub fn new(variant: Variant) -> VariantPosition {
        match variant {
            Variant::Chess => Chess::default().into(),
            Variant::Atomic => Atomic::default().into(),
            Variant::Antichess => Antichess::default().into(),
            Variant::KingOfTheHill => KingOfTheHill::default().into(),
            Variant::ThreeCheck => ThreeCheck::default().into(),
            Variant::Crazyhouse => Crazyhouse::default().into(),
            Variant::RacingKings => RacingKings::default().into(),
            Variant::Horde => Horde::default().into(),
        }
    }

    #[allow(clippy::result_large_err)] // Ok variant is also large
    pub fn from_setup(
        variant: Variant,
        setup: Setup,
        mode: CastlingMode,
    ) -> Result<VariantPosition, PositionError<VariantPosition>> {
        fn wrap<F, P, U>(result: Result<P, PositionError<P>>, f: F) -> Result<U, PositionError<U>>
        where
            F: FnOnce(P) -> U,
        {
            match result {
                Ok(p) => Ok(f(p)),
                Err(PositionError { errors, pos }) => Err(PositionError {
                    errors,
                    pos: f(pos),
                }),
            }
        }

        match variant {
            Variant::Chess => wrap(Chess::from_setup(setup, mode), VariantPosition::Chess),
            Variant::Atomic => wrap(Atomic::from_setup(setup, mode), VariantPosition::Atomic),
            Variant::Antichess => wrap(
                Antichess::from_setup(setup, mode),
                VariantPosition::Antichess,
            ),
            Variant::KingOfTheHill => wrap(
                KingOfTheHill::from_setup(setup, mode),
                VariantPosition::KingOfTheHill,
            ),
            Variant::ThreeCheck => wrap(
                ThreeCheck::from_setup(setup, mode),
                VariantPosition::ThreeCheck,
            ),
            Variant::Crazyhouse => wrap(
                Crazyhouse::from_setup(setup, mode),
                VariantPosition::Crazyhouse,
            ),
            Variant::RacingKings => wrap(
                RacingKings::from_setup(setup, mode),
                VariantPosition::RacingKings,
            ),
            Variant::Horde => wrap(Horde::from_setup(setup, mode), VariantPosition::Horde),
        }
    }

    #[allow(clippy::result_large_err)] // Ok variant is also large
    pub fn swap_turn(self) -> Result<VariantPosition, PositionError<VariantPosition>> {
        let mode = self.castles().mode();
        let variant = self.variant();
        let mut setup = self.to_setup(EnPassantMode::Always);
        setup.swap_turn();
        VariantPosition::from_setup(variant, setup, mode)
    }

    pub const fn variant(&self) -> Variant {
        match self {
            VariantPosition::Chess(_) => Variant::Chess,
            VariantPosition::Atomic(_) => Variant::Atomic,
            VariantPosition::Antichess(_) => Variant::Antichess,
            VariantPosition::KingOfTheHill(_) => Variant::KingOfTheHill,
            VariantPosition::ThreeCheck(_) => Variant::ThreeCheck,
            VariantPosition::Crazyhouse(_) => Variant::Crazyhouse,
            VariantPosition::RacingKings(_) => Variant::RacingKings,
            VariantPosition::Horde(_) => Variant::Horde,
        }
    }

    /// Borrows the position as a dynamically dispatched [`Position`].
    pub fn as_dyn(&self) -> &dyn Position {
        match self {
            VariantPosition::Chess(pos) => pos,
            VariantPosition::Atomic(pos) => pos,
            VariantPosition::Antichess(pos) => pos,
            VariantPosition::KingOfTheHill(pos) => pos,
            VariantPosition::ThreeCheck(pos) => pos,
            VariantPosition::Crazyhouse(pos) => pos,
            VariantPosition::RacingKings(pos) => pos,
            VariantPosition::Horde(pos) => pos,
        }
    }

    /// Mutably borrows the position as a dynamically dispatched
    /// [`Position`].
    pub fn as_dyn_mut(&mut self) -> &mut dyn Position {
        match self {
            VariantPosition::Chess(pos) => pos,
            VariantPosition::Atomic(pos) => pos,
            VariantPosition::Antichess(pos) => pos,
            VariantPosition::KingOfTheHill(pos) => pos,
            VariantPosition::ThreeCheck(pos) => pos,
            VariantPosition::Crazyhouse(pos) => pos,
            VariantPosition::RacingKings(pos) => pos,
            VariantPosition::Horde(pos) => pos,
        }
    }

    /// Calls `f` with the concrete position of the variant.
    ///
    /// Dispatches on the variant once, so that `f` is monomorphized for each
    /// variant and can use the [`Position`] methods without further dynamic
    /// dispatch.
    ///
    /// # Example
    ///
    /// ```
    /// use shakmaty::{
    ///     Position,
    ///     variant::{Variant, VariantPosition, WithPosition},
    /// };
    ///
    /// struct CountUs;
    ///
    /// impl WithPosition for CountUs {
    ///     type Output = usize;
    ///
    ///     fn call<P: Position>(self, pos: &P) -> usize {
    ///         pos.us().count()
    ///     }
    /// }
    ///
    /// let pos = VariantPosition::new(Variant::Horde);
    /// assert_eq!(pos.with_position(CountUs), 36);
    /// ```
    #[inline]
    pub fn with_position<F: WithPosition>(&self, f: F) -> F::Output {
        match self {
            VariantPosition::Chess(pos) => f.call(pos),
            VariantPosition::Atomic(pos) => f.call(pos),
            VariantPosition::Antichess(pos) => f.call(pos),
            VariantPosition::KingOfTheHill(pos) => f.call(pos),
            VariantPosition::ThreeCheck(pos) => f.call(pos),
            VariantPosition::Crazyhouse(pos) => f.call(pos),
            VariantPosition::RacingKings(pos) => f.call(pos),
            VariantPosition::Horde(pos) => f.call(pos),
        }
    }

    /// Calls `f` with the concrete position of the variant, mutably.
    ///
    /// See [`VariantPosition::with_position()`].
    #[inline]
    pub fn with_position_mut<F: WithPositionMut>(&mut self, f: F) -> F::Output {
        match self {
            VariantPosition::Chess(pos) => f.call(pos),
            VariantPosition::Atomic(pos) => f.call(pos),
            VariantPosition::Antichess(pos) => f.call(pos),
            VariantPosition::KingOfTheHill(pos) => f.call(pos),
            VariantPosition::ThreeCheck(pos) => f.call(pos),
            VariantPosition::Crazyhouse(pos) => f.call(pos),
            VariantPosition::RacingKings(pos) => f.call(pos),
            VariantPosition::Horde(pos) => f.call(pos),
        }
    }

    /// Calls `f` with the concrete position of the variant, by value.
    ///
    /// See [`VariantPosition::with_position()`].
    #[inline]
    pub fn into_with_position<F: WithPositionOwned>(self, f: F) -> F::Output {
        match self {
            VariantPosition::Chess(pos) => f.call(pos),
            VariantPosition::Atomic(pos) => f.call(pos),
            VariantPosition::Antichess(pos) => f.call(pos),
            VariantPosition::KingOfTheHill(pos) => f.call(pos),
            VariantPosition::ThreeCheck(pos) => f.call(pos),
            VariantPosition::Crazyhouse(pos) => f.call(pos),
            VariantPosition::RacingKings(pos) => f.call(pos),
            VariantPosition::Horde(pos) => f.call(pos),
        }
    }
}

/// Operation on a position of any variant. See
/// [`VariantPosition::with_position()`].
pub trait WithPosition {
    type Output;

    fn call<P: Position>(self, pos: &P) -> Self::Output;
}

/// Operation on a mutable position of any variant. See
/// [`VariantPosition::with_position_mut()`].
pub trait WithPositionMut {
    type Output;

    fn call<P: Position>(self, pos: &mut P) -> Self::Output;
}

/// Operation consuming a position of any variant. See
/// [`VariantPosition::into_with_position()`].
pub trait WithPositionOwned {
    type Output;

    fn call<P: Position>(self, pos: P) -> Self::Output;
}

impl Position for VariantPosition {
    fn board(&self) -> &Board {
        match self {
            VariantPosition::Chess(pos) => pos.board(),
            VariantPosition::Atomic(pos) => pos.board(),
            VariantPosition::Antichess(pos) => pos.board(),
            VariantPosition::KingOfTheHill(pos) => pos.board(),
            VariantPosition::ThreeCheck(pos) => pos.board(),
            VariantPosition::Crazyhouse(pos) => pos.board(),
            VariantPosition::RacingKings(pos) => pos.board(),
            VariantPosition::Horde(pos) => pos.board(),
        }
    }

    fn promoted(&self) -> Bitboard {
        match self {
            VariantPosition::Chess(pos) => pos.promoted(),
            VariantPosition::Atomic(pos) => pos.promoted(),
            VariantPosition::Antichess(pos) => pos.promoted(),
            VariantPosition::KingOfTheHill(pos) => pos.promoted(),
            VariantPosition::ThreeCheck(pos) => pos.promoted(),
            VariantPosition::Crazyhouse(pos) => pos.promoted(),
            VariantPosition::RacingKings(pos) => pos.promoted(),
            VariantPosition::Horde(pos) => pos.promoted(),
        }
    }

    fn pockets(&self) -> Option<&ByColor<ByRole<u8>>> {
        match self {
            VariantPosition::Chess(pos) => pos.pockets(),
            VariantPosition::Atomic(pos) => pos.pockets(),
            VariantPosition::Antichess(pos) => pos.pockets(),
            VariantPosition::KingOfTheHill(pos) => pos.pockets(),
            VariantPosition::ThreeCheck(pos) => pos.pockets(),
            VariantPosition::Crazyhouse(pos) => pos.pockets(),
            VariantPosition::RacingKings(pos) => pos.pockets(),
            VariantPosition::Horde(pos) => pos.pockets(),
        }
    }

    fn turn(&self) -> Color {
        match self {
            VariantPosition::Chess(pos) => pos.turn(),
            VariantPosition::Atomic(pos) => pos.turn(),
            VariantPosition::Antichess(pos) => pos.turn(),
            VariantPosition::KingOfTheHill(pos) => pos.turn(),
            VariantPosition::ThreeCheck(pos) => pos.turn(),
            VariantPosition::Crazyhouse(pos) => pos.turn(),
            VariantPosition::RacingKings(pos) => pos.turn(),
            VariantPosition::Horde(pos) => pos.turn(),
        }
    }

    fn castles(&self) -> &Castles {
        match self {
            VariantPosition::Chess(pos) => pos.castles(),
            VariantPosition::Atomic(pos) => pos.castles(),
            VariantPosition::Antichess(pos) => pos.castles(),
            VariantPosition::KingOfTheHill(pos) => pos.castles(),
            VariantPosition::ThreeCheck(pos) => pos.castles(),
            VariantPosition::Crazyhouse(pos) => pos.castles(),
            VariantPosition::RacingKings(pos) => pos.castles(),
            VariantPosition::Horde(pos) => pos.castles(),
        }
    }

    fn maybe_ep_square(&self) -> Option<Square> {
        match self {
            VariantPosition::Chess(pos) => pos.maybe_ep_square(),
            VariantPosition::Atomic(pos) => pos.maybe_ep_square(),
            VariantPosition::Antichess(pos) => pos.maybe_ep_square(),
            VariantPosition::KingOfTheHill(pos) => pos.maybe_ep_square(),
            VariantPosition::ThreeCheck(pos) => pos.maybe_ep_square(),
            VariantPosition::Crazyhouse(pos) => pos.maybe_ep_square(),
            VariantPosition::RacingKings(pos) => pos.maybe_ep_square(),
            VariantPosition::Horde(pos) => pos.maybe_ep_square(),
        }
    }

    fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> {
        match self {
            VariantPosition::Chess(pos) => pos.remaining_checks(),
            VariantPosition::Atomic(pos) => pos.remaining_checks(),
            VariantPosition::Antichess(pos) => pos.remaining_checks(),
            VariantPosition::KingOfTheHill(pos) => pos.remaining_checks(),
            VariantPosition::ThreeCheck(pos) => pos.remaining_checks(),
            VariantPosition::Crazyhouse(pos) => pos.remaining_checks(),
            VariantPosition::RacingKings(pos) => pos.remaining_checks(),
            VariantPosition::Horde(pos) => pos.remaining_checks(),
        }
    }

    fn halfmoves(&self) -> u32 {
        match self {
            VariantPosition::Chess(pos) => pos.halfmoves(),
            VariantPosition::Atomic(pos) => pos.halfmoves(),
            VariantPosition::Antichess(pos) => pos.halfmoves(),
            VariantPosition::KingOfTheHill(pos) => pos.halfmoves(),
            VariantPosition::ThreeCheck(pos) => pos.halfmoves(),
            VariantPosition::Crazyhouse(pos) => pos.halfmoves(),
            VariantPosition::RacingKings(pos) => pos.halfmoves(),
            VariantPosition::Horde(pos) => pos.halfmoves(),
        }
    }

    fn fullmoves(&self) -> NonZeroU32 {
        match self {
            VariantPosition::Chess(pos) => pos.fullmoves(),
            VariantPosition::Atomic(pos) => pos.fullmoves(),
            VariantPosition::Antichess(pos) => pos.fullmoves(),
            VariantPosition::KingOfTheHill(pos) => pos.fullmoves(),
            VariantPosition::ThreeCheck(pos) => pos.fullmoves(),
            VariantPosition::Crazyhouse(pos) => pos.fullmoves(),
            VariantPosition::RacingKings(pos) => pos.fullmoves(),
            VariantPosition::Horde(pos) => pos.fullmoves(),
        }
    }

    fn to_setup(&self, mode: EnPassantMode) -> Setup {
        match self {
            VariantPosition::Chess(pos) => pos.to_setup(mode),
            VariantPosition::Atomic(pos) => pos.to_setup(mode),
            VariantPosition::Antichess(pos) => pos.to_setup(mode),
            VariantPosition::KingOfTheHill(pos) => pos.to_setup(mode),
            VariantPosition::ThreeCheck(pos) => pos.to_setup(mode),
            VariantPosition::Crazyhouse(pos) => pos.to_setup(mode),
            VariantPosition::RacingKings(pos) => pos.to_setup(mode),
            VariantPosition::Horde(pos) => pos.to_setup(mode),
        }
    }

    fn legal_moves(&self) -> MoveList {
        match self {
            VariantPosition::Chess(pos) => pos.legal_moves(),
            VariantPosition::Atomic(pos) => pos.legal_moves(),
            VariantPosition::Antichess(pos) => pos.legal_moves(),
            VariantPosition::KingOfTheHill(pos) => pos.legal_moves(),
            VariantPosition::ThreeCheck(pos) => pos.legal_moves(),
            VariantPosition::Crazyhouse(pos) => pos.legal_moves(),
            VariantPosition::RacingKings(pos) => pos.legal_moves(),
            VariantPosition::Horde(pos) => pos.legal_moves(),
        }
    }

    fn san_candidates(&self, role: Role, to: Square) -> MoveList {
        match self {
            VariantPosition::Chess(pos) => pos.san_candidates(role, to),
            VariantPosition::Atomic(pos) => pos.san_candidates(role, to),
            VariantPosition::Antichess(pos) => pos.san_candidates(role, to),
            VariantPosition::KingOfTheHill(pos) => pos.san_candidates(role, to),
            VariantPosition::ThreeCheck(pos) => pos.san_candidates(role, to),
            VariantPosition::Crazyhouse(pos) => pos.san_candidates(role, to),
            VariantPosition::RacingKings(pos) => pos.san_candidates(role, to),
            VariantPosition::Horde(pos) => pos.san_candidates(role, to),
        }
    }

    fn castling_moves(&self, side: CastlingSide) -> MoveList {
        match self {
            VariantPosition::Chess(pos) => pos.castling_moves(side),
            VariantPosition::Atomic(pos) => pos.castling_moves(side),
            VariantPosition::Antichess(pos) => pos.castling_moves(side),
            VariantPosition::KingOfTheHill(pos) => pos.castling_moves(side),
            VariantPosition::ThreeCheck(pos) => pos.castling_moves(side),
            VariantPosition::Crazyhouse(pos) => pos.castling_moves(side),
            VariantPosition::RacingKings(pos) => pos.castling_moves(side),
            VariantPosition::Horde(pos) => pos.castling_moves(side),
        }
    }

    fn en_passant_moves(&self) -> MoveList {
        match self {
            VariantPosition::Chess(pos) => pos.en_passant_moves(),
            VariantPosition::Atomic(pos) => pos.en_passant_moves(),
            VariantPosition::Antichess(pos) => pos.en_passant_moves(),
            VariantPosition::KingOfTheHill(pos) => pos.en_passant_moves(),
            VariantPosition::ThreeCheck(pos) => pos.en_passant_moves(),
            VariantPosition::Crazyhouse(pos) => pos.en_passant_moves(),
            VariantPosition::RacingKings(pos) => pos.en_passant_moves(),
            VariantPosition::Horde(pos) => pos.en_passant_moves(),
        }
    }

    fn capture_moves(&self) -> MoveList {
        match self {
            VariantPosition::Chess(pos) => pos.capture_moves(),
            VariantPosition::Atomic(pos) => pos.capture_moves(),
            VariantPosition::Antichess(pos) => pos.capture_moves(),
            VariantPosition::KingOfTheHill(pos) => pos.capture_moves(),
            VariantPosition::ThreeCheck(pos) => pos.capture_moves(),
            VariantPosition::Crazyhouse(pos) => pos.capture_moves(),
            VariantPosition::RacingKings(pos) => pos.capture_moves(),
            VariantPosition::Horde(pos) => pos.capture_moves(),
        }
    }

    fn promotion_moves(&self) -> MoveList {
        match self {
            VariantPosition::Chess(pos) => pos.promotion_moves(),
            VariantPosition::Atomic(pos) => pos.promotion_moves(),
            VariantPosition::Antichess(pos) => pos.promotion_moves(),
            VariantPosition::KingOfTheHill(pos) => pos.promotion_moves(),
            VariantPosition::ThreeCheck(pos) => pos.promotion_moves(),
            VariantPosition::Crazyhouse(pos) => pos.promotion_moves(),
            VariantPosition::RacingKings(pos) => pos.promotion_moves(),
            VariantPosition::Horde(pos) => pos.promotion_moves(),
        }
    }

    fn is_irreversible(&self, m: Move) -> bool {
        match self {
            VariantPosition::Chess(pos) => pos.is_irreversible(m),
            VariantPosition::Atomic(pos) => pos.is_irreversible(m),
            VariantPosition::Antichess(pos) => pos.is_irreversible(m),
            VariantPosition::KingOfTheHill(pos) => pos.is_irreversible(m),
            VariantPosition::ThreeCheck(pos) => pos.is_irreversible(m),
            VariantPosition::Crazyhouse(pos) => pos.is_irreversible(m),
            VariantPosition::RacingKings(pos) => pos.is_irreversible(m),
            VariantPosition::Horde(pos) => pos.is_irreversible(m),
        }
    }

    fn king_attackers(&self, square: Square, attacker: Color, occupied: Bitboard) -> Bitboard {
        match self {
            VariantPosition::Chess(pos) => pos.king_attackers(square, attacker, occupied),
            VariantPosition::Atomic(pos) => pos.king_attackers(square, attacker, occupied),
            VariantPosition::Antichess(pos) => pos.king_attackers(square, attacker, occupied),
            VariantPosition::KingOfTheHill(pos) => pos.king_attackers(square, attacker, occupied),
            VariantPosition::ThreeCheck(pos) => pos.king_attackers(square, attacker, occupied),
            VariantPosition::Crazyhouse(pos) => pos.king_attackers(square, attacker, occupied),
            VariantPosition::RacingKings(pos) => pos.king_attackers(square, attacker, occupied),
            VariantPosition::Horde(pos) => pos.king_attackers(square, attacker, occupied),
        }
    }

    fn is_variant_end(&self) -> bool {
        match self {
            VariantPosition::Chess(pos) => pos.is_variant_end(),
            VariantPosition::Atomic(pos) => pos.is_variant_end(),
            VariantPosition::Antichess(pos) => pos.is_variant_end(),
            VariantPosition::KingOfTheHill(pos) => pos.is_variant_end(),
            VariantPosition::ThreeCheck(pos) => pos.is_variant_end(),
            VariantPosition::Crazyhouse(pos) => pos.is_variant_end(),
            VariantPosition::RacingKings(pos) => pos.is_variant_end(),
            VariantPosition::Horde(pos) => pos.is_variant_end(),
        }
    }

    fn has_insufficient_material(&self, color: Color) -> bool {
        match self {
            VariantPosition::Chess(pos) => pos.has_insufficient_material(color),
            VariantPosition::Atomic(pos) => pos.has_insufficient_material(color),
            VariantPosition::Antichess(pos) => pos.has_insufficient_material(color),
            VariantPosition::KingOfTheHill(pos) => pos.has_insufficient_material(color),
            VariantPosition::ThreeCheck(pos) => pos.has_insufficient_material(color),
            VariantPosition::Crazyhouse(pos) => pos.has_insufficient_material(color),
            VariantPosition::RacingKings(pos) => pos.has_insufficient_material(color),
            VariantPosition::Horde(pos) => pos.has_insufficient_material(color),
        }
    }

    fn variant_outcome(&self) -> Outcome {
        match self {
            VariantPosition::Chess(pos) => pos.variant_outcome(),
            VariantPosition::Atomic(pos) => pos.variant_outcome(),
            VariantPosition::Antichess(pos) => pos.variant_outcome(),
            VariantPosition::KingOfTheHill(pos) => pos.variant_outcome(),
            VariantPosition::ThreeCheck(pos) => pos.variant_outcome(),
            VariantPosition::Crazyhouse(pos) => pos.variant_outcome(),
            VariantPosition::RacingKings(pos) => pos.variant_outcome(),
            VariantPosition::Horde(pos) => pos.variant_outcome(),
        }
    }

    fn play_unchecked(&mut self, m: Move) {
        match self {
            VariantPosition::Chess(pos) => pos.play_unchecked(m),
            VariantPosition::Atomic(pos) => pos.play_unchecked(m),
            VariantPosition::Antichess(pos) => pos.play_unchecked(m),
            VariantPosition::KingOfTheHill(pos) => pos.play_unchecked(m),
            VariantPosition::ThreeCheck(pos) => pos.play_unchecked(m),
            VariantPosition::Crazyhouse(pos) => pos.play_unchecked(m),
            VariantPosition::RacingKings(pos) => pos.play_unchecked(m),
            VariantPosition::Horde(pos) => pos.play_unchecked(m),
        }
    }

    fn zobrist_hash<V: ZobristValue>(&self, mode: EnPassantMode) -> V {
        match self {
            VariantPosition::Chess(pos) => pos.zobrist_hash(mode),
            VariantPosition::Atomic(pos) => pos.zobrist_hash(mode),
            VariantPosition::Antichess(pos) => pos.zobrist_hash(mode),
            VariantPosition::KingOfTheHill(pos) => pos.zobrist_hash(mode),
            VariantPosition::ThreeCheck(pos) => pos.zobrist_hash(mode),
            VariantPosition::Crazyhouse(pos) => pos.zobrist_hash(mode),
            VariantPosition::RacingKings(pos) => pos.zobrist_hash(mode),
            VariantPosition::Horde(pos) => pos.zobrist_hash(mode),
        }
    }

    fn update_zobrist_hash<V: ZobristValue>(
        &self,
        current: V,
        m: Move,
        mode: EnPassantMode,
    ) -> Option<V> {
        match self {
            VariantPosition::Chess(pos) => pos.update_zobrist_hash(current, m, mode),
            VariantPosition::Atomic(pos) => pos.update_zobrist_hash(current, m, mode),
            VariantPosition::Antichess(pos) => pos.update_zobrist_hash(current, m, mode),
            VariantPosition::KingOfTheHill(pos) => pos.update_zobrist_hash(current, m, mode),
            VariantPosition::ThreeCheck(pos) => pos.update_zobrist_hash(current, m, mode),
            VariantPosition::Crazyhouse(pos) => pos.update_zobrist_hash(current, m, mode),
            VariantPosition::RacingKings(pos) => pos.update_zobrist_hash(current, m, mode),
            VariantPosition::Horde(pos) => pos.update_zobrist_hash(current, m, mode),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variant_position_play() {
        let pos = VariantPosition::new(Variant::Chess);
        let pos = pos
            .play(Move::Normal {
                role: Role::Knight,
                from: Square::G1,
                to: Square::F3,
                capture: None,
                promotion: None,
            })
            .expect("legal move");
        assert_eq!(pos.variant(), Variant::Chess);
    }
}
