//! Chess variants.
//!
//! These are games played with normal chess pieces but special rules.
//! Every chess variant implements [`FromSetup`] and [`Position`].

use core::{fmt, num::NonZeroU32, str, str::FromStr};

pub use crate::position::{
    variant::{Antichess, Atomic, Crazyhouse, Horde, KingOfTheHill, RacingKings, ThreeCheck},
    Chess,
};
use crate::{
    Bitboard, Board, ByColor, ByRole, Castles, CastlingMode, CastlingSide, Color, EnPassantMode,
    FromSetup, Move, MoveList, Outcome, Position, PositionError, RemainingChecks, Role, Setup,
    Square,
};

/// Discriminant of [`VariantPosition`].
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

#[cfg(feature = "std")]
impl std::error::Error for ParseVariantError {}

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
        let mut setup = self.into_setup(EnPassantMode::Always);
        setup.swap_turn();
        VariantPosition::from_setup(variant, setup, mode)
    }

    pub fn variant(&self) -> Variant {
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

    fn borrow(&self) -> &dyn Position {
        match *self {
            VariantPosition::Chess(ref pos) => pos,
            VariantPosition::Atomic(ref pos) => pos,
            VariantPosition::Antichess(ref pos) => pos,
            VariantPosition::KingOfTheHill(ref pos) => pos,
            VariantPosition::ThreeCheck(ref pos) => pos,
            VariantPosition::Crazyhouse(ref pos) => pos,
            VariantPosition::RacingKings(ref pos) => pos,
            VariantPosition::Horde(ref pos) => pos,
        }
    }

    fn borrow_mut(&mut self) -> &mut dyn Position {
        match *self {
            VariantPosition::Chess(ref mut pos) => pos,
            VariantPosition::Atomic(ref mut pos) => pos,
            VariantPosition::Antichess(ref mut pos) => pos,
            VariantPosition::KingOfTheHill(ref mut pos) => pos,
            VariantPosition::ThreeCheck(ref mut pos) => pos,
            VariantPosition::Crazyhouse(ref mut pos) => pos,
            VariantPosition::RacingKings(ref mut pos) => pos,
            VariantPosition::Horde(ref mut pos) => pos,
        }
    }
}

impl Position for VariantPosition {
    fn board(&self) -> &Board {
        self.borrow().board()
    }

    fn promoted(&self) -> Bitboard {
        self.borrow().promoted()
    }

    fn pockets(&self) -> Option<&ByColor<ByRole<u8>>> {
        self.borrow().pockets()
    }

    fn turn(&self) -> Color {
        self.borrow().turn()
    }

    fn castles(&self) -> &Castles {
        self.borrow().castles()
    }

    fn maybe_ep_square(&self) -> Option<Square> {
        self.borrow().maybe_ep_square()
    }

    fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> {
        self.borrow().remaining_checks()
    }

    fn halfmoves(&self) -> u32 {
        self.borrow().halfmoves()
    }

    fn fullmoves(&self) -> NonZeroU32 {
        self.borrow().fullmoves()
    }

    fn into_setup(self, mode: EnPassantMode) -> Setup {
        match self {
            VariantPosition::Chess(pos) => pos.into_setup(mode),
            VariantPosition::Atomic(pos) => pos.into_setup(mode),
            VariantPosition::Antichess(pos) => pos.into_setup(mode),
            VariantPosition::KingOfTheHill(pos) => pos.into_setup(mode),
            VariantPosition::ThreeCheck(pos) => pos.into_setup(mode),
            VariantPosition::Horde(pos) => pos.into_setup(mode),
            VariantPosition::RacingKings(pos) => pos.into_setup(mode),
            VariantPosition::Crazyhouse(pos) => pos.into_setup(mode),
        }
    }
    fn legal_moves(&self) -> MoveList {
        self.borrow().legal_moves()
    }

    fn san_candidates(&self, role: Role, to: Square) -> MoveList {
        self.borrow().san_candidates(role, to)
    }

    fn castling_moves(&self, side: CastlingSide) -> MoveList {
        self.borrow().castling_moves(side)
    }

    fn en_passant_moves(&self) -> MoveList {
        self.borrow().en_passant_moves()
    }

    fn capture_moves(&self) -> MoveList {
        self.borrow().capture_moves()
    }

    fn promotion_moves(&self) -> MoveList {
        self.borrow().promotion_moves()
    }

    fn is_irreversible(&self, m: &Move) -> bool {
        self.borrow().is_irreversible(m)
    }

    fn king_attackers(&self, square: Square, attacker: Color, occupied: Bitboard) -> Bitboard {
        self.borrow().king_attackers(square, attacker, occupied)
    }

    fn is_variant_end(&self) -> bool {
        self.borrow().is_variant_end()
    }

    fn has_insufficient_material(&self, color: Color) -> bool {
        self.borrow().has_insufficient_material(color)
    }

    fn variant_outcome(&self) -> Option<Outcome> {
        self.borrow().variant_outcome()
    }

    fn play_unchecked(&mut self, m: &Move) {
        self.borrow_mut().play_unchecked(m);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variant_position_play() {
        let pos = VariantPosition::new(Variant::Chess);
        let pos = pos
            .play(&Move::Normal {
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
