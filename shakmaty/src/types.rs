use core::{
    fmt::{self, Display},
    num,
};

use crate::{
    ByColor, Color, Role,
    util::{AppendAscii, out_of_range_error},
};

/// A piece with [`Color`] and [`Role`].
#[allow(missing_docs)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Piece {
    pub color: Color,
    pub role: Role,
}

impl Piece {
    pub const fn char(self) -> char {
        match self.color {
            Color::White => self.role.upper_char(),
            Color::Black => self.role.char(),
        }
    }

    pub const fn from_char(ch: char) -> Option<Piece> {
        let Some(role) = Role::from_char(ch) else {
            return None;
        };
        Some(role.of(Color::from_white(32 & ch as u8 == 0)))
    }
}

#[cfg(feature = "bincode")]
impl bincode::Encode for Piece {
    #[rustfmt::skip]
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        u8::encode(
            &match *self {
                Piece { role: Role::Pawn, color: Color::White } => 0,
                Piece { role: Role::Pawn, color: Color::Black } => 1,
                Piece { role: Role::Knight, color: Color::White } => 2,
                Piece { role: Role::Knight, color: Color::Black } => 3,
                Piece { role: Role::Bishop, color: Color::White } => 4,
                Piece { role: Role::Bishop, color: Color::Black } => 5,
                Piece { role: Role::Rook, color: Color::White } => 6,
                Piece { role: Role::Rook, color: Color::Black } => 7,
                Piece { role: Role::Queen, color: Color::White } => 8,
                Piece { role: Role::Queen, color: Color::Black } => 9,
                Piece { role: Role::King, color: Color::White } => 10,
                Piece { role: Role::King, color: Color::Black } => 11,
            },
            encoder,
        )
    }
}

#[cfg(feature = "bincode")]
impl<Config> bincode::Decode<Config> for Piece {
    #[rustfmt::skip]
    fn decode<D: bincode::de::Decoder>(decoder: &mut D) -> Result<Self, bincode::error::DecodeError> {
        Ok(match u8::decode(decoder)? {
            0 => Piece { role: Role::Pawn, color: Color::White },
            1 => Piece { role: Role::Pawn, color: Color::Black },
            2 => Piece { role: Role::Knight, color: Color::White },
            3 => Piece { role: Role::Knight, color: Color::Black },
            4 => Piece { role: Role::Bishop, color: Color::White },
            5 => Piece { role: Role::Bishop, color: Color::Black },
            6 => Piece { role: Role::Rook, color: Color::White },
            7 => Piece { role: Role::Rook, color: Color::Black },
            8 => Piece { role: Role::Queen, color: Color::White },
            9 => Piece { role: Role::Queen, color: Color::Black },
            10 => Piece { role: Role::King, color: Color::White },
            11 => Piece { role: Role::King, color: Color::Black },
            _ => return Err(bincode::error::DecodeError::Other("invalid Piece")),
        })
    }
}

#[cfg(feature = "bincode")]
bincode::impl_borrow_decode!(Piece);

/// `Standard` or `Chess960`.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum CastlingMode {
    /// Castling notation and validity requirements for standard chess.
    ///
    /// UCI notation describes the king movement, e.g., `e1g1`.
    ///
    /// Castling rights are only valid for kings and rooks on their own
    /// backrank. Kings must be on the e-file. Rooks must be on the a-file or
    /// h-file.
    Standard,
    /// Castling notation for Chess960 and basic validity requirements.
    ///
    /// UCI notation describes a king move to the corresponding rook, e.g.,
    /// `e1h1`.
    ///
    /// Castling rights are only valid for kings and rooks on their own
    /// backrank. Each player can have only one king-side and queen-side
    /// castling right respectively.
    ///
    /// Beyond the castling rights possible in Chess960 starting positions,
    /// the white and black rook files need not be the same. The king can be
    /// on any backrank square, including the a/h-file.
    Chess960,
}

impl CastlingMode {
    pub const fn from_standard(standard: bool) -> CastlingMode {
        if standard {
            CastlingMode::Standard
        } else {
            CastlingMode::Chess960
        }
    }

    pub const fn from_chess960(chess960: bool) -> CastlingMode {
        if chess960 {
            CastlingMode::Chess960
        } else {
            CastlingMode::Standard
        }
    }

    pub const fn is_standard(self) -> bool {
        matches!(self, CastlingMode::Standard)
    }

    pub const fn is_chess960(self) -> bool {
        matches!(self, CastlingMode::Chess960)
    }
}

/// When to include the en passant square.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum EnPassantMode {
    /// Only if there is a fully legal en passant move.
    Legal,
    /// Only when a pawn has been advanced by two steps and there is an
    /// enemy pawn next to it.
    ///
    /// Follows the X-FEN specification.
    PseudoLegal,
    /// Whenever a pawn has been advanced by two steps.
    ///
    /// Follows the FEN specification.
    Always,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_role_order() {
        assert!(Role::Pawn < Role::Knight);
        assert!(Role::Knight < Role::Bishop);
        assert!(Role::Bishop < Role::Rook);
        assert!(Role::Rook < Role::Queen);
        assert!(Role::Queen < Role::King);
    }
}

/// The number of checks the respective side needs to give in order to win
/// (in a game of Three-Check).
///
/// # Examples
///
/// ```
/// use shakmaty::{ByColor, RemainingChecks};
///
/// let mut remaining_checks = ByColor::<RemainingChecks>::default();
/// assert_eq!(remaining_checks.white, RemainingChecks::new(3));
/// assert_eq!(remaining_checks.black, RemainingChecks::new(3));
///
/// for _ in 0..5 {
///     remaining_checks.white = remaining_checks.white.saturating_sub(1);
/// }
///
/// assert!(remaining_checks.white.is_zero());
/// ```
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Hash)]
pub struct RemainingChecks(u32);

impl Default for RemainingChecks {
    fn default() -> RemainingChecks {
        RemainingChecks(3)
    }
}

impl RemainingChecks {
    /// Constructs a new [`RemainingChecks`] value.
    ///
    /// # Panics
    ///
    /// Panics if `n > 3`.
    #[track_caller]
    pub const fn new(n: u32) -> RemainingChecks {
        assert!(n <= 3);
        RemainingChecks(n)
    }

    pub const fn is_zero(self) -> bool {
        self.0 == 0
    }

    #[must_use]
    pub const fn saturating_sub(self, n: u32) -> RemainingChecks {
        RemainingChecks(self.0.saturating_sub(n))
    }
}

#[cfg(feature = "arbitrary")]
impl arbitrary::Arbitrary<'_> for RemainingChecks {
    fn arbitrary(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<RemainingChecks> {
        u.int_in_range::<u8>(0..=3)
            .map(|n| RemainingChecks(u32::from(n)))
    }

    #[inline]
    fn size_hint(_depth: usize) -> (usize, Option<usize>) {
        (1, Some(1))
    }
}

macro_rules! int_from_remaining_checks_impl {
    ($($t:ty)+) => {
        $(impl From<RemainingChecks> for $t {
            #[inline]
            fn from(RemainingChecks(checks): RemainingChecks) -> $t {
                checks as $t
            }
        })+
    }
}

int_from_remaining_checks_impl! { u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize }

macro_rules! try_remaining_checks_from_int_impl {
    ($($t:ty)+) => {
        $(impl core::convert::TryFrom<$t> for RemainingChecks {
            type Error = num::TryFromIntError;

            #[inline]
            fn try_from(value: $t) -> Result<RemainingChecks, Self::Error> {
                let n = u32::try_from(value)?;
                if n <= 3 {
                    Ok(RemainingChecks::new(n))
                } else {
                    Err(out_of_range_error())
                }
            }
        })+
    }
}

try_remaining_checks_from_int_impl! { u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize }

impl ByColor<RemainingChecks> {
    pub(crate) fn append_to<W: AppendAscii>(self, f: &mut W) -> Result<(), W::Error> {
        f.append_u32(self.white.0)?;
        f.append_ascii('+')?;
        f.append_u32(self.black.0)
    }
}

impl Display for ByColor<RemainingChecks> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.append_to(f)
    }
}

#[cfg(feature = "bincode")]
impl bincode::Encode for RemainingChecks {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        u8::from(*self).encode(encoder)
    }
}

#[cfg(feature = "bincode")]
impl<Config> bincode::Decode<Config> for RemainingChecks {
    fn decode<D: bincode::de::Decoder>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        RemainingChecks::try_from(u8::decode(decoder)?)
            .map_err(|_| bincode::error::DecodeError::Other("invalid RemainingChecks"))
    }
}

#[cfg(feature = "bincode")]
bincode::impl_borrow_decode!(RemainingChecks);
