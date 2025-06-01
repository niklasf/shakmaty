//! Binary encodings that balance compression and encoding/decoding speed.
//!
//! # Packing
//!
//! ```
//! use shakmaty::{Chess, EnPassantMode, packed::PackedSetup, Position};
//!
//! let pos = Chess::default();
//! let setup = pos.to_setup(EnPassantMode::Always);
//! let packed = PackedSetup::pack_standard(&setup)?;
//! let bytes = packed.as_bytes();
//! assert!(bytes.len() <= PackedSetup::MAX_BYTES);
//! # Ok::<_, shakmaty::packed::PackSetupError>(())
//! ```
//!
//! # Unpacking
//!
//! ```
//! # use shakmaty::{Chess, EnPassantMode, packed::PackedSetup, Position};
//! #
//! # fn main() -> Result<(), Box<dyn core::error::Error>> {
//! #     let packed = PackedSetup::pack_standard(&Chess::default().to_setup(EnPassantMode::Always))?;
//! #     let bytes = packed.as_bytes();
//! use shakmaty::{CastlingMode, FromSetup};
//!
//! let packed = PackedSetup::try_from_bytes(bytes)?;
//! let setup = packed.unpack_standard()?;
//! let pos = Chess::from_setup(setup, CastlingMode::Chess960)?;
//! assert_eq!(pos, Chess::default());
//! #     Ok(())
//! # }
//! ```

use core::{array::TryFromSliceError, error, fmt, fmt::Display, mem, num::NonZeroU32};

#[cfg(feature = "variant")]
use crate::variant::Variant;
use crate::{
    util::try_from_slice_error, Bitboard, Board, ByColor, ByRole, Color, Piece, Rank, Role, Setup,
    Square,
};

/// A compactly encoded board, standard chess setup, or variant setup.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackedSetup {
    inner: [u8; PackedSetup::MAX_BYTES],
}

impl PackedSetup {
    /// Maximum number of bytes needed to encode any representable setup.
    pub const MAX_BYTES: usize = 8 /* occupancy */ +
        64 / 2 /* piece nibbles */ +
        5 /* halfmoves */ +
        5 /* ply */ +
        1 /* variant */ +
        5 + 8 /* crazyhouse pockets and promoted pieces */;

    /// Construct a compactly packed equivalent of [`Setup::empty()`].
    pub const fn empty() -> PackedSetup {
        PackedSetup {
            inner: [0; PackedSetup::MAX_BYTES],
        }
    }

    /// Unwrap the packed byte representation.
    ///
    /// Guaranteed not to have trailing zero bytes.
    pub fn as_bytes(&self) -> &[u8] {
        // Trim trailing zeroes
        let mut bytes = &self.inner[..];
        while let [rest @ .., last] = bytes {
            if *last == 0 {
                bytes = rest
            } else {
                break;
            }
        }
        bytes
    }

    /// Wrap a given byte represetation.
    ///
    /// Ignores trailing zero bytes within the maximum length.
    ///
    /// Supports
    /// [Stockfish's nnue-pytorch](https://github.com/official-stockfish/nnue-pytorch)
    /// position format.
    ///
    /// Also supports
    /// [Lichess's binary FEN](https://lichess.org/@/revoof/blog/adapting-nnue-pytorchs-binary-position-format-for-lichess/cpeeAMeY).
    ///
    /// # Errors
    ///
    /// Errors if `bytes` is longer than `PackedSetup::MAX_BYTES`.
    ///
    /// Success does not mean that `bytes` represents a valid `Setup`. This
    /// is validated later, when unpacking.
    pub fn try_from_bytes(bytes: &[u8]) -> Result<PackedSetup, TryFromSliceError> {
        let mut packed = PackedSetup::empty();
        let dst = packed
            .inner
            .get_mut(..bytes.len())
            .ok_or_else(try_from_slice_error)?;
        dst.copy_from_slice(bytes);
        Ok(packed)
    }

    /// Construct a compactly packed equivalent of the given board.
    pub fn pack_board(board: &Board) -> PackedSetup {
        PackedSetup::pack_standard(&Setup {
            board: board.clone(),
            ..Setup::default()
        })
        .expect("all boards representable")
    }

    /// Construct a compactly packed equivalent of the given standard chess
    /// setup.
    ///
    /// # Errors
    ///
    /// Errors when an illegal standard chess setup can not be packed
    /// losslessly.
    ///
    /// * No matching pawn on the correct side of the board for the en passant
    ///   square.
    /// * Not all castling rights have matching unmoved rooks.
    /// * Remaining Three-check checks (but this is standard chess).
    /// * Crazyhouse pockets (but this is standard chess).
    pub fn pack_standard(setup: &Setup) -> Result<PackedSetup, PackSetupError> {
        PackedSetup::pack_internal(setup, setup.halfmoves, setup.fullmoves, 0)
    }

    /// Construct a compactly packed equivalent of the given standard chess
    /// setup, ignoring move counters.
    ///
    /// # Errors
    ///
    /// Same error conditions as [`PackedSetup::pack_standard()`].
    pub fn pack_standard_normalized(setup: &Setup) -> Result<PackedSetup, PackSetupError> {
        PackedSetup::pack_internal(setup, 0, NonZeroU32::MIN, 0)
    }

    /// Construct a compactly packed equivalent of the given variant setup.
    ///
    /// # Errors
    ///
    /// Errors when an illegal variant setup can not be packed losslessly.
    ///
    /// * No matching pawn on the correct side of the board for the en passant
    ///   square.
    /// * Not all castling rights have matching unmoved rooks.
    /// * Remaining checks, but variant is not Three-check.
    /// * Pockets, but variant is not Crazyhouse.
    /// * More than 15 Crazyhouse pocket pieces of any type and color.
    #[cfg(feature = "variant")]
    pub fn pack_variant(setup: &Setup, variant: Variant) -> Result<PackedSetup, PackSetupError> {
        PackedSetup::pack_internal(
            setup,
            setup.halfmoves,
            setup.fullmoves,
            variant_to_byte(variant),
        )
    }

    /// Construct a compactly packed equivalent of the given variant setup,
    /// ignoring move counters.
    ///
    /// # Errors
    ///
    /// Same error conditions as [`PackedSetup::pack_variant()`].
    #[cfg(feature = "variant")]
    pub fn pack_variant_normalized(
        setup: &Setup,
        variant: Variant,
    ) -> Result<PackedSetup, PackSetupError> {
        PackedSetup::pack_internal(setup, 0, NonZeroU32::MIN, variant_to_byte(variant))
    }

    fn pack_internal(
        setup: &Setup,
        halfmoves: u32,
        fullmoves: NonZeroU32,
        variant: u8,
    ) -> Result<PackedSetup, PackSetupError> {
        let mut packed = PackedSetup::empty();
        let mut writer = Writer::new(&mut packed.inner);

        writer.write_u64(setup.board.occupied().into());

        let mut pawn_pushed_to = setup.ep_square.map(|sq| sq.xor(Square::A2));
        let mut unmoved_rooks = setup.castling_rights;
        #[rustfmt::skip]
        let mut pack_piece = |sq: Square, piece: Piece| -> u8 {
            match piece {
                Piece { role: Role::Pawn, color } if color == Color::from_white(sq.rank() <= Rank::Fourth) && pawn_pushed_to.take_if(|pawn| *pawn == sq).is_some() => 12,
                Piece { role: Role::Pawn, color: Color::White } => 0,
                Piece { role: Role::Pawn, color: Color::Black } => 1,
                Piece { role: Role::Knight, color: Color::White } => 2,
                Piece { role: Role::Knight, color: Color::Black } => 3,
                Piece { role: Role::Bishop, color: Color::White } => 4,
                Piece { role: Role::Bishop, color: Color::Black } => 5,
                Piece { role: Role::Rook, color: Color::White } => if unmoved_rooks.remove(sq) { 13 } else { 6 },
                Piece { role: Role::Rook, color: Color::Black } => if unmoved_rooks.remove(sq) { 14 } else { 7 },
                Piece { role: Role::Queen, color: Color::White } => 8,
                Piece { role: Role::Queen, color: Color::Black } => 9,
                Piece { role: Role::King, color: Color::White } => 10,
                Piece { role: Role::King, color: Color::Black } => setup.turn.fold_wb(11, 15),
            }
        };
        let mut pieces = setup.board.iter();
        while let Some((sq, piece)) = pieces.next() {
            writer.write_nibbles_unchecked(
                pack_piece(sq, piece),
                pieces.next().map_or(0, |(sq, piece)| pack_piece(sq, piece)),
            );
        }
        if pawn_pushed_to.is_some() {
            return Err(PackSetupError::EpSquare);
        }
        if unmoved_rooks.any() {
            return Err(PackSetupError::CastlingRights);
        }

        let ply = (u64::from(u32::from(fullmoves)) - 1) * 2 + setup.turn.fold_wb(0, 1);
        let broken_turn = setup.turn.is_black()
            && (setup.board.by_role(Role::King) & setup.board.by_color(Color::Black)).is_empty();

        if halfmoves > 0 || ply > 1 || broken_turn || variant != 0 {
            writer.write_leb128(u64::from(halfmoves));
        }

        if ply > 1 || broken_turn || variant != 0 {
            writer.write_leb128(ply);
        }

        if variant != 0 {
            writer.write_u8(variant);
        }

        if variant == VARIANT_CRAZYHOUSE {
            let pockets = setup.pockets.unwrap_or_default();
            writer
                .write_nibbles(pockets.white.pawn, pockets.black.pawn)
                .map_err(|()| PackSetupError::Pockets)?;
            writer
                .write_nibbles(pockets.white.knight, pockets.black.knight)
                .map_err(|()| PackSetupError::Pockets)?;
            writer
                .write_nibbles(pockets.white.bishop, pockets.black.bishop)
                .map_err(|()| PackSetupError::Pockets)?;
            writer
                .write_nibbles(pockets.white.rook, pockets.black.rook)
                .map_err(|()| PackSetupError::Pockets)?;
            writer
                .write_nibbles(pockets.white.queen, pockets.black.queen)
                .map_err(|()| PackSetupError::Pockets)?;
            if setup.promoted.any() {
                writer.write_u64(setup.promoted.into());
            }
        } else if setup.pockets.is_some() {
            return Err(PackSetupError::Pockets);
        }

        if variant == VARIANT_THREECHECK {
            let remaining_checks = setup.remaining_checks.unwrap_or_default();
            writer.write_nibbles_unchecked(
                remaining_checks.white.into(),
                remaining_checks.black.into(),
            );
        } else if setup.remaining_checks.is_some() {
            return Err(PackSetupError::RemainingChecks);
        }

        Ok(packed)
    }

    /// Unpack an encoded board.
    ///
    /// # Errors
    ///
    /// Invalid encoding.
    pub fn unpack_board(&self) -> Result<Board, UnpackSetupError> {
        Ok(self.unpack_standard()?.board)
    }

    /// Unpack a standard chess setup.
    ///
    /// # Errors
    ///
    /// Invalid encoding or not standard chess.
    pub fn unpack_standard(&self) -> Result<Setup, UnpackSetupError> {
        let (setup, variant) = self.unpack_internal()?;
        if !matches!(variant, 0 | 2 | 3) {
            return Err(UnpackSetupError { _priv: () });
        }
        Ok(setup)
    }

    /// Unpack a variant setup.
    ///
    /// # Errors
    ///
    /// Invalid encoding.
    #[cfg(feature = "variant")]
    pub fn unpack_variant(&self) -> Result<(Setup, crate::variant::Variant), UnpackSetupError> {
        let (setup, variant) = self.unpack_internal()?;
        Ok((setup, variant_from_byte(variant)?))
    }

    fn unpack_internal(&self) -> Result<(Setup, u8), UnpackSetupError> {
        let mut reader = Reader::new(&self.inner);

        let mut setup = Setup::empty();

        #[rustfmt::skip]
        let mut unpack_piece = |sq: Square, packed: u8| -> Result<(), UnpackSetupError> {
            setup.board.set_piece_at(
                sq,
                match packed {
                    0 => Piece { color: Color::White, role: Role::Pawn },
                    1 => Piece { color: Color::Black, role: Role::Pawn },
                    2 => Piece { color: Color::White, role: Role::Knight },
                    3 => Piece { color: Color::Black, role: Role::Knight },
                    4 => Piece { color: Color::White, role: Role::Bishop },
                    5 => Piece { color: Color::Black, role: Role::Bishop },
                    6 => Piece { color: Color::White, role: Role::Rook },
                    7 => Piece { color: Color::Black, role: Role::Rook },
                    8 => Piece { color: Color::White, role: Role::Queen },
                    9 => Piece { color: Color::Black, role: Role::Queen },
                    10 => Piece { color: Color::White, role: Role::King },
                    11 => Piece { color: Color::Black, role: Role::King },
                    12 => {
                        setup.ep_square = Some(sq.xor(Square::A2));
                        Color::from_white(sq.rank() <= Rank::Fourth).pawn()
                    },
                    13 => {
                        setup.castling_rights.add(sq);
                        Piece { color: Color::White, role: Role::Rook }
                    },
                    14 => {
                        setup.castling_rights.add(sq);
                        Piece { color: Color::Black, role: Role::Rook }
                    },
                    15 => {
                        setup.turn = Color::Black;
                        Piece { color: Color::Black, role: Role::King }
                    },
                    _ => return Err(UnpackSetupError { _priv: () }) // Invalid packed piece
                }
            );
            Ok(())
        };

        let mut occupied = Bitboard(reader.read_u64()).into_iter();
        while let Some(sq) = occupied.next() {
            let (lo, hi) = reader.read_nibbles();
            unpack_piece(sq, lo)?;
            if let Some(sq) = occupied.next() {
                unpack_piece(sq, hi)?;
            }
        }

        setup.halfmoves = reader
            .read_leb128(5)
            .try_into()
            .map_err(|_| UnpackSetupError { _priv: () })?;
        let ply = reader.read_leb128(5);
        let variant = reader.read_u8();

        if ply % 2 == 1 {
            setup.turn = Color::Black;
        }

        setup.fullmoves = u32::try_from(1 + ply / 2)
            .ok()
            .and_then(NonZeroU32::new)
            .ok_or(UnpackSetupError { _priv: () })?;

        if variant == VARIANT_CRAZYHOUSE {
            let (wp, bp) = reader.read_nibbles();
            let (wn, bn) = reader.read_nibbles();
            let (wb, bb) = reader.read_nibbles();
            let (wr, br) = reader.read_nibbles();
            let (wq, bq) = reader.read_nibbles();
            setup.pockets = Some(ByColor {
                white: ByRole {
                    pawn: wp,
                    knight: wn,
                    bishop: wb,
                    rook: wr,
                    queen: wq,
                    king: 0,
                },
                black: ByRole {
                    pawn: bp,
                    knight: bn,
                    bishop: bb,
                    rook: br,
                    queen: bq,
                    king: 0,
                },
            });
        }

        if variant == VARIANT_THREECHECK {
            let (lo, hi) = reader.read_nibbles();
            setup.remaining_checks = Some(ByColor {
                white: lo.try_into().map_err(|_| UnpackSetupError { _priv: () })?,
                black: hi.try_into().map_err(|_| UnpackSetupError { _priv: () })?,
            });
        }

        Ok((setup, variant))
    }
}

#[derive(Debug, Clone)]
pub struct UnpackSetupError {
    _priv: (),
}

impl Display for UnpackSetupError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("invalid packed setup encoding")
    }
}

impl error::Error for UnpackSetupError {}

/// Error when packing an unrepresentable setup.
#[derive(Debug, Clone)]
pub enum PackSetupError {
    EpSquare,
    CastlingRights,
    RemainingChecks,
    Pockets,
}

impl Display for PackSetupError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            PackSetupError::EpSquare => "unrepresentable ep square",
            PackSetupError::CastlingRights => "unrepresentable castling rights",
            PackSetupError::RemainingChecks => "unrepresentable remaining checks",
            PackSetupError::Pockets => "unrepresentable pockets",
        })
    }
}

/// Error when unpacking an invalid or unexpected encoding.
impl error::Error for PackSetupError {}

impl TryFrom<&[u8]> for PackedSetup {
    type Error = TryFromSliceError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        PackedSetup::try_from_bytes(value)
    }
}

const VARIANT_CRAZYHOUSE: u8 = 1;
const VARIANT_THREECHECK: u8 = 5;

#[cfg(feature = "variant")]
fn variant_to_byte(variant: Variant) -> u8 {
    use crate::variant::Variant;

    match variant {
        Variant::Chess => 0,
        Variant::Crazyhouse => VARIANT_CRAZYHOUSE,
        Variant::KingOfTheHill => 4,
        Variant::ThreeCheck => VARIANT_THREECHECK,
        Variant::Antichess => 6,
        Variant::Atomic => 7,
        Variant::Horde => 8,
        Variant::RacingKings => 9,
    }
}

#[cfg(feature = "variant")]
fn variant_from_byte(variant: u8) -> Result<Variant, UnpackSetupError> {
    use crate::variant::Variant;

    Ok(match variant {
        0 | 2 | 3 => Variant::Chess,
        VARIANT_CRAZYHOUSE => Variant::Crazyhouse,
        4 => Variant::KingOfTheHill,
        VARIANT_THREECHECK => Variant::ThreeCheck,
        6 => Variant::Antichess,
        7 => Variant::Atomic,
        8 => Variant::Horde,
        9 => Variant::RacingKings,
        _ => return Err(UnpackSetupError { _priv: () }),
    })
}

struct Writer<'a> {
    inner: &'a mut [u8],
}

impl Writer<'_> {
    fn new(dst: &mut [u8]) -> Writer<'_> {
        Writer { inner: dst }
    }

    #[inline]
    fn write_u8(&mut self, n: u8) {
        let (head, tail) = mem::take(&mut self.inner).split_at_mut(1);
        head[0] = n;
        self.inner = tail;
    }

    fn write_u64(&mut self, n: u64) {
        let (head, tail) = mem::take(&mut self.inner).split_at_mut(8);
        head[..].copy_from_slice(&n.to_be_bytes());
        self.inner = tail;
    }

    #[inline]
    fn write_nibbles_unchecked(&mut self, lo: u8, hi: u8) {
        debug_assert!(lo & 0xf == lo);
        debug_assert!(hi & 0xf == hi);
        self.write_u8(lo | (hi << 4))
    }

    fn write_nibbles(&mut self, lo: u8, hi: u8) -> Result<(), ()> {
        if lo & 0xf == lo || hi & 0xf == hi {
            self.write_nibbles_unchecked(lo, hi);
            Ok(())
        } else {
            Err(())
        }
    }

    fn write_leb128(&mut self, mut n: u64) {
        while n > 127 {
            self.write_u8(n as u8 | 128);
            n >>= 7;
        }
        self.write_u8(n as u8);
    }
}

struct Reader<'a> {
    inner: &'a [u8],
}

impl Reader<'_> {
    fn new(src: &[u8]) -> Reader<'_> {
        Reader { inner: src }
    }

    #[inline]
    fn read_u8(&mut self) -> u8 {
        let (head, tail) = self.inner.split_at(1);
        self.inner = tail;
        head[0]
    }

    fn read_u64(&mut self) -> u64 {
        let (head, tail) = self.inner.split_at(8);
        self.inner = tail;
        u64::from_be_bytes(head.try_into().unwrap())
    }

    fn read_nibbles(&mut self) -> (u8, u8) {
        let n = self.read_u8();
        (n & 0xf, n >> 4)
    }

    fn read_leb128(&mut self, max_bytes: usize) -> u64 {
        let mut n = 0;
        let mut shift = 0;
        for _ in 0..max_bytes {
            let byte = self.read_u8();
            n |= u64::from(byte & 127) << shift;
            shift += 7;
            if byte & 128 == 0 {
                break;
            }
        }
        n
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for PackedSetup {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_bytes(self.as_bytes())
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for PackedSetup {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct PackedSetupVisitor;

        impl serde::de::Visitor<'_> for PackedSetupVisitor {
            type Value = PackedSetup;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("packed setup bytes")
            }

            fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                PackedSetup::try_from_bytes(value).map_err(serde::de::Error::custom)
            }
        }

        deserializer.deserialize_bytes(PackedSetupVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_write_u8() {
        let mut buf = [0; 2];

        let mut writer = Writer::new(&mut buf);
        writer.write_u8(1);
        writer.write_u8(2);

        let mut reader = Reader::new(&buf);
        assert_eq!(reader.read_u8(), 1);
        assert_eq!(reader.read_u8(), 2);
    }

    #[test]
    fn test_read_write_u64() {
        let mut buf = [0; 16];

        let mut writer = Writer::new(&mut buf);
        writer.write_u64(0x1234_5678_9abc_def0);
        writer.write_u64(u64::MAX);

        let mut reader = Reader::new(&buf);
        assert_eq!(reader.read_u64(), 0x1234_5678_9abc_def0);
        assert_eq!(reader.read_u64(), u64::MAX);
    }

    #[test]
    fn test_read_write_leb128() {
        let mut buf = [0; 16];

        let mut writer = Writer::new(&mut buf);
        writer.write_leb128(u64::from(u32::MAX));
        writer.write_leb128(u64::from(u32::MAX) * 2 + 1);
        writer.write_leb128(1);
        writer.write_leb128(0);

        let mut reader = Reader::new(&buf);
        assert_eq!(reader.read_leb128(5), u64::from(u32::MAX));
        assert_eq!(reader.read_leb128(5), u64::from(u32::MAX) * 2 + 1);
        assert_eq!(reader.read_leb128(5), 1);
        assert_eq!(reader.read_leb128(5), 0);
    }
}
