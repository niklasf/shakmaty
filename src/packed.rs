use crate::Setup;
use core::array::TryFromSliceError;
use core::mem;
use core::num::NonZeroU32;

use crate::util::try_from_slice_error;

// The format:
//
// 8 bytes: occupancy
// 32 bytes: Up to 64 piece nibbles
// 6 byte: leb128 ply
// 5 byte: leb128 half move clock
// 1 byte: variant info
// + 1 byte: check counts
// + 5 bytes: pockets
//   8 bytes: promoted flags
// ---
// 65 bytes

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackedSetup([u8; PackedSetup::MAX_BYTES]);

impl PackedSetup {
    pub const MAX_BYTES: usize = 8 + 64 / 2 + 6 + 5 + 1 + 5 + 8;

    pub const fn empty() -> PackedSetup {
        PackedSetup([0; PackedSetup::MAX_BYTES])
    }

    pub fn as_bytes(&self) -> &[u8] {
        // Trim trailing zeroes
        let mut bytes = &self.0[..];
        while let [rest @ .., last] = bytes {
            if *last == 0 {
                bytes = rest
            } else {
                break;
            }
        }
        bytes
    }

    pub fn try_from_bytes(bytes: &[u8]) -> Result<PackedSetup, TryFromSliceError> {
        let mut packed = PackedSetup::empty();
        let dst = packed
            .0
            .get_mut(..bytes.len())
            .ok_or_else(try_from_slice_error)?;
        dst.copy_from_slice(bytes);
        Ok(packed)
    }

    pub fn pack_chess(setup: &Setup) -> PackedSetup {
        PackedSetup::pack_internal(setup, setup.halfmoves, setup.fullmoves, 0)
    }

    pub fn pack_chess_normalized(setup: &Setup) -> PackedSetup {
        PackedSetup::pack_internal(setup, 0, NonZeroU32::MIN, 0)
    }

    #[cfg(feature = "variant")]
    pub fn pack_variant(setup: &Setup, variant: Variant) -> PackedSetup {
        PackedSetup::pack_internal(
            setup,
            setup.halfmoves,
            setup.fullmoves,
            variant_to_byte(variant),
        )
    }

    #[cfg(feature = "variant")]
    pub fn pack_variant_normalized(setup: &Setup, variant: Variant) -> PackedSetup {
        PackedSetup::pack_internal(setup, 0, NonZerou32::MIN, variant_to_byte(variant))
    }

    fn pack_internal(
        setup: &Setup,
        halfmoves: u32,
        fullmoves: NonZeroU32,
        variant: u8,
    ) -> PackedSetup {
        todo!()
    }

    pub fn unpack_standard(&self) -> Setup {
        let (setup, _) = self.unpack_internal();
        setup
    }

    #[cfg(feature = "variant")]
    pub fn unpack_variant(&self) -> (Setup, Variant) {
        let (setup, variant) = self.unpack_internal();
        (setup, byte_to_variant(variant))
    }

    fn unpack_internal(&self) -> (Setup, u8) {
        todo!()
    }
}

impl TryFrom<&[u8]> for PackedSetup {
    type Error = TryFromSliceError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        PackedSetup::try_from_bytes(value)
    }
}

#[cfg(feature = "variant")]
fn variant_to_byte(variant: Variant) -> u8 {
    match variant {
        Variant::Chess => 0,
        Variant::KingOfTheHill => 4,
        Variant::ThreeCheck => 5,
        Variant::Antichess => 6,
        Variant::Atomic => 7,
        Variant::Horde => 8,
        Variant::RacingKings => 9,
    }
}

#[cfg(feature = "variant")]
fn byte_to_variant(variant: u8) -> Variant {
    match variant {
        4 => Variant::KingOfTheHill,
        5 => Variant::ThreeCheck,
        6 => Variant::Antichess,
        7 => Variant::Atomic,
        8 => Variant::Horde,
        9 => Variant::RacingKings,
        _ => Variant::Chess,
    }
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

    fn write_nibbles(&mut self, lo: u8, hi: u8) {
        debug_assert!(lo & 0xf == lo);
        debug_assert!(hi & 0xf == hi);
        self.write_u8(lo | (hi << 4))
    }

    fn write_leb128(&mut self, mut n: u64) {
        while n > 127 {
            self.write_u8(n as u8 | 128);
            n = n >> 7;
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
