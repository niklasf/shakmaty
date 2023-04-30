//! Zobrist hashing for positions.
//!
//! # Stability
//!
//! The Zobrist hash for each position is guaranteed to be stable. Changing
//! hash values is considered a semver breaking change and will be noted in the
//! changelog.
//!
//! # Warning: Forged collisions
//!
//! Zobrist hashes have good collision resistance, but can be
//! forged efficiently.
//!
//! Additionally, in this implementation, impossible positions with
//! more than standard material in Crazyhouse pockets are particularly
//! prone to collisions.
//!
//! # Examples
//!
//! ```
//! use shakmaty::{Chess, EnPassantMode, zobrist::{Zobrist64, ZobristHash}};
//!
//! let pos = Chess::default();
//! assert_eq!(pos.zobrist_hash::<Zobrist64>(EnPassantMode::Legal), Zobrist64(0x463b96181691fc9c));
//! ```

use core::{
    fmt,
    hash::{Hash, Hasher},
    ops::{BitXor, BitXorAssign},
};

use crate::{
    Board, CastlingSide, Color, EnPassantMode, File, Piece, Position, RemainingChecks, Role, Square,
};

/// Integer type that can be returned as a Zobrist hash.
pub trait ZobristValue: BitXorAssign + Default + Copy {
    fn zobrist_for_piece(square: Square, piece: Piece) -> Self;
    fn zobrist_for_white_turn() -> Self;
    fn zobrist_for_castling_right(color: Color, side: CastlingSide) -> Self;
    fn zobrist_for_en_passant_file(file: File) -> Self;
    fn zobrist_for_remaining_checks(color: Color, remaining: RemainingChecks) -> Self;
    fn zobrist_for_promoted(square: Square) -> Self;
    fn zobrist_for_pocket(color: Color, role: Role, pieces: u8) -> Self;
}

macro_rules! zobrist_value_impl {
    ($t:ident, $proxy:ty, $bits:literal) => {
        #[derive(Default, Copy, Clone, Eq)]
        #[doc = "A [`ZobristValue`] with "]
        #[doc = stringify!($bits)]
        #[doc = " bits."]
        pub struct $t(pub $proxy);

        impl PartialEq for $t {
            #[inline]
            fn eq(&self, other: &$t) -> bool {
                self.0 == other.0
            }
        }

        impl From<$proxy> for $t {
            #[inline]
            fn from(value: $proxy) -> $t {
                $t(value)
            }
        }
        impl From<$t> for $proxy {
            #[inline]
            fn from(value: $t) -> $proxy {
                value.0
            }
        }

        impl BitXor for $t {
            type Output = $t;
            #[inline]
            fn bitxor(self, other: $t) -> $t {
                $t(self.0 ^ other.0)
            }
        }
        impl BitXor<&$t> for $t {
            type Output = $t;
            #[inline]
            fn bitxor(self, other: &$t) -> $t {
                $t(self.0 ^ other.0)
            }
        }
        impl BitXor<&$t> for &$t {
            type Output = $t;
            #[inline]
            fn bitxor(self, other: &$t) -> $t {
                $t(self.0 ^ other.0)
            }
        }
        impl BitXor<$t> for &$t {
            type Output = $t;
            #[inline]
            fn bitxor(self, other: $t) -> $t {
                $t(self.0 ^ other.0)
            }
        }
        impl BitXorAssign for $t {
            #[inline]
            fn bitxor_assign(&mut self, rhs: $t) {
                self.0 ^= rhs.0;
            }
        }
        impl BitXorAssign<&$t> for $t {
            #[inline]
            fn bitxor_assign(&mut self, rhs: &$t) {
                self.0 ^= rhs.0;
            }
        }

        impl fmt::Debug for $t {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!(stringify!($t), "({:#x})"), self.0)
            }
        }
        impl fmt::UpperHex for $t {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::UpperHex::fmt(&self.0, f)
            }
        }
        impl fmt::LowerHex for $t {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::LowerHex::fmt(&self.0, f)
            }
        }
        impl fmt::Octal for $t {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Octal::fmt(&self.0, f)
            }
        }
        impl fmt::Binary for $t {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Binary::fmt(&self.0, f)
            }
        }

        impl ZobristValue for $t {
            #[inline]
            fn zobrist_for_piece(square: Square, piece: Piece) -> $t {
                let piece_idx = (usize::from(piece.role) - 1) * 2 + piece.color as usize;
                $t(PIECE_MASKS[64 * piece_idx + usize::from(square)] as $proxy)
            }

            #[inline]
            fn zobrist_for_white_turn() -> $t {
                $t(WHITE_TURN_MASK as $proxy)
            }

            #[inline]
            fn zobrist_for_castling_right(color: Color, side: CastlingSide) -> $t {
                $t(CASTLING_RIGHT_MASKS[match (color, side) {
                    (Color::White, CastlingSide::KingSide) => 0,
                    (Color::White, CastlingSide::QueenSide) => 1,
                    (Color::Black, CastlingSide::KingSide) => 2,
                    (Color::Black, CastlingSide::QueenSide) => 3,
                }] as $proxy)
            }

            #[inline]
            fn zobrist_for_en_passant_file(file: File) -> $t {
                $t(EN_PASSANT_FILE_MASKS[usize::from(file)] as $proxy)
            }

            #[inline]
            fn zobrist_for_remaining_checks(color: Color, remaining: RemainingChecks) -> $t {
                if remaining < RemainingChecks::default() {
                    $t(
                        REMAINING_CHECKS_MASKS[usize::from(remaining) + color.fold_wb(0, 3)]
                            as $proxy,
                    )
                } else {
                    <$t>::default()
                }
            }

            #[inline]
            fn zobrist_for_promoted(square: Square) -> $t {
                $t(PROMOTED_MASKS[usize::from(square)] as $proxy)
            }

            #[inline]
            fn zobrist_for_pocket(color: Color, role: Role, pieces: u8) -> $t {
                if 0 < pieces && pieces <= 16 {
                    let color_idx = color as usize;
                    let role_idx = usize::from(role) - 1;
                    let pieces_idx = usize::from(pieces) - 1;
                    $t(POCKET_MASKS[color_idx * 6 * 16 + role_idx * 16 + pieces_idx] as $proxy)
                } else {
                    <$t>::default()
                }
            }
        }

        #[cfg(feature = "nohash-hasher")]
        impl nohash_hasher::IsEnabled for $t {}
    };
}

zobrist_value_impl! { Zobrist8, u8, 8 }
zobrist_value_impl! { Zobrist16, u16, 16 }
zobrist_value_impl! { Zobrist32, u32, 32 }
zobrist_value_impl! { Zobrist64, u64, 64 }
zobrist_value_impl! { Zobrist128, u128, 128 }

impl Hash for Zobrist128 {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        state.write_u64(self.0 as u64); // Truncating!
    }
}
impl Hash for Zobrist64 {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        state.write_u64(self.0);
    }
}
impl Hash for Zobrist32 {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        state.write_u32(self.0);
    }
}
impl Hash for Zobrist16 {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        state.write_u16(self.0);
    }
}
impl Hash for Zobrist8 {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        state.write_u8(self.0);
    }
}

impl From<Zobrist128> for Zobrist64 {
    fn from(value: Zobrist128) -> Zobrist64 {
        Zobrist64(value.0 as u64)
    }
}
impl From<Zobrist128> for Zobrist32 {
    fn from(value: Zobrist128) -> Zobrist32 {
        Zobrist32(value.0 as u32)
    }
}
impl From<Zobrist128> for Zobrist16 {
    fn from(value: Zobrist128) -> Zobrist16 {
        Zobrist16(value.0 as u16)
    }
}
impl From<Zobrist128> for Zobrist8 {
    fn from(value: Zobrist128) -> Zobrist8 {
        Zobrist8(value.0 as u8)
    }
}

impl From<Zobrist64> for Zobrist32 {
    fn from(value: Zobrist64) -> Zobrist32 {
        Zobrist32(value.0 as u32)
    }
}
impl From<Zobrist64> for Zobrist16 {
    fn from(value: Zobrist64) -> Zobrist16 {
        Zobrist16(value.0 as u16)
    }
}
impl From<Zobrist64> for Zobrist8 {
    fn from(value: Zobrist64) -> Zobrist8 {
        Zobrist8(value.0 as u8)
    }
}

impl From<Zobrist32> for Zobrist16 {
    fn from(value: Zobrist32) -> Zobrist16 {
        Zobrist16(value.0 as u16)
    }
}
impl From<Zobrist32> for Zobrist8 {
    fn from(value: Zobrist32) -> Zobrist8 {
        Zobrist8(value.0 as u8)
    }
}

impl From<Zobrist16> for Zobrist8 {
    fn from(value: Zobrist16) -> Zobrist8 {
        Zobrist8(value.0 as u8)
    }
}

/// Supports Zobrist hashing.
pub trait ZobristHash {
    /// Computes the Zobrist hash of the position from scratch. The hash
    /// includes the position, except halfmove clock and fullmove number.
    fn zobrist_hash<V: ZobristValue>(&self, mode: EnPassantMode) -> V;
}

impl<P: Position> ZobristHash for P {
    fn zobrist_hash<V: ZobristValue>(&self, mode: EnPassantMode) -> V {
        let mut zobrist = hash_board(self.board());

        for sq in self.promoted() {
            zobrist ^= V::zobrist_for_promoted(sq);
        }

        if let Some(pockets) = self.pockets() {
            for (color, pocket) in pockets.as_ref().zip_color() {
                for role in Role::ALL {
                    zobrist ^= V::zobrist_for_pocket(color, role, *pocket.get(role));
                }
            }
        }

        if self.turn() == Color::White {
            zobrist ^= V::zobrist_for_white_turn();
        }

        let castles = self.castles();
        for color in Color::ALL {
            for side in CastlingSide::ALL {
                if castles.has(color, side) {
                    zobrist ^= V::zobrist_for_castling_right(color, side);
                }
            }
        }

        if let Some(sq) = self.ep_square(mode) {
            zobrist ^= V::zobrist_for_en_passant_file(sq.file());
        }

        if let Some(remaining_checks) = self.remaining_checks() {
            for (color, remaining) in remaining_checks.as_ref().zip_color() {
                zobrist ^= V::zobrist_for_remaining_checks(color, *remaining);
            }
        }

        zobrist
    }
}

fn hash_board<V: ZobristValue>(board: &Board) -> V {
    // Order optimized for cache efficiency.
    let mut zobrist = V::default();
    for role in Role::ALL {
        for color in [Color::Black, Color::White] {
            let piece = role.of(color);
            for sq in board.by_piece(piece) {
                zobrist ^= V::zobrist_for_piece(sq, piece);
            }
        }
    }
    zobrist
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{fen::Fen, CastlingMode, Chess};

    #[test]
    fn test_polyglot() {
        let reference_values = [
            (
                "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
                Zobrist64(0x463b96181691fc9c),
            ),
            (
                "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1",
                Zobrist64(0x823c9b50fd114196),
            ),
            (
                "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
                Zobrist64(0x0756b94461c50fb0),
            ),
            (
                "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2",
                Zobrist64(0x662fafb965db29d4),
            ),
            (
                "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3",
                Zobrist64(0x22a48b5a8e47ff78),
            ),
            (
                "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 1 3",
                Zobrist64(0x652a607ca3f242c1),
            ),
            (
                "rnbq1bnr/ppp1pkpp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR w - - 2 4",
                Zobrist64(0x00fdd303c946bdd9),
            ),
            (
                "rnbqkbnr/p1pppppp/8/8/PpP4P/8/1P1PPPP1/RNBQKBNR b KQkq c3 0 3",
                Zobrist64(0x3c8123ea7b067637),
            ),
            (
                "rnbqkbnr/p1pppppp/8/8/P6P/R1p5/1P1PPPP1/1NBQKBNR b Kkq - 1 4",
                Zobrist64(0x5c3f9b829b279560),
            ),
        ];

        for (fen, expected) in reference_values {
            let pos: Chess = fen
                .parse::<Fen>()
                .expect("valid fen")
                .into_position(CastlingMode::Standard)
                .expect("legal position");

            assert_eq!(
                pos.zobrist_hash::<Zobrist64>(EnPassantMode::Legal),
                expected,
                "{}",
                fen
            );
        }
    }

    #[cfg(feature = "variant")]
    #[test]
    fn test_variants_not_distinguished() {
        // Useful when indexing a table of opening names by Zorbist hash.
        let chess: Zobrist128 = Chess::default().zobrist_hash(EnPassantMode::Legal);
        let crazyhouse = crate::variant::Crazyhouse::default().zobrist_hash(EnPassantMode::Legal);
        let three_check = crate::variant::ThreeCheck::default().zobrist_hash(EnPassantMode::Legal);
        let king_of_the_hill =
            crate::variant::KingOfTheHill::default().zobrist_hash(EnPassantMode::Legal);
        assert_eq!(chess, crazyhouse);
        assert_eq!(chess, three_check);
        assert_eq!(chess, king_of_the_hill);
    }

    #[test]
    fn test_full_pockets() {
        // 8/8/8/7k/8/8/3K4/8[ppppppppppppppppnnnnbbbbrrrrqq] w - - 0 54
        for color in Color::ALL {
            assert_ne!(
                Zobrist64::zobrist_for_pocket(color, Role::Pawn, 16),
                Zobrist64::zobrist_for_pocket(color, Role::Pawn, 15)
            );
            assert_ne!(
                Zobrist64::zobrist_for_pocket(color, Role::Queen, 2),
                Zobrist64::zobrist_for_pocket(color, Role::Queen, 1)
            );
        }
    }

    #[cfg(feature = "nohash-hasher")]
    #[test]
    fn test_nohash_hasher() {
        let mut hasher = nohash_hasher::NoHashHasher::<Zobrist128>::default();
        Zobrist128(128).hash(&mut hasher);
        assert_eq!(hasher.finish(), 128);

        let mut hasher = nohash_hasher::NoHashHasher::<Zobrist64>::default();
        Zobrist64(64).hash(&mut hasher);
        assert_eq!(hasher.finish(), 64);

        let mut hasher = nohash_hasher::NoHashHasher::<Zobrist32>::default();
        Zobrist32(32).hash(&mut hasher);
        assert_eq!(hasher.finish(), 32);

        let mut hasher = nohash_hasher::NoHashHasher::<Zobrist16>::default();
        Zobrist16(16).hash(&mut hasher);
        assert_eq!(hasher.finish(), 16);

        let mut hasher = nohash_hasher::NoHashHasher::<Zobrist8>::default();
        Zobrist8(8).hash(&mut hasher);
        assert_eq!(hasher.finish(), 8);
    }
}

const PIECE_MASKS: [u128; 64 * 6 * 2] = [
    0x52b3_75aa_7c0d_7bac_9d39_247e_3377_6d41,
    0x208d_169a_534f_2cf5_2af7_3980_05aa_a5c7,
    0x8981_5137_22b4_7f24_44db_0150_2462_3547,
    0x09b8_f20f_910a_8ff7_9c15_f73e_62a7_6ae2,
    0x0b8e_a702_5520_9cc0_7583_4465_489c_0c89,
    0xa688_a979_1f02_7500_3290_ac3a_2030_01bf,
    0x19b8_8b8f_faed_8f55_0fbb_ad1f_6104_2279,
    0x88bf_7822_d00d_5526_e83a_908f_f2fb_60ca,
    0xdb7b_f62a_b390_b71b_0d7e_765d_5875_5c10,
    0x33a6_ac1d_85c9_f22f_1a08_3822_ceaf_e02d,
    0x55ab_2a27_271d_42ac_9605_d5f0_e25e_c3b0,
    0x40a2_1ff9_c803_fca4_d021_ff5c_d13a_2ed5,
    0x3c16_9aeb_80a1_d5d2_40bd_f15d_4a67_2e32,
    0x8768_4e27_293e_cf96_0113_5514_6fd5_6395,
    0xf8b9_1b39_d4c6_997c_5db4_8320_46f3_d9e5,
    0x1d5f_744f_312f_d467_239f_8b2d_7ff7_19cc,
    0xeda1_8c45_2d5d_e5b4_05d1_a1ae_85b4_9aa1,
    0x7497_db88_8ecc_da0f_679f_848f_6e8f_c971,
    0x94c1_bb70_1674_9887_7449_bbff_801f_ed0b,
    0x35b2_3d66_3606_fde2_7d11_cdb1_c3b7_adf0,
    0x17b4_ae80_b818_4845_82c7_709e_781e_b7cc,
    0x8bd9_8922_a308_9d8e_f321_8f1c_9510_786c,
    0xfec7_7fb0_7cea_5e84_3314_78f3_af51_bbe6,
    0xe153_a54d_23c9_3a8a_4bb3_8de5_e721_9443,
    0xa196_fa76_c244_05eb_aa64_9c6e_bcfd_50fc,
    0x6f33_3e11_d079_240a_8dbd_98a3_52af_d40b,
    0x23b8_d480_df5b_b521_87d2_074b_81d7_9217,
    0x634a_daec_002b_3000_19f3_c751_d3e9_2ae1,
    0x3d0e_41d6_5872_d549_b4ab_30f0_62b1_9abf,
    0x5aba_8390_8462_b892_7b05_00ac_4204_7ac4,
    0x2645_7864_aff2_88af_c945_2ca8_1a09_d85d,
    0x43f1_0561_015d_a64e_24aa_6c51_4da2_7500,
    0x545c_c628_5df4_2807_4c9f_3442_7501_b447,
    0xa714_0dc7_b82e_96ef_14a6_8fd7_3c91_0841,
    0xb1dc_adc8_fe30_a8d4_a71b_9b83_461c_bd93,
    0x72eb_d048_ba37_3ac4_0348_8b95_b0f1_850f,
    0x2eb0_ddf1_351a_1adb_637b_2b34_ff93_c040,
    0x9cdc_8c44_a201_836d_09d1_bc9a_3dd9_0a94,
    0x0afc_1fb4_5a72_8973_3575_6683_34a1_dd3b,
    0x58c8_fa41_5b96_ec95_735e_2b97_a4c4_5a23,
    0x497a_9b9a_7f9f_8872_1872_7070_f1bd_400b,
    0xbff8_4079_9ee0_5fdf_1fcb_acd2_59bf_02e7,
    0xe4ec_1554_316c_2704_d310_a7c2_ce9b_6555,
    0x3c9f_0c8b_89f3_1f3e_bf98_3fe0_fe5d_8244,
    0x4a60_1b99_475b_af4e_9f74_d14f_7454_a824,
    0x6c65_e138_6536_c3a9_51eb_dc4a_b9ba_3035,
    0xb60a_571d_59e8_a485_5c82_c505_db9a_b0fa,
    0xe23c_5d70_4569_6d85_fcf7_fe8a_3430_b241,
    0xc9d4_d61b_569e_c607_3253_a729_b9ba_3dde,
    0xce9e_d71a_6d18_deb2_8c74_c368_081b_3075,
    0x2dbc_1655_9bdb_a870_b9bc_6c87_167c_33e7,
    0x50cd_a533_9d83_6c83_7ef4_8f2b_8302_4e20,
    0x9809_1ef4_f2ab_1ed3_11d5_05d4_c351_bd7f,
    0xf580_3ac1_7fc4_5ecf_6568_fca9_2c76_a243,
    0x0973_0ef1_5a78_c687_4de0_b0f4_0f32_a7b8,
    0xf8bb_209d_715a_b566_96d6_9346_0cc3_7e5d,
    0x0c5b_201d_6cb8_9a50_42e2_40cb_6368_9f2f,
    0x5257_1fbf_abb4_a367_6d2b_dcda_e291_9661,
    0x1b1d_b822_6989_0861_4288_0b02_36e4_d951,
    0x9423_f70e_d512_f1ea_5f0f_4a58_9817_1bb6,
    0x79e4_48c7_2183_e2a5_39f8_90f5_79f9_2f88,
    0x3c88_a0cf_5b85_2900_93c5_b5f4_7356_388b,
    0x9e12_f819_acaa_6653_63dc_359d_8d23_1b78,
    0xc6f0_9266_299a_5902_ec16_ca8a_ea98_ad76,
    0x3e8c_ad22_10fc_e3f3_5355_f900_c2a8_2dc7,
    0xa386_8eff_5334_6da1_07fb_9f85_5a99_7142,
    0x61de_5496_186b_0d70_5093_417a_a8a7_ed5e,
    0xe17a_09f0_e53b_c940_7bcb_c38d_a25a_7f3c,
    0xe0ff_e83a_fe44_ec11_19fc_8a76_8cf4_b6d4,
    0xf35a_5e31_84c1_c980_637a_7780_decf_c0d9,
    0x8339_0d9b_2e75_63a6_8249_a47a_ee0e_41f7,
    0x950f_1473_7ed6_be5b_79ad_6955_01e7_d1e8,
    0x6df4_2fcf_a743_809d_14ac_baf4_777d_5776,
    0x0f2b_1872_ba3f_ef30_f145_b6be_ccde_a195,
    0x0417_1b94_f58c_5d2e_dabf_2ac8_2017_52fc,
    0x78b0_5fea_0dc7_7c38_24c3_c94d_f9c8_d3f6,
    0xa76d_df41_aa67_5504_bb6e_2924_f039_12ea,
    0xe634_bc8f_87d0_fe75_0ce2_6c0b_95c9_80d9,
    0x2dbf_77a8_8512_37de_a49c_d132_bfbf_7cc4,
    0x10f9_b7d9_9683_6741_e99d_662a_f424_3939,
    0x26f6_547b_b047_1fb0_27e6_ad78_9116_5c3f,
    0xf727_c06d_b8bb_34e0_8535_f040_b974_4ff1,
    0x2898_4171_f866_b615_54b3_f4fa_5f40_d873,
    0x349d_2450_78c3_12ef_72b1_2c32_127f_ed2b,
    0x99f8_f1ab_94a1_3206_ee95_4d3c_7b41_1f47,
    0x967d_7e5e_9956_6e67_9a85_ac90_9a24_eaa1,
    0x470f_da10_3f94_76cc_70ac_4cd9_f04f_21f5,
    0x37da_d4fc_dedc_6db8_f9b8_9d3e_99a0_75c2,
    0x99f9_1b1c_d65c_50f0_87b3_e2b2_b5c9_07b1,
    0x6d89_c29c_b703_4aef_a366_e5b8_c54f_48b8,
    0x824c_9daa_114a_11c7_ae4a_9346_cc3f_7cf2,
    0xf3b1_ee14_5059_39c6_1920_c04d_4726_7bbd,
    0x2fa0_d39c_bee0_5ced_87bf_02c6_b49e_2ae9,
    0xc0c4_3ea8_a642_a49c_0922_37ac_237f_3859,
    0x4824_a871_bca3_4e17_ff07_f64e_f8ed_14d0,
    0x394b_500f_07f9_6989_8de8_dca9_f03c_c54e,
    0x7c6e_fb4d_c9be_a9d9_9c16_3326_4db4_9c89,
    0xca09_213b_feb3_6c6a_b3f2_2c3d_0b0b_38ed,
    0x0069_832f_9f2b_d0b5_390e_5fb4_4d01_144b,
    0xf092_a01d_0d44_20da_5bfe_a5b4_7127_68e9,
    0x6952_e201_5db3_9c5a_1e10_3291_1fa7_8984,
    0xf399_3e2a_4cdf_615e_9a74_acb9_64e7_8cb3,
    0x7d3d_dc6b_ef2e_d6f2_4f80_f7a0_35da_fb04,
    0xa704_0db3_8e23_3bac_6304_d09a_0b37_38c4,
    0xa8b5_9fe4_836f_fc08_2171_e646_8302_3a08,
    0xc55d_bb54_3604_14a9_5b9b_63eb_9cef_f80c,
    0x3e24_4653_59dc_03c0_506a_acf4_8988_9342,
    0xd27a_416e_d84c_c3b7_1881_afc9_a3a7_01d6,
    0x7a77_677e_0de6_20c4_6503_0804_4075_0644,
    0x30ca_cd32_e031_3d3b_dfd3_9533_9cdb_f4a7,
    0xdc69_52fd_3e61_c11a_ef92_7dbc_f00c_20f2,
    0x08ab_1642_b512_9e01_7b32_f7d1_e036_80ec,
    0xaa8e_0962_b8ee_bcdc_b9fd_7620_e731_6243,
    0x6dda_36ba_cc3b_2e2c_05a7_e8a5_7db9_1b77,
    0xba82_f4e2_cb60_b43e_b588_9c6e_1563_0a75,
    0x509d_a4ba_5295_c4a5_4a75_0a09_ce95_73f7,
    0x36a1_8fa3_8c3d_74c6_cf46_4cec_899a_2f8a,
    0xb9e5_6524_81a6_df69_f538_639c_e705_b824,
    0x5f49_46b7_dd41_d1c7_3c79_a0ff_5580_ef7f,
    0xfd7a_4fb7_bfe1_d23d_ede6_c87f_8477_609d,
    0x32e0_b2b6_8ea8_3031_799e_81f0_5bc9_3f31,
    0xf25f_cb24_f0c1_9623_8653_6b8c_f342_8a8c,
    0xa317_676d_c1eb_8797_97d7_374c_6008_7b73,
    0xf754_b557_c09a_e146_a246_637c_ff32_8532,
    0x5bbd_920f_ffe5_fa7a_043f_cae6_0cc0_eba0,
    0x5b3c_978e_296d_2280_920e_4495_35dd_359e,
    0xca9c_1ea3_4fc1_484f_70eb_093b_15b2_90cc,
    0x470c_408e_3b3d_2dc5_73a1_9219_1659_1cbd,
    0x6a07_7209_3f97_e152_5643_6c9f_e1a1_aa8d,
    0xfa76_d367_19e7_e5e3_efac_4b70_633b_8f81,
    0x2e79_9233_a544_062a_bb21_5798_d45d_f7af,
    0xe003_4511_44a0_3be8_45f2_0042_f24f_1768,
    0x974d_8f4e_e692_ed35_930f_80f4_e8eb_7462,
    0xd30a_fadf_c4dc_52f5_ff67_12ff_cfd7_5ea1,
    0x278d_de02_bf30_c1da_ae62_3fd6_7468_aa70,
    0x8b7e_3a2b_f5a0_61a3_dd2c_5bc8_4bc8_d8fc,
    0xd144_3752_e511_a579_7eed_120d_54cf_2dd9,
    0xd1b0_2672_a0ec_44cf_22fe_5454_0116_5f1c,
    0xba30_05c8_5125_14f1_c918_00e9_8fb9_9929,
    0x2e3b_8621_1f6b_4295_808b_d68e_6ac1_0365,
    0x0574_31bf_eaa9_d6f5_dec4_6814_5b76_05f6,
    0xa348_ddd6_6378_afaf_1bed_e3a3_aef5_3302,
    0x4a18_1777_5b08_6ce1_4353_9603_d6c5_5602,
    0x184b_1c98_3a6a_1a77_aa96_9b5c_691c_cb7a,
    0xcb70_392e_7b7d_b185_a878_32d3_92ef_ee56,
    0x3b1b_1166_a648_330e_6594_2c7b_3c7e_11ae,
    0xe520_1b15_5f51_cc30_ded2_d633_cad0_04f6,
    0x4d05_3ee8_65f2_1b96_21f0_8570_f420_e565,
    0xd8d8_062e_343d_9c66_b415_938d_7da9_4e3c,
    0x1350_7b31_a966_da7d_91b8_59e5_9ecb_6350,
    0xd637_536d_2e7a_58b0_10cf_f333_e0ed_804a,
    0xcbca_e035_eab8_24a0_28ae_d140_be0b_b7dd,
    0x8c77_fe1e_1b06_691b_c5cc_1d89_724f_a456,
    0xfd78_41ed_1ab4_b961_5648_f680_f11a_2741,
    0xcaa8_8da0_1766_9d53_2d25_5069_f0b7_dab3,
    0xabda_5baf_650b_3675_9bc5_a38e_f729_abd4,
    0xfe14_d0c6_b8f1_1e97_ef2f_0543_08f6_a2bc,
    0xc134_23f8_1c4b_9adf_af20_42f5_cc5c_2858,
    0x3450_7a35_0393_5243_4804_12ba_b7f5_be2a,
    0xec50_4bd0_c7ae_79a1_aef3_af4a_563d_fe43,
    0x0bc7_61ea_4004_d2ae_19af_e59a_e451_497f,
    0x3a07_4807_8a78_fd4d_5259_3803_dff1_e840,
    0xc5f3_6bde_8caa_93fe_f4f0_76e6_5f2c_e6f0,
    0x5b22_99dc_4408_0278_1137_9625_747d_5af3,
    0x3f99_cbeb_6ec6_53fa_bce5_d224_8682_c115,
    0x48eb_cfc0_04b5_24ca_9da4_243d_e836_994f,
    0xd227_8829_cd34_4d05_066f_70b3_3fe0_9017,
    0x5fe6_37e5_8fc1_c0f3_4dc4_de18_9b67_1a1c,
    0x0a4b_136f_25a6_5a32_5103_9ab7_7124_57c3,
    0x4119_314b_520d_04d9_c07a_3f80_c31f_b4b4,
    0x5354_a8b0_8947_cc8e_b46e_e9c5_e64a_6e7c,
    0x6001_d6a9_4517_300b_b381_9a42_abe6_1c87,
    0x1459_7a07_4f13_3855_21a0_0793_3a52_2a20,
    0xdc9a_6baf_92ff_de03_2df1_6f76_1598_aa4f,
    0xc5cb_c527_0de9_86b0_763c_4a13_71b3_68fd,
    0x95c7_2d49_bd75_60be_f793_c467_02e0_86a0,
    0x12b4_37e4_c286_737a_d728_8e01_2aeb_8d31,
    0xaa7c_6f89_f144_2c5d_de33_6a2a_4bc1_c44b,
    0x1a3e_bbf3_17bf_c4d8_0bf6_92b3_8d07_9f23,
    0x4ad3_c9fa_863a_5aa3_2c60_4a7a_1773_26b3,
    0xc7c9_4147_de66_3b5b_4850_e73e_03eb_6064,
    0x840e_7fe4_b35d_4a4b_cfc4_47f1_e53c_8e1b,
    0x8921_1091_26e2_3341_b05c_a3f5_6426_8d99,
    0xa18a_4f12_c127_de17_9ae1_82c8_bc94_74e8,
    0x4719_73e4_dc6e_fb4b_a4fc_4bd4_fc55_58ca,
    0xc723_867b_98d0_7330_e755_178d_58fc_4e76,
    0xf5fc_c6de_3509_50d1_69b9_7db1_a4c0_3dfe,
    0xb909_13e0_2bde_576a_f9b5_b7c4_acc6_7c96,
    0x5554_c92f_272b_73c5_fc6a_82d6_4b86_55fb,
    0x3738_a3f0_fdf5_d9c6_9c68_4cb6_c4d2_4417,
    0x3771_f25e_5e27_8ee3_8ec9_7d29_1745_6ed0,
    0xa9d5_8a81_2b10_906e_6703_df9d_2924_e97e,
    0x2814_f2a1_9d86_70eb_c547_f57e_42a7_444e,
    0x5ced_6d61_7e9d_6b4d_78e3_7644_e7ca_d29e,
    0x69be_27bd_c682_e06a_fe9a_44e9_362f_05fa,
    0x945e_3cd5_4c7a_41f4_08bd_35cc_3833_6615,
    0xfac8_25c2_9c4e_52fc_9315_e5eb_3a12_9ace,
    0x95c3_8063_3671_f3c0_9406_1b87_1e04_df75,
    0xb1f0_f11b_309f_849f_df1d_9f9d_784b_a010,
    0x36b7_ac17_862b_c4ac_3bba_57b6_8871_b59d,
    0x8b89_835b_5e73_1ac5_d2b7_adee_ded1_f73f,
    0x1221_38b6_76fc_6561_f7a2_55d8_3bc3_73f8,
    0xce31_07b8_58b3_68ea_d7f4_f244_8c0c_eb81,
    0xaa14_dd37_33de_4203_d95b_e88c_d210_ffa7,
    0xec4e_a680_5a8d_ad1e_336f_52f8_ff47_28e7,
    0xce5c_d593_8049_dcf0_a740_49da_c312_ac71,
    0x2115_6227_c06a_4b0b_a2f6_1bb6_e437_fdb5,
    0xda13_d541_802c_4d5e_4f2a_5cb0_7f6a_35b3,
    0xfbf0_3d5c_9f78_3bb2_87d3_80bd_a5bf_7859,
    0xe512_fa9f_d5c6_8b8a_16b9_f7e0_6c45_3a21,
    0x30bd_781b_2227_7dcd_7ba2_484c_8a0f_d54e,
    0x5662_5e22_aef3_16ec_f3a6_78ca_d9a2_e38c,
    0xed75_251a_7102_4db6_39b0_bf7d_de43_7ba2,
    0xb468_dc45_b39c_de2f_fcaf_55c1_bf8a_4424,
    0x68b3_3836_bea9_a0a0_18fc_f680_573f_a594,
    0x3187_565f_03cc_0d85_4c05_63b8_9f49_5ac3,
    0x9bbc_591d_dc43_447f_40e0_8793_1a00_930d,
    0xc53a_2945_8191_d2db_8cff_a941_2eb6_42c1,
    0x6bc2_6380_3d69_1ec8_68ca_3905_3261_169f,
    0x04cc_a686_2885_8bac_7a1e_e967_d275_79e2,
    0xa20a_13cf_fa46_79d1_9d1d_60e5_076f_5b6f,
    0x8572_5a1e_096e_1abf_3810_e399_b6f6_5ba2,
    0xbc98_6393_043f_78d5_3209_5b6d_4ab5_f9b1,
    0x0fa4_7125_507c_cb12_35ca_b621_09dd_038a,
    0xc2c2_7b60_c8b2_ce36_a90b_2449_9fcf_afb1,
    0x2175_20a8_09c9_7da6_77a2_25a0_7cc2_c6bd,
    0x552a_d48c_9661_7c16_513e_5e63_4c70_e331,
    0x758c_0637_4011_44ae_4361_c0ca_3f69_2f12,
    0xf1ae_50d5_91ae_b10f_d941_aca4_4b20_a45b,
    0x0c12_7280_b892_40a3_528f_7c86_02c5_807b,
    0xa9a8_cd5d_dd77_37b0_52ab_92be_b961_3989,
    0x8506_683f_3e28_c050_9d1d_fa2e_fc55_7f73,
    0x8105_b057_3483_941f_722f_f175_f572_c348,
    0xd00b_cf69_74e8_788c_1d12_60a5_1107_fe97,
    0x3311_a2a4_e61f_c638_7a24_9a57_ec0c_9ba2,
    0x5b31_cba0_35ff_4f50_0420_8fe9_e8f7_f2d6,
    0x9ef0_4914_1a01_e743_5a11_0c60_58b9_20a0,
    0x3355_b7a6_3e03_cf20_0cd9_a497_658a_5698,
    0x78bf_716c_2f94_ffcf_56fd_23c8_f971_5a4c,
    0x2323_04d6_a359_676e_284c_847b_9d88_7aae,
    0xffee_bdd0_4f15_816e_04fe_abfb_bdb6_19cb,
    0x594f_dc90_c434_a4fd_742e_1e65_1c60_ba83,
    0xba5c_c088_b72c_0942_9a96_32e6_5904_ad3c,
    0x036e_fdc3_0e38_9de2_881b_82a1_3b51_b9e2,
    0x5038_b23c_9af1_74d2_506e_6744_cd97_4924,
    0x9b5e_64f3_0447_4d48_b018_3db5_6ffc_6a79,
    0x280a_4b8c_73c2_e8d8_0ed9_b915_c66e_d37e,
    0xfda0_76be_88bc_c507_5e11_e86d_5873_d484,
    0xafc8_96ae_852c_60c2_f678_647e_3519_ac6e,
    0xbe90_3340_939e_63fd_1b85_d488_d0f2_0cc5,
    0x7a97_bd60_aba4_c349_dab9_fe65_25d8_9021,
    0xf62e_51f1_7859_7cf9_0d15_1d86_adb7_3615,
    0x8f9a_b427_11b6_63dc_a865_a54e_dcc0_f019,
    0xc8d0_03d1_19dc_ac63_93c4_2566_aef9_8ffb,
    0xef21_01c3_2ada_ed38_99e7_afea_be00_0731,
    0xdde8_1906_502a_d1b0_48cb_ff08_6ddf_285a,
    0x1497_56bc_2136_8632_7f9b_6af1_ebf7_8baf,
    0x25c8_0f32_3a51_6eaa_5862_7e1a_149b_ba21,
    0x3ea0_39f7_ff28_ae8e_2cd1_6e2a_bd79_1e33,
    0x0caf_481f_4006_3dd8_d363_eff5_f097_7996,
    0xbce2_3e10_6b1e_efd7_0ce2_a38c_344a_6eed,
    0x1085_3ea8_2a5c_cb34_1a80_4aad_b9cf_a741,
    0xe7c7_6ac3_dbbf_8c8c_907f_3042_1d78_c5de,
    0x1624_c0ce_1532_313d_501f_65ed_b303_4d07,
    0x5f38_95b2_5d7b_4744_3762_4ae5_a48f_a6e9,
    0xfbe3_63cb_b55a_913e_957b_af61_700c_ff4e,
    0x3585_0e8f_6340_0ddd_3a6c_2793_4e31_188a,
    0x3d30_0047_b5dd_de66_d495_0353_6abc_a345,
    0x1c1c_7ca8_b338_6353_088e_0495_89c4_32e0,
    0x986e_c52a_c2c8_8cec_f943_aee7_febf_21b8,
    0xc93b_616a_554d_23c8_6c3b_8e3e_3361_39d3,
    0x211d_7b57_59da_7504_364f_6ffa_464e_e52e,
    0xf266_3fc5_9b54_1585_d60f_6dce_dc31_4222,
    0xf57f_efea_db21_b029_5696_3b0d_ca41_8fc0,
    0x30fd_60d9_ee26_0966_16f5_0edf_91e5_13af,
    0x3c29_da00_0d5b_9a08_ef19_5591_4b60_9f93,
    0xd0d6_203f_a69d_a0ba_5656_01c0_364e_3228,
    0x8167_e4bd_87c6_f05e_ecb5_3939_887e_8175,
    0x5c06_3405_c62f_8154_bac7_a9a1_8531_294b,
    0xb86f_e57d_5308_1fe6_b344_c470_397b_ba52,
    0xeb60_ad08_0cd5_73fe_65d3_4954_daf3_cebd,
    0xbfbb_b416_0263_5b78_b4b8_1b3f_a975_11e2,
    0x4e39_d536_b723_213c_b422_0611_93d6_f6a7,
    0x56d7_e046_8df1_5a47_0715_8240_1c38_434d,
    0x9e60_1537_348e_d732_7a13_f18b_bedc_4ff5,
    0xee2e_827d_9faa_74c1_bc40_97b1_16c5_24d2,
    0xe433_02e0_d517_a316_59b9_7885_e2f2_ea28,
    0xf662_e9ba_781a_1fae_9917_0a5d_c311_5544,
    0xe83d_a2ef_ce44_2856_6f42_3357_e7c6_a9f9,
    0x143a_e097_749a_513a_3259_28ee_6e6f_8794,
    0xa203_386e_6a86_f7c7_d0e4_3662_28b0_3343,
    0x7390_5f8c_5056_ecee_565c_31f7_de89_ea27,
    0x0da0_7ce4_4c01_42e4_30f5_6114_8411_9414,
    0x8b9e_9700_3ef0_1d2e_d873_db39_1292_ed4f,
    0xd8c6_66a6_6584_0842_7bd9_4e1d_8e17_debc,
    0x8bb1_069b_ba16_9263_c7d9_f168_64a7_6e94,
    0x6bdc_866d_7daa_19dc_947a_e053_ee56_e63c,
    0xe111_5bb3_f8ad_0cfe_c8c9_3882_f947_5f5f,
    0x0859_ae34_a51e_d77c_3a9b_f55b_a91f_81ca,
    0xf1d7_3663_c53a_0156_d9a1_1fbb_3d98_08e4,
    0x6692_83df_212c_93db_0fd2_2063_edc2_9fca,
    0x7489_abc0_8bd4_db15_b3f2_56d8_aca0_b0b9,
    0xf9d2_b26d_0375_aab0_b030_31a8_b451_6e84,
    0xde48_56e7_777e_27d1_35dd_37d5_8714_48af,
    0x2cae_af61_386f_a1f2_e9f6_082b_0554_2e4e,
    0x7c6d_4b00_383f_052a_ebfa_fa33_d725_4b59,
    0xb194_3df6_ea36_87ff_9255_abb5_0d53_2280,
    0x7e4d_1bac_a94d_a20d_b9ab_4ce5_7f2d_34f3,
    0x38d1_a6b6_448f_dc40_6935_01d6_2829_7551,
    0x8aed_5305_1756_d212_c62c_58f9_7dd9_49bf,
    0x1805_fa74_82c6_0f4e_cd45_4f8f_19c5_126a,
    0x24c9_891c_2f4d_b0b1_bbe8_3f4e_cc2b_decb,
    0xbb70_3190_b30e_b664_dc84_2b7e_2819_e230,
    0xb52f_857f_41e6_8fce_ba89_142e_0075_03b8,
    0xeb5a_7a71_4d4e_c1a1_a3bc_941d_0a50_61cb,
    0xc413_34a3_6d42_11ea_e9f6_760e_32cd_8021,
    0xe601_88ec_b537_010d_09c7_e552_bc76_492f,
    0xff67_dd93_2e57_55cc_852f_5493_4da5_5cc9,
    0x14c9_2be5_5246_7cfb_8107_fccf_064f_cf56,
    0xc94f_b8ea_e42b_3453_0989_54d5_1fff_6580,
    0x0bd0_0704_2735_acc6_23b7_0edb_1955_c4bf,
    0xb9dd_82de_bb9a_bd3d_c330_de42_6430_f69d,
    0x3312_b675_a5bc_5dfb_4715_ed43_e8a4_5c0a,
    0xd2e9_447e_6c6d_3509_a8d7_e4da_b780_a08d,
    0xd4d7_71a8_9c91_beb6_0572_b974_f03c_e0bb,
    0x31d0_7241_8324_8884_b57d_2e98_5e14_19c7,
    0xe722_9acd_b568_bc00_e8d9_ecbe_2cf3_d73f,
    0xb4ef_94e8_251e_cb66_2fe4_b171_70e5_9750,
    0xac81_7cad_3cfb_00ed_1131_7ba8_7905_e790,
    0x3fe8_6989_0b69_552c_7fbf_21ec_8a1f_45ec,
    0x91ae_c362_daa3_7fcd_1725_cabf_cb04_5b00,
    0xfb25_f88c_a887_ca77_964e_915c_d5e2_b207,
    0x053b_ff88_6db0_847c_3e2b_8bcb_f016_d66d,
    0x991f_d564_1b66_6e80_be74_44e3_9328_a0ac,
    0x24fc_37ad_820e_d73a_f85b_2b4f_bcde_44b7,
    0x2b2b_cac1_fa28_086e_4935_3fea_39ba_63b1,
    0x4c1a_3a21_90e0_8f26_1dd0_1aaf_cd53_486a,
    0x8e71_8591_cf07_851e_1fca_8a92_fd71_9f85,
    0xb14f_adbd_3baa_703f_fc7c_95d8_2735_7afa,
    0x8f1e_b8cc_7eed_d98e_18a6_a990_c8b3_5ebd,
    0x89ed_662f_01bc_b2fd_cccb_7005_c6b9_c28d,
    0xa263_fa3b_9a32_5a8f_3bdb_b92c_43b1_7f26,
    0xbae9_f4e1_4d09_637c_aa70_b5b4_f896_95a2,
    0x6be0_76b5_2945_a007_e94c_39a5_4a98_307f,
    0xd264_830e_7dc7_f906_b7a0_b174_cff6_f36e,
    0xc260_59b7_8ed6_854f_d4db_a847_29af_48ad,
    0x148d_ca9a_9b0c_8474_2e18_bc1a_d970_4a68,
    0x9749_c690_73ba_feb8_2de0_966d_af2f_8b1c,
    0xbba6_f466_2b0c_fd3c_b9c1_1d5b_1e43_a07e,
    0x6201_03f0_1e5b_63f8_6497_2d68_dee3_3360,
    0x4c78_20f9_50a4_c583_9462_8d38_d0c2_0584,
    0xe126_2fa8_ff1d_3269_dbc0_d2b6_ab90_a559,
    0x8f51_21c2_8730_29ef_d273_3c43_35c6_a72f,
    0x4fb3_edb5_4d50_7b36_7e75_d99d_94a7_0f4d,
    0xf859_4c47_0632_ebb6_6ced_1983_376f_a72b,
    0xb6e8_76e7_8ecf_5164_97fc_aacb_f030_bc24,
    0xafeb_0a5d_8071_50f5_7b77_497b_3250_3b12,
    0xf651_bea4_c88f_caae_8547_eddf_b81c_cb94,
    0xbfbc_e123_f031_77da_7999_9cdf_f709_02cb,
    0xb6aa_0fd2_2855_e81c_cffe_1939_438e_9b24,
    0xa240_adf5_4d70_b24e_8296_26e3_892d_95d7,
    0x732e_a6db_834b_f5a4_92fa_e242_91f2_b3f1,
    0xb472_31e0_7ae8_b35f_63e2_2c14_7b9c_3403,
    0x8055_4c03_9ab7_af15_c678_b6d8_6028_4a1c,
    0xdb2e_8329_7a30_b541_5873_8888_5065_9ae7,
    0xd58b_2a39_6b5a_1669_0981_dcd2_96a8_736d,
    0x2151_156a_affd_f4b7_9f65_789a_6509_a440,
    0x6547_9a62_9704_845e_9ff3_8fed_72e9_052f,
    0xa9be_d810_39cf_1c6d_e479_ee5b_9930_578c,
    0xd1a3_f98b_97ee_a710_e7f2_8ecd_2d49_eecd,
    0xff78_a5d7_2aa1_8fe3_56c0_74a5_81ea_17fe,
    0x96d1_a983_0ba7_ffd3_5544_f7d7_74b1_4aef,
    0xe19f_501d_cdf1_16db_7b3f_0195_fc6f_290f,
    0x8e68_f253_a278_535d_1215_3635_b2c0_cf57,
    0xdc75_c481_b2cd_c0fa_7f51_26db_ba5e_0ca7,
    0x7162_fa40_8d90_42fd_7a76_956c_3eaf_b413,
    0x9fad_ef0b_ce1a_7da1_3d57_74a1_1d31_ab39,
    0x5421_bbff_426d_4e84_8a1b_0838_21f4_0cb4,
    0xe794_4bee_b699_c0e7_7b4a_38e3_2537_df62,
    0x2ed8_b2db_0379_9071_9501_1364_6d1d_6e03,
    0xfc4e_188d_f10d_5454_4da8_979a_0041_e8a9,
    0x9de7_df26_c145_7c8f_3bc3_6e07_8f75_15d7,
    0x8fd7_3517_b47f_22c8_5d0a_12f2_7ad3_10d1,
    0xa300_5e22_b1bd_3e53_7f9d_1a2e_1ebe_1327,
    0x2bb2_c23c_899c_bc9e_da3a_361b_1c51_57b1,
    0xbfb0_766d_2652_6dbf_dcdd_7d20_903d_0c25,
    0x166d_bdc3_8309_e26b_3683_3336_d068_f707,
    0x6796_f7e1_1a4c_3cba_ce68_341f_7989_3389,
    0x562c_093e_14a8_7ff2_ab90_9016_8dd0_5f34,
    0x3627_e637_e241_3092_4395_4b32_52dc_25e5,
    0x2404_1470_2e63_067a_b438_c2b6_7f98_e5e9,
    0xa6f6_3494_f774_323c_10dc_d78e_3851_a492,
    0xc19e_a801_fb34_4afb_dbc2_7ab5_4478_22bf,
    0x9d18_e8c2_1a6c_8c61_9b3c_db65_f82c_a382,
    0x87f9_503f_9fde_d941_b67b_7896_167b_4c84,
    0x3e50_98f6_27c4_ed6c_bfce_d1b0_048e_ac50,
    0x6eb0_6344_3f58_c2ae_a911_9b60_369f_febd,
    0x3427_2006_16f6_5462_1fff_7ac8_0904_bf45,
    0xe3b7_4fe5_5c76_691f_ac12_fb17_1817_eee7,
    0xd631_6d50_08f9_32ec_af08_da91_77dd_a93d,
    0x35a0_d4ca_7416_842f_1b0c_ab93_6e65_c744,
    0xde46_8328_6c56_072d_b559_eb1d_04e5_e932,
    0x1587_5465_7dc0_f21d_c37b_45b3_f8d6_f2ba,
    0x6d80_8b47_2208_eab2_c3a9_dc22_8caa_c9e9,
    0xcb20_8f7c_937f_44e6_f3b8_b667_5a65_07ff,
    0xdff1_dc38_532c_3a2e_9fc4_77de_4ed6_81da,
    0x64ee_6958_558a_5d83_6737_8d8e_ccef_96cb,
    0xf103_b408_d124_5a6d_6dd8_56d9_4d25_9236,
    0xa1f1_9c51_8c3e_0b41_a319_ce15_b0b4_db31,
    0x48f0_c2ff_4281_9bfd_0739_7375_1f12_dd5e,
    0x1472_ba92_5a4c_6123_8a8e_849e_b327_81a5,
    0xe3b7_5066_0989_609a_e192_5c71_2852_79f5,
    0x6dd7_60ab_49ed_7373_74c0_4bf1_790c_0efe,
    0x6792_6c59_3a78_bcaa_4dda_4815_3c94_938a,
    0x5978_ff00_9a18_007c_9d26_6d6a_1cc0_542c,
    0x5568_e6bf_328b_448e_7440_fb81_6508_c4fe,
    0x7cc9_0fbe_d116_5f2b_1332_8503_df48_229f,
    0x156f_28d7_28f9_7cba_d6bf_7bae_e43c_ac40,
    0x2edf_603e_c74b_4900_4838_d65f_6ef6_748f,
    0xd48f_2990_33fa_9c9a_1e15_2328_f331_8dea,
    0x5437_5176_6a18_d326_8f84_19a3_48f2_96bf,
    0x04f2_5bdd_180f_31cb_72c8_834a_5957_b511,
    0xc66a_3569_f903_b0dc_d7a0_23a7_3260_b45c,
    0xce61_b42c_7eea_d35c_94eb_c8ab_cfb5_6dae,
    0xa705_d681_44ca_af00_9fc1_0d0f_9899_93e0,
    0x3371_ede2_9684_98fb_de68_a235_5b93_cae6,
    0x1c0c_9a22_0f8f_bf8a_a44c_fe79_ae53_8bbe,
    0x6631_fc26_158f_aebd_9d1d_84fc_ce37_1425,
    0xef0e_c337_ff2a_ef59_51d2_b1ab_2ddf_b636,
    0xc979_ef82_43e7_1d7a_2fd7_e4b9_e72c_d38c,
    0xd6c1_a706_01c9_1112_65ca_5b96_b755_2210,
    0xf3a1_da18_6614_1057_dd69_a0d8_ab3b_546d,
    0xd3e2_b4c6_98f2_a99e_604d_51b2_5fbf_70e2,
    0x1b4f_5d57_60ac_5121_73aa_8a56_4fb7_ac9e,
    0x27d0_b28f_28e7_d0ef_1a8c_1e99_2b94_1148,
    0xf3a2_d309_d1e7_2bc0_aac4_0a27_03d9_bea0,
    0x1294_c73b_3f91_4dda_764d_beae_7fa4_f3a6,
    0x56da_b15d_c8b3_fc48_1e99_b96e_70a9_be8b,
    0x5b77_d320_2095_d45c_2c5e_9deb_57ef_4743,
    0xe0d3_17e2_6a17_ae47_3a93_8fee_32d2_9981,
    0x5b1d_671e_1706_9897_26e6_db8f_fdf5_adfe,
    0x937d_c438_ef99_030b_4693_56c5_04ec_9f9d,
    0xce09_ea57_087e_bea9_c876_3c5b_08d1_908c,
    0xd0d8_fac3_d4cf_a048_3f6c_6af8_59d8_0055,
    0x27a5_8868_62d5_6b94_7f7c_c394_20a3_a545,
    0x5916_7366_1c00_d80b_9bfb_227e_bdf4_c5ce,
    0xd1cc_44ae_71a9_791d_8903_9d79_d6fc_5c5c,
    0xdf5a_715a_da20_9d36_8fe8_8b57_305e_2ab6,
    0x4911_7194_4e67_7dae_a09e_8c8c_35ab_96de,
    0xd0cf_9236_7e04_163c_fa7e_3939_8332_5753,
    0x67b6_a5b0_51ce_8d5c_d6b6_d0ec_c617_c699,
    0x88d4_1953_044d_621e_dfea_21ea_9e75_57e3,
    0xb93b_cd9d_d369_fe49_b67c_1fa4_8168_0af8,
    0x7999_9db5_7b23_5430_ca1e_3785_a9e7_24e5,
    0x20ba_d52d_d13a_90d4_1cfc_8bed_0d68_1639,
    0x4ccc_c5ae_16a2_9dd2_d18d_8549_d140_caea,
    0xa061_5c69_803b_77c7_4ed0_fe7e_9dc9_1335,
    0x2e2c_b093_8ba8_01a0_e4db_f063_4473_f5d2,
    0x7ba9_1914_cab5_0528_1761_f93a_44d5_aefe,
    0x2738_8731_08dc_ba8a_5389_8e4c_3910_da55,
    0x4502_a978_9913_1230_734d_e818_1f6e_c39a,
    0x3810_8697_800e_8bb5_2680_b122_baa2_8d97,
    0xa46e_de14_5d66_a90b_298a_f231_c85b_afab,
    0xe99d_73fe_edfd_98b3_7983_eed3_7408_47d5,
    0x3267_a96b_ed60_4a38_66c1_a2a1_a60c_d889,
    0xad66_f2c8_1cc3_fc42_9e17_e496_42a3_e4c1,
    0xfa2d_e5ba_2d8c_3693_edb4_54e7_badc_0805,
    0xd4ca_1c86_d116_bcbd_50b7_04ca_b602_c329,
    0x525f_7774_ee1d_de6b_4cc3_17fb_9cdd_d023,
    0xa8e3_4668_2e28_83b9_66b4_835d_9eaf_ea22,
    0xae03_d84b_90df_4cb2_219b_97e2_6ffc_81bd,
    0xd127_35c3_d08e_24d9_261e_4e4c_0a33_3a9d,
    0xb737_b467_cee7_1d3a_1fe2_cca7_6517_db90,
    0x3697_0fe3_34b2_a37e_d750_4dfa_8816_edbb,
    0xd5c7_3db7_872b_e26f_b957_1fa0_4dc0_89c8,
    0x8aeb_0ed5_6a41_77fa_1ddc_0325_259b_27de,
    0x0199_f29d_bf7a_1802_cf3f_4688_801e_b9aa,
    0x1cab_a957_a1ff_78f0_f4f5_d05c_10ca_b243,
    0x2abf_d2ec_bf62_492e_38b6_525c_21a4_2b0e,
    0xadd3_70c3_cd31_6a3e_36f6_0e2b_a4fa_6800,
    0x08a3_07d2_18ff_bcbf_eb35_9380_3173_e0ce,
    0x7bf0_2813_994b_261f_9c4c_d625_7c5a_3603,
    0x8942_9036_6274_ef43_af0c_317d_32ad_aa8a,
    0xc082_1c96_5822_94b4_258e_5a80_c720_4c4b,
    0xbd2c_e07a_63da_7db1_8b88_9d62_4d44_885d,
    0x03ba_38df_61db_a3a6_f4d1_4597_e660_f855,
    0x4bc8_ce1e_706b_b08d_d434_7f66_ec89_41c3,
    0x7dca_263e_a902_4a3c_e699_ed85_b0df_b40d,
    0x4e87_6b14_0c9c_da33_2472_f620_7c2d_0484,
    0x5bda_bc35_a2fa_1b4e_c2a1_e7b5_b459_aeb5,
    0x383a_46ec_e18c_27c4_ab4f_6451_cc1d_45ec,
    0x532e_e826_c31a_9b81_6376_7572_ae3d_6174,
    0x84f3_d8fa_ecfd_7924_a59e_0bd1_0173_1a28,
    0xba90_5cc3_71f0_66d9_116d_0016_cb94_8f09,
    0x3da8_8231_8279_b416_2cf9_c8ca_052f_6e9f,
    0xb356_6f6d_ccae_32ed_0b09_0a75_60a9_68e3,
    0x6eb1_3ec5_ca4c_b179_abee_ddb2_dde0_6ff1,
    0xab38_2663_03f6_72c0_58ef_c10b_06a2_068d,
    0xc5e2_3f86_9808_4689_c6e5_7a78_fbd9_86e0,
    0x502f_e8a1_ce0a_1bc6_2eab_8ca6_3ce8_02d7,
    0x4664_fe51_5409_3ec2_14a1_9564_0116_f336,
    0x3e39_d0fe_fb4f_faf8_7c08_28dd_624e_c390,
    0x4162_ffb2_b58a_ed88_d74b_be77_e611_6ac7,
    0x5e1e_505c_7c91_6883_8044_56af_10f5_fb53,
    0x2185_b1f3_4d27_5173_ebe9_ea2a_df43_21c7,
    0xdb89_8d04_8727_1365_0321_9a39_ee58_7a30,
    0xddb7_e20f_4f0b_0a5f_4978_7fef_17af_9924,
    0xe39e_cdbb_8083_0b57_a1e9_300c_d852_0548,
    0xd1e1_2d09_eb3d_6c76_5b45_e522_e4b1_b4ef,
    0xf33d_e059_1295_1acf_b49c_3b39_9509_1a36,
    0x7ddc_a4e7_cf6a_2022_d449_0ad5_26f1_4431,
    0x37da_e739_34d2_b45c_12a8_f216_af94_18c2,
    0xddf7_d6c0_8847_b906_001f_837c_c735_0524,
    0x76a4_f4c4_bd5b_4dc4_1877_b51e_57a7_64d5,
    0xde91_6ef3_cda0_4b7a_a285_3b80_f17f_58ee,
    0xf79a_5f06_f054_8dcf_993e_1de7_2d36_d310,
    0x7980_b63a_9726_39bc_b359_8080_ce64_a656,
    0xc327_c42e_fcd3_dcb0_252f_59cf_0d9f_04bb,
    0x8ae9_9f5a_1377_1265_d23c_8e17_6d11_3600,
    0xaeb9_6c60_887b_f568_1bda_0492_e7e4_586e,
    0x0262_cf9c_fb1b_e6f7_21e0_bd50_26c6_19bf,
    0x96f2_8db3_af9f_8eaf_3b09_7ada_f088_f94e,
    0xbd74_fc2e_2cd1_30ed_8d14_dedb_30be_846e,
    0xbfb4_0b3b_f213_0455_f95c_ffa2_3af5_f6f4,
    0x0d27_3904_28f9_09cc_3871_7007_61b3_f743,
    0xe641_18d4_1047_bff4_ca67_2b91_e9e4_fa16,
    0x9f17_5874_ee74_dfc0_64c8_e531_bff5_3b55,
    0x3c41_2787_59da_9b49_2412_60ed_4ad1_e87d,
    0xce24_3a58_7f0b_6bbd_106c_09b9_72d2_e822,
    0x1979_0b6c_ab0e_f9bc_7fba_1954_10e5_ca30,
    0x1b7b_b956_b20e_0ff1_7884_d9bc_6cb5_69d8,
    0xfcd5_5d2a_3d92_556c_0647_dfed_cd89_4a29,
    0x4f6a_4915_71d2_a627_6357_3ff0_3e22_4774,
    0x2a4b_d439_b908_5684_4fc8_e956_0f91_b123,
    0xb8eb_5b22_d497_aa9d_1db9_56e4_5027_5779,
    0xf6cb_137e_8867_9a76_b8d9_1274_b9e9_d4fb,
    0x22bc_9905_78e7_dd2c_a2eb_ee47_e2fb_fce1,
    0x01fc_46a5_af41_ba72_d9f1_f30c_cd97_fb09,
    0x9ee5_40b9_ce7e_922a_efed_53d7_5fd6_4e6b,
    0x5a82_3d7d_a29d_e33e_2e6d_02c3_6017_f67f,
    0xdcb5_4247_ed75_0238_a9aa_4d20_db08_4e9b,
    0xbbe3_4604_4327_e21a_b64b_e8d8_b253_96c1,
    0x3905_a0da_abfe_04e3_70cb_6af7_c2d5_bcf0,
    0x1670_290a_b911_8147_98f0_76a4_f7a2_322e,
    0x6816_3cb7_77ea_b10d_bf84_4708_05e6_9b5f,
    0xed24_0589_e171_a9a1_94c3_251f_06f9_0cf3,
    0x2281_ad67_174b_b66b_3e00_3e61_6a65_91e9,
    0x579e_d522_cb20_efa0_b925_a6cd_0421_aff3,
    0x9c31_9f6b_6fd2_554a_61bd_d130_7c66_e300,
    0x0dd4_faaa_fa80_e520_bf8d_5108_e27e_0d48,
    0x74bb_ae7f_87e8_60f9_240a_b57a_8b88_8b20,
    0xdff3_18b4_d732_a759_fc87_614b_af28_7e07,
    0xc371_ee9d_7d2a_f132_ef02_cdd0_6ffd_b432,
    0x76cc_4be2_658d_da80_a108_2c04_66df_6c0a,
    0xfd9e_506d_75af_7df5_8215_e577_0013_32c8,
    0xc5a0_d6e0_2e70_3f74_d39b_b9c3_a48d_b6cf,
    0x6317_01e4_2e35_f5cf_2738_2596_3430_5c14,
    0x3055_4020_95f7_43c0_61cf_4f94_c97d_f93d,
    0x37e8_9e5d_eff3_4268_1b6b_aca2_ae4e_125b,
    0xcc01_cd9d_386a_49cb_758f_450c_8857_2e0b,
    0x04a3_2825_1bd5_828f_959f_587d_507a_8359,
    0xc8dc_0ff4_362a_8c8f_b063_e962_e045_f54d,
    0xee81_4f9b_3bce_7af1_60e8_ed72_c0df_f5d1,
    0x57ae_92f6_b7f0_94ef_7b64_9785_5532_6f9f,
    0x0dbc_0ba7_bb1c_2445_fd08_0d23_6da8_14ba,
    0xec57_a417_b3ae_8ad1_8c90_fd9b_083f_4558,
    0x08d8_ec9d_3a3a_3a85_106f_72fe_81e2_c590,
    0x18a2_e3da_a4c6_bbe3_7976_033a_39f7_d952,
    0x68f8_161c_3538_8cf9_a4ec_0132_764c_a04b,
    0xe6e7_cc97_33d7_aa2f_733e_a705_fae4_fa77,
    0x206c_16a4_93a1_94f6_b4d8_f77b_c3e5_6167,
    0x2211_53de_c7e3_7554_9e21_f4f9_03b3_3fd9,
    0xd110_651a_a3d8_3db7_9d76_5e41_9fb6_9f6d,
    0x841c_2076_74a2_a59d_d30c_088b_a61e_a5ef,
    0xc09b_4da2_5303_cd0e_5d94_337f_bfaf_7f5b,
    0xe48f_ec76_c3d4_85e3_1a4e_4822_eb4d_7a59,
    0x4c37_f288_95f6_b9e0_6ffe_73e8_1b63_7fb3,
    0x97fe_8d6e_8425_ea84_ddf9_57bc_36d8_b9ca,
    0xe131_7625_1539_f9db_64d0_e29e_ea88_38b3,
    0x4e88_7953_d437_13f5_08dd_9bdf_d96b_9f63,
    0x32dc_49e0_f8b9_6e5c_087e_79e5_a57d_1d13,
    0x1240_f031_5707_df84_e328_e230_e3e2_b3fb,
    0xe22f_60c8_78f0_3163_1c25_59e3_0f09_46be,
    0x733e_de41_31a1_662d_720b_f5f2_6f4d_2eaa,
    0x5c82_e1f5_8657_d112_b077_4d26_1cc6_09db,
    0x6b39_b173_0052_9a95_443f_64ec_5a37_1195,
    0x9a36_6e73_434c_37a3_4112_cf68_649a_260e,
    0x3af0_4822_ac63_ca77_d813_f2fa_b7f5_c5ca,
    0x6c20_030d_cbb3_e153_660d_3257_3808_41ee,
    0x812b_c00a_41d9_de43_59ac_2c78_73f9_10a3,
    0x9d69_8757_f729_c30b_e846_9638_7767_1a17,
    0x7644_07e6_839c_724e_93b6_33ab_fa34_69f8,
    0x347b_8bcf_7aa8_a816_c0c0_f5a6_0ef4_cdcf,
    0xa15c_bc1e_7357_7012_caf2_1ecd_4377_b28c,
    0x8387_a4b0_8cc2_8a2e_5727_7707_199b_8175,
    0xce82_7a97_fe83_0269_506c_11b9_d90e_8b1d,
    0xfc36_77ad_2158_9c7a_d83c_c268_7a19_255f,
    0xc902_d33e_0a46_4ec1_4a29_c646_5a31_4cd1,
    0xfb39_2946_99a1_e2aa_ed2d_f212_1623_5097,
    0xd7bf_168e_258f_8f01_b563_5c95_ff72_96e2,
    0x2e60_2229_d2b3_7a22_22af_003a_b672_e811,
    0x821b_c37e_2c1f_8912_52e7_6259_6bf6_8235,
    0xc57a_1ba0_f260_acc5_9aeb_a33a_c6ec_c6b0,
    0xce78_cb66_7346_e38c_944f_6de0_9134_dfb6,
    0xbb39_eae7_a290_fcb0_6c47_bec8_83a7_de39,
    0x1bbc_42a4_5ca2_0618_6ad0_47c4_30a1_2104,
    0xef37_e286_f590_dcfb_a5b1_cfdb_a0ab_4067,
    0xd970_8ed7_ec64_1deb_7c45_d833_aff0_7862,
    0x12c5_e2f4_8472_4605_5092_ef95_0a16_da0b,
    0xf3e8_5d7b_8a77_b033_9338_e69c_052b_8e7b,
    0x8ee1_7c00_2143_8904_455a_4b4c_fe30_e3f5,
    0x462f_7fba_7c28_68f2_6b02_e631_95ad_0cf8,
    0xfb7a_14d6_137f_a5b1_6b17_b224_bad6_bf27,
    0x99ce_ec99_465f_e08c_d1e0_ccd2_5bb9_c169,
    0xa4be_0f00_1787_9351_de0c_89a5_56b9_ae70,
    0x48c9_c74e_faef_f5a7_5006_5e53_5a21_3cf6,
    0x682f_784b_a23d_c02e_9c11_69fa_2777_b874,
    0x226e_96a5_4011_3353_78ed_efd6_94af_1eed,
    0xaa04_b725_850e_2e32_6dc9_3d95_26a5_0e68,
    0xbc33_38f5_3303_ff19_ee97_f453_f067_91ed,
    0x94d8_0fc8_e255_9d54_32ab_0edb_6967_03d3,
    0xb470_b50e_6769_b2e4_3a68_53c7_e707_57a7,
    0x9d5c_af79_d12f_737c_3186_5ced_6120_f37d,
    0x0335_ce67_90cc_e15a_67fe_f95d_9260_7890,
    0xa9d8_8c05_289c_bd6d_1f2b_1d1f_15f6_dc9c,
    0xf3d1_d29b_153d_bd5e_b69e_38a8_965c_6b65,
    0x4104_7645_1284_9057_aa91_19ff_184c_ccf4,
    0x1d6d_02af_2005_1f53_f43c_7328_73f2_4c13,
    0xc8e4_8715_2b25_6129_fb4a_3d79_4a9a_80d2,
    0xaf60_d64f_bda8_f2ce_3550_c232_1fd6_109c,
    0xbf6d_091a_0541_7402_371f_77e7_6bb8_417e,
    0xda18_4d8f_f54f_3fc1_6bfa_9aae_5ec0_5779,
    0xb836_57ca_6633_e2ac_cd04_f3ff_001a_4778,
    0x75d3_e0b8_feed_bdb7_e327_3522_0644_80ca,
    0x26a5_8648_fd56_deab_9f91_508b_ffcf_c14a,
    0x5da2_dbf1_0323_e25c_049a_7f41_061a_9e60,
    0x1e3e_d1a3_9072_4f66_fcb6_be43_a9f2_fe9b,
    0xc761_5961_9f3a_f003_08de_8a1c_7797_da9b,
    0xb4af_22b5_f5b2_6389_8f98_87e6_0787_35a1,
    0x4667_92d7_a2e6_c6fa_b5b4_071d_bfc7_3a66,
    0x66fe_aacd_911b_81a2_230e_343d_fba0_8d33,
    0x94c1_e650_8f5f_1f47_43ed_7f5a_0fae_657d,
    0x4206_c6c8_0ef8_fd9f_3a88_a0fb_bcb0_5c63,
    0x3c1d_ea6d_0afc_ae52_2187_4b8b_4d2d_bc4f,
    0xe79c_f804_4680_e415_1bde_a12e_35f6_a8c9,
    0x4867_6fcc_8584_4eb5_53c0_65c6_c8e6_3528,
    0x62f2_0ecb_3011_44ad_e34a_1d25_0e7a_8d6b,
    0x2778_709c_b5ff_3fc7_d6b0_4d3b_7651_dd7e,
    0xd87c_6801_0650_c250_5e90_277e_7cb3_9e2d,
    0xbe61_c9fc_8adc_7260_2c04_6f22_062d_c67d,
    0xeaec_3f66_9523_6d35_b10b_b459_132d_0a26,
    0xd47e_9167_9b5b_befc_3fa9_ddfb_67e2_f199,
    0x1956_9361_7db7_534c_0e09_b88e_1914_f7af,
    0x2725_134b_52fd_2c81_10e8_b35a_f3ee_ab37,
    0x2277_0ffa_079a_1704_9eed_eca8_e272_b933,
    0x012c_2a69_dda5_ad22_d4c7_18bc_4ae8_ae5f,
    0x0933_8c04_f427_a66f_8153_6d60_1170_fc20,
    0x0d85_fd75_19aa_9dec_91b5_34f8_8581_8a06,
    0x0246_a0f9_fd64_2861_ec81_77f8_3f90_0978,
    0xac61_5f38_f5a4_51ea_190e_714f_ada5_156e,
    0x07a4_4f64_3033_6f1c_b592_bf39_b036_4963,
    0x2184_3034_22b5_3201_89c3_50c8_93ae_7dc1,
    0x6e47_c775_85ab_8164_ac04_2e70_f8b3_83f2,
    0x55d7_5131_c650_586c_b49b_52e5_87a1_ee60,
    0x4205_14ac_9286_37fa_fb15_2fe3_ff26_da89,
    0xbb00_290a_9289_ce13_3e66_6e6f_69ae_2c15,
    0x321f_8022_ccc2_553a_3b54_4ebe_544c_19f9,
    0x7eff_cb24_d14c_9d18_e805_a1e2_90cf_2456,
    0xdc41_8c09_511a_5174_24b3_3c9d_7ed2_5117,
    0x1130_c8b2_334d_05c7_e747_3342_7b72_f0c1,
    0x57f1_0554_d8e9_323b_0a80_4d18_b709_7475,
    0x90a5_ce5a_89ea_0b56_57e3_306d_881e_db4f,
    0xc273_27f9_36e6_8d1b_4ae7_d6a3_6eb5_dbcb,
    0x417b_730c_b2a9_66b0_2d8d_5432_1570_64c8,
    0x1d30_1ea1_5b8e_a672_d1e6_49de_1e7f_268b,
    0xf14d_ee33_99dd_f91c_8a32_8a1c_edfe_552c,
    0xbd98_9097_807f_7fbf_07a3_aec7_9624_c7da,
    0xc3a0_533a_6d96_954b_8454_7ddc_3e20_3c94,
    0x1992_5751_60c4_3696_990a_98fd_5071_d263,
    0xd494_93b5_23ad_7777_1a4f_f126_16ee_fc89,
    0xb77c_0f9c_f9e4_36f2_f6f7_fd14_3171_4200,
    0xe627_02fe_f789_82ef_30c0_5b1b_a332_f41c,
    0x236a_476e_d946_6eb7_8d26_36b8_1555_a786,
    0xe304_cfd6_1b4f_416c_46c9_feb5_5d12_0902,
    0x2cb2_ea09_2e5f_1215_ccec_0a73_b49c_9921,
    0xe200_4d8b_7660_e169_4e9d_2827_355f_c492,
    0x7fca_be79_c444_2ade_19eb_b029_435d_cb0f,
    0x3ec3_d068_6df2_4ad5_4659_d2b7_4384_8a2c,
    0x8f3d_9e86_c7b3_1fff_963e_f2c9_6b33_be31,
    0x6ba1_30e1_edb0_873d_74f8_5198_b05a_2e7d,
    0x40fe_52ea_aac0_4a87_5a0f_544d_d2b1_fb18,
    0x789e_b8c7_4c29_b5bd_0372_7073_c2e1_34b1,
    0x4630_d701_99d8_fe85_c7f6_aa2d_e59a_ea61,
    0xc4f6_e06e_6ead_b36d_3527_87ba_a0d7_c22f,
    0x2d9c_d053_6bdd_c355_9853_eab6_3b5e_0b35,
    0x9522_ac8d_318d_e072_abbd_cdd7_ed5c_0860,
    0xdac1_5872_053f_b2a8_cf05_daf5_ac8d_77b0,
    0x4b63_b051_2042_2ba5_49ca_d48c_ebf4_a71e,
    0xc4f7_97ac_06e0_c775_7a4c_10ec_2158_c4a6,
    0x4b9e_fbbe_d8c2_bd98_d9e9_2aa2_46bf_719e,
    0x67b2_d764_0bd6_d12a_13ae_978d_09fe_5557,
    0x6921_6ca2_1f6e_1bf4_7304_99af_9215_49ff,
    0x49ab_0589_118f_e345_4e4b_705b_9290_3ba4,
    0x90b6_72aa_f076_4986_ff57_7222_c14f_0a3a,
    0xc24a_a6db_9b0e_9300_55b6_344c_f97a_afae,
    0x8478_cc06_efce_550e_b862_225b_055b_6960,
    0x82e3_ec2c_cd28_350f_cac0_9afb_ddd2_cdb4,
    0xbc05_9bb7_4b99_3690_daf8_e982_9fe9_6b5f,
    0xe311_11d1_4445_238b_b5fd_fc5d_3132_c498,
    0xa7ed_1df7_7e79_a736_310c_b380_db6f_7503,
    0xa137_dc05_8288_8c63_e87f_bb46_217a_360e,
    0x7630_e700_4028_1934_2102_ae46_6ebb_1148,
    0x2c17_c3b7_4634_a6d6_f854_9e1a_3aa5_e00d,
    0x05ef_f03a_838a_4fb4_07a6_9afd_cc42_261a,
    0x3c28_d21d_40d4_f80e_c4c1_18bf_e78f_eaae,
    0xbeb9_cec1_8b16_3f7c_f9f4_892e_d96b_d438,
    0xdc75_1952_9748_4115_1af3_dbe2_5d8f_45da,
    0x5489_55be_3bde_572b_f5b4_b0b0_d2de_eeb4,
    0xc828_1bde_4d28_0b81_962a_ceef_a82e_1c84,
    0x914d_a129_a132_922b_046e_3eca_af45_3ce9,
    0xff11_d08f_1cee_77a4_f05d_1296_8194_9a4c,
    0x8070_295b_d01f_6bfd_9647_81ce_734b_3c84,
    0x006a_9346_f317_a9c9_9c2e_d440_81ce_5fbd,
    0x37e6_2ccd_f973_9fc3_522e_23f3_925e_319e,
    0x5cbf_8b75_3a7e_703c_177e_00f9_fc32_f791,
    0xfe0f_162f_cebe_01c8_2bc6_0a63_a6f3_b3f2,
    0x094f_481a_19d4_64ff_222b_bfae_6172_5606,
    0x9737_e370_b3c6_79cf_4862_89dd_cc3d_6780,
    0x60d6_7575_f3b9_b1c2_7dc7_785b_8efd_fc80,
    0x3948_fbaf_4119_6093_8af3_8731_c02b_a980,
    0x0c20_4d1c_fdbf_6e2a_1fab_64ea_29a2_ddf7,
    0xb7d2_d42a_9be2_9c2a_e4d9_4293_22cd_065a,
    0x49fa_e729_fc29_74b3_9da0_58c6_7844_f20c,
    0x6fb2_6356_dad9_8ed6_24c0_e332_b700_19b0,
    0x4847_b6cc_14ee_ffd4_2330_03b5_a6cf_e6ad,
    0x3b52_85a9_f015_2c99_d586_bd01_c5c2_17f6,
    0xff1f_e7f4_b91c_ff4c_5e56_3788_5f29_bc2b,
    0x16e2_0774_363e_99d0_7eba_726d_8c94_094b,
    0x437d_1aa9_cb41_59e0_0a56_a5f0_bfe3_9272,
    0x7bdc_c9d5_c1c4_da0b_d794_76a8_4ee2_0d06,
    0x8fd0_8773_3782_eecd_9e4c_1269_baa4_bf37,
    0xbf94_dee3_ad47_8cda_17ef_ee45_b0de_e640,
    0x83c9_4bbe_4c62_3bf5_1d95_b0a5_fcf9_0bc6,
    0x08fd_a03a_a45e_e9ba_93cb_e0b6_99c2_585d,
    0xc188_f92d_4d40_3856_65fa_4f22_7a2b_6d79,
    0xfd66_11df_b123_45fc_d5f9_e858_2925_04d5,
    0xef00_3ffd_18ae_cc12_c2b5_a03f_7147_1a6f,
    0xb7b4_74cb_f293_4019_5930_0222_b456_1e00,
    0xbc2a_55e5_8b30_deee_ce2f_8642_ca07_12dc,
    0x3e77_de18_337c_da42_7ca9_723f_bb2e_8988,
    0x2f81_e058_ffb7_5885_2785_3383_47f2_ba08,
    0x8f0c_300c_f585_707e_c61b_b3a1_41e5_0e8c,
    0xcf4f_4c53_6e0e_2af2_150f_361d_ab9d_ec26,
    0x24c8_1ee5_fd39_a8e4_9f6a_419d_3825_95f4,
    0x2841_577e_66ad_726e_64a5_3dc9_24fe_7ac9,
    0x6809_0c81_a135_7214_142d_e49f_ff7a_7c3d,
    0xa6aa_70d4_4a61_3a24_0c33_5248_857f_a9e7,
    0xdb80_5d26_087f_4db9_0a9c_32d5_eae4_5305,
    0x6dd9_54b4_5a12_2182_e6c4_2178_c4bb_b92e,
    0xc347_29fd_9f19_48a3_71f1_ce24_90d2_0b07,
    0xf068_2ca0_764c_c153_f1bc_c3d2_75af_e51a,
    0xbf29_8242_79ca_73e1_e728_e8c8_3c33_4074,
    0xf0c7_0fb4_e725_caff_96fb_f83a_1288_4624,
    0x1e18_cad8_09e9_eedc_81a1_549f_d657_3da5,
    0xa893_a8fa_258f_383e_5fa7_867c_af35_e149,
    0xe78c_30cc_1179_b849_5698_6e2e_f3ed_091b,
    0x2e44_32e6_ce49_96d9_917f_1dd5_f888_6c61,
    0x576e_c7a8_4e0b_932d_d20d_8c88_c8ff_e65f,
];

const WHITE_TURN_MASK: u128 = 0x3815_e537_b622_2c85_f8d6_26aa_af27_8509;

const CASTLING_RIGHT_MASKS: [u128; 2 * 2] = [
    0xca3c_7f8d_050c_44ba_31d7_1dce_64b2_c310,
    0x8f50_a115_834e_5414_f165_b587_df89_8190,
    0x7756_8e6e_6151_6b92_a57e_6339_dd2c_f3a0,
    0xd153_e6cf_8d19_84ea_1ef6_e6db_b196_1ec9,
];

const EN_PASSANT_FILE_MASKS: [u128; 8] = [
    0x1309_9942_ab63_3504_70cc_73d9_0bc2_6e24,
    0x946c_7352_9a2f_3850_e21a_6b35_df0c_3ad7,
    0x3d1a_dc27_d706_b921_003a_93d8_b280_6962,
    0x994b_8bd2_60c3_fad2_1c99_ded3_3cb8_90a1,
    0xf4cf_0c83_cace_7fe4_cf31_45de_0add_4289,
    0x5480_7a18_b695_2e27_d0e4_427a_5514_fb72,
    0xe2a1_aff4_0d08_315c_77c6_21cc_9fb3_a483,
    0x47ec_43ff_bc09_2584_67a3_4dac_4356_550b,
];

const REMAINING_CHECKS_MASKS: [u128; 3 * 2] = [
    0x6a2a_d922_a69a_13e9_1d6d_c0ee_61ce_803e,
    0x49b5_72c7_9420_27d5_c628_4b65_3d38_e96a,
    0x08c2_e927_1dc9_1e69_803f_5fb0_d2f9_7fae,
    0x088d_fad9_83bb_7913_b183_ccc9_e73d_f9ed,
    0x90a8_52ca_cfc0_adeb_fdee_f116_02d6_b443,
    0xc8ce_065f_15fe_38f5_1b0c_e419_8b38_01a6,
];

const PROMOTED_MASKS: [u128; 64] = [
    0x2b91_78eb_57f3_db25_2f99_00cc_2b7a_19ca,
    0x17d1_6678_351d_3778_f752_35be_b018_86d3,
    0x88b1_7afb_dae8_36b1_8ae7_e298_89ac_9964,
    0xa898_5bb0_47c3_88b1_ad30_091c_e7cb_4204,
    0x2577_b875_de12_0fd2_aae1_1877_3ddd_4e4d,
    0xbdf2_fdea_13ee_21fd_8ec5_14ce_4736_aa07,
    0x3bcf_1cf1_605d_a5e7_26a4_12bd_8cef_4f15,
    0x6ea6_f4fb_2ca4_d856_1bdc_e26b_d9af_059f,
    0xb119_8a96_21b2_37f4_ea5f_4ade_5acc_0516,
    0x6c87_686b_2878_2362_69ab_7ebc_0765_0565,
    0x5158_703d_9536_de86_3e65_5f89_5a18_8a1c,
    0xb312_6faf_bea7_5501_f394_f688_2a11_4d65,
    0x80d3_3358_5254_7580_3173_cfa2_be5b_d4d3,
    0xc5af_c4f4_4d5a_b019_434d_20d2_ca00_ae71,
    0x7aff_7035_ddcd_e586_3ba2_97f7_3d33_8c93,
    0xc338_837c_9531_20b2_099b_a1b0_205a_5ea5,
    0x7f8b_4b71_5fc6_eee8_c49f_050b_5e1c_5653,
    0x76d1_cd96_2b7c_0005_e14e_ec50_a9c6_90e8,
    0xad3d_b13d_420b_8915_2571_cc79_f4ce_0169,
    0xe5a6_0762_8406_1351_de0f_98d6_002f_4323,
    0x7f61_ddc8_b19d_e688_0682_220b_02e5_c3e8,
    0x8648_ca9b_e569_6f33_cb90_0d3a_6b38_c39d,
    0x469d_c25e_1b32_d323_2462_0fbf_09d5_0d66,
    0x4f52_9fa2_8957_8425_0f40_a9b2_781a_119d,
    0x82d5_50f6_a40a_3d66_83c6_980d_f0d0_4932,
    0x8e79_1844_cfb8_7476_ab6f_9af7_20cb_5df4,
    0xb229_ab8b_b605_4dad_1c90_6974_166e_e8d4,
    0x50a0_b7e3_c796_ad7e_9c1b_a3db_0784_ebda,
    0x186c_0c4a_7a5d_68d1_81a1_9098_d16a_a929,
    0x7483_01e9_e571_a670_fce5_6173_c63c_cefd,
    0x2119_955c_d577_ee40_43cb_7aa2_0c62_09c2,
    0x6c57_c138_0ffb_21fb_7e96_e2ae_8692_4bab,
    0xadd6_07ff_5aaa_f995_0186_0725_034b_0fef,
    0xd41e_c439_c73a_3e0f_f74d_3690_66ec_4e96,
    0x3ee3_43c1_d983_7a9e_1ae9_962c_6e0d_1232,
    0x9770_7414_bf74_3321_5d66_fa46_5ccf_c560,
    0xe6a8_ec45_3ed6_7917_e9c1_3ae1_fc36_afaa,
    0x0384_5f08_49e1_83df_caec_4035_fb84_0be4,
    0xccd5_f9e7_ce7e_601f_839d_28ad_afad_0f8f,
    0xe2c0_4ba3_4842_32d8_e470_3b6e_3042_2003,
    0x5d9b_925a_3e6a_f022_1e2f_d5b2_827d_5e43,
    0xa852_b770_63b9_e148_96f1_e8d8_b94b_d960,
    0x279d_2c0c_53ec_aac6_90f2_075c_3f43_960c,
    0x39f0_827a_4f81_1c72_c48e_0774_c4f9_134f,
    0x3b06_4b86_1451_7b48_f17e_5f6a_2cb0_00c7,
    0xdb19_bcf0_00dd_394a_6248_409b_f55a_4925,
    0xa63d_ddb6_17c5_9634_967b_d94e_b305_05cc,
    0xece8_3ffa_46fd_173a_e91e_8985_3f9e_844f,
    0x01c4_d4d3_9c11_557f_b841_038e_2419_3f08,
    0xa618_dd21_a5f6_b67f_46f3_b25c_ae82_a6cc,
    0x8356_bdf4_4056_3fb0_3e97_e042_449e_3ed5,
    0xf35a_d55d_7273_51a1_868a_166a_f46d_cbd2,
    0xa3a0_c83e_c173_d41c_f71b_e788_b3fd_1a7a,
    0xa8b2_fe93_fc5d_dd19_cb6d_6541_0533_cc37,
    0x984f_d612_fa39_36bd_7e30_d705_59ef_aedc,
    0x71c7_e142_bb02_9d2a_32db_0f5c_a181_59ce,
    0x759f_8a94_60c6_34a6_97a9_116e_8742_28c5,
    0x6fa2_ca2c_a3bd_eb90_85ee_68ee_3a17_5297,
    0xea5f_0228_fc31_4b97_076a_1417_0b40_9e2a,
    0xc40c_265d_97e7_63c0_bad4_9d47_dc95_855b,
    0x201d_68d0_d89f_0e31_6361_87d9_4ded_991e,
    0x1b76_9456_c778_79cf_962e_5097_1f09_cfab,
    0xa8ba_af8b_31da_09a5_8f16_c910_d677_6589,
    0x06a7_9ac4_14c9_632e_7e3d_e4bf_bef5_566f,
];

const POCKET_MASKS: [u128; 2 * 6 * 16] = [
    0x6e21_a47d_5b56_1a1d_b262_e9f9_d612_3320,
    0x4263_a757_e414_fe44_9153_3947_cdaa_8bec,
    0x93c4_3f67_cf55_b53f_a13b_56b4_5723_a3d4,
    0xa897_32f3_39d3_5eec_9a35_cce2_9ca3_ac75,
    0x5df4_ac25_c29f_bebf_2716_940e_1d4f_28d7,
    0x6205_9230_cedc_d78f_7447_209c_fb79_3066,
    0x9b0f_7261_932d_2c8e_5cf9_1d8a_e640_2e1a,
    0xfc0f_99f0_0e0c_c0e7_4625_588d_3848_7ac5,
    0xcba8_a5a0_2c35_1aa5_e42e_c619_1353_e3bd,
    0x77a6_c01b_d017_4d69_478e_6cc8_f6b2_dada,
    0xc200_e526_4100_e463_1726_fc94_8b99_4b87,
    0x3f34_0a89_f525_effe_fb9d_2e5a_66b4_6741,
    0xf748_b2be_597b_2f7a_7f66_8e40_1ffe_9e6f,
    0xf7b2_eddf_7b88_38c2_ee4d_6fe1_1c46_a236,
    0xbdec_7e9f_4317_a678_006c_b700_6425_9959,
    0x5e02_2239_fdf2_0b36_3353_5a7c_4def_1b24,
    0xfdd9_2fa0_5b03_b629_479e_792f_8171_fc29,
    0x80cd_ab95_a899_27dc_656a_6e71_de97_0975,
    0x92cb_516b_8eba_4a30_cada_3e48_618a_1c2b,
    0xac95_0f8b_ce3a_f2d9_b37a_d726_2db9_c99e,
    0x9a43_060d_3aaa_e01a_85ae_2540_2a31_1d5d,
    0xf12b_2f60_12f9_333c_3de4_e82d_52db_b44c,
    0x8ae9_143e_ce61_584a_b1c8_4996_7446_4c21,
    0x676f_7093_0e72_993c_f1c1_853c_c682_7b84,
    0x25e0_f5f0_1447_6a1f_51f9_7ed3_ba00_4fb0,
    0x1e33_827f_042d_e978_00da_9ede_878e_3e98,
    0xdd15_8e4a_7838_524d_3cd0_fd65_8e1c_db12,
    0x2b75_316d_fa1b_15e2_ac29_40b6_88a1_d0f9,
    0x0283_b57f_325e_a495_e51a_cb5b_336d_b0df,
    0x146e_60f5_6ab9_1765_cf75_17fb_dcb1_6174,
    0xf50d_2497_f8b1_2819_dfe9_01ab_a4a2_ced3,
    0x7600_c53f_3c60_308b_24bf_d4b7_2c88_52eb,
    0xb752_08a6_056d_c7e9_f085_bcd9_7118_83d4,
    0xe5f0_eb83_c20e_921b_41b7_1908_a3d8_6274,
    0x7529_d0a0_c95f_08ed_6d60_4cc0_a2df_1a69,
    0x33c7_b70e_e04a_511c_aedf_8291_e004_8c39,
    0x727a_256d_ad06_cc11_09d3_c83f_5954_7935,
    0x1b26_058e_1ba7_3008_257d_5c7e_bc71_8242,
    0x09c4_8fc7_e167_f3b0_56ac_1c99_8f5c_2ede,
    0x954f_57cd_f607_6cd4_a25c_0b06_7993_7316,
    0x32fa_cc37_b50d_925e_a9a2_a7e2_00fa_a936,
    0x2596_9816_8b89_f941_b8e7_ca47_16cf_9d49,
    0xb447_9bfb_3575_d3fc_9b25_3f89_247c_4c1d,
    0xe69d_2038_0ad4_5ef5_1e70_1e2a_73f9_dc4b,
    0x0ea7_2e45_5dfb_08e6_cdf3_51b2_89aa_5a84,
    0x7f83_4fab_7161_3f89_2e4e_118f_c45f_dc0d,
    0x0bef_7b04_290f_a4d3_8024_7d70_885a_d5ce,
    0x425d_9a92_61ab_b5b9_0a99_dccf_ce31_6ca0,
    0x231b_fd9d_fb70_f61e_b555_3435_dae7_6840,
    0xf2ce_79a6_9837_967f_ee56_2004_d5d1_4158,
    0xf901_2eb6_947f_5b8c_551b_5fa3_ec71_66a2,
    0xe867_f9c7_0350_3bf1_2dbb_493c_6e9f_ec06,
    0xee98_c901_0d1d_3cbc_f06b_4c65_f4bb_14a1,
    0x95fc_0aa1_2223_89e8_5f0b_44d9_8013_acb9,
    0xbf75_33eb_b6c9_9102_ce7d_bafa_734b_ba8a,
    0xbc41_5372_0b7d_8489_e009_c0e3_55a7_7913,
    0xe892_965f_4753_afa0_2191_8f47_3cb6_decf,
    0xeaec_3e07_7478_1d2e_dcf1_1e80_dc14_763f,
    0x00b6_98a9_c339_0404_7ac2_1357_500f_b0c6,
    0x84c9_4d9a_3d13_f9b5_28ab_e0a3_761e_326c,
    0xe4a9_10d5_16e8_0a37_30b8_e3da_17d3_4c6e,
    0x30a9_9425_bba7_3df4_d999_d38f_fa5d_771e,
    0x1fe9_2363_cf09_9b7e_8a7e_0d13_67d7_0b28,
    0x3fce_173a_5e42_7cb2_9157_bfe7_ac07_1796,
    0x3cf0_44d0_bd7b_fb26_adda_94b2_1edd_779a,
    0x14dd_9fdf_d382_638a_6f55_5cf7_856f_0d63,
    0xa4ec_5d64_1160_68be_5b2a_5b27_88ad_c947,
    0x970d_0225_c315_5df4_500c_782c_8c56_2a42,
    0x96bb_58fa_f0fd_1692_20f8_b3f7_059d_8884,
    0x183f_9136_b816_0b83_79c8_90ed_3e95_f3f4,
    0x85c1_79d2_2cce_92b9_e64d_bd47_4ddc_f8ca,
    0xd355_d775_2b44_05ff_a949_66fb_f7f2_70d5,
    0xda7b_7aa3_7223_0d64_2473_b4e6_ad9f_aa9a,
    0x8218_ddc4_550f_6260_98ab_df9f_a4b4_87e6,
    0x090f_2692_c727_e1a1_75fa_1ecb_0717_029a,
    0xce74_c18e_0f75_f7f8_f605_3757_646a_08ba,
    0x0750_81d9_944c_f832_060e_2788_d998_13aa,
    0x90fb_3433_62ef_172d_5fa6_1c63_681e_bbc8,
    0xac07_cd32_cdd4_a6c5_90bb_f42d_b708_006a,
    0x3684_e6c1_eaf9_e5f5_b525_460e_c1c1_5916,
    0x0f51_5afc_1356_a5ca_2696_070a_4502_024d,
    0x282b_bc7b_2209_e436_1580_8744_2731_df68,
    0xc017_2402_afbb_cbc5_6501_0c3e_a0ac_fdcf,
    0x34e3_45bf_a3c4_7ceb_b28e_cdf3_05a7_a831,
    0xa568_2469_0a26_a07d_fd03_7a2e_2a2e_54e8,
    0x82de_3ff2_26f8_520b_5f09_f376_3f6a_4882,
    0xd6e5_02a6_09ba_9dde_e012_5e53_c4e6_4b83,
    0x3b40_c681_d5b6_e330_1de4_4a24_4be3_752a,
    0x23c2_3128_38f0_0cc0_f789_19df_b05f_031c,
    0x6e63_a230_b2ea_b193_bf81_caeb_ad91_d8e1,
    0x9a08_aa69_fb12_1fc8_bc37_80dc_e0bd_58d5,
    0x551b_e806_bb80_e780_65b5_fb1a_fa5c_5714,
    0x5ab7_fd28_c7b9_6a6b_b7dd_b798_d0c1_ff23,
    0xb327_7b90_3da3_eab9_a823_d99d_1504_f4d6,
    0xaf84_fcbb_2d16_fa25_a3c5_26e0_7f1c_f98d,
    0x1c53_b33d_e31b_a544_a848_f93e_7a83_ece4,
    0x4186_f24f_ae7b_9c26_21e3_9416_00ab_aaec,
    0x4bcc_d3e2_48e2_a69c_534a_0704_49a9_238d,
    0x8d6b_46ae_e9ae_0606_f86c_2e4b_b82d_3923,
    0x7b28_1261_c9e1_028e_a594_b44c_256b_41f8,
    0x6c50_1072_a110_8abe_f150_3a71_0531_b677,
    0xa8c4_da9b_a7c8_a22a_171a_27a1_b991_1e11,
    0x88d5_489e_8bc2_9294_d8d8_a26a_c022_ebe6,
    0x1381_a967_122b_289a_151c_a9f6_4135_2f33,
    0xdc94_4a00_8e97_acd7_553b_499d_c1ea_e685,
    0x6383_8f93_6ef4_25de_0137_684b_ed65_e27e,
    0x39bd_0519_7aca_ddb5_254a_12bd_9efa_3535,
    0xf587_1ddb_63b0_aecf_f836_1f0b_0a35_ef6d,
    0xcd9a_19a4_adda_c30e_a7b7_e76b_7ff8_2166,
    0x08c6_dce8_8d2b_fe12_e266_e006_7bc7_f396,
    0xea8c_0b4c_4097_cc43_bdd8_a903_7f5d_0298,
    0x74fb_47cb_2de1_371f_2d59_77c3_f88a_2a31,
    0x8c24_06a6_0633_f3a5_4587_cd65_1a3b_b45f,
    0xc403_8765_8bd2_d9bf_bcdc_3c56_ad97_1eb0,
    0x4616_dae4_1e7f_6769_248b_2073_706e_1844,
    0xff41_e4fd_3d1a_34ee_ab03_444d_fb15_bd0a,
    0xd9c4_e712_cc56_1f6a_bcaf_f313_4756_ab78,
    0x3935_20a3_1d54_572c_eea8_44cf_3e1d_b285,
    0x1cc0_2ca6_2684_138e_b917_fdb8_0f35_5116,
    0xc1b9_f65a_9989_c1d5_2193_1f55_9ece_fa34,
    0x86e3_a796_6174_637c_7170_c643_6114_a4c2,
    0x80d8_247d_1168_300a_73d2_a7c1_017a_2aaf,
    0x1996_e2ac_2b93_8629_3b85_5d17_55dc_e20e,
    0xb26f_0062_6363_9c6a_37e3_5078_817f_0dbd,
    0xfe1f_7c18_126a_bdd7_e59b_1e33_89a1_aad3,
    0x83b9_090f_7e58_e659_bad1_1ebe_3c3d_f239,
    0x2825_3df5_164c_46a3_d54a_ad8a_64c6_5c27,
    0x3448_dc02_2a87_a231_eadb_37a4_f7fb_eb4c,
    0x72eb_d4eb_6f76_301d_5453_586c_4984_a81c,
    0x1f5c_f5c0_27f9_df47_b677_7cd5_e1b1_6bcc,
    0xbcda_2313_c8ee_1152_24b6_9016_1baa_0d65,
    0xb2eb_c333_9460_3af6_5bd3_613d_2ee2_22fd,
    0x84b3_b1b6_fa01_cb1a_c928_bda0_35f8_c39d,
    0xe16b_42d5_3a9e_cf6e_29e8_eeca_9b09_c735,
    0x27b2_4383_307c_7a88_dc35_bba3_f78e_d4ee,
    0x0163_5963_de0d_35f7_1753_c5b3_ef82_0c81,
    0xd566_7fb9_42e5_2b2f_ef33_68ab_5656_5ae7,
    0x958f_aa73_25a9_7d06_ebf4_8bd3_5c4e_ad40,
    0x7043_3ecb_73b7_ad92_529b_39d0_15e4_9755,
    0x4ab8_a46b_4c36_eaaa_697e_a706_20e9_8751,
    0x1869_111d_7c4f_e1a8_ebca_be3d_4cbc_0212,
    0xfcd4_6428_ff1e_2a1e_2f42_4e66_9d2a_7f1b,
    0xb65e_4b53_6bfa_3559_6ae4_7a9c_2230_2f58,
    0x180b_7095_853d_37f3_83f9_a7b5_7452_3121,
    0xbb02_26e8_c154_3063_77b6_860d_aa7a_39d5,
    0x708d_753a_0092_df11_1611_e306_f167_e512,
    0x9992_315a_0c7a_dfe5_2d78_f39e_1adb_aa9d,
    0xe125_0829_4de8_e35e_1aad_a836_dedb_3ba7,
    0x4996_0c59_7d11_9ace_f379_9175_3c7d_f558,
    0x57b7_ccbb_21f7_4d1c_e808_40e6_23a1_9d08,
    0xee13_3936_2cfc_2fde_cb86_5270_dc12_ea71,
    0x6938_614f_83f2_eadb_350f_4bc8_d3ab_3787,
    0x225c_ad0b_9f39_3fce_8d0e_2e1e_3609_b3b0,
    0xc182_48d8_4de0_1920_96d3_be73_348c_c215,
    0x5176_67a2_5d5d_4dda_a8fa_ccf9_0f40_06ba,
    0x4c85_0d85_739b_f00d_4e64_02cd_300e_37f4,
    0xb14b_ec81_3f45_6e88_e9f5_d81a_6c4e_bc04,
    0xf451_ce39_caa4_6649_3127_edce_389b_25d4,
    0xb5a7_0d2a_7d51_278f_7d1c_0961_900c_7fc6,
    0x586d_2606_687d_8bd8_745a_35d9_9954_6397,
    0x2f0a_3192_e89a_ae69_ee16_ac7e_dc0a_b87a,
    0x5622_33c3_3351_4b66_2c61_c062_20f1_5ad6,
    0xce07_6f19_270b_e140_5faa_92f8_0472_83fe,
    0x514d_0d13_8314_1fd0_2875_5293_7fe1_0b06,
    0x79cc_7716_c5a0_bcdb_fc6f_01a7_90e7_b387,
    0x70d5_58d9_9c09_9cd4_1a81_5748_3920_14a9,
    0x93e4_a1eb_c793_cd9f_247d_9f29_5ed9_8529,
    0x05a8_49ee_d2af_e12b_a507_da51_7084_b6bf,
    0x9778_5a97_b347_5d08_7fc7_5297_e547_ee09,
    0x7a6f_f517_33b6_0e02_9c00_1de0_0030_a7db,
    0xd451_52c0_77f2_476c_dcea_a53f_7ca6_ea65,
    0x3c5d_f4f9_d7d5_c1ca_8c59_69da_7a57_26d3,
    0xcf12_a391_ff1b_694c_aca0_5f0c_0d8b_ff7c,
    0xc288_a95f_133c_e1e3_bafe_c4f2_09d1_72df,
    0x64c9_7efb_318b_3a03_4a83_b8ca_5258_098b,
    0x8779_0ec7_dec7_dc2c_79e2_016d_acd1_48ef,
    0x969f_7f76_7f92_58ca_902b_ad8a_8d8b_8104,
    0x84b1_ff01_20f7_59f5_87e6_89a6_2d87_f03f,
    0xa19d_df47_73ef_d2a1_2fb5_7fd2_52d5_2552,
    0xa75f_9c5e_34cc_a8e6_6eb4_8132_2177_4e02,
    0x521c_8a92_a2a3_e260_4281_066c_9a5a_7407,
    0xc50f_0a89_eea1_6db0_4f53_f7c7_b022_f55d,
    0x6e13_7415_e776_5cd5_66f1_3e42_05c0_1ea2,
    0x578a_e522_a1ac_d365_9d64_5bfe_c6b6_698c,
    0xb128_f938_00ce_9611_d8f0_c9a0_3a4c_201d,
    0x220e_4eac_cb1b_e67b_e33f_0569_aa83_31c4,
    0x987b_37d3_c132_0443_919a_4f35_461a_3b24,
    0x223e_00d7_caa5_7a43_b4de_88f5_6962_e1fa,
    0xf077_a836_28e3_293b_bcff_222b_0252_0b0d,
    0xa0bf_5607_e3b1_8b86_22aa_5366_5a3f_23de,
    0xb4e0_aff4_e90c_cb0d_22c3_ce12_1986_d762,
    0x484e_a0a4_8998_be25_fa2c_98ac_27e2_e5b3,
];
