//! Zobrist hashing for positions.
//!
//! # Examples
//!
//! ```
//! use shakmaty::Chess;
//! use shakmaty::zobrist::ZobristHash;
//!
//! let pos = Chess::default();
//! assert_eq!(pos.zobrist_hash::<u64>(), 0x463b96181691fc9c);
//! ```

use crate::{
    Bitboard, Board, ByColor, Castles, CastlingMode, CastlingSide, Color, FromSetup,
    Material, Move, MoveList, Outcome, Piece, Position, PositionError, RemainingChecks, Role,
    Setup, Square,
    Chess,
    File,
};
use std::ops::BitXorAssign;
use std::num::NonZeroU32;

/// Integer type that can be returned as a Zobrist hash.
pub trait ZobristValue: BitXorAssign + Default {
    fn zobrist_for_piece(square: Square, piece: Piece) -> Self;
    fn zobrist_for_white_turn() -> Self;
    fn zobrist_for_castling_right(color: Color, side: CastlingSide) -> Self;
    fn zobrist_for_en_passant_file(file: File) -> Self;
    fn zobrist_for_remaining_checks(color: Color, remaining: u8) -> Self;
    fn zobrist_for_promoted(square: Square) -> Self;
    fn zobrist_for_pocket(color: Color, role: Role, pieces: u8) -> Self;
}

macro_rules! zobrist_value_impl {
    ($($t:ty)+) => {
        $(impl ZobristValue for $t {
            fn zobrist_for_piece(square: Square, piece: Piece) -> $t {
                let piece_idx = (usize::from(piece.role) - 1) * 2 + piece.color as usize;
                PIECE_MASKS[64 * piece_idx + usize::from(square)] as $t
            }

            fn zobrist_for_white_turn() -> $t {
                WHITE_TURN_MASK as $t
            }

            fn zobrist_for_castling_right(color: Color, side: CastlingSide) -> $t {
                CASTLING_RIGHT_MASKS[match (color, side) {
                    (Color::White, CastlingSide::KingSide) => 0,
                    (Color::White, CastlingSide::QueenSide) => 1,
                    (Color::Black, CastlingSide::KingSide) => 2,
                    (Color::Black, CastlingSide::QueenSide) => 3,
                }] as $t
            }

            fn zobrist_for_en_passant_file(file: File) -> $t {
                EN_PASSANT_FILE_MASKS[usize::from(file)] as $t
            }

            fn zobrist_for_remaining_checks(color: Color, remaining: u8) -> $t {
                if remaining < 3 {
                    REMAINING_CHECKS_MASKS[usize::from(remaining) + color.fold(0, 3)] as $t
                } else {
                    <$t>::default()
                }
            }

            fn zobrist_for_promoted(square: Square) -> $t {
                PROMOTED_MASKS[usize::from(square)] as $t
            }

            fn zobrist_for_pocket(color: Color, role: Role, pieces: u8) -> $t {
                if pieces > 0 {
                    POCKET_MASKS[usize::from(pieces - 1)] as $t // TODO
                } else {
                    <$t>::default()
                }
            }
        })+
    }
}

zobrist_value_impl! { u8 u16 u32 u64 u128 }

/// Supports Zobrist hashing.
pub trait ZobristHash {
    /// Computes the Zobrist hash from scratch.
    fn zobrist_hash<V: ZobristValue>(&self) -> V;

    /// Prepares an incremental update of the Zobrist hash before playing move
    /// `m` in `self`. Returns a new intermediate Zobrist hash, or `None`
    /// if incremental updating is not supported.
    fn prepare_incremental_zobrist_hash<V: ZobristValue>(&self, previous: V, m: &Move) -> Option<V> {
        None
    }

    /// Finalizes an incremental update of the Zobrist hash after playing move
    /// `m` in `self`. Returns the new Zobrist hash, or `None` if incremental
    /// updating is not supported.
    fn finalize_incremental_zobrist_hash<V: ZobristValue>(&self, intermediate: V, m: &Move) -> Option<V> {
        None
    }
}

impl ZobristHash for Chess {
    fn zobrist_hash<V: ZobristValue>(&self) -> V { hash_position(self) }
}

#[cfg(feature = "variant")]
mod variant {
    impl ZobristHash for Antichess {
        fn zobrist_hash<V: ZobristValue>(&self) -> V { hash_position(self) }
    }

    impl ZobristHash for Atomic {
        fn zobrist_hash<V: ZobristValue>(&self) -> V { hash_position(self) }
    }

    impl ZobristHash for Horde {
        fn zobrist_hash<V: ZobristValue>(&self) -> V { hash_position(self) }
    }

    impl ZobristHash for KingOfTheHill {
        fn zobrist_hash<V: ZobristValue>(&self) -> V { hash_position(self) }
    }

    impl ZobristHash for RacingKings {
        fn zobrist_hash<V: ZobristValue>(&self) -> V { hash_position(self) }
    }
}

/// A wrapper for [`Position`] that maintains an incremental Zobrist hash.
#[derive(Debug, Clone)]
pub struct Zobrist<P, V> {
    pos: P,
    zobrist: Option<V>,
}

impl<P, V> Zobrist<P, V> {
    pub fn new(pos: P) -> Zobrist<P, V> {
        Zobrist {
            pos,
            zobrist: None,
        }
    }

    pub fn into_inner(self) -> P {
        self.pos
    }
}

impl<P: Default, V> Default for Zobrist<P, V> {
    fn default() -> Zobrist<P, V> {
        Self::new(P::default())
    }
}

impl<P: FromSetup + Position, V> FromSetup for Zobrist<P, V> {
    fn from_setup(setup: &dyn Setup, mode: CastlingMode) -> Result<Self, PositionError<Self>> {
        match P::from_setup(setup, mode) {
            Ok(pos) => Ok(Zobrist::new(pos)),
            Err(err) => Err(PositionError {
                pos: Zobrist::new(err.pos),
                errors: err.errors,
            }),
        }
    }
}

impl<P: Setup, V> Setup for Zobrist<P, V> {
    fn board(&self) -> &Board { self.pos.board() }
    fn promoted(&self) -> Bitboard { self.pos.promoted() }
    fn pockets(&self) -> Option<&Material> { self.pos.pockets() }
    fn turn(&self) -> Color { self.pos.turn() }
    fn castling_rights(&self) -> Bitboard { self.pos.castling_rights() }
    fn ep_square(&self) -> Option<Square> { self.pos.ep_square() }
    fn remaining_checks(&self) -> Option<&ByColor<RemainingChecks>> { self.pos.remaining_checks() }
    fn halfmoves(&self) -> u32 { self.pos.halfmoves() }
    fn fullmoves(&self) -> NonZeroU32 { self.pos.fullmoves() }
}

impl<P: Position + ZobristHash, V: ZobristValue> Position for Zobrist<P, V> {
    fn legal_moves(&self) -> MoveList { self.pos.legal_moves() }
    fn san_candidates(&self, role: Role, to: Square) -> MoveList { self.pos.san_candidates(role, to) }
    fn castling_moves(&self, side: CastlingSide) -> MoveList { self.pos.castling_moves(side) }
    fn en_passant_moves(&self) -> MoveList { self.pos.en_passant_moves() }
    fn capture_moves(&self) -> MoveList { self.pos.capture_moves() }
    fn promotion_moves(&self) -> MoveList { self.pos.promotion_moves() }
    fn is_irreversible(&self, m: &Move) -> bool { self.pos.is_irreversible(m) }
    fn king_attackers(&self, square: Square, attacker: Color, occupied: Bitboard) -> Bitboard { self.pos.king_attackers(square, attacker, occupied) }
    fn castles(&self) -> &Castles { self.pos.castles() }
    fn is_variant_end(&self) -> bool { self.pos.is_variant_end() }
    fn has_insufficient_material(&self, color: Color) -> bool { self.pos.has_insufficient_material(color) }
    fn variant_outcome(&self) -> Option<Outcome> { self.pos.variant_outcome() }

    fn play_unchecked(&mut self, m: &Move) {
        self.zobrist = self.zobrist.take().and_then(|value| self.pos.prepare_incremental_zobrist_hash(value, m));
        self.pos.play_unchecked(m);
        self.zobrist = self.zobrist.take().and_then(|value| self.pos.finalize_incremental_zobrist_hash(value, m));
    }
}

fn hash_board<V: ZobristValue>(board: &Board) -> V {
    let mut zobrist = V::default();
    for (sq, piece) in board.pieces() {
        zobrist ^= V::zobrist_for_piece(sq, piece);
    }
    zobrist
}

fn hash_position<P: Position, V: ZobristValue>(pos: &P) -> V {
    let mut zobrist = hash_board(pos.board());

    if pos.turn() == Color::White {
        zobrist ^= V::zobrist_for_white_turn();
    }

    let castles = pos.castles();
    if castles.has(Color::White, CastlingSide::KingSide) {
        zobrist ^= V::zobrist_for_castling_right(Color::White, CastlingSide::KingSide);
    }
    if castles.has(Color::White, CastlingSide::QueenSide) {
        zobrist ^= V::zobrist_for_castling_right(Color::White, CastlingSide::QueenSide);
    }
    if castles.has(Color::Black, CastlingSide::KingSide) {
        zobrist ^= V::zobrist_for_castling_right(Color::Black, CastlingSide::KingSide);
    }
    if castles.has(Color::Black, CastlingSide::QueenSide) {
        zobrist ^= V::zobrist_for_castling_right(Color::Black, CastlingSide::QueenSide);
    }

    if let Some(sq) = pos.ep_square() {
        zobrist ^= V::zobrist_for_en_passant_file(sq.file());
    }

    zobrist
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Chess;
    use crate::fen::Fen;

    #[test]
    fn test_polyglot() {
        let reference_values = [
            ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 0x463b96181691fc9c),
            ("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1", 0x823c9b50fd114196),
            ("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2", 0x0756b94461c50fb0),
            ("rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2", 0x662fafb965db29d4),
            ("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3", 0x22a48b5a8e47ff78),
            ("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 1 3", 0x652a607ca3f242c1),
            ("rnbq1bnr/ppp1pkpp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR w - - 2 4", 0x00fdd303c946bdd9),
            ("rnbqkbnr/p1pppppp/8/8/PpP4P/8/1P1PPPP1/RNBQKBNR b KQkq c3 0 3", 0x3c8123ea7b067637),
            ("rnbqkbnr/p1pppppp/8/8/P6P/R1p5/1P1PPPP1/1NBQKBNR b Kkq - 1 4", 0x5c3f9b829b279560),
        ];

        for (fen, expected) in reference_values {
            let pos: Chess = fen.parse::<Fen>()
                .expect("valid fen")
                .position(CastlingMode::Standard)
                .expect("legal position");

            assert_eq!(pos.zobrist_hash::<u64>(), expected, "{}", fen);
        }
    }
}

const PIECE_MASKS: [u128; 64 * 6 * 2] = [
    0x52b375aa7c0d7bac_9d39247e33776d41,
    0x208d169a534f2cf5_2af7398005aaa5c7,
    0x8981513722b47f24_44db015024623547,
    0x09b8f20f910a8ff7_9c15f73e62a76ae2,
    0x0b8ea70255209cc0_75834465489c0c89,
    0xa688a9791f027500_3290ac3a203001bf,
    0x19b88b8ffaed8f55_0fbbad1f61042279,
    0x88bf7822d00d5526_e83a908ff2fb60ca,
    0xdb7bf62ab390b71b_0d7e765d58755c10,
    0x33a6ac1d85c9f22f_1a083822ceafe02d,
    0x55ab2a27271d42ac_9605d5f0e25ec3b0,
    0x40a21ff9c803fca4_d021ff5cd13a2ed5,
    0x3c169aeb80a1d5d2_40bdf15d4a672e32,
    0x87684e27293ecf96_011355146fd56395,
    0xf8b91b39d4c6997c_5db4832046f3d9e5,
    0x1d5f744f312fd467_239f8b2d7ff719cc,
    0xeda18c452d5de5b4_05d1a1ae85b49aa1,
    0x7497db888eccda0f_679f848f6e8fc971,
    0x94c1bb7016749887_7449bbff801fed0b,
    0x35b23d663606fde2_7d11cdb1c3b7adf0,
    0x17b4ae80b8184845_82c7709e781eb7cc,
    0x8bd98922a3089d8e_f3218f1c9510786c,
    0xfec77fb07cea5e84_331478f3af51bbe6,
    0xe153a54d23c93a8a_4bb38de5e7219443,
    0xa196fa76c24405eb_aa649c6ebcfd50fc,
    0x6f333e11d079240a_8dbd98a352afd40b,
    0x23b8d480df5bb521_87d2074b81d79217,
    0x634adaec002b3000_19f3c751d3e92ae1,
    0x3d0e41d65872d549_b4ab30f062b19abf,
    0x5aba83908462b892_7b0500ac42047ac4,
    0x26457864aff288af_c9452ca81a09d85d,
    0x43f10561015da64e_24aa6c514da27500,
    0x545cc6285df42807_4c9f34427501b447,
    0xa7140dc7b82e96ef_14a68fd73c910841,
    0xb1dcadc8fe30a8d4_a71b9b83461cbd93,
    0x72ebd048ba373ac4_03488b95b0f1850f,
    0x2eb0ddf1351a1adb_637b2b34ff93c040,
    0x9cdc8c44a201836d_09d1bc9a3dd90a94,
    0x0afc1fb45a728973_3575668334a1dd3b,
    0x58c8fa415b96ec95_735e2b97a4c45a23,
    0x497a9b9a7f9f8872_18727070f1bd400b,
    0xbff840799ee05fdf_1fcbacd259bf02e7,
    0xe4ec1554316c2704_d310a7c2ce9b6555,
    0x3c9f0c8b89f31f3e_bf983fe0fe5d8244,
    0x4a601b99475baf4e_9f74d14f7454a824,
    0x6c65e1386536c3a9_51ebdc4ab9ba3035,
    0xb60a571d59e8a485_5c82c505db9ab0fa,
    0xe23c5d7045696d85_fcf7fe8a3430b241,
    0xc9d4d61b569ec607_3253a729b9ba3dde,
    0xce9ed71a6d18deb2_8c74c368081b3075,
    0x2dbc16559bdba870_b9bc6c87167c33e7,
    0x50cda5339d836c83_7ef48f2b83024e20,
    0x98091ef4f2ab1ed3_11d505d4c351bd7f,
    0xf5803ac17fc45ecf_6568fca92c76a243,
    0x09730ef15a78c687_4de0b0f40f32a7b8,
    0xf8bb209d715ab566_96d693460cc37e5d,
    0x0c5b201d6cb89a50_42e240cb63689f2f,
    0x52571fbfabb4a367_6d2bdcdae2919661,
    0x1b1db82269890861_42880b0236e4d951,
    0x9423f70ed512f1ea_5f0f4a5898171bb6,
    0x79e448c72183e2a5_39f890f579f92f88,
    0x3c88a0cf5b852900_93c5b5f47356388b,
    0x9e12f819acaa6653_63dc359d8d231b78,
    0xc6f09266299a5902_ec16ca8aea98ad76,
    0x3e8cad2210fce3f3_5355f900c2a82dc7,
    0xa3868eff53346da1_07fb9f855a997142,
    0x61de5496186b0d70_5093417aa8a7ed5e,
    0xe17a09f0e53bc940_7bcbc38da25a7f3c,
    0xe0ffe83afe44ec11_19fc8a768cf4b6d4,
    0xf35a5e3184c1c980_637a7780decfc0d9,
    0x83390d9b2e7563a6_8249a47aee0e41f7,
    0x950f14737ed6be5b_79ad695501e7d1e8,
    0x6df42fcfa743809d_14acbaf4777d5776,
    0x0f2b1872ba3fef30_f145b6beccdea195,
    0x04171b94f58c5d2e_dabf2ac8201752fc,
    0x78b05fea0dc77c38_24c3c94df9c8d3f6,
    0xa76ddf41aa675504_bb6e2924f03912ea,
    0xe634bc8f87d0fe75_0ce26c0b95c980d9,
    0x2dbf77a8851237de_a49cd132bfbf7cc4,
    0x10f9b7d996836741_e99d662af4243939,
    0x26f6547bb0471fb0_27e6ad7891165c3f,
    0xf727c06db8bb34e0_8535f040b9744ff1,
    0x28984171f866b615_54b3f4fa5f40d873,
    0x349d245078c312ef_72b12c32127fed2b,
    0x99f8f1ab94a13206_ee954d3c7b411f47,
    0x967d7e5e99566e67_9a85ac909a24eaa1,
    0x470fda103f9476cc_70ac4cd9f04f21f5,
    0x37dad4fcdedc6db8_f9b89d3e99a075c2,
    0x99f91b1cd65c50f0_87b3e2b2b5c907b1,
    0x6d89c29cb7034aef_a366e5b8c54f48b8,
    0x824c9daa114a11c7_ae4a9346cc3f7cf2,
    0xf3b1ee14505939c6_1920c04d47267bbd,
    0x2fa0d39cbee05ced_87bf02c6b49e2ae9,
    0xc0c43ea8a642a49c_092237ac237f3859,
    0x4824a871bca34e17_ff07f64ef8ed14d0,
    0x394b500f07f96989_8de8dca9f03cc54e,
    0x7c6efb4dc9bea9d9_9c1633264db49c89,
    0xca09213bfeb36c6a_b3f22c3d0b0b38ed,
    0x0069832f9f2bd0b5_390e5fb44d01144b,
    0xf092a01d0d4420da_5bfea5b4712768e9,
    0x6952e2015db39c5a_1e1032911fa78984,
    0xf3993e2a4cdf615e_9a74acb964e78cb3,
    0x7d3ddc6bef2ed6f2_4f80f7a035dafb04,
    0xa7040db38e233bac_6304d09a0b3738c4,
    0xa8b59fe4836ffc08_2171e64683023a08,
    0xc55dbb54360414a9_5b9b63eb9ceff80c,
    0x3e24465359dc03c0_506aacf489889342,
    0xd27a416ed84cc3b7_1881afc9a3a701d6,
    0x7a77677e0de620c4_6503080440750644,
    0x30cacd32e0313d3b_dfd395339cdbf4a7,
    0xdc6952fd3e61c11a_ef927dbcf00c20f2,
    0x08ab1642b5129e01_7b32f7d1e03680ec,
    0xaa8e0962b8eebcdc_b9fd7620e7316243,
    0x6dda36bacc3b2e2c_05a7e8a57db91b77,
    0xba82f4e2cb60b43e_b5889c6e15630a75,
    0x509da4ba5295c4a5_4a750a09ce9573f7,
    0x36a18fa38c3d74c6_cf464cec899a2f8a,
    0xb9e5652481a6df69_f538639ce705b824,
    0x5f4946b7dd41d1c7_3c79a0ff5580ef7f,
    0xfd7a4fb7bfe1d23d_ede6c87f8477609d,
    0x32e0b2b68ea83031_799e81f05bc93f31,
    0xf25fcb24f0c19623_86536b8cf3428a8c,
    0xa317676dc1eb8797_97d7374c60087b73,
    0xf754b557c09ae146_a246637cff328532,
    0x5bbd920fffe5fa7a_043fcae60cc0eba0,
    0x5b3c978e296d2280_920e449535dd359e,
    0xca9c1ea34fc1484f_70eb093b15b290cc,
    0x470c408e3b3d2dc5_73a1921916591cbd,
    0x6a0772093f97e152_56436c9fe1a1aa8d,
    0xfa76d36719e7e5e3_efac4b70633b8f81,
    0x2e799233a544062a_bb215798d45df7af,
    0xe003451144a03be8_45f20042f24f1768,
    0x974d8f4ee692ed35_930f80f4e8eb7462,
    0xd30afadfc4dc52f5_ff6712ffcfd75ea1,
    0x278dde02bf30c1da_ae623fd67468aa70,
    0x8b7e3a2bf5a061a3_dd2c5bc84bc8d8fc,
    0xd1443752e511a579_7eed120d54cf2dd9,
    0xd1b02672a0ec44cf_22fe545401165f1c,
    0xba3005c8512514f1_c91800e98fb99929,
    0x2e3b86211f6b4295_808bd68e6ac10365,
    0x057431bfeaa9d6f5_dec468145b7605f6,
    0xa348ddd66378afaf_1bede3a3aef53302,
    0x4a1817775b086ce1_43539603d6c55602,
    0x184b1c983a6a1a77_aa969b5c691ccb7a,
    0xcb70392e7b7db185_a87832d392efee56,
    0x3b1b1166a648330e_65942c7b3c7e11ae,
    0xe5201b155f51cc30_ded2d633cad004f6,
    0x4d053ee865f21b96_21f08570f420e565,
    0xd8d8062e343d9c66_b415938d7da94e3c,
    0x13507b31a966da7d_91b859e59ecb6350,
    0xd637536d2e7a58b0_10cff333e0ed804a,
    0xcbcae035eab824a0_28aed140be0bb7dd,
    0x8c77fe1e1b06691b_c5cc1d89724fa456,
    0xfd7841ed1ab4b961_5648f680f11a2741,
    0xcaa88da017669d53_2d255069f0b7dab3,
    0xabda5baf650b3675_9bc5a38ef729abd4,
    0xfe14d0c6b8f11e97_ef2f054308f6a2bc,
    0xc13423f81c4b9adf_af2042f5cc5c2858,
    0x34507a3503935243_480412bab7f5be2a,
    0xec504bd0c7ae79a1_aef3af4a563dfe43,
    0x0bc761ea4004d2ae_19afe59ae451497f,
    0x3a0748078a78fd4d_52593803dff1e840,
    0xc5f36bde8caa93fe_f4f076e65f2ce6f0,
    0x5b2299dc44080278_11379625747d5af3,
    0x3f99cbeb6ec653fa_bce5d2248682c115,
    0x48ebcfc004b524ca_9da4243de836994f,
    0xd2278829cd344d05_066f70b33fe09017,
    0x5fe637e58fc1c0f3_4dc4de189b671a1c,
    0x0a4b136f25a65a32_51039ab7712457c3,
    0x4119314b520d04d9_c07a3f80c31fb4b4,
    0x5354a8b08947cc8e_b46ee9c5e64a6e7c,
    0x6001d6a94517300b_b3819a42abe61c87,
    0x14597a074f133855_21a007933a522a20,
    0xdc9a6baf92ffde03_2df16f761598aa4f,
    0xc5cbc5270de986b0_763c4a1371b368fd,
    0x95c72d49bd7560be_f793c46702e086a0,
    0x12b437e4c286737a_d7288e012aeb8d31,
    0xaa7c6f89f1442c5d_de336a2a4bc1c44b,
    0x1a3ebbf317bfc4d8_0bf692b38d079f23,
    0x4ad3c9fa863a5aa3_2c604a7a177326b3,
    0xc7c94147de663b5b_4850e73e03eb6064,
    0x840e7fe4b35d4a4b_cfc447f1e53c8e1b,
    0x8921109126e23341_b05ca3f564268d99,
    0xa18a4f12c127de17_9ae182c8bc9474e8,
    0x471973e4dc6efb4b_a4fc4bd4fc5558ca,
    0xc723867b98d07330_e755178d58fc4e76,
    0xf5fcc6de350950d1_69b97db1a4c03dfe,
    0xb90913e02bde576a_f9b5b7c4acc67c96,
    0x5554c92f272b73c5_fc6a82d64b8655fb,
    0x3738a3f0fdf5d9c6_9c684cb6c4d24417,
    0x3771f25e5e278ee3_8ec97d2917456ed0,
    0xa9d58a812b10906e_6703df9d2924e97e,
    0x2814f2a19d8670eb_c547f57e42a7444e,
    0x5ced6d617e9d6b4d_78e37644e7cad29e,
    0x69be27bdc682e06a_fe9a44e9362f05fa,
    0x945e3cd54c7a41f4_08bd35cc38336615,
    0xfac825c29c4e52fc_9315e5eb3a129ace,
    0x95c380633671f3c0_94061b871e04df75,
    0xb1f0f11b309f849f_df1d9f9d784ba010,
    0x36b7ac17862bc4ac_3bba57b68871b59d,
    0x8b89835b5e731ac5_d2b7adeeded1f73f,
    0x122138b676fc6561_f7a255d83bc373f8,
    0xce3107b858b368ea_d7f4f2448c0ceb81,
    0xaa14dd3733de4203_d95be88cd210ffa7,
    0xec4ea6805a8dad1e_336f52f8ff4728e7,
    0xce5cd5938049dcf0_a74049dac312ac71,
    0x21156227c06a4b0b_a2f61bb6e437fdb5,
    0xda13d541802c4d5e_4f2a5cb07f6a35b3,
    0xfbf03d5c9f783bb2_87d380bda5bf7859,
    0xe512fa9fd5c68b8a_16b9f7e06c453a21,
    0x30bd781b22277dcd_7ba2484c8a0fd54e,
    0x56625e22aef316ec_f3a678cad9a2e38c,
    0xed75251a71024db6_39b0bf7dde437ba2,
    0xb468dc45b39cde2f_fcaf55c1bf8a4424,
    0x68b33836bea9a0a0_18fcf680573fa594,
    0x3187565f03cc0d85_4c0563b89f495ac3,
    0x9bbc591ddc43447f_40e087931a00930d,
    0xc53a29458191d2db_8cffa9412eb642c1,
    0x6bc263803d691ec8_68ca39053261169f,
    0x04cca68628858bac_7a1ee967d27579e2,
    0xa20a13cffa4679d1_9d1d60e5076f5b6f,
    0x85725a1e096e1abf_3810e399b6f65ba2,
    0xbc986393043f78d5_32095b6d4ab5f9b1,
    0x0fa47125507ccb12_35cab62109dd038a,
    0xc2c27b60c8b2ce36_a90b24499fcfafb1,
    0x217520a809c97da6_77a225a07cc2c6bd,
    0x552ad48c96617c16_513e5e634c70e331,
    0x758c0637401144ae_4361c0ca3f692f12,
    0xf1ae50d591aeb10f_d941aca44b20a45b,
    0x0c127280b89240a3_528f7c8602c5807b,
    0xa9a8cd5ddd7737b0_52ab92beb9613989,
    0x8506683f3e28c050_9d1dfa2efc557f73,
    0x8105b0573483941f_722ff175f572c348,
    0xd00bcf6974e8788c_1d1260a51107fe97,
    0x3311a2a4e61fc638_7a249a57ec0c9ba2,
    0x5b31cba035ff4f50_04208fe9e8f7f2d6,
    0x9ef049141a01e743_5a110c6058b920a0,
    0x3355b7a63e03cf20_0cd9a497658a5698,
    0x78bf716c2f94ffcf_56fd23c8f9715a4c,
    0x232304d6a359676e_284c847b9d887aae,
    0xffeebdd04f15816e_04feabfbbdb619cb,
    0x594fdc90c434a4fd_742e1e651c60ba83,
    0xba5cc088b72c0942_9a9632e65904ad3c,
    0x036efdc30e389de2_881b82a13b51b9e2,
    0x5038b23c9af174d2_506e6744cd974924,
    0x9b5e64f304474d48_b0183db56ffc6a79,
    0x280a4b8c73c2e8d8_0ed9b915c66ed37e,
    0xfda076be88bcc507_5e11e86d5873d484,
    0xafc896ae852c60c2_f678647e3519ac6e,
    0xbe903340939e63fd_1b85d488d0f20cc5,
    0x7a97bd60aba4c349_dab9fe6525d89021,
    0xf62e51f178597cf9_0d151d86adb73615,
    0x8f9ab42711b663dc_a865a54edcc0f019,
    0xc8d003d119dcac63_93c42566aef98ffb,
    0xef2101c32adaed38_99e7afeabe000731,
    0xdde81906502ad1b0_48cbff086ddf285a,
    0x149756bc21368632_7f9b6af1ebf78baf,
    0x25c80f323a516eaa_58627e1a149bba21,
    0x3ea039f7ff28ae8e_2cd16e2abd791e33,
    0x0caf481f40063dd8_d363eff5f0977996,
    0xbce23e106b1eefd7_0ce2a38c344a6eed,
    0x10853ea82a5ccb34_1a804aadb9cfa741,
    0xe7c76ac3dbbf8c8c_907f30421d78c5de,
    0x1624c0ce1532313d_501f65edb3034d07,
    0x5f3895b25d7b4744_37624ae5a48fa6e9,
    0xfbe363cbb55a913e_957baf61700cff4e,
    0x35850e8f63400ddd_3a6c27934e31188a,
    0x3d300047b5ddde66_d49503536abca345,
    0x1c1c7ca8b3386353_088e049589c432e0,
    0x986ec52ac2c88cec_f943aee7febf21b8,
    0xc93b616a554d23c8_6c3b8e3e336139d3,
    0x211d7b5759da7504_364f6ffa464ee52e,
    0xf2663fc59b541585_d60f6dcedc314222,
    0xf57fefeadb21b029_56963b0dca418fc0,
    0x30fd60d9ee260966_16f50edf91e513af,
    0x3c29da000d5b9a08_ef1955914b609f93,
    0xd0d6203fa69da0ba_565601c0364e3228,
    0x8167e4bd87c6f05e_ecb53939887e8175,
    0x5c063405c62f8154_bac7a9a18531294b,
    0xb86fe57d53081fe6_b344c470397bba52,
    0xeb60ad080cd573fe_65d34954daf3cebd,
    0xbfbbb41602635b78_b4b81b3fa97511e2,
    0x4e39d536b723213c_b422061193d6f6a7,
    0x56d7e0468df15a47_071582401c38434d,
    0x9e601537348ed732_7a13f18bbedc4ff5,
    0xee2e827d9faa74c1_bc4097b116c524d2,
    0xe43302e0d517a316_59b97885e2f2ea28,
    0xf662e9ba781a1fae_99170a5dc3115544,
    0xe83da2efce442856_6f423357e7c6a9f9,
    0x143ae097749a513a_325928ee6e6f8794,
    0xa203386e6a86f7c7_d0e4366228b03343,
    0x73905f8c5056ecee_565c31f7de89ea27,
    0x0da07ce44c0142e4_30f5611484119414,
    0x8b9e97003ef01d2e_d873db391292ed4f,
    0xd8c666a665840842_7bd94e1d8e17debc,
    0x8bb1069bba169263_c7d9f16864a76e94,
    0x6bdc866d7daa19dc_947ae053ee56e63c,
    0xe1115bb3f8ad0cfe_c8c93882f9475f5f,
    0x0859ae34a51ed77c_3a9bf55ba91f81ca,
    0xf1d73663c53a0156_d9a11fbb3d9808e4,
    0x669283df212c93db_0fd22063edc29fca,
    0x7489abc08bd4db15_b3f256d8aca0b0b9,
    0xf9d2b26d0375aab0_b03031a8b4516e84,
    0xde4856e7777e27d1_35dd37d5871448af,
    0x2caeaf61386fa1f2_e9f6082b05542e4e,
    0x7c6d4b00383f052a_ebfafa33d7254b59,
    0xb1943df6ea3687ff_9255abb50d532280,
    0x7e4d1baca94da20d_b9ab4ce57f2d34f3,
    0x38d1a6b6448fdc40_693501d628297551,
    0x8aed53051756d212_c62c58f97dd949bf,
    0x1805fa7482c60f4e_cd454f8f19c5126a,
    0x24c9891c2f4db0b1_bbe83f4ecc2bdecb,
    0xbb703190b30eb664_dc842b7e2819e230,
    0xb52f857f41e68fce_ba89142e007503b8,
    0xeb5a7a714d4ec1a1_a3bc941d0a5061cb,
    0xc41334a36d4211ea_e9f6760e32cd8021,
    0xe60188ecb537010d_09c7e552bc76492f,
    0xff67dd932e5755cc_852f54934da55cc9,
    0x14c92be552467cfb_8107fccf064fcf56,
    0xc94fb8eae42b3453_098954d51fff6580,
    0x0bd007042735acc6_23b70edb1955c4bf,
    0xb9dd82debb9abd3d_c330de426430f69d,
    0x3312b675a5bc5dfb_4715ed43e8a45c0a,
    0xd2e9447e6c6d3509_a8d7e4dab780a08d,
    0xd4d771a89c91beb6_0572b974f03ce0bb,
    0x31d0724183248884_b57d2e985e1419c7,
    0xe7229acdb568bc00_e8d9ecbe2cf3d73f,
    0xb4ef94e8251ecb66_2fe4b17170e59750,
    0xac817cad3cfb00ed_11317ba87905e790,
    0x3fe869890b69552c_7fbf21ec8a1f45ec,
    0x91aec362daa37fcd_1725cabfcb045b00,
    0xfb25f88ca887ca77_964e915cd5e2b207,
    0x053bff886db0847c_3e2b8bcbf016d66d,
    0x991fd5641b666e80_be7444e39328a0ac,
    0x24fc37ad820ed73a_f85b2b4fbcde44b7,
    0x2b2bcac1fa28086e_49353fea39ba63b1,
    0x4c1a3a2190e08f26_1dd01aafcd53486a,
    0x8e718591cf07851e_1fca8a92fd719f85,
    0xb14fadbd3baa703f_fc7c95d827357afa,
    0x8f1eb8cc7eedd98e_18a6a990c8b35ebd,
    0x89ed662f01bcb2fd_cccb7005c6b9c28d,
    0xa263fa3b9a325a8f_3bdbb92c43b17f26,
    0xbae9f4e14d09637c_aa70b5b4f89695a2,
    0x6be076b52945a007_e94c39a54a98307f,
    0xd264830e7dc7f906_b7a0b174cff6f36e,
    0xc26059b78ed6854f_d4dba84729af48ad,
    0x148dca9a9b0c8474_2e18bc1ad9704a68,
    0x9749c69073bafeb8_2de0966daf2f8b1c,
    0xbba6f4662b0cfd3c_b9c11d5b1e43a07e,
    0x620103f01e5b63f8_64972d68dee33360,
    0x4c7820f950a4c583_94628d38d0c20584,
    0xe1262fa8ff1d3269_dbc0d2b6ab90a559,
    0x8f5121c2873029ef_d2733c4335c6a72f,
    0x4fb3edb54d507b36_7e75d99d94a70f4d,
    0xf8594c470632ebb6_6ced1983376fa72b,
    0xb6e876e78ecf5164_97fcaacbf030bc24,
    0xafeb0a5d807150f5_7b77497b32503b12,
    0xf651bea4c88fcaae_8547eddfb81ccb94,
    0xbfbce123f03177da_79999cdff70902cb,
    0xb6aa0fd22855e81c_cffe1939438e9b24,
    0xa240adf54d70b24e_829626e3892d95d7,
    0x732ea6db834bf5a4_92fae24291f2b3f1,
    0xb47231e07ae8b35f_63e22c147b9c3403,
    0x80554c039ab7af15_c678b6d860284a1c,
    0xdb2e83297a30b541_5873888850659ae7,
    0xd58b2a396b5a1669_0981dcd296a8736d,
    0x2151156aaffdf4b7_9f65789a6509a440,
    0x65479a629704845e_9ff38fed72e9052f,
    0xa9bed81039cf1c6d_e479ee5b9930578c,
    0xd1a3f98b97eea710_e7f28ecd2d49eecd,
    0xff78a5d72aa18fe3_56c074a581ea17fe,
    0x96d1a9830ba7ffd3_5544f7d774b14aef,
    0xe19f501dcdf116db_7b3f0195fc6f290f,
    0x8e68f253a278535d_12153635b2c0cf57,
    0xdc75c481b2cdc0fa_7f5126dbba5e0ca7,
    0x7162fa408d9042fd_7a76956c3eafb413,
    0x9fadef0bce1a7da1_3d5774a11d31ab39,
    0x5421bbff426d4e84_8a1b083821f40cb4,
    0xe7944beeb699c0e7_7b4a38e32537df62,
    0x2ed8b2db03799071_950113646d1d6e03,
    0xfc4e188df10d5454_4da8979a0041e8a9,
    0x9de7df26c1457c8f_3bc36e078f7515d7,
    0x8fd73517b47f22c8_5d0a12f27ad310d1,
    0xa3005e22b1bd3e53_7f9d1a2e1ebe1327,
    0x2bb2c23c899cbc9e_da3a361b1c5157b1,
    0xbfb0766d26526dbf_dcdd7d20903d0c25,
    0x166dbdc38309e26b_36833336d068f707,
    0x6796f7e11a4c3cba_ce68341f79893389,
    0x562c093e14a87ff2_ab9090168dd05f34,
    0x3627e637e2413092_43954b3252dc25e5,
    0x240414702e63067a_b438c2b67f98e5e9,
    0xa6f63494f774323c_10dcd78e3851a492,
    0xc19ea801fb344afb_dbc27ab5447822bf,
    0x9d18e8c21a6c8c61_9b3cdb65f82ca382,
    0x87f9503f9fded941_b67b7896167b4c84,
    0x3e5098f627c4ed6c_bfced1b0048eac50,
    0x6eb063443f58c2ae_a9119b60369ffebd,
    0x3427200616f65462_1fff7ac80904bf45,
    0xe3b74fe55c76691f_ac12fb171817eee7,
    0xd6316d5008f932ec_af08da9177dda93d,
    0x35a0d4ca7416842f_1b0cab936e65c744,
    0xde4683286c56072d_b559eb1d04e5e932,
    0x158754657dc0f21d_c37b45b3f8d6f2ba,
    0x6d808b472208eab2_c3a9dc228caac9e9,
    0xcb208f7c937f44e6_f3b8b6675a6507ff,
    0xdff1dc38532c3a2e_9fc477de4ed681da,
    0x64ee6958558a5d83_67378d8eccef96cb,
    0xf103b408d1245a6d_6dd856d94d259236,
    0xa1f19c518c3e0b41_a319ce15b0b4db31,
    0x48f0c2ff42819bfd_073973751f12dd5e,
    0x1472ba925a4c6123_8a8e849eb32781a5,
    0xe3b750660989609a_e1925c71285279f5,
    0x6dd760ab49ed7373_74c04bf1790c0efe,
    0x67926c593a78bcaa_4dda48153c94938a,
    0x5978ff009a18007c_9d266d6a1cc0542c,
    0x5568e6bf328b448e_7440fb816508c4fe,
    0x7cc90fbed1165f2b_13328503df48229f,
    0x156f28d728f97cba_d6bf7baee43cac40,
    0x2edf603ec74b4900_4838d65f6ef6748f,
    0xd48f299033fa9c9a_1e152328f3318dea,
    0x543751766a18d326_8f8419a348f296bf,
    0x04f25bdd180f31cb_72c8834a5957b511,
    0xc66a3569f903b0dc_d7a023a73260b45c,
    0xce61b42c7eead35c_94ebc8abcfb56dae,
    0xa705d68144caaf00_9fc10d0f989993e0,
    0x3371ede2968498fb_de68a2355b93cae6,
    0x1c0c9a220f8fbf8a_a44cfe79ae538bbe,
    0x6631fc26158faebd_9d1d84fcce371425,
    0xef0ec337ff2aef59_51d2b1ab2ddfb636,
    0xc979ef8243e71d7a_2fd7e4b9e72cd38c,
    0xd6c1a70601c91112_65ca5b96b7552210,
    0xf3a1da1866141057_dd69a0d8ab3b546d,
    0xd3e2b4c698f2a99e_604d51b25fbf70e2,
    0x1b4f5d5760ac5121_73aa8a564fb7ac9e,
    0x27d0b28f28e7d0ef_1a8c1e992b941148,
    0xf3a2d309d1e72bc0_aac40a2703d9bea0,
    0x1294c73b3f914dda_764dbeae7fa4f3a6,
    0x56dab15dc8b3fc48_1e99b96e70a9be8b,
    0x5b77d3202095d45c_2c5e9deb57ef4743,
    0xe0d317e26a17ae47_3a938fee32d29981,
    0x5b1d671e17069897_26e6db8ffdf5adfe,
    0x937dc438ef99030b_469356c504ec9f9d,
    0xce09ea57087ebea9_c8763c5b08d1908c,
    0xd0d8fac3d4cfa048_3f6c6af859d80055,
    0x27a5886862d56b94_7f7cc39420a3a545,
    0x591673661c00d80b_9bfb227ebdf4c5ce,
    0xd1cc44ae71a9791d_89039d79d6fc5c5c,
    0xdf5a715ada209d36_8fe88b57305e2ab6,
    0x491171944e677dae_a09e8c8c35ab96de,
    0xd0cf92367e04163c_fa7e393983325753,
    0x67b6a5b051ce8d5c_d6b6d0ecc617c699,
    0x88d41953044d621e_dfea21ea9e7557e3,
    0xb93bcd9dd369fe49_b67c1fa481680af8,
    0x79999db57b235430_ca1e3785a9e724e5,
    0x20bad52dd13a90d4_1cfc8bed0d681639,
    0x4cccc5ae16a29dd2_d18d8549d140caea,
    0xa0615c69803b77c7_4ed0fe7e9dc91335,
    0x2e2cb0938ba801a0_e4dbf0634473f5d2,
    0x7ba91914cab50528_1761f93a44d5aefe,
    0x2738873108dcba8a_53898e4c3910da55,
    0x4502a97899131230_734de8181f6ec39a,
    0x38108697800e8bb5_2680b122baa28d97,
    0xa46ede145d66a90b_298af231c85bafab,
    0xe99d73feedfd98b3_7983eed3740847d5,
    0x3267a96bed604a38_66c1a2a1a60cd889,
    0xad66f2c81cc3fc42_9e17e49642a3e4c1,
    0xfa2de5ba2d8c3693_edb454e7badc0805,
    0xd4ca1c86d116bcbd_50b704cab602c329,
    0x525f7774ee1dde6b_4cc317fb9cddd023,
    0xa8e346682e2883b9_66b4835d9eafea22,
    0xae03d84b90df4cb2_219b97e26ffc81bd,
    0xd12735c3d08e24d9_261e4e4c0a333a9d,
    0xb737b467cee71d3a_1fe2cca76517db90,
    0x36970fe334b2a37e_d7504dfa8816edbb,
    0xd5c73db7872be26f_b9571fa04dc089c8,
    0x8aeb0ed56a4177fa_1ddc0325259b27de,
    0x0199f29dbf7a1802_cf3f4688801eb9aa,
    0x1caba957a1ff78f0_f4f5d05c10cab243,
    0x2abfd2ecbf62492e_38b6525c21a42b0e,
    0xadd370c3cd316a3e_36f60e2ba4fa6800,
    0x08a307d218ffbcbf_eb3593803173e0ce,
    0x7bf02813994b261f_9c4cd6257c5a3603,
    0x894290366274ef43_af0c317d32adaa8a,
    0xc0821c96582294b4_258e5a80c7204c4b,
    0xbd2ce07a63da7db1_8b889d624d44885d,
    0x03ba38df61dba3a6_f4d14597e660f855,
    0x4bc8ce1e706bb08d_d4347f66ec8941c3,
    0x7dca263ea9024a3c_e699ed85b0dfb40d,
    0x4e876b140c9cda33_2472f6207c2d0484,
    0x5bdabc35a2fa1b4e_c2a1e7b5b459aeb5,
    0x383a46ece18c27c4_ab4f6451cc1d45ec,
    0x532ee826c31a9b81_63767572ae3d6174,
    0x84f3d8faecfd7924_a59e0bd101731a28,
    0xba905cc371f066d9_116d0016cb948f09,
    0x3da882318279b416_2cf9c8ca052f6e9f,
    0xb3566f6dccae32ed_0b090a7560a968e3,
    0x6eb13ec5ca4cb179_abeeddb2dde06ff1,
    0xab38266303f672c0_58efc10b06a2068d,
    0xc5e23f8698084689_c6e57a78fbd986e0,
    0x502fe8a1ce0a1bc6_2eab8ca63ce802d7,
    0x4664fe5154093ec2_14a195640116f336,
    0x3e39d0fefb4ffaf8_7c0828dd624ec390,
    0x4162ffb2b58aed88_d74bbe77e6116ac7,
    0x5e1e505c7c916883_804456af10f5fb53,
    0x2185b1f34d275173_ebe9ea2adf4321c7,
    0xdb898d0487271365_03219a39ee587a30,
    0xddb7e20f4f0b0a5f_49787fef17af9924,
    0xe39ecdbb80830b57_a1e9300cd8520548,
    0xd1e12d09eb3d6c76_5b45e522e4b1b4ef,
    0xf33de05912951acf_b49c3b3995091a36,
    0x7ddca4e7cf6a2022_d4490ad526f14431,
    0x37dae73934d2b45c_12a8f216af9418c2,
    0xddf7d6c08847b906_001f837cc7350524,
    0x76a4f4c4bd5b4dc4_1877b51e57a764d5,
    0xde916ef3cda04b7a_a2853b80f17f58ee,
    0xf79a5f06f0548dcf_993e1de72d36d310,
    0x7980b63a972639bc_b3598080ce64a656,
    0xc327c42efcd3dcb0_252f59cf0d9f04bb,
    0x8ae99f5a13771265_d23c8e176d113600,
    0xaeb96c60887bf568_1bda0492e7e4586e,
    0x0262cf9cfb1be6f7_21e0bd5026c619bf,
    0x96f28db3af9f8eaf_3b097adaf088f94e,
    0xbd74fc2e2cd130ed_8d14dedb30be846e,
    0xbfb40b3bf2130455_f95cffa23af5f6f4,
    0x0d27390428f909cc_3871700761b3f743,
    0xe64118d41047bff4_ca672b91e9e4fa16,
    0x9f175874ee74dfc0_64c8e531bff53b55,
    0x3c41278759da9b49_241260ed4ad1e87d,
    0xce243a587f0b6bbd_106c09b972d2e822,
    0x19790b6cab0ef9bc_7fba195410e5ca30,
    0x1b7bb956b20e0ff1_7884d9bc6cb569d8,
    0xfcd55d2a3d92556c_0647dfedcd894a29,
    0x4f6a491571d2a627_63573ff03e224774,
    0x2a4bd439b9085684_4fc8e9560f91b123,
    0xb8eb5b22d497aa9d_1db956e450275779,
    0xf6cb137e88679a76_b8d91274b9e9d4fb,
    0x22bc990578e7dd2c_a2ebee47e2fbfce1,
    0x01fc46a5af41ba72_d9f1f30ccd97fb09,
    0x9ee540b9ce7e922a_efed53d75fd64e6b,
    0x5a823d7da29de33e_2e6d02c36017f67f,
    0xdcb54247ed750238_a9aa4d20db084e9b,
    0xbbe346044327e21a_b64be8d8b25396c1,
    0x3905a0daabfe04e3_70cb6af7c2d5bcf0,
    0x1670290ab9118147_98f076a4f7a2322e,
    0x68163cb777eab10d_bf84470805e69b5f,
    0xed240589e171a9a1_94c3251f06f90cf3,
    0x2281ad67174bb66b_3e003e616a6591e9,
    0x579ed522cb20efa0_b925a6cd0421aff3,
    0x9c319f6b6fd2554a_61bdd1307c66e300,
    0x0dd4faaafa80e520_bf8d5108e27e0d48,
    0x74bbae7f87e860f9_240ab57a8b888b20,
    0xdff318b4d732a759_fc87614baf287e07,
    0xc371ee9d7d2af132_ef02cdd06ffdb432,
    0x76cc4be2658dda80_a1082c0466df6c0a,
    0xfd9e506d75af7df5_8215e577001332c8,
    0xc5a0d6e02e703f74_d39bb9c3a48db6cf,
    0x631701e42e35f5cf_2738259634305c14,
    0x3055402095f743c0_61cf4f94c97df93d,
    0x37e89e5deff34268_1b6baca2ae4e125b,
    0xcc01cd9d386a49cb_758f450c88572e0b,
    0x04a328251bd5828f_959f587d507a8359,
    0xc8dc0ff4362a8c8f_b063e962e045f54d,
    0xee814f9b3bce7af1_60e8ed72c0dff5d1,
    0x57ae92f6b7f094ef_7b64978555326f9f,
    0x0dbc0ba7bb1c2445_fd080d236da814ba,
    0xec57a417b3ae8ad1_8c90fd9b083f4558,
    0x08d8ec9d3a3a3a85_106f72fe81e2c590,
    0x18a2e3daa4c6bbe3_7976033a39f7d952,
    0x68f8161c35388cf9_a4ec0132764ca04b,
    0xe6e7cc9733d7aa2f_733ea705fae4fa77,
    0x206c16a493a194f6_b4d8f77bc3e56167,
    0x221153dec7e37554_9e21f4f903b33fd9,
    0xd110651aa3d83db7_9d765e419fb69f6d,
    0x841c207674a2a59d_d30c088ba61ea5ef,
    0xc09b4da25303cd0e_5d94337fbfaf7f5b,
    0xe48fec76c3d485e3_1a4e4822eb4d7a59,
    0x4c37f28895f6b9e0_6ffe73e81b637fb3,
    0x97fe8d6e8425ea84_ddf957bc36d8b9ca,
    0xe13176251539f9db_64d0e29eea8838b3,
    0x4e887953d43713f5_08dd9bdfd96b9f63,
    0x32dc49e0f8b96e5c_087e79e5a57d1d13,
    0x1240f0315707df84_e328e230e3e2b3fb,
    0xe22f60c878f03163_1c2559e30f0946be,
    0x733ede4131a1662d_720bf5f26f4d2eaa,
    0x5c82e1f58657d112_b0774d261cc609db,
    0x6b39b17300529a95_443f64ec5a371195,
    0x9a366e73434c37a3_4112cf68649a260e,
    0x3af04822ac63ca77_d813f2fab7f5c5ca,
    0x6c20030dcbb3e153_660d3257380841ee,
    0x812bc00a41d9de43_59ac2c7873f910a3,
    0x9d698757f729c30b_e846963877671a17,
    0x764407e6839c724e_93b633abfa3469f8,
    0x347b8bcf7aa8a816_c0c0f5a60ef4cdcf,
    0xa15cbc1e73577012_caf21ecd4377b28c,
    0x8387a4b08cc28a2e_57277707199b8175,
    0xce827a97fe830269_506c11b9d90e8b1d,
    0xfc3677ad21589c7a_d83cc2687a19255f,
    0xc902d33e0a464ec1_4a29c6465a314cd1,
    0xfb39294699a1e2aa_ed2df21216235097,
    0xd7bf168e258f8f01_b5635c95ff7296e2,
    0x2e602229d2b37a22_22af003ab672e811,
    0x821bc37e2c1f8912_52e762596bf68235,
    0xc57a1ba0f260acc5_9aeba33ac6ecc6b0,
    0xce78cb667346e38c_944f6de09134dfb6,
    0xbb39eae7a290fcb0_6c47bec883a7de39,
    0x1bbc42a45ca20618_6ad047c430a12104,
    0xef37e286f590dcfb_a5b1cfdba0ab4067,
    0xd9708ed7ec641deb_7c45d833aff07862,
    0x12c5e2f484724605_5092ef950a16da0b,
    0xf3e85d7b8a77b033_9338e69c052b8e7b,
    0x8ee17c0021438904_455a4b4cfe30e3f5,
    0x462f7fba7c2868f2_6b02e63195ad0cf8,
    0xfb7a14d6137fa5b1_6b17b224bad6bf27,
    0x99ceec99465fe08c_d1e0ccd25bb9c169,
    0xa4be0f0017879351_de0c89a556b9ae70,
    0x48c9c74efaeff5a7_50065e535a213cf6,
    0x682f784ba23dc02e_9c1169fa2777b874,
    0x226e96a540113353_78edefd694af1eed,
    0xaa04b725850e2e32_6dc93d9526a50e68,
    0xbc3338f53303ff19_ee97f453f06791ed,
    0x94d80fc8e2559d54_32ab0edb696703d3,
    0xb470b50e6769b2e4_3a6853c7e70757a7,
    0x9d5caf79d12f737c_31865ced6120f37d,
    0x0335ce6790cce15a_67fef95d92607890,
    0xa9d88c05289cbd6d_1f2b1d1f15f6dc9c,
    0xf3d1d29b153dbd5e_b69e38a8965c6b65,
    0x4104764512849057_aa9119ff184cccf4,
    0x1d6d02af20051f53_f43c732873f24c13,
    0xc8e487152b256129_fb4a3d794a9a80d2,
    0xaf60d64fbda8f2ce_3550c2321fd6109c,
    0xbf6d091a05417402_371f77e76bb8417e,
    0xda184d8ff54f3fc1_6bfa9aae5ec05779,
    0xb83657ca6633e2ac_cd04f3ff001a4778,
    0x75d3e0b8feedbdb7_e3273522064480ca,
    0x26a58648fd56deab_9f91508bffcfc14a,
    0x5da2dbf10323e25c_049a7f41061a9e60,
    0x1e3ed1a390724f66_fcb6be43a9f2fe9b,
    0xc76159619f3af003_08de8a1c7797da9b,
    0xb4af22b5f5b26389_8f9887e6078735a1,
    0x466792d7a2e6c6fa_b5b4071dbfc73a66,
    0x66feaacd911b81a2_230e343dfba08d33,
    0x94c1e6508f5f1f47_43ed7f5a0fae657d,
    0x4206c6c80ef8fd9f_3a88a0fbbcb05c63,
    0x3c1dea6d0afcae52_21874b8b4d2dbc4f,
    0xe79cf8044680e415_1bdea12e35f6a8c9,
    0x48676fcc85844eb5_53c065c6c8e63528,
    0x62f20ecb301144ad_e34a1d250e7a8d6b,
    0x2778709cb5ff3fc7_d6b04d3b7651dd7e,
    0xd87c68010650c250_5e90277e7cb39e2d,
    0xbe61c9fc8adc7260_2c046f22062dc67d,
    0xeaec3f6695236d35_b10bb459132d0a26,
    0xd47e91679b5bbefc_3fa9ddfb67e2f199,
    0x195693617db7534c_0e09b88e1914f7af,
    0x2725134b52fd2c81_10e8b35af3eeab37,
    0x22770ffa079a1704_9eedeca8e272b933,
    0x012c2a69dda5ad22_d4c718bc4ae8ae5f,
    0x09338c04f427a66f_81536d601170fc20,
    0x0d85fd7519aa9dec_91b534f885818a06,
    0x0246a0f9fd642861_ec8177f83f900978,
    0xac615f38f5a451ea_190e714fada5156e,
    0x07a44f6430336f1c_b592bf39b0364963,
    0x2184303422b53201_89c350c893ae7dc1,
    0x6e47c77585ab8164_ac042e70f8b383f2,
    0x55d75131c650586c_b49b52e587a1ee60,
    0x420514ac928637fa_fb152fe3ff26da89,
    0xbb00290a9289ce13_3e666e6f69ae2c15,
    0x321f8022ccc2553a_3b544ebe544c19f9,
    0x7effcb24d14c9d18_e805a1e290cf2456,
    0xdc418c09511a5174_24b33c9d7ed25117,
    0x1130c8b2334d05c7_e74733427b72f0c1,
    0x57f10554d8e9323b_0a804d18b7097475,
    0x90a5ce5a89ea0b56_57e3306d881edb4f,
    0xc27327f936e68d1b_4ae7d6a36eb5dbcb,
    0x417b730cb2a966b0_2d8d5432157064c8,
    0x1d301ea15b8ea672_d1e649de1e7f268b,
    0xf14dee3399ddf91c_8a328a1cedfe552c,
    0xbd989097807f7fbf_07a3aec79624c7da,
    0xc3a0533a6d96954b_84547ddc3e203c94,
    0x1992575160c43696_990a98fd5071d263,
    0xd49493b523ad7777_1a4ff12616eefc89,
    0xb77c0f9cf9e436f2_f6f7fd1431714200,
    0xe62702fef78982ef_30c05b1ba332f41c,
    0x236a476ed9466eb7_8d2636b81555a786,
    0xe304cfd61b4f416c_46c9feb55d120902,
    0x2cb2ea092e5f1215_ccec0a73b49c9921,
    0xe2004d8b7660e169_4e9d2827355fc492,
    0x7fcabe79c4442ade_19ebb029435dcb0f,
    0x3ec3d0686df24ad5_4659d2b743848a2c,
    0x8f3d9e86c7b31fff_963ef2c96b33be31,
    0x6ba130e1edb0873d_74f85198b05a2e7d,
    0x40fe52eaaac04a87_5a0f544dd2b1fb18,
    0x789eb8c74c29b5bd_03727073c2e134b1,
    0x4630d70199d8fe85_c7f6aa2de59aea61,
    0xc4f6e06e6eadb36d_352787baa0d7c22f,
    0x2d9cd0536bddc355_9853eab63b5e0b35,
    0x9522ac8d318de072_abbdcdd7ed5c0860,
    0xdac15872053fb2a8_cf05daf5ac8d77b0,
    0x4b63b05120422ba5_49cad48cebf4a71e,
    0xc4f797ac06e0c775_7a4c10ec2158c4a6,
    0x4b9efbbed8c2bd98_d9e92aa246bf719e,
    0x67b2d7640bd6d12a_13ae978d09fe5557,
    0x69216ca21f6e1bf4_730499af921549ff,
    0x49ab0589118fe345_4e4b705b92903ba4,
    0x90b672aaf0764986_ff577222c14f0a3a,
    0xc24aa6db9b0e9300_55b6344cf97aafae,
    0x8478cc06efce550e_b862225b055b6960,
    0x82e3ec2ccd28350f_cac09afbddd2cdb4,
    0xbc059bb74b993690_daf8e9829fe96b5f,
    0xe31111d14445238b_b5fdfc5d3132c498,
    0xa7ed1df77e79a736_310cb380db6f7503,
    0xa137dc0582888c63_e87fbb46217a360e,
    0x7630e70040281934_2102ae466ebb1148,
    0x2c17c3b74634a6d6_f8549e1a3aa5e00d,
    0x05eff03a838a4fb4_07a69afdcc42261a,
    0x3c28d21d40d4f80e_c4c118bfe78feaae,
    0xbeb9cec18b163f7c_f9f4892ed96bd438,
    0xdc75195297484115_1af3dbe25d8f45da,
    0x548955be3bde572b_f5b4b0b0d2deeeb4,
    0xc8281bde4d280b81_962aceefa82e1c84,
    0x914da129a132922b_046e3ecaaf453ce9,
    0xff11d08f1cee77a4_f05d129681949a4c,
    0x8070295bd01f6bfd_964781ce734b3c84,
    0x006a9346f317a9c9_9c2ed44081ce5fbd,
    0x37e62ccdf9739fc3_522e23f3925e319e,
    0x5cbf8b753a7e703c_177e00f9fc32f791,
    0xfe0f162fcebe01c8_2bc60a63a6f3b3f2,
    0x094f481a19d464ff_222bbfae61725606,
    0x9737e370b3c679cf_486289ddcc3d6780,
    0x60d67575f3b9b1c2_7dc7785b8efdfc80,
    0x3948fbaf41196093_8af38731c02ba980,
    0x0c204d1cfdbf6e2a_1fab64ea29a2ddf7,
    0xb7d2d42a9be29c2a_e4d9429322cd065a,
    0x49fae729fc2974b3_9da058c67844f20c,
    0x6fb26356dad98ed6_24c0e332b70019b0,
    0x4847b6cc14eeffd4_233003b5a6cfe6ad,
    0x3b5285a9f0152c99_d586bd01c5c217f6,
    0xff1fe7f4b91cff4c_5e5637885f29bc2b,
    0x16e20774363e99d0_7eba726d8c94094b,
    0x437d1aa9cb4159e0_0a56a5f0bfe39272,
    0x7bdcc9d5c1c4da0b_d79476a84ee20d06,
    0x8fd087733782eecd_9e4c1269baa4bf37,
    0xbf94dee3ad478cda_17efee45b0dee640,
    0x83c94bbe4c623bf5_1d95b0a5fcf90bc6,
    0x08fda03aa45ee9ba_93cbe0b699c2585d,
    0xc188f92d4d403856_65fa4f227a2b6d79,
    0xfd6611dfb12345fc_d5f9e858292504d5,
    0xef003ffd18aecc12_c2b5a03f71471a6f,
    0xb7b474cbf2934019_59300222b4561e00,
    0xbc2a55e58b30deee_ce2f8642ca0712dc,
    0x3e77de18337cda42_7ca9723fbb2e8988,
    0x2f81e058ffb75885_2785338347f2ba08,
    0x8f0c300cf585707e_c61bb3a141e50e8c,
    0xcf4f4c536e0e2af2_150f361dab9dec26,
    0x24c81ee5fd39a8e4_9f6a419d382595f4,
    0x2841577e66ad726e_64a53dc924fe7ac9,
    0x68090c81a1357214_142de49fff7a7c3d,
    0xa6aa70d44a613a24_0c335248857fa9e7,
    0xdb805d26087f4db9_0a9c32d5eae45305,
    0x6dd954b45a122182_e6c42178c4bbb92e,
    0xc34729fd9f1948a3_71f1ce2490d20b07,
    0xf0682ca0764cc153_f1bcc3d275afe51a,
    0xbf29824279ca73e1_e728e8c83c334074,
    0xf0c70fb4e725caff_96fbf83a12884624,
    0x1e18cad809e9eedc_81a1549fd6573da5,
    0xa893a8fa258f383e_5fa7867caf35e149,
    0xe78c30cc1179b849_56986e2ef3ed091b,
    0x2e4432e6ce4996d9_917f1dd5f8886c61,
    0x576ec7a84e0b932d_d20d8c88c8ffe65f,
];

const WHITE_TURN_MASK: u128 = 0x3815e537b6222c85_f8d626aaaf278509;

const CASTLING_RIGHT_MASKS: [u128; 2 * 2] = [
    0xca3c7f8d050c44ba_31d71dce64b2c310,
    0x8f50a115834e5414_f165b587df898190,
    0x77568e6e61516b92_a57e6339dd2cf3a0,
    0xd153e6cf8d1984ea_1ef6e6dbb1961ec9,
];

const EN_PASSANT_FILE_MASKS: [u128; 8] = [
    0x13099942ab633504_70cc73d90bc26e24,
    0x946c73529a2f3850_e21a6b35df0c3ad7,
    0x3d1adc27d706b921_003a93d8b2806962,
    0x994b8bd260c3fad2_1c99ded33cb890a1,
    0xf4cf0c83cace7fe4_cf3145de0add4289,
    0x54807a18b6952e27_d0e4427a5514fb72,
    0xe2a1aff40d08315c_77c621cc9fb3a483,
    0x47ec43ffbc092584_67a34dac4356550b,
];

const REMAINING_CHECKS_MASKS: [u128; 3 * 2] = [
    0x6a2ad922a69a13e9_1d6dc0ee61ce803e,
    0x49b572c7942027d5_c6284b653d38e96a,
    0x08c2e9271dc91e69_803f5fb0d2f97fae,
    0x088dfad983bb7913_b183ccc9e73df9ed,
    0x90a852cacfc0adeb_fdeef11602d6b443,
    0xc8ce065f15fe38f5_1b0ce4198b3801a6,
];

const PROMOTED_MASKS: [u128; 64] = [
    0x2b9178eb57f3db25_2f9900cc2b7a19ca,
    0x17d16678351d3778_f75235beb01886d3,
    0x88b17afbdae836b1_8ae7e29889ac9964,
    0xa8985bb047c388b1_ad30091ce7cb4204,
    0x2577b875de120fd2_aae118773ddd4e4d,
    0xbdf2fdea13ee21fd_8ec514ce4736aa07,
    0x3bcf1cf1605da5e7_26a412bd8cef4f15,
    0x6ea6f4fb2ca4d856_1bdce26bd9af059f,
    0xb1198a9621b237f4_ea5f4ade5acc0516,
    0x6c87686b28782362_69ab7ebc07650565,
    0x5158703d9536de86_3e655f895a188a1c,
    0xb3126fafbea75501_f394f6882a114d65,
    0x80d3335852547580_3173cfa2be5bd4d3,
    0xc5afc4f44d5ab019_434d20d2ca00ae71,
    0x7aff7035ddcde586_3ba297f73d338c93,
    0xc338837c953120b2_099ba1b0205a5ea5,
    0x7f8b4b715fc6eee8_c49f050b5e1c5653,
    0x76d1cd962b7c0005_e14eec50a9c690e8,
    0xad3db13d420b8915_2571cc79f4ce0169,
    0xe5a6076284061351_de0f98d6002f4323,
    0x7f61ddc8b19de688_0682220b02e5c3e8,
    0x8648ca9be5696f33_cb900d3a6b38c39d,
    0x469dc25e1b32d323_24620fbf09d50d66,
    0x4f529fa289578425_0f40a9b2781a119d,
    0x82d550f6a40a3d66_83c6980df0d04932,
    0x8e791844cfb87476_ab6f9af720cb5df4,
    0xb229ab8bb6054dad_1c906974166ee8d4,
    0x50a0b7e3c796ad7e_9c1ba3db0784ebda,
    0x186c0c4a7a5d68d1_81a19098d16aa929,
    0x748301e9e571a670_fce56173c63ccefd,
    0x2119955cd577ee40_43cb7aa20c6209c2,
    0x6c57c1380ffb21fb_7e96e2ae86924bab,
    0xadd607ff5aaaf995_01860725034b0fef,
    0xd41ec439c73a3e0f_f74d369066ec4e96,
    0x3ee343c1d9837a9e_1ae9962c6e0d1232,
    0x97707414bf743321_5d66fa465ccfc560,
    0xe6a8ec453ed67917_e9c13ae1fc36afaa,
    0x03845f0849e183df_caec4035fb840be4,
    0xccd5f9e7ce7e601f_839d28adafad0f8f,
    0xe2c04ba3484232d8_e4703b6e30422003,
    0x5d9b925a3e6af022_1e2fd5b2827d5e43,
    0xa852b77063b9e148_96f1e8d8b94bd960,
    0x279d2c0c53ecaac6_90f2075c3f43960c,
    0x39f0827a4f811c72_c48e0774c4f9134f,
    0x3b064b8614517b48_f17e5f6a2cb000c7,
    0xdb19bcf000dd394a_6248409bf55a4925,
    0xa63dddb617c59634_967bd94eb30505cc,
    0xece83ffa46fd173a_e91e89853f9e844f,
    0x01c4d4d39c11557f_b841038e24193f08,
    0xa618dd21a5f6b67f_46f3b25cae82a6cc,
    0x8356bdf440563fb0_3e97e042449e3ed5,
    0xf35ad55d727351a1_868a166af46dcbd2,
    0xa3a0c83ec173d41c_f71be788b3fd1a7a,
    0xa8b2fe93fc5ddd19_cb6d65410533cc37,
    0x984fd612fa3936bd_7e30d70559efaedc,
    0x71c7e142bb029d2a_32db0f5ca18159ce,
    0x759f8a9460c634a6_97a9116e874228c5,
    0x6fa2ca2ca3bdeb90_85ee68ee3a175297,
    0xea5f0228fc314b97_076a14170b409e2a,
    0xc40c265d97e763c0_bad49d47dc95855b,
    0x201d68d0d89f0e31_636187d94ded991e,
    0x1b769456c77879cf_962e50971f09cfab,
    0xa8baaf8b31da09a5_8f16c910d6776589,
    0x06a79ac414c9632e_7e3de4bfbef5566f,
];

const POCKET_MASKS: [u128; 5 * 2 * 15] = [
    0x6e21a47d5b561a1d_b262e9f9d6123320,
    0x4263a757e414fe44_91533947cdaa8bec,
    0x93c43f67cf55b53f_a13b56b45723a3d4,
    0xa89732f339d35eec_9a35cce29ca3ac75,
    0x5df4ac25c29fbebf_2716940e1d4f28d7,
    0x62059230cedcd78f_7447209cfb793066,
    0x9b0f7261932d2c8e_5cf91d8ae6402e1a,
    0xfc0f99f00e0cc0e7_4625588d38487ac5,
    0xcba8a5a02c351aa5_e42ec6191353e3bd,
    0x77a6c01bd0174d69_478e6cc8f6b2dada,
    0xc200e5264100e463_1726fc948b994b87,
    0x3f340a89f525effe_fb9d2e5a66b46741,
    0xf748b2be597b2f7a_7f668e401ffe9e6f,
    0xf7b2eddf7b8838c2_ee4d6fe11c46a236,
    0xbdec7e9f4317a678_006cb70064259959,
    0x5e022239fdf20b36_33535a7c4def1b24,
    0xfdd92fa05b03b629_479e792f8171fc29,
    0x80cdab95a89927dc_656a6e71de970975,
    0x92cb516b8eba4a30_cada3e48618a1c2b,
    0xac950f8bce3af2d9_b37ad7262db9c99e,
    0x9a43060d3aaae01a_85ae25402a311d5d,
    0xf12b2f6012f9333c_3de4e82d52dbb44c,
    0x8ae9143ece61584a_b1c8499674464c21,
    0x676f70930e72993c_f1c1853cc6827b84,
    0x25e0f5f014476a1f_51f97ed3ba004fb0,
    0x1e33827f042de978_00da9ede878e3e98,
    0xdd158e4a7838524d_3cd0fd658e1cdb12,
    0x2b75316dfa1b15e2_ac2940b688a1d0f9,
    0x0283b57f325ea495_e51acb5b336db0df,
    0x146e60f56ab91765_cf7517fbdcb16174,
    0xf50d2497f8b12819_dfe901aba4a2ced3,
    0x7600c53f3c60308b_24bfd4b72c8852eb,
    0xb75208a6056dc7e9_f085bcd9711883d4,
    0xe5f0eb83c20e921b_41b71908a3d86274,
    0x7529d0a0c95f08ed_6d604cc0a2df1a69,
    0x33c7b70ee04a511c_aedf8291e0048c39,
    0x727a256dad06cc11_09d3c83f59547935,
    0x1b26058e1ba73008_257d5c7ebc718242,
    0x09c48fc7e167f3b0_56ac1c998f5c2ede,
    0x954f57cdf6076cd4_a25c0b0679937316,
    0x32facc37b50d925e_a9a2a7e200faa936,
    0x259698168b89f941_b8e7ca4716cf9d49,
    0xb4479bfb3575d3fc_9b253f89247c4c1d,
    0xe69d20380ad45ef5_1e701e2a73f9dc4b,
    0x0ea72e455dfb08e6_cdf351b289aa5a84,
    0x7f834fab71613f89_2e4e118fc45fdc0d,
    0x0bef7b04290fa4d3_80247d70885ad5ce,
    0x425d9a9261abb5b9_0a99dccfce316ca0,
    0x231bfd9dfb70f61e_b5553435dae76840,
    0xf2ce79a69837967f_ee562004d5d14158,
    0xf9012eb6947f5b8c_551b5fa3ec7166a2,
    0xe867f9c703503bf1_2dbb493c6e9fec06,
    0xee98c9010d1d3cbc_f06b4c65f4bb14a1,
    0x95fc0aa1222389e8_5f0b44d98013acb9,
    0xbf7533ebb6c99102_ce7dbafa734bba8a,
    0xbc4153720b7d8489_e009c0e355a77913,
    0xe892965f4753afa0_21918f473cb6decf,
    0xeaec3e0774781d2e_dcf11e80dc14763f,
    0x00b698a9c3390404_7ac21357500fb0c6,
    0x84c94d9a3d13f9b5_28abe0a3761e326c,
    0xe4a910d516e80a37_30b8e3da17d34c6e,
    0x30a99425bba73df4_d999d38ffa5d771e,
    0x1fe92363cf099b7e_8a7e0d1367d70b28,
    0x3fce173a5e427cb2_9157bfe7ac071796,
    0x3cf044d0bd7bfb26_adda94b21edd779a,
    0x14dd9fdfd382638a_6f555cf7856f0d63,
    0xa4ec5d64116068be_5b2a5b2788adc947,
    0x970d0225c3155df4_500c782c8c562a42,
    0x96bb58faf0fd1692_20f8b3f7059d8884,
    0x183f9136b8160b83_79c890ed3e95f3f4,
    0x85c179d22cce92b9_e64dbd474ddcf8ca,
    0xd355d7752b4405ff_a94966fbf7f270d5,
    0xda7b7aa372230d64_2473b4e6ad9faa9a,
    0x8218ddc4550f6260_98abdf9fa4b487e6,
    0x090f2692c727e1a1_75fa1ecb0717029a,
    0xce74c18e0f75f7f8_f6053757646a08ba,
    0x075081d9944cf832_060e2788d99813aa,
    0x90fb343362ef172d_5fa61c63681ebbc8,
    0xac07cd32cdd4a6c5_90bbf42db708006a,
    0x3684e6c1eaf9e5f5_b525460ec1c15916,
    0x0f515afc1356a5ca_2696070a4502024d,
    0x282bbc7b2209e436_158087442731df68,
    0xc0172402afbbcbc5_65010c3ea0acfdcf,
    0x34e345bfa3c47ceb_b28ecdf305a7a831,
    0xa56824690a26a07d_fd037a2e2a2e54e8,
    0x82de3ff226f8520b_5f09f3763f6a4882,
    0xd6e502a609ba9dde_e0125e53c4e64b83,
    0x3b40c681d5b6e330_1de44a244be3752a,
    0x23c2312838f00cc0_f78919dfb05f031c,
    0x6e63a230b2eab193_bf81caebad91d8e1,
    0x9a08aa69fb121fc8_bc3780dce0bd58d5,
    0x551be806bb80e780_65b5fb1afa5c5714,
    0x5ab7fd28c7b96a6b_b7ddb798d0c1ff23,
    0xb3277b903da3eab9_a823d99d1504f4d6,
    0xaf84fcbb2d16fa25_a3c526e07f1cf98d,
    0x1c53b33de31ba544_a848f93e7a83ece4,
    0x4186f24fae7b9c26_21e3941600abaaec,
    0x4bccd3e248e2a69c_534a070449a9238d,
    0x8d6b46aee9ae0606_f86c2e4bb82d3923,
    0x7b281261c9e1028e_a594b44c256b41f8,
    0x6c501072a1108abe_f1503a710531b677,
    0xa8c4da9ba7c8a22a_171a27a1b9911e11,
    0x88d5489e8bc29294_d8d8a26ac022ebe6,
    0x1381a967122b289a_151ca9f641352f33,
    0xdc944a008e97acd7_553b499dc1eae685,
    0x63838f936ef425de_0137684bed65e27e,
    0x39bd05197acaddb5_254a12bd9efa3535,
    0xf5871ddb63b0aecf_f8361f0b0a35ef6d,
    0xcd9a19a4addac30e_a7b7e76b7ff82166,
    0x08c6dce88d2bfe12_e266e0067bc7f396,
    0xea8c0b4c4097cc43_bdd8a9037f5d0298,
    0x74fb47cb2de1371f_2d5977c3f88a2a31,
    0x8c2406a60633f3a5_4587cd651a3bb45f,
    0xc40387658bd2d9bf_bcdc3c56ad971eb0,
    0x4616dae41e7f6769_248b2073706e1844,
    0xff41e4fd3d1a34ee_ab03444dfb15bd0a,
    0xd9c4e712cc561f6a_bcaff3134756ab78,
    0x393520a31d54572c_eea844cf3e1db285,
    0x1cc02ca62684138e_b917fdb80f355116,
    0xc1b9f65a9989c1d5_21931f559ecefa34,
    0x86e3a7966174637c_7170c6436114a4c2,
    0x80d8247d1168300a_73d2a7c1017a2aaf,
    0x1996e2ac2b938629_3b855d1755dce20e,
    0xb26f006263639c6a_37e35078817f0dbd,
    0xfe1f7c18126abdd7_e59b1e3389a1aad3,
    0x83b9090f7e58e659_bad11ebe3c3df239,
    0x28253df5164c46a3_d54aad8a64c65c27,
    0x3448dc022a87a231_eadb37a4f7fbeb4c,
    0x72ebd4eb6f76301d_5453586c4984a81c,
    0x1f5cf5c027f9df47_b6777cd5e1b16bcc,
    0xbcda2313c8ee1152_24b690161baa0d65,
    0xb2ebc33394603af6_5bd3613d2ee222fd,
    0x84b3b1b6fa01cb1a_c928bda035f8c39d,
    0xe16b42d53a9ecf6e_29e8eeca9b09c735,
    0x27b24383307c7a88_dc35bba3f78ed4ee,
    0x01635963de0d35f7_1753c5b3ef820c81,
    0xd5667fb942e52b2f_ef3368ab56565ae7,
    0x958faa7325a97d06_ebf48bd35c4ead40,
    0x70433ecb73b7ad92_529b39d015e49755,
    0x4ab8a46b4c36eaaa_697ea70620e98751,
    0x1869111d7c4fe1a8_ebcabe3d4cbc0212,
    0xfcd46428ff1e2a1e_2f424e669d2a7f1b,
    0xb65e4b536bfa3559_6ae47a9c22302f58,
    0x180b7095853d37f3_83f9a7b574523121,
    0xbb0226e8c1543063_77b6860daa7a39d5,
    0x708d753a0092df11_1611e306f167e512,
    0x9992315a0c7adfe5_2d78f39e1adbaa9d,
    0xe12508294de8e35e_1aada836dedb3ba7,
    0x49960c597d119ace_f37991753c7df558,
    0x57b7ccbb21f74d1c_e80840e623a19d08,
];
