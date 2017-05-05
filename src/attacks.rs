//! Attack and ray tables.
//!
//! # Examples
//!
//! ```
//! # use shakmaty::square;
//! # use shakmaty::Bitboard;
//! use shakmaty::attacks;
//!
//! let occupied = Bitboard::rank(5); // blocking pieces
//! let attacks = attacks::bishop_attacks(square::C2, occupied);
//! // . . . . . . . .
//! // . . . . . . . .
//! // 0 0 0 0 0 0 1 0
//! // . . . . . 1 . .
//! // 1 . . . 1 . . .
//! // . 1 . 1 . . . .
//! // . . . . . . . .
//! // . 1 . 1 . . . .
//!
//! assert!(attacks.contains(square::G6));
//! assert!(!attacks.contains(square::H7));
//! ```

use square;
use square::Square;
use bitboard::Bitboard;
use types::{Color, Role, Piece};

const ROOK_DELTAS: [i8; 4] = [8, 1, -8, -1];
const BISHOP_DELTAS: [i8; 4] = [9, 7, -9, -7];

const KNIGHT_ATTACKS: [u64; 64] = [132096, 329728, 659712, 1319424, 2638848, 5277696, 10489856, 4202496, 33816580, 84410376, 168886289, 337772578, 675545156, 1351090312, 2685403152, 1075839008, 8657044482, 21609056261, 43234889994, 86469779988, 172939559976, 345879119952, 687463207072, 275414786112, 2216203387392, 5531918402816, 11068131838464, 22136263676928, 44272527353856, 88545054707712, 175990581010432, 70506185244672, 567348067172352, 1416171111120896, 2833441750646784, 5666883501293568, 11333767002587136, 22667534005174272, 45053588738670592, 18049583422636032, 145241105196122112, 362539804446949376, 725361088165576704, 1450722176331153408, 2901444352662306816, 5802888705324613632, 11533718717099671552, 4620693356194824192, 288234782788157440, 576469569871282176, 1224997833292120064, 2449995666584240128, 4899991333168480256, 9799982666336960512, 1152939783987658752, 2305878468463689728, 1128098930098176, 2257297371824128, 4796069720358912, 9592139440717824, 19184278881435648, 38368557762871296, 4679521487814656, 9077567998918656];

const KING_ATTACKS: [u64; 64] = [770, 1797, 3594, 7188, 14376, 28752, 57504, 49216, 197123, 460039, 920078, 1840156, 3680312, 7360624, 14721248, 12599488, 50463488, 117769984, 235539968, 471079936, 942159872, 1884319744, 3768639488, 3225468928, 12918652928, 30149115904, 60298231808, 120596463616, 241192927232, 482385854464, 964771708928, 825720045568, 3307175149568, 7718173671424, 15436347342848, 30872694685696, 61745389371392, 123490778742784, 246981557485568, 211384331665408, 846636838289408, 1975852459884544, 3951704919769088, 7903409839538176, 15806819679076352, 31613639358152704, 63227278716305408, 54114388906344448, 216739030602088448, 505818229730443264, 1011636459460886528, 2023272918921773056, 4046545837843546112, 8093091675687092224, 16186183351374184448, 13853283560024178688, 144959613005987840, 362258295026614272, 724516590053228544, 1449033180106457088, 2898066360212914176, 5796132720425828352, 11592265440851656704, 4665729213955833856];

const BLACK_PAWN_ATTACKS: [u64; 64] = [0, 0, 0, 0, 0, 0, 0, 0, 2, 5, 10, 20, 40, 80, 160, 64, 512, 1280, 2560, 5120, 10240, 20480, 40960, 16384, 131072, 327680, 655360, 1310720, 2621440, 5242880, 10485760, 4194304, 33554432, 83886080, 167772160, 335544320, 671088640, 1342177280, 2684354560, 1073741824, 8589934592, 21474836480, 42949672960, 85899345920, 171798691840, 343597383680, 687194767360, 274877906944, 2199023255552, 5497558138880, 10995116277760, 21990232555520, 43980465111040, 87960930222080, 175921860444160, 70368744177664, 562949953421312, 1407374883553280, 2814749767106560, 5629499534213120, 11258999068426240, 22517998136852480, 45035996273704960, 18014398509481984];

const WHITE_PAWN_ATTACKS: [u64; 64] = [512, 1280, 2560, 5120, 10240, 20480, 40960, 16384, 131072, 327680, 655360, 1310720, 2621440, 5242880, 10485760, 4194304, 33554432, 83886080, 167772160, 335544320, 671088640, 1342177280, 2684354560, 1073741824, 8589934592, 21474836480, 42949672960, 85899345920, 171798691840, 343597383680, 687194767360, 274877906944, 2199023255552, 5497558138880, 10995116277760, 21990232555520, 43980465111040, 87960930222080, 175921860444160, 70368744177664, 562949953421312, 1407374883553280, 2814749767106560, 5629499534213120, 11258999068426240, 22517998136852480, 45035996273704960, 18014398509481984, 144115188075855872, 360287970189639680, 720575940379279360, 1441151880758558720, 2882303761517117440, 5764607523034234880, 11529215046068469760, 4611686018427387904, 0, 0, 0, 0, 0, 0, 0, 0];

fn sliding_attacks(sq: Square, occupied: Bitboard, deltas: &[i8]) -> Bitboard {
    let mut attack = Bitboard(0);

    for delta in deltas {
        let mut previous = sq;

        while let Some(s) = previous.offset(*delta) {
            if square::distance(s, previous) > 2 {
                break;
            }

            attack.add(s);

            if occupied.contains(s) {
                break;
            }

            previous = s;
        }
    }

    attack
}

fn init_magics(indexes: &mut[usize], masks: &mut[Bitboard], ranges: &mut[Bitboard], attacks: &mut[u16], deltas: &[i8]) {
    for s in 0..64 {
        let sq = Square::from_index(s).unwrap();

        let range = sliding_attacks(sq, Bitboard(0), deltas);
        ranges[s as usize] = range;

        let edges = ((Bitboard::rank(0) | Bitboard::rank(7)) & !Bitboard::rank(sq.rank())) |
                    ((Bitboard::file(0) | Bitboard::file(7)) & !Bitboard::file(sq.file()));

        let mask = range & !edges;
        masks[s as usize] = mask;

        for subset in mask.subsets() {
            let index = indexes[s as usize] + subset.extract(mask) as usize;
            attacks[index] = sliding_attacks(sq, subset, deltas).extract(range) as u16;
        }

        if s < 63 {
            indexes[s as usize + 1] = indexes[s as usize] + (1 << mask.count());
        }
    }
}

struct Precomp {
    rook_indexes: [usize; 64],
    rook_masks: [Bitboard; 64],
    rook_ranges: [Bitboard; 64],
    rook_attacks: [u16; 0x19000],

    bishop_indexes: [usize; 64],
    bishop_masks: [Bitboard; 64],
    bishop_ranges: [Bitboard; 64],
    bishop_attacks: [u16; 0x1480],

    bb_rays: [[Bitboard; 64]; 64],
    bb_between: [[Bitboard; 64]; 64],
}

impl Precomp {
    /// Precomputes attack tables. The struct uses about 200 KiB of
    /// stack memory.
    pub fn new() -> Precomp {
        let mut precomp = Precomp {
            rook_indexes: [0; 64],
            rook_masks: [Bitboard(0); 64],
            rook_ranges: [Bitboard(0); 64],
            rook_attacks: [0; 0x19000],

            bishop_indexes: [0; 64],
            bishop_masks: [Bitboard(0); 64],
            bishop_ranges: [Bitboard(0); 64],
            bishop_attacks: [0; 0x1480],

            bb_rays: [[Bitboard(0); 64]; 64],
            bb_between: [[Bitboard(0); 64]; 64],
        };

        init_magics(&mut precomp.rook_indexes,
                    &mut precomp.rook_masks,
                    &mut precomp.rook_ranges,
                    &mut precomp.rook_attacks,
                    &ROOK_DELTAS);

        init_magics(&mut precomp.bishop_indexes,
                    &mut precomp.bishop_masks,
                    &mut precomp.bishop_ranges,
                    &mut precomp.bishop_attacks,
                    &BISHOP_DELTAS);

        for a in 0..64 {
            let sa = Square::from_index(a as i8).unwrap();

            for b in 0..64 {
                let sb = Square::from_index(b as i8).unwrap();

                if precomp.bishop_attacks(sa, Bitboard(0)).contains(sb) {
                    precomp.bb_rays[a][b] =
                        (precomp.bishop_attacks(sa, Bitboard(0)) &
                         precomp.bishop_attacks(sb, Bitboard(0))).with(sa).with(sb);
                    precomp.bb_between[a][b] =
                        precomp.bishop_attacks(sa, Bitboard::from_square(sb)) &
                        precomp.bishop_attacks(sb, Bitboard::from_square(sa));
                } else if precomp.rook_attacks(sa, Bitboard(0)).contains(sb) {
                    precomp.bb_rays[a][b] =
                        (precomp.rook_attacks(sa, Bitboard(0)) &
                         precomp.rook_attacks(sb, Bitboard(0))).with(sa).with(sb);
                    precomp.bb_between[a][b] =
                        precomp.rook_attacks(sa, Bitboard::from_square(sb)) &
                        precomp.rook_attacks(sb, Bitboard::from_square(sa));
                }
            }
        }

        precomp
    }

    pub fn rook_attacks(&self, sq: Square, occupied: Bitboard) -> Bitboard {
        let mask = self.rook_masks[sq.index() as usize];
        let range = self.rook_ranges[sq.index() as usize];
        let index = self.rook_indexes[sq.index() as usize] + occupied.extract(mask) as usize;
        Bitboard::deposit(self.rook_attacks[index] as u64, range)
    }

    pub fn bishop_attacks(&self, sq: Square, occupied: Bitboard) -> Bitboard {
        let mask = self.bishop_masks[sq.index() as usize];
        let range = self.bishop_ranges[sq.index() as usize];
        let index = self.bishop_indexes[sq.index() as usize] + occupied.extract(mask) as usize;
        Bitboard::deposit(self.bishop_attacks[index] as u64, range)
    }

    pub fn queen_attacks(&self, sq: Square, occupied: Bitboard) -> Bitboard {
        self.rook_attacks(sq, occupied) | self.bishop_attacks(sq, occupied)
    }
}

lazy_static! {
    static ref PRECOMP: Precomp = Precomp::new();
}

pub fn pawn_attacks(color: Color, sq: Square) -> Bitboard {
    Bitboard(match color {
        Color::White => WHITE_PAWN_ATTACKS[sq.index() as usize],
        Color::Black => BLACK_PAWN_ATTACKS[sq.index() as usize],
    })
}

pub fn knight_attacks(sq: Square) -> Bitboard {
    Bitboard(KNIGHT_ATTACKS[sq.index() as usize])
}

pub fn king_attacks(sq: Square) -> Bitboard {
    Bitboard(KING_ATTACKS[sq.index() as usize])
}

pub fn rook_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    PRECOMP.rook_attacks(sq, occupied)
}

pub fn bishop_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    PRECOMP.bishop_attacks(sq, occupied)
}

pub fn queen_attacks(sq: Square, occupied: Bitboard) -> Bitboard {
    PRECOMP.queen_attacks(sq, occupied)
}

pub fn attacks(sq: Square, piece: &Piece, occupied: Bitboard) -> Bitboard {
    match piece.role {
        Role::Pawn   => pawn_attacks(piece.color, sq),
        Role::Knight => knight_attacks(sq),
        Role::Bishop => bishop_attacks(sq, occupied),
        Role::Rook   => rook_attacks(sq, occupied),
        Role::Queen  => queen_attacks(sq, occupied),
        Role::King   => king_attacks(sq)
    }
}

/// The rank, file or diagonal with the two squares (or an empty `Bitboard`
/// if they are not aligned).
///
/// ```
/// # use shakmaty::square;
/// # use shakmaty::attacks;
///
/// let ray = attacks::ray(square::E2, square::G4);
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . . . . .
/// // . . . . . . . 1
/// // . . . . . . 1 .
/// // . . . . . 1 . .
/// // . . . . 1 . . .
/// // . . . 1 . . . .
/// ```
pub fn ray(a: Square, b: Square) -> Bitboard {
    PRECOMP.bb_rays[a.index() as usize][b.index() as usize]
}

/// Like `ray`, but just the squares in-between (exluding the bounds).
pub fn between(a: Square, b: Square) -> Bitboard {
    PRECOMP.bb_between[a.index() as usize][b.index() as usize]
}

/// Tests if all three squares are aligned on a rank, file or diagonal.
pub fn aligned(a: Square, b: Square, c: Square) -> bool {
    ray(a, b).contains(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sliding_rook_attacks() {
        let attack = sliding_attacks(square::D6, Bitboard(0x3f7f28802826f5b9), &ROOK_DELTAS);
        assert_eq!(attack, Bitboard(0x8370808000000));
    }

    #[test]
    fn test_rook_attacks() {
        assert_eq!(rook_attacks(square::D6, Bitboard(0x3f7f28802826f5b9)), Bitboard(0x8370808000000));
    }
}
