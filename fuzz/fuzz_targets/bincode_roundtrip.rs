#![no_main]

use arbitrary::Arbitrary;
use bincode::{Decode, Encode};
use libfuzzer_sys::fuzz_target;
use shakmaty::{
    Bitboard, Board, ByCastlingSide, ByColor, ByRole, CastlingSide, Color, File, KnownOutcome,
    Outcome, Piece, Rank, RemainingChecks, Role, Square, san, uci::UciMove, variant::Variant,
};

#[derive(Arbitrary, Debug, Eq, PartialEq, Encode, Decode)]
struct Payload(Vec<u8>);

#[derive(Arbitrary, Debug, Eq, PartialEq, Encode, Decode)]
struct Data {
    castling_side: CastlingSide,
    by_castling_side: ByCastlingSide<Payload>,
    board: Board,
    piece: Piece,
    remaining_checks: RemainingChecks,
    uci_move: UciMove,
    variant: Variant,
    bitboard: Bitboard,
    color: Color,
    by_color: ByColor<Payload>,
    file: File,
    rank: Rank,
    square: Square,
    san_suffix: san::Suffix,
    role: Role,
    by_role: ByRole<Payload>,
    known_outcome: KnownOutcome,
    outcome: Outcome,
}

fuzz_target!(|data: Data| {
    let config = bincode::config::standard();
    let encoded = bincode::encode_to_vec(&data, config).unwrap();
    let (decoded, _) = bincode::decode_from_slice(&encoded, config).unwrap();
    assert_eq!(data, decoded);
});
