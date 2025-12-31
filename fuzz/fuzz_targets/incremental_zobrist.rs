#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use shakmaty::{EnPassantMode, Position, variant::VariantPosition, zobrist::Zobrist64};

#[derive(Debug, Arbitrary)]
struct Data {
    mode: EnPassantMode,
    pos: VariantPosition,
    move_indexes: Vec<u8>,
}

fuzz_target!(|data: Data| {
    let mut data = data;

    let mut hash = Some(data.pos.zobrist_hash::<Zobrist64>(data.mode));

    for index in &data.move_indexes {
        if let Some(hash) = hash {
            assert_eq!(hash, data.pos.zobrist_hash(data.mode));
        }

        let legals = data.pos.legal_moves();
        let Some(idx) = usize::from(*index).checked_rem(legals.len()) else {
            break;
        };
        let m = legals[idx];

        hash = data.pos.update_zobrist_hash(
            hash.unwrap_or_else(|| data.pos.zobrist_hash(data.mode)),
            m,
            data.mode,
        );
        data.pos.play_unchecked(m);
    }
});
