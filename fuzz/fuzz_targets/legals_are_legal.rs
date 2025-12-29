#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use shakmaty::{Move, Position, variant::VariantPosition};

#[derive(Debug, Arbitrary)]
struct Data {
    candidate: Move,
    pos: VariantPosition,
}

fuzz_target!(|data: Data| {
    let legals = data.pos.legal_moves();
    assert_eq!(
        legals.contains(&data.candidate),
        data.pos.is_legal(data.candidate)
    );
    assert!(legals.iter().all(|&m| data.pos.is_legal(m)));
});
