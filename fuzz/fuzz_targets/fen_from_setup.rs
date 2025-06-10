#![no_main]

use libfuzzer_sys::fuzz_target;
use shakmaty::{fen::Fen, Setup};

fuzz_target!(|setup: Setup| {
    let Ok(fen) = Fen::try_from_setup(setup.clone()) else {
        return;
    };
    let fen = fen.to_string();
    let roundtripped = Fen::from_ascii(fen.as_bytes()).expect("roundtrip via: {fen}");
    assert_eq!(setup, roundtripped.into_setup());
});
