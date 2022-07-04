#![no_main]

use libfuzzer_sys::fuzz_target;

use shakmaty::fen::Fen;

fuzz_target!(|data: &[u8]| {
    if let Ok(fen) = Fen::from_ascii(data) {
        let roundtripped = Fen::from_ascii(fen.to_string().as_bytes()).expect("roundtrip");
        assert_eq!(fen, roundtripped);
    }
});
