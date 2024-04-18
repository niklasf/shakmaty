#![no_main]

use libfuzzer_sys::fuzz_target;
use shakmaty::uci::UciMove;

fuzz_target!(|data: &[u8]| {
    if let Ok(uci) = UciMove::from_ascii(data) {
        let roundtripped = UciMove::from_ascii(uci.to_string().as_bytes()).expect("roundtrip");
        assert_eq!(uci, roundtripped);
    }
});
