#![no_main]

use libfuzzer_sys::fuzz_target;
use shakmaty::Square;

fuzz_target!(|data: &[u8]| {
    if let Ok(square) = Square::from_ascii(data) {
        let roundtripped = Square::from_ascii(square.to_string().as_bytes()).expect("roundtrip");
        assert_eq!(square, roundtripped);
    }
});
