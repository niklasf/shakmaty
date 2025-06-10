#![no_main]

use libfuzzer_sys::fuzz_target;
use shakmaty::san::SanPlus;

fuzz_target!(|san: SanPlus| {
    let roundtripped = SanPlus::from_ascii(san.to_string().as_bytes()).expect("roundtrip");
    assert_eq!(san, roundtripped);
});
