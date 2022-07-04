#![no_main]

use libfuzzer_sys::fuzz_target;
use shakmaty::san::SanPlus;

fuzz_target!(|data: &[u8]| {
    if let Ok(san) = SanPlus::from_ascii(data) {
        let roundtripped = SanPlus::from_ascii(san.to_string().as_bytes()).expect("roundtrip");
        assert_eq!(san, roundtripped);
    }
});
