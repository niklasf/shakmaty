#![no_main]

use arbitrary::{Arbitrary as _, Unstructured};
use libfuzzer_sys::fuzz_target;
use shakmaty::san::SanPlus;

fuzz_target!(|data: &[u8]| {
    let mut unstructured = Unstructured::new(data);
    let Ok(san) = SanPlus::arbitrary(&mut unstructured) else {
        return;
    };
    let roundtripped = SanPlus::from_ascii(san.to_string().as_bytes()).expect("roundtrip");
    assert_eq!(san, roundtripped);
});
