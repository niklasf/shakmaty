#![no_main]

use libfuzzer_sys::fuzz_target;

use shakmaty::Square;

fuzz_target!(|data: &[u8]| {
    let _ = Square::from_ascii(data);
});
