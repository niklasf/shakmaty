#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate shakmaty;

use shakmaty::Square;

fuzz_target!(|data: &[u8]| {
    let _ = Square::from_bytes(data);
});
