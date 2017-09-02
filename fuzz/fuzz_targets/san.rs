#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate shakmaty;

use shakmaty::san::San;

fuzz_target!(|data: &[u8]| {
    let _ = San::from_bytes(data);
});
