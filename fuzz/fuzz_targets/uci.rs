#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate shakmaty;

use shakmaty::uci::Uci;

fuzz_target!(|data: &[u8]| {
    let _ = Uci::from_bytes(data);
});
