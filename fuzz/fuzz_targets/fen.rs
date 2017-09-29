#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate shakmaty;

use shakmaty::fen::Fen;

fuzz_target!(|data: &[u8]| {
    let _ = Fen::from_bytes(data);
});
