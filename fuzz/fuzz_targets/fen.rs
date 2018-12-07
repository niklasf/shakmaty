#![no_main]

use libfuzzer_sys::fuzz_target;

use shakmaty::fen::Fen;

fuzz_target!(|data: &[u8]| {
    let _ = Fen::from_ascii(data);
});
