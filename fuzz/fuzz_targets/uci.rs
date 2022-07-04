#![no_main]

use libfuzzer_sys::fuzz_target;
use shakmaty::uci::Uci;

fuzz_target!(|data: &[u8]| {
    let _ = Uci::from_ascii(data);
});
