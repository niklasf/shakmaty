#![no_main]

use libfuzzer_sys::fuzz_target;

use shakmaty::san::San;

fuzz_target!(|data: &[u8]| {
    let _ = San::from_ascii(data);
});
