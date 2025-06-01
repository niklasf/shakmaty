#![no_main]

use libfuzzer_sys::fuzz_target;
use shakmaty::packed::PackedSetup;

fuzz_target!(|data: &[u8]| {
    let Ok(packed_setup) = PackedSetup::try_from_bytes(data) else {
        return;
    };
    let Ok((setup, variant)) = packed_setup.unpack_variant() else {
        return;
    };
    let repacked = PackedSetup::pack_variant(&setup, variant).expect("repacked");
    let (unpacked_setup, unpacked_variant) = repacked.unpack_variant().expect("roundtrip");
    assert_eq!(unpacked_setup, setup);
    assert_eq!(unpacked_variant, variant);
});
