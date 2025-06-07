#![no_main]

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;
use shakmaty::{packed::PackedSetup, variant::Variant, Setup};

#[derive(Arbitrary)]
struct VariantSetup {
    setup: Setup,
    variant: Variant,
}

fuzz_target!(|data: &[u8]| {
    let mut unstructured = Unstructured::new(data);
    let Ok(VariantSetup { setup, variant }) = VariantSetup::arbitrary(&mut unstructured) else {
        return;
    };
    let Ok(packed_setup) = PackedSetup::pack_variant(&setup, variant) else {
        return;
    };
    let packed_setup = PackedSetup::try_from_bytes(packed_setup.as_bytes()).expect("roundtrip");
    let (unpacked_setup, unpacked_variant) = packed_setup.unpack_variant().expect("unpack");
    assert_eq!(unpacked_setup, setup);
    assert_eq!(unpacked_variant, variant);
});
