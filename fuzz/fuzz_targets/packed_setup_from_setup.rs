#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use shakmaty::{packed::PackedSetup, variant::Variant, Setup};

#[derive(Arbitrary, Debug)]
struct VariantSetup {
    setup: Setup,
    variant: Variant,
}

fuzz_target!(|data: VariantSetup| {
    let VariantSetup { mut setup, variant } = data;

    let Ok(packed_setup) = PackedSetup::pack_variant(&setup, variant) else {
        return;
    };
    let packed_setup = PackedSetup::try_from_bytes(packed_setup.as_bytes()).expect("roundtrip");
    let (unpacked_setup, unpacked_variant) = packed_setup.unpack_variant().expect("unpack");

    // For the comparison, assume that the roundtrip fills variant-based default
    // values for pockets and remaining checks.
    if variant == Variant::Crazyhouse {
        setup.pockets = Some(setup.pockets.unwrap_or_default());
    }
    if variant == Variant::ThreeCheck {
        setup.remaining_checks = Some(setup.remaining_checks.unwrap_or_default());
    }

    assert_eq!(unpacked_setup, setup);
    assert_eq!(unpacked_variant, variant);
});
