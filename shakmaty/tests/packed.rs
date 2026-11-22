/// Reference test vectors for Lichess binary FEN, adapted from
/// <https://github.com/lichess-org/scalachess/blob/8c94e2087f83affb9718fd2be19c34866c9a1a22/test-kit/src/test/scala/format/BinaryFenTest.scala>.
///
/// The `lichess` encoding may differ from the `packed` encoding
/// only due to variant headers for Chess960 and From Position (which
/// are really just Chess), and trailing zeros.
#[cfg(feature = "variant")]
#[test]
fn test_packed_setup_reference() {
    use serde::Deserialize;
    use serde_with::{DisplayFromStr, serde_as};
    use shakmaty::{fen::Fen, packed::PackedSetup, variant::Variant};

    #[serde_as]
    #[derive(Deserialize)]
    struct Record {
        #[serde_as(as = "DisplayFromStr")]
        variant: Variant,
        #[serde_as(as = "DisplayFromStr")]
        fen: Fen,
        lichess: String,
        packed: String,
    }

    fn unhex(s: &str) -> Vec<u8> {
        (0..s.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&s[i..i + 2], 16).expect("hex"))
            .collect()
    }

    let mut reader = csv::Reader::from_path("tests/packed_setup.csv").expect("reader");

    for (i, record) in reader.deserialize().enumerate() {
        let record: Record = record.expect("record");
        let line = i + 2;
        let expected = (record.fen.into_setup(), record.variant);

        for bytes in [&record.lichess, &record.packed] {
            assert_eq!(
                PackedSetup::try_from_bytes(&unhex(bytes))
                    .expect("bytes")
                    .unpack_variant()
                    .expect("unpack"),
                expected,
                "line {line}"
            );
        }

        assert_eq!(
            PackedSetup::pack_variant(&expected.0, expected.1)
                .expect("pack")
                .as_bytes(),
            unhex(&record.packed),
            "line {line}"
        );
    }
}
