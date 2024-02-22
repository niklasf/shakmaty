#[cfg(feature = "variant")]
#[test]
fn test_zobrist_reference() {
    use serde::Deserialize;
    use serde_with::{
        formats::SpaceSeparator, serde_as, DisplayFromStr, FromInto, StringWithSeparator,
    };
    use shakmaty::{
        uci::Uci,
        variant::{Variant, VariantPosition},
        zobrist::{Zobrist128, ZobristHash as _},
        EnPassantMode, Position as _,
    };

    #[serde_as]
    #[derive(Deserialize)]
    struct Record {
        #[serde_as(as = "DisplayFromStr")]
        variant: Variant,
        #[serde_as(as = "StringWithSeparator<SpaceSeparator, Uci>")]
        uci: Vec<Uci>,
        #[serde_as(as = "FromInto<u128>")]
        zobrist: Zobrist128,
    }

    let mut reader = csv::Reader::from_path("tests/zobrist.csv").expect("reader");

    for (i, record) in reader.deserialize().enumerate() {
        let record: Record = record.expect("record");

        let mut pos = VariantPosition::new(record.variant);

        for uci in record.uci {
            let m = uci.to_move(&pos).expect("legal uci");
            pos.play_unchecked(&m);
        }

        assert_eq!(
            pos.zobrist_hash::<Zobrist128>(EnPassantMode::Legal),
            record.zobrist,
            "line {}",
            i + 1
        )
    }
}
