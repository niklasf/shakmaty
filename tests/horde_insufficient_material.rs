#[cfg(feature = "variant")]
#[test]
fn white_insufficient_material_in_horde() {
    use serde::Deserialize;
    use serde_with::{serde_as, DisplayFromStr};
    use shakmaty::{fen::Fen, variant::Horde, CastlingMode, Color, Position};

    #[serde_as]
    #[derive(Deserialize)]
    struct Record {
        #[serde_as(as = "DisplayFromStr")]
        fen: Fen,
        white_has_insufficient_material: bool,
        comment: String,
    }

    let mut reader =
        csv::Reader::from_path("tests/horde_insufficient_material.csv").expect("reader");

    for (i, record) in reader.deserialize().enumerate() {
        let record: Record = record.expect("record");
        let pos: Horde = record
            .fen
            .clone()
            .into_position(CastlingMode::Chess960)
            .expect("valid position");

        assert_eq!(
            pos.has_insufficient_material(Color::White),
            record.white_has_insufficient_material,
            "{} with comment {:?} in line {}",
            record.fen,
            record.comment,
            i + 1
        );
    }
}
