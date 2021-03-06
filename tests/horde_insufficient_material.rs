#[cfg(feature = "variant")]
#[test]
fn white_insufficient_material_in_horde() {
    use shakmaty::fen::Fen;
    use shakmaty::variant::Horde;
    use shakmaty::Color;
    use shakmaty::FromSetup;
    use shakmaty::{CastlingMode, Position};

    use std::fs::File;
    use std::io::prelude::*;
    use std::io::BufReader;

    fn assert_white_insufficient_material_in_horde<P>(
        fen: &str,
        expected_outcome: bool,
        comment: &str,
    ) where
        P: Position + FromSetup,
    {
        let pos: P = fen
            .parse::<Fen>()
            .expect("valid fen")
            .position(CastlingMode::Chess960)
            .expect("valid position");

        let has_insufficient_material = pos.has_insufficient_material(Color::White);

        assert_eq!(
            has_insufficient_material,
            expected_outcome,
            "
\n
{:?}\n
{:?}
{:?}\n
computed_outcome={:?}
expected_outcome={:?}\n\n",
            comment,
            pos.board(),
            fen,
            has_insufficient_material,
            expected_outcome
        );
    }


    let file =
        File::open("tests/horde_insufficient_material.csv").expect("failed to open test suite");
    let reader = BufReader::new(file);

    for line in reader.lines().map(|l| l.unwrap()) {
        let mut columns = line.split(',');

        assert_white_insufficient_material_in_horde::<Horde>(
            columns.next().unwrap(),
            columns.next().unwrap() == "true",
            columns.next().unwrap(),
        );
    }
}
