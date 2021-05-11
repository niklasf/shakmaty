#[cfg(feature = "variant")]
#[test]
fn white_insufficient_material_in_horde() {
    use shakmaty::{CastlingMode,Position};
    use shakmaty::variant::Horde;
    use shakmaty::fen::Fen;
    use shakmaty::Color;
    use shakmaty::FromSetup;

    use std::io::BufReader;
    use std::io::prelude::*;
    use std::fs::File;


    fn assert_white_insufficient_material_in_horde<P>(fen: &str, expected_outcome:bool, comment: &str)
    where
        P: Position + FromSetup,
    {
        let pos: P = fen.parse::<Fen>()
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
            comment,pos.board(),fen,has_insufficient_material,expected_outcome
        );
    }


	let file = File::open("tests/horde_insufficient_material.csv").expect("failed to open test suite");
	let reader = BufReader::new(file);
	let mut current;

	for line in reader.lines() {
		current = line.unwrap();
		let fen_expected_comment: Vec<&str> = current.split(',').collect();

		assert_white_insufficient_material_in_horde::<Horde>(
			fen_expected_comment[0],
			if fen_expected_comment[1]=="true" {true} else {false},
			fen_expected_comment[2]
		);
	}

}
