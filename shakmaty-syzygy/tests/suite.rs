extern crate shakmaty;
extern crate shakmaty_syzygy;
extern crate csv;

use shakmaty::{Position, Chess};
use shakmaty_syzygy::{Tablebases, Syzygy};
use shakmaty::fen::Fen;

fn test_csv<S: Position + Clone + Syzygy>(path: &str) {
    let mut tables = Tablebases::new();
    tables.open_directory("/opt/syzygy/regular/syzygy").expect("good tables");

    let mut reader = csv::Reader::from_path(path).expect("reader");

    for line in reader.records() {
        let record = line.expect("record");

        let fen: Fen = record
            .get(0).expect("fen field")
            .parse().expect("valid fen");

        let expected_wdl: i8 = record
            .get(1).expect("wdl field")
            .parse().expect("valid wdl");

        let pos: S = fen.position().expect("legal");

        if pos.board().pawns().any() {
            // TODO: Skip pawnful tables for now
            continue;
        }

        let wdl = tables.probe_wdl(&pos).expect("probe");
        assert_eq!(i8::from(wdl), expected_wdl);
    }
}

#[test]
fn test_regular() {
    test_csv::<Chess>("tests/regular.csv");
}
