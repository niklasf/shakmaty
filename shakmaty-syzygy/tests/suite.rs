extern crate csv;
extern crate shakmaty;
extern crate shakmaty_syzygy;

use shakmaty::fen::Fen;
use shakmaty::variants::{Atomic, Chess, Giveaway};
use shakmaty::Position;
use shakmaty_syzygy::{Syzygy, Tablebases};

fn test_csv<S: Position + Clone + Syzygy>(path: &str) {
    let mut tables = Tablebases::new();
    tables.add_directory("tables/regular").expect("read directory");
    tables.add_directory("tables/atomic").expect("read directory");
    tables.add_directory("tables/giveaway").expect("read directory");

    let mut reader = csv::Reader::from_path(path).expect("reader");

    for line in reader.records() {
        let record = line.expect("record");

        let fen: Fen = record
            .get(0).expect("fen field")
            .parse().expect("valid fen");

        let expected_wdl: i8 = record
            .get(1).expect("wdl field")
            .parse().expect("valid wdl");

        let expected_dtz: i16 = record
            .get(2).expect("dtz field")
            .parse().expect("valid dtz");

        let pos: S = fen.position().expect("legal");

        println!("{} | wdl: {} | dtz: {}", fen, expected_wdl, expected_dtz);

        let wdl = tables.probe_wdl(&pos).expect("probe wdl");
        assert_eq!(i8::from(wdl), expected_wdl);

        let dtz = tables.probe_dtz(&pos).expect("probe dtz");
        assert_eq!(i16::from(dtz), expected_dtz);
    }
}

#[test]
fn test_regular() {
    test_csv::<Chess>("tests/regular.csv");
}

#[test]
fn test_atomic() {
    test_csv::<Atomic>("tests/atomic.csv");
}

#[test]
fn test_giveaway() {
    test_csv::<Giveaway>("tests/giveaway.csv");
}
