use shakmaty::{fen::Fen, CastlingMode, Chess, FromSetup, Position};
use shakmaty_syzygy::{Syzygy, Tablebase};

fn test_csv<S>(path: &str)
where
    S: Position + FromSetup + Syzygy + Clone,
{
    let mut tables = Tablebase::new();
    tables
        .add_directory("tables/chess")
        .expect("read directory");
    tables
        .add_directory("tables/atomic")
        .expect("read directory");
    tables
        .add_directory("tables/antichess")
        .expect("read directory");

    let mut reader = csv::Reader::from_path(path).expect("reader");

    for line in reader.records() {
        let record = line.expect("record");

        let fen: Fen = record
            .get(0)
            .expect("fen field")
            .parse()
            .expect("valid fen");

        let expected_wdl: i8 = record
            .get(1)
            .expect("wdl field")
            .parse()
            .expect("valid wdl");

        let expected_dtz: i32 = record
            .get(2)
            .expect("dtz field")
            .parse()
            .expect("valid dtz");

        let pos: S = fen
            .clone()
            .into_position(CastlingMode::Chess960)
            .expect("legal");

        println!("{} | wdl: {} | dtz: {}", fen, expected_wdl, expected_dtz);

        match tables.probe_wdl_after_zeroing(&pos) {
            Ok(wdl) => assert_eq!(i8::from(wdl), expected_wdl),
            Err(err) => panic!("probe wdl: {}", err),
        }

        match tables.probe_dtz(&pos) {
            Ok(dtz) => assert_eq!(i32::from(dtz.ignore_rounding()), expected_dtz),
            Err(err) => panic!("probe dtz: {}", err),
        }
    }
}

#[test]
fn test_chess() {
    test_csv::<Chess>("tests/chess.csv");
}

#[cfg(feature = "variant")]
#[test]
fn test_atomic() {
    test_csv::<shakmaty::variant::Atomic>("tests/atomic.csv");
}

#[cfg(feature = "variant")]
#[test]
fn test_antichess() {
    test_csv::<shakmaty::variant::Antichess>("tests/antichess.csv");
}
