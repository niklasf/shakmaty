use std::{
    fs::File,
    io::{BufReader, prelude::*},
};

use shakmaty::{CastlingMode, Chess, FromSetup, Position, PositionError, fen::Fen, perft};

fn test_perft_file_impl<P, F>(path: &str, node_limit: u64, handle_error: F)
where
    P: Position + FromSetup + Default + Clone,
    F: Fn(PositionError<P>) -> P,
{
    let file = File::open(path).expect("failed to open test suite");
    let reader = BufReader::new(file);

    let mut pos = P::default();

    for line in reader.lines().map(|l| l.unwrap()) {
        println!("{line}");

        let trimmed = line.trim();
        let mut slices = trimmed.splitn(2, ' ');

        match slices.next() {
            Some("epd") => {
                pos = slices
                    .next()
                    .expect("missing epd")
                    .parse::<Fen>()
                    .expect("invalid fen")
                    .into_position(CastlingMode::Chess960)
                    .unwrap_or_else(|err| handle_error(err));
            }
            Some("perft") => {
                let mut params = slices.next().expect("missing perft params").splitn(2, ' ');

                let depth = params
                    .next()
                    .expect("missing perft depth")
                    .parse()
                    .expect("depth not an integer");

                let nodes = params
                    .next()
                    .expect("missing perft nodes")
                    .parse()
                    .expect("nodes not an integer");

                if nodes <= node_limit {
                    assert_eq!(perft(&pos, depth), nodes);
                }
            }
            _ => {}
        }
    }
}

fn test_perft_file<P>(path: &str, node_limit: u64)
where
    P: Position + FromSetup + Default + Clone,
{
    test_perft_file_impl::<P, _>(path, node_limit, |err| panic!("illegal epd: {}", err));
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_tricky_perft() {
    test_perft_file_impl::<Chess, _>("tests/tricky.perft", 100_000, |err| {
        err.ignore_impossible_check()
            .or_else(PositionError::ignore_too_much_material)
            .expect("illegal epd")
    });
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_random_perft() {
    test_perft_file::<Chess>("tests/random.perft", 10_000);
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_chess960_perft() {
    test_perft_file::<Chess>("tests/chess960.perft", 100_000);
}

#[cfg(feature = "variant")]
#[test]
#[cfg_attr(miri, ignore)]
fn test_variant_perft() {
    use shakmaty::variant;

    test_perft_file::<variant::Antichess>("tests/antichess.perft", 1_000_000);
    test_perft_file::<variant::Atomic>("tests/atomic.perft", 1_000_000);
    test_perft_file::<variant::Crazyhouse>("tests/crazyhouse.perft", 1_000_000);
    test_perft_file::<variant::Horde>("tests/horde.perft", 1_000_000);
    test_perft_file::<variant::RacingKings>("tests/racingkings.perft", 1_000_000);
    test_perft_file::<variant::ThreeCheck>("tests/3check.perft", 1_000_000);
}
