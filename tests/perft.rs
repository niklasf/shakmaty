use std::{
    fs::File,
    io::{prelude::*, BufReader},
};

use shakmaty::{fen::Fen, perft, CastlingMode, Chess, FromSetup, Position, PositionError};

fn test_perft_file<P>(path: &str, node_limit: u64)
where
    P: Position + FromSetup + Default + Clone,
{
    let file = File::open(path).expect("failed to open test suite");
    let reader = BufReader::new(file);

    let mut pos = P::default();

    for line in reader.lines().map(|l| l.unwrap()) {
        println!("{}", line);

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
                    .or_else(PositionError::ignore_impossible_check)
                    .expect("illegal fen");
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

// macro for generating tests
macro_rules! gen_tests {
    ($($fn_name:ident, $t:ty, $path:tt, $num:expr,)+) => {
        $(
            #[test]
            #[cfg_attr(miri, ignore)]
            fn $fn_name() {
                test_perft_file::<$t>($path, $num);
            }
        )+
    }
}

gen_tests! {
    test_tricky,      Chess,       "tests/tricky.perft",        100_000,
    test_random,      Chess,       "tests/random.perft",         10_000,
    test_chess960,    Chess,       "tests/chess960.perft",      100_000,
}

#[cfg(feature = "variant")]
use shakmaty::variant::{Antichess, Atomic, Crazyhouse, Horde, RacingKings, ThreeCheck};
#[cfg(feature = "variant")]
gen_tests! {
    test_atomic,      Atomic,      "tests/atomic.perft",      1_000_000,
    test_antichess,   Antichess,   "tests/antichess.perft",   1_000_000,
    test_crazyhouse,  Crazyhouse,  "tests/crazyhouse.perft",  1_000_000,
    test_racingkings, RacingKings, "tests/racingkings.perft", 1_000_000,
    test_horde,       Horde,       "tests/horde.perft",       1_000_000,
    test_3check,      ThreeCheck,  "tests/3check.perft",      1_000_000,
}
