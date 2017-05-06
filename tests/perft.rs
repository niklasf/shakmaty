extern crate shakmaty;

use shakmaty::Position;
use shakmaty::Chess;
use shakmaty::variants::{Crazyhouse, Giveaway, KingOfTheHill, Horde, Atomic, RacingKings};
use shakmaty::fen::Fen;
use shakmaty::perft;

use std::io::BufReader;
use std::io::prelude::*;
use std::fs::File;

fn test_perft_file<P: Position>(path: &str, node_limit: usize) {
    let file = File::open(path).expect("failed to open test suite");
    let reader = BufReader::new(file);

    let mut pos = P::default();

    for line in reader.lines().map(|l| l.unwrap()) {
        println!("{}", line);

        let trimmed = line.trim();
        let mut slices = trimmed.splitn(2, ' ');

        match slices.next() {
            Some("epd") => {
                pos = slices.next().expect("missing epd")
                            .parse::<Fen>().expect("invalid fen")
                            .position().expect("illegal fen");
            },
            Some("perft") => {
                let mut params = slices.next().expect("missing perft params").splitn(2, ' ');

                let depth = params.next().expect("missing perft depth")
                                  .parse().expect("depth not an integer");

                let nodes = params.next().expect("missing perft nodes")
                                  .parse().expect("nodes not an integer");

                if nodes <= node_limit {
                    assert_eq!(perft::perft(&pos, depth), nodes);
                }
            },
            _ => {},
        }
    }
}

#[test]
fn test_random() {
    test_perft_file::<Chess>("tests/random.perft", 10000);
}

#[test]
fn test_tricky() {
    test_perft_file::<Chess>("tests/tricky.perft", 1000000);
}

#[test]
fn test_crazyhouse() {
    test_perft_file::<Crazyhouse>("tests/crazyhouse.perft", 1000000);
}

#[test]
fn test_giveaway() {
    test_perft_file::<Giveaway>("tests/giveaway.perft", 1000000);
}

#[test]
fn test_kingofthehill() {
    test_perft_file::<KingOfTheHill>("tests/kingofthehill.perft", 1000000);
}

#[test]
fn test_horde() {
    test_perft_file::<Horde>("tests/horde.perft", 1000000);
}

#[test]
fn test_atomic() {
    test_perft_file::<Atomic>("tests/atomic.perft", 1000000);
}

#[test]
fn test_racing_kings() {
    test_perft_file::<RacingKings>("tests/racingkings.perft", 1000000);
}
