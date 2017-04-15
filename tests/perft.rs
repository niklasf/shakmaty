extern crate shakmaty;

use shakmaty::position::Standard;
use shakmaty::perft::perft;
use shakmaty::Precomp;

use std::io::BufReader;
use std::io::prelude::*;
use std::fs::File;

fn test_perft_file(path: &str, node_limit: usize) {
    let precomp = Precomp::new();

    let file = File::open(path).unwrap();
    let reader = BufReader::new(file);

    let mut pos = Standard::default();

    for line in reader.lines().map(|l| l.unwrap()) {
        let trimmed = line.trim();
        let mut slices = trimmed.splitn(2, ' ');

        match slices.next() {
            Some("epd") => {
                pos = Standard::from_fen(slices.next().unwrap()).unwrap()
            },
            Some("perft") => {
                let mut params = slices.next().unwrap().splitn(2, ' ');
                let depth = params.next().unwrap().parse().unwrap();
                let nodes = params.next().unwrap().parse().unwrap();

                if nodes <= node_limit {
                    assert_eq!(perft(&pos, depth, &precomp), nodes);
                }
            },
            _ => {},
        }
    }
}

#[test]
fn test_random() {
    test_perft_file("tests/random.perft", 10000);
}

#[test]
fn test_tricky() {
    test_perft_file("tests/tricky.perft", 1000000);
}
