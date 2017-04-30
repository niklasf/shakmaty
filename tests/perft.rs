extern crate shakmaty;

use shakmaty::Position;
use shakmaty::Variant;
use shakmaty::variant::Standard;
use shakmaty::perft;

use std::io::BufReader;
use std::io::prelude::*;
use std::fs::File;

fn test_perft_file<V: Variant>(path: &str, node_limit: usize) {
    let file = File::open(path).unwrap();
    let reader = BufReader::new(file);

    let mut pos = V::default();

    for line in reader.lines().map(|l| l.unwrap()) {
        println!("{}", line);

        let trimmed = line.trim();
        let mut slices = trimmed.splitn(2, ' ');

        match slices.next() {
            Some("epd") => {
                pos = V::from_fen(slices.next().unwrap()).unwrap()
            },
            Some("perft") => {
                let mut params = slices.next().unwrap().splitn(2, ' ');
                let depth = params.next().unwrap().parse().unwrap();
                let nodes = params.next().unwrap().parse().unwrap();

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
    test_perft_file::<Standard>("tests/random.perft", 10000);
}

#[test]
fn test_tricky() {
    test_perft_file::<Standard>("tests/tricky.perft", 1000000);
}
