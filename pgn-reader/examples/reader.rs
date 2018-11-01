extern crate pgn_reader;

use std::fs::File;
use std::env;
use pgn_reader::reader::PgnReader;

fn main() {
    for arg in env::args().skip(1) {
        let file = File::open(&arg).expect("fopen");
        let mut reader = PgnReader::new(file);
        reader.read_game();
    }
}
