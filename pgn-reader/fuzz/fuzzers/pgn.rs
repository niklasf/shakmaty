#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate pgn_reader;

use pgn_reader::{Reader, Visitor};

impl Visitor for () {
    type Result = ();
    fn end_game(&mut self, _game: &[u8]) { }
}

fuzz_target!(|data: &[u8]| {
    let _ = Reader::new((), data).read_all();
});
