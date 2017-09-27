#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate pgn_reader;

use pgn_reader::{Reader, Visitor};

struct MyVisitor;

impl Visitor for MyVisitor {
    type Result = ();
    fn end_game(&mut self, _game: &[u8]) { }
}

fuzz_target!(|data: &[u8]| {
    let mut visitor = MyVisitor;
    let _ = Reader::new(&mut visitor, data).read_all();
});
