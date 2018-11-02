#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate pgn_reader;

use pgn_reader::{BufferedReader, Visitor};

struct MyVisitor;

impl Visitor for MyVisitor {
    type Result = ();
    fn end_game(&mut self) { }
}

fuzz_target!(|data: &[u8]| {
    let mut visitor = MyVisitor;
    let _ = BufferedReader::new_cursor(data).read_all(&mut visitor);
});
