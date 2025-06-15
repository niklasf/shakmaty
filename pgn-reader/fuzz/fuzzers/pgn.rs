#![no_main]

use libfuzzer_sys::fuzz_target;

use pgn_reader::{BufferedReader, Visitor};
use std::io::Cursor;

struct MyVisitor;

impl Visitor for MyVisitor {
    type Result = ();
    fn end_game(&mut self) {}
}

fuzz_target!(|data: &[u8]| {
    let mut visitor = MyVisitor;
    let _ = BufferedReader::new(Cursor::new(data)).read_all(&mut visitor);
});
