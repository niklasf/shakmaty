#![no_main]

use std::{convert::Infallible, io::Cursor};

use libfuzzer_sys::fuzz_target;
use pgn_reader::{BufferedReader, Visitor};

struct MyVisitor;

impl Visitor for MyVisitor {
    type Output = ();
    type Error = Infallible;
    fn end_game(&mut self) {}
}

fuzz_target!(|data: &[u8]| {
    let mut visitor = MyVisitor;
    let _ = BufferedReader::new(Cursor::new(data)).read_all(&mut visitor);
});
