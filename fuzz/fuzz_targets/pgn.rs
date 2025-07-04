#![no_main]

use libfuzzer_sys::fuzz_target;
use pgn_reader::{Reader, Visitor};

struct MyVisitor;

impl Visitor for MyVisitor {
    type Output = ();

    fn end_game(&mut self) {}
}

fuzz_target!(|data: &[u8]| {
    // Test reading
    let mut reader = Reader::new(data);
    while matches!(reader.read_game(&mut MyVisitor), Err(_) | Ok(Some(_))) {}

    // Test skipping
    let mut reader = Reader::new(data);
    while matches!(reader.skip_game(), Err(_) | Ok(true)) {}
});
