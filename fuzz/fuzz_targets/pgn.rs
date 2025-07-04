#![no_main]

use std::ops::ControlFlow;

use libfuzzer_sys::fuzz_target;
use pgn_reader::{Reader, Visitor};

struct NoopVisitor;

impl Visitor for NoopVisitor {
    type Tags = ();
    type Movetext = ();
    type Output = ();

    fn begin_tags(&mut self) -> std::ops::ControlFlow<Self::Output, Self::Tags> {
        ControlFlow::Continue(())
    }

    fn begin_movetext(&mut self, _tags: Self::Tags) -> ControlFlow<Self::Output, Self::Movetext> {
        ControlFlow::Continue(())
    }

    fn end_game(&mut self, _movetext: Self::Movetext) -> Self::Output {}
}

fuzz_target!(|data: &[u8]| {
    // Test reading
    let mut reader = Reader::new(data);
    while matches!(reader.read_game(&mut NoopVisitor), Err(_) | Ok(Some(_))) {}

    // Test skipping
    let mut reader = Reader::new(data);
    while matches!(reader.skip_game(), Err(_) | Ok(true)) {}
});
