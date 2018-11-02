extern crate pgn_reader;
extern crate memmap;
extern crate madvise;

use std::fs::File;
use std::env;
use std::str;
use pgn_reader::RawHeader;
use pgn_reader::reader::{PgnReader, SliceReader, Visitor};
use memmap::Mmap;
use madvise::{AccessPattern, AdviseMemory};

struct MyVisitor;

impl Visitor for MyVisitor {
    type Result = ();

    fn header(&mut self, _key: &[u8], _value: RawHeader<'_>) {
        //println!("{:?}: {:?}", String::from_utf8_lossy(key), value);
    }

    fn end_game(&mut self) { }
}

fn main() {
    for arg in env::args().skip(1) {
        let file = File::open(&arg).expect("fopen");

        //let pgn = unsafe { Mmap::map(&file).expect("mmap") };
        //pgn.advise_memory_access(AccessPattern::Sequential).expect("madvise");
        //let mut reader = SliceReader::new(&pgn);

        let mut reader = PgnReader::new(file);
        let mut count = 0;
        while reader.read_game(&mut MyVisitor).unwrap().is_some() {
            count += 1;
        }
        println!("found {} games", count);

    }
}
