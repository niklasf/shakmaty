extern crate pgn_reader;
extern crate memmap;
extern crate madvise;

use std::env;

use pgn_reader::{Reader, Visitor, San};
use memmap::{Mmap, Protection};
use madvise::{AccessPattern, AdviseMemory};

struct Stats {
    moves: usize,
    games: usize,
}

impl Stats {
    fn new() -> Stats {
        Stats {
            moves: 0,
            games: 0,
        }
    }
}

impl Visitor for Stats {
    type Result = ();

    fn end_game(&mut self, _: &[u8]) {
        self.games += 1;
    }

    fn san(&mut self, _: San) {
        self.moves += 1;
    }
}

fn main() {
    for arg in env::args().skip(1) {
        let mmap = Mmap::open_path(&arg, Protection::Read).expect("mmap");
        let pgn = unsafe { mmap.as_slice() };
        pgn.advise_memory_access(AccessPattern::Sequential).expect("madvise");

        let mut stats = Stats::new();
        Reader::new(&mut stats, pgn).read_all();

        println!("{}: {} games, {} moves", arg, stats.games, stats.moves);
    }
}
