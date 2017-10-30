// Counts games, moves and other tokens in PGNs.
// Usage: cargo run --release --example stats -- [PGN]...

extern crate pgn_reader;
extern crate memmap;
extern crate madvise;

use std::env;
use std::fs::File;

use pgn_reader::{Reader, Visitor, San, Nag, Outcome};
use memmap::Mmap;
use madvise::{AccessPattern, AdviseMemory};

#[derive(Debug, Default)]
struct Stats {
    games: usize,
    headers: usize,
    sans: usize,
    nags: usize,
    comments: usize,
    variations: usize,
    outcomes: usize,
}

impl Stats {
    fn new() -> Stats {
        Stats::default()
    }
}

impl<'pgn> Visitor<'pgn> for Stats {
    type Result = ();

    fn end_game(&mut self, game: &'pgn [u8]) {
        if !game.is_empty() {
            self.games += 1;
        }
    }

    fn header(&mut self, _key: &'pgn [u8], _value: &'pgn [u8]) {
        self.headers += 1;
    }

    fn san(&mut self, _san: San) {
        self.sans += 1;
    }

    fn nag(&mut self, _nag: Nag) {
        self.nags += 1;
    }

    fn comment(&mut self, _comment: &'pgn [u8]) {
        self.comments += 1;
    }

    fn end_variation(&mut self) {
        self.variations += 1;
    }

    fn outcome(&mut self, _outcome: Outcome) {
        self.outcomes += 1;
    }
}

fn main() {
    for arg in env::args().skip(1) {
        let file = File::open(&arg).expect("fopen");
        let pgn = unsafe { Mmap::map(&file).expect("mmap") };
        pgn.advise_memory_access(AccessPattern::Sequential).expect("madvise");

        let mut stats = Stats::new();
        Reader::new(&mut stats, &pgn[..]).read_all();

        println!("{}: {:?}", arg, stats);
    }
}
