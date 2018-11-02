// Validates moves in PGNs.
// Usage: cargo run --release --example validate -- [PGN]...

extern crate pgn_reader;
extern crate memmap;
extern crate madvise;
extern crate shakmaty;

use pgn_reader::{Visitor, Skip, Reader, RawHeader, San};

use shakmaty::{Chess, Position};
use shakmaty::fen::Fen;

use memmap::Mmap;
use madvise::{AccessPattern, AdviseMemory};

use std::env;
use std::fs::File;

struct Validator {
    games: usize,
    pos: Chess,
    success: bool,
}

impl Validator {
    fn new() -> Validator {
        Validator { games: 0, pos: Chess::default(), success: true }
    }
}

impl<'pgn> Visitor<'pgn> for Validator {
    type Result = bool;

    fn begin_game(&mut self) {
        self.games += 1;
        self.pos = Chess::default();
        self.success = true;
    }

    fn header(&mut self, key: &'pgn [u8], value: RawHeader<'pgn>) {
        // Support games from a non-standard starting position.
        if key == b"FEN" {
            let fen = match Fen::from_ascii(value.as_bytes()) {
                Ok(fen) => fen,
                Err(err) => {
                    eprintln!("invalid fen header in game {}: {} ({:?})", self.games, err, value);
                    self.success = false;
                    return;
                },
            };

            self.pos = match fen.position() {
                Ok(pos) => pos,
                Err(err) => {
                    eprintln!("illegal fen header in game {}: {} ({})", self.games, err, fen);
                    self.success = false;
                    return;
                },
            };
        }
    }

    fn end_headers(&mut self) -> Skip {
        Skip(!self.success)
    }

    fn begin_variation(&mut self) -> Skip {
        Skip(true) // stay in the mainline
    }

    fn san(&mut self, san: San) {
        if self.success {
            match san.to_move(&self.pos) {
                Ok(m) => self.pos.play_unchecked(&m),
                Err(err) => {
                    eprintln!("error in game {}: {} {}", self.games, err, san);
                    self.success = false;
                },
            }
        }
    }

    fn end_game(&mut self, _game: &'pgn [u8]) -> Self::Result {
        self.success
    }
}

fn main() {
    let mut success = true;

    for arg in env::args().skip(1) {
        let file = File::open(&arg).expect("fopen");
        let pgn = unsafe { Mmap::map(&file).expect("mmap") };
        pgn.advise_memory_access(AccessPattern::Sequential).expect("madvise");

        let mut validator = Validator::new();
        success &= Reader::new(&mut validator, &pgn[..]).into_iter().all(|s| s);

        println!("{}: {}", arg, if success { "success" } else { "errors" });
    }

    if !success {
        ::std::process::exit(1);
    }
}
