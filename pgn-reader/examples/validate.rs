// Validates moves in PGNs.
// Usage: cargo run --release --example validate -- [PGN]...

extern crate pgn_reader;
extern crate memmap;
extern crate madvise;
extern crate shakmaty;

use pgn_reader::{Visitor, Skip, Reader, San};

use shakmaty::{Chess, Position};
use shakmaty::fen::Fen;

use memmap::{Mmap, Protection};
use madvise::{AccessPattern, AdviseMemory};

use std::env;
use std::str;

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

    fn header(&mut self, key: &'pgn [u8], value: &'pgn [u8]) {
        // Support games from a non-standard starting position.
        if key == b"FEN" {
            let pos = str::from_utf8(value).ok()
                .and_then(|h| h.parse::<Fen>().ok())
                .and_then(|f| f.position().ok());

            if let Some(pos) = pos {
                self.pos = pos;
            } else {
                eprintln!("invalid fen header in game {}: {:?}", self.games, value);
                self.success = false;
            }
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
        let mmap = Mmap::open_path(&arg, Protection::Read).expect("mmap");
        let pgn = unsafe { mmap.as_slice() };
        pgn.advise_memory_access(AccessPattern::Sequential).expect("madvise");

        let mut validator = Validator::new();
        success &= Reader::new(&mut validator, pgn).into_iter().all(|s| s);

        println!("{}: {}", arg, if success { "success" } else { "errors" });
    }

    if !success {
        ::std::process::exit(1);
    }
}
