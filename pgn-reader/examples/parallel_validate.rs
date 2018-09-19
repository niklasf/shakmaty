// Validates moves in PGNs.
// Usage: cargo run --release --example validate -- [PGN]...

extern crate pgn_reader;
extern crate memmap;
extern crate madvise;
extern crate shakmaty;
extern crate chan;

use pgn_reader::{Visitor, Skip, Reader, San};

use shakmaty::{Chess, Position};
use shakmaty::fen::Fen;

use memmap::Mmap;
use madvise::{AccessPattern, AdviseMemory};

use std::env;
use std::fs::File;
use std::mem;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

struct Game {
    index: usize,
    pos: Chess,
    sans: Vec<San>,
    success: bool,
}

impl Game {
    fn validate(mut self) -> bool {
        self.success && {
            for san in self.sans {
                let m = match san.to_move(&self.pos) {
                    Ok(m) => m,
                    Err(_) => return false,
                };

                self.pos.play_unchecked(&m);
            }
            true
        }
    }
}

struct Validator {
    games: usize,
    game: Game,
}

impl Validator {
    fn new() -> Validator {
        Validator {
            games: 0,
            game: Game {
                index: 0,
                pos: Chess::default(),
                sans: Vec::new(),
                success: true,
            }
        }
    }
}

impl<'pgn> Visitor<'pgn> for Validator {
    type Result = Game;

    fn begin_game(&mut self) {
        self.games += 1;
    }

    fn header(&mut self, key: &'pgn [u8], value: &'pgn [u8]) {
        // Support games from a non-standard starting position.
        if key == b"FEN" {
            let fen = match Fen::from_ascii(value) {
                Ok(fen) => fen,
                Err(err) => {
                    eprintln!("invalid fen header in game {}: {} ({:?})", self.games, err, value);
                    self.game.success = false;
                    return;
                },
            };

            self.game.pos = match fen.position() {
                Ok(pos) => pos,
                Err(err) => {
                    eprintln!("illegal fen header in game {}: {} ({})", self.games, err, fen);
                    self.game.success = false;
                    return;
                },
            };
        }
    }

    fn end_headers(&mut self) -> Skip {
        Skip(!self.game.success)
    }

    fn begin_variation(&mut self) -> Skip {
        Skip(true) // stay in the mainline
    }

    fn san(&mut self, san: San) {
        if self.game.success {
            self.game.sans.push(san);
        }
    }

    fn end_game(&mut self, _game: &'pgn [u8]) -> Self::Result {
        mem::replace(&mut self.game, Game {
            index: self.games,
            pos: Chess::default(),
            sans: Vec::with_capacity(80),
            success: true,
        })
    }
}

fn main() {
    let mut complete_success = true;

    for arg in env::args().skip(1) {
        let success = Arc::new(AtomicBool::new(true));

        let file = File::open(&arg).expect("fopen");
        let pgn = unsafe { Mmap::map(&file).expect("mmap") };
        pgn.advise_memory_access(AccessPattern::Sequential).expect("madvise");

        let mut validator = Validator::new();
        let (send, recv) = chan::sync(256);

        std::thread::spawn(move || {
            for game in Reader::new(&mut validator, &pgn[..]) {
                send.send(game);
            }
        });

        let wg = chan::WaitGroup::new();

        for _ in 0..2 {
            wg.add(1);
            let wg = wg.clone();
            let recv = recv.clone();
            let success = success.clone();
            std::thread::spawn(move || {
                for game in recv.iter() {
                    let index = game.index;
                    if !game.validate() {
                        eprintln!("illegal move in game {}", index);
                        success.store(false, Ordering::SeqCst);
                    }
                }
                wg.done();
            });
        }

        wg.wait();

        println!("{}: {}", arg, if success.load(Ordering::SeqCst) { "success" } else { "errors" });
        complete_success &= success.load(Ordering::SeqCst);
    }

    if !complete_success {
        ::std::process::exit(1);
    }
}
