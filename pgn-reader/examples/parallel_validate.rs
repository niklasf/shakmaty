// Validates moves in PGNs.
// Usage: cargo run --release --example validate -- [PGN]...

extern crate crossbeam;
extern crate pgn_reader;
extern crate shakmaty;

use std::env;
use std::fs::File;
use std::mem;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use shakmaty::{Chess, Position};
use shakmaty::fen::Fen;

use pgn_reader::{Visitor, Skip, BufferedReader, RawHeader, San, SanPlus};

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

impl Visitor for Validator {
    type Result = Game;

    fn begin_game(&mut self) {
        self.games += 1;
    }

    fn header(&mut self, key: &[u8], value: RawHeader<'_>) {
        // Support games from a non-standard starting position.
        if key == b"FEN" {
            let fen = match Fen::from_ascii(value.as_bytes()) {
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

    fn san(&mut self, san_plus: SanPlus) {
        if self.game.success {
            self.game.sans.push(san_plus.san);
        }
    }

    fn end_game(&mut self) -> Self::Result {
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

        let mut validator = Validator::new();
        let (send, recv) = crossbeam::channel::bounded(128);

        crossbeam::scope(|scope| {
            scope.spawn(move |_| {
                for game in BufferedReader::new(file).into_iter(&mut validator) {
                    send.send(game.expect("io")).unwrap();
                }
            });

            for _ in 0..3 {
                let recv = recv.clone();
                let success = success.clone();
                scope.spawn(move |_| {
                    for game in recv {
                        let index = game.index;
                        if !game.validate() {
                            eprintln!("illegal move in game {}", index);
                            success.store(false, Ordering::SeqCst);
                        }
                    }
                });
            }
        }).unwrap();

        let success = success.load(Ordering::SeqCst);
        println!("{}: {}", arg, if success { "success" } else { "errors" });
        complete_success &= success;
    }

    if !complete_success {
        ::std::process::exit(1);
    }
}
