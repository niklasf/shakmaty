// Validates moves in PGNs.
// Usage: cargo run --release --example validate -- [PGN]...

use std::{
    env,
    fs::File,
    io, mem,
    ops::ControlFlow,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

use pgn_reader::{
    RawTag, Reader, SanPlus, Skip, Visitor,
    shakmaty::{CastlingMode, Chess, Position, fen::Fen, san::San},
};

struct Game {
    index: usize,
    pos: Chess,
    sans: Vec<San>,
    success: bool,
}

impl Game {
    fn new_with_index(index: usize) -> Game {
        Game {
            index,
            pos: Chess::new(),
            sans: Vec::with_capacity(80),
            success: true,
        }
    }

    fn validate(mut self) -> bool {
        self.success && {
            for san in self.sans {
                let m = match san.to_move(&self.pos) {
                    Ok(m) => m,
                    Err(_) => return false,
                };

                self.pos.play_unchecked(m);
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
            },
        }
    }
}

impl Visitor for Validator {
    type Output = Game;

    fn begin_tags(&mut self) -> ControlFlow<Self::Output> {
        self.games += 1;
        ControlFlow::Continue(())
    }

    fn tag(&mut self, name: &[u8], value: RawTag<'_>) -> ControlFlow<Self::Output> {
        // Support games from a non-standard starting position.
        if name == b"FEN" {
            let fen = match Fen::from_ascii(value.as_bytes()) {
                Ok(fen) => fen,
                Err(err) => {
                    eprintln!(
                        "invalid fen tag in game {}: {} ({:?})",
                        self.games, err, value
                    );
                    self.game.success = false;
                    return ControlFlow::Break(mem::replace(
                        &mut self.game,
                        Game::new_with_index(self.games),
                    ));
                }
            };

            self.game.pos = match fen.into_position(CastlingMode::Chess960) {
                Ok(pos) => pos,
                Err(err) => {
                    eprintln!(
                        "illegal fen tag in game {}: {} ({:?})",
                        self.games, err, value
                    );
                    self.game.success = false;
                    return ControlFlow::Break(mem::replace(
                        &mut self.game,
                        Game::new_with_index(self.games),
                    ));
                }
            };
        }
        ControlFlow::Continue(())
    }

    fn begin_variation(&mut self) -> ControlFlow<Self::Output, Skip> {
        ControlFlow::Continue(Skip(true)) // stay in the mainline
    }

    fn san(&mut self, san_plus: SanPlus) -> ControlFlow<Self::Output> {
        self.game.sans.push(san_plus.san);
        ControlFlow::Continue(())
    }

    fn end_game(&mut self) -> Self::Output {
        mem::replace(&mut self.game, Game::new_with_index(self.games))
    }
}

fn main() {
    let mut complete_success = true;

    for arg in env::args().skip(1) {
        let success = Arc::new(AtomicBool::new(true));

        let file = File::open(&arg).expect("fopen");

        let uncompressed: Box<dyn io::Read + Send> = if arg.ends_with(".zst") {
            Box::new(zstd::Decoder::new(file).expect("zst decoder"))
        } else if arg.ends_with(".bz2") {
            Box::new(bzip2::read::MultiBzDecoder::new(file))
        } else if arg.ends_with(".xz") {
            Box::new(xz2::read::XzDecoder::new(file))
        } else if arg.ends_with(".gz") {
            Box::new(flate2::read::GzDecoder::new(file))
        } else if arg.ends_with(".lz4") {
            Box::new(lz4::Decoder::new(file).expect("lz4 decoder"))
        } else {
            Box::new(file)
        };

        let mut validator = Validator::new();
        let (send, recv) = crossbeam::channel::bounded(128);

        crossbeam::scope(|scope| {
            scope.spawn(move |_| {
                for game in Reader::new(uncompressed).read_games(&mut validator) {
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
                            eprintln!("illegal move in game {index}");
                            success.store(false, Ordering::SeqCst);
                        }
                    }
                });
            }
        })
        .unwrap();

        let success = success.load(Ordering::SeqCst);
        println!("{}: {}", arg, if success { "success" } else { "errors" });
        complete_success &= success;
    }

    if !complete_success {
        ::std::process::exit(1);
    }
}
