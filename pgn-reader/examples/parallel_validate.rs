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

use pgn_reader::{BufferedReader, RawTag, San, SanPlus, Skip, Visitor};
use shakmaty::{CastlingMode, Chess, Position, fen::Fen};

struct Game {
    index: usize,
    pos: Chess,
    sans: Vec<San>,
}

impl Game {
    fn validate(&mut self) -> bool {
        for san in &self.sans {
            let m = match san.to_move(&self.pos) {
                Ok(m) => m,
                Err(_) => return false,
            };

            self.pos.play_unchecked(m);
        }

        true
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
            },
        }
    }
}

impl Visitor for Validator {
    type Output = Game;
    type Break = anyhow::Error;

    fn begin_tags(&mut self) -> ControlFlow<Self::Break> {
        self.games += 1;

        ControlFlow::Continue(())
    }

    fn tag(&mut self, name: &[u8], value: RawTag<'_>) -> ControlFlow<Self::Break> {
        // Support games from a non-standard starting position.
        if name == b"FEN" {
            let fen = match Fen::from_ascii(value.as_bytes()) {
                Ok(fen) => fen,
                Err(e) => return ControlFlow::Break(anyhow::Error::new(e)),
            };

            self.game.pos = match fen.into_position(CastlingMode::Chess960) {
                Ok(fen) => fen,
                Err(e) => return ControlFlow::Break(anyhow::Error::new(e)),
            };
        }

        ControlFlow::Continue(())
    }

    fn san(&mut self, san_plus: SanPlus) -> ControlFlow<Self::Break> {
        self.game.sans.push(san_plus.san);

        ControlFlow::Continue(())
    }

    fn begin_variation(&mut self) -> ControlFlow<Self::Break, Skip> {
        ControlFlow::Continue(Skip(true)) // stay in the mainline
    }

    fn end_game(&mut self) -> Self::Output {
        mem::replace(
            &mut self.game,
            Game {
                index: self.games,
                pos: Chess::default(),
                sans: Vec::with_capacity(80),
            },
        )
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
                for game in BufferedReader::new(uncompressed).into_iter(&mut validator) {
                    send.send(game.expect("io")).unwrap();
                }
            });

            for _ in 0..3 {
                let recv = recv.clone();
                let success = success.clone();
                scope.spawn(move |_| {
                    for game in recv {
                        match game {
                            ControlFlow::Continue(mut game) => {
                                if !game.validate() {
                                    eprintln!("illegal move in game {}", game.index);
                                    success.store(false, Ordering::SeqCst);
                                }
                            }
                            ControlFlow::Break(error) => eprintln!("{error}"),
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
