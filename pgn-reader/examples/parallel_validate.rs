// Validates moves in PGNs.
// Usage: cargo run --release --example validate -- [PGN]...

use std::{
    env,
    error::Error,
    fs::File,
    io,
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

type RawFen = Vec<u8>;

struct Game {
    index: usize,
    fen: Option<RawFen>,
    sans: Vec<San>,
}

impl Game {
    fn validate(self) -> Result<(), Box<dyn Error>> {
        let fen = self.fen.map(|fen| Fen::from_ascii(&fen)).transpose()?;

        let mut pos: Chess = fen
            .map(|fen| fen.into_position(CastlingMode::Chess960))
            .transpose()?
            .unwrap_or_default();

        for san in self.sans {
            pos.play_unchecked(san.to_move(&pos)?);
        }

        Ok(())
    }
}

struct Validator {
    games: usize,
}

impl Validator {
    fn new() -> Validator {
        Validator { games: 0 }
    }
}

impl Visitor for Validator {
    type Tags = Option<RawFen>;
    type Movetext = Game;
    type Output = Game;

    fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
        self.games += 1;
        ControlFlow::Continue(None)
    }

    fn tag(
        &mut self,
        tags: &mut Self::Tags,
        name: &[u8],
        value: RawTag<'_>,
    ) -> ControlFlow<Self::Output> {
        // Support games from a non-standard starting position.
        if name == b"FEN" {
            tags.replace(value.decode().into_owned());
        }
        ControlFlow::Continue(())
    }

    fn begin_movetext(&mut self, tags: Self::Tags) -> ControlFlow<Self::Output, Self::Movetext> {
        ControlFlow::Continue(Game {
            index: self.games,
            fen: tags,
            sans: Vec::with_capacity(80),
        })
    }

    fn begin_variation(
        &mut self,
        _movetext: &mut Self::Movetext,
    ) -> ControlFlow<Self::Output, Skip> {
        ControlFlow::Continue(Skip(true)) // stay in the mainline
    }

    fn san(
        &mut self,
        movetext: &mut Self::Movetext,
        san_plus: SanPlus,
    ) -> ControlFlow<Self::Output> {
        movetext.sans.push(san_plus.san);
        ControlFlow::Continue(())
    }

    fn end_game(&mut self, movetext: Self::Movetext) -> Self::Output {
        movetext
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
                        if let Err(err) = game.validate() {
                            eprintln!("error in game {index}: {err}");
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
