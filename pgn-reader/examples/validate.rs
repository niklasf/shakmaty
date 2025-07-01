// Validates moves in PGNs.
// Usage: cargo run --release --example validate -- [PGN]...

use std::{
    env,
    fmt::{Display, Formatter},
    fs::File,
    io,
    ops::ControlFlow,
    process,
};

use pgn_reader::{BufferedReader, RawTag, SanPlus, Skip, Visitor};
use shakmaty::{
    CastlingMode, Chess, Position, PositionError,
    fen::{Fen, ParseFenError},
    san::SanError,
};

struct Validator {
    games: usize,
    pos: Chess,
}

impl Validator {
    fn new() -> Self {
        Self {
            games: 0,
            pos: Chess::default(),
        }
    }
}

#[derive(Debug)]
enum ValidatorError {
    InvalidFen {
        game: usize,
        fen: String,
        error: ParseFenError,
    },
    IllegalFen {
        game: usize,
        fen: Fen,
        error: Box<PositionError<Chess>>,
    },
    IllegalSan {
        game: usize,
        san: String,
        error: SanError,
    },
}

impl Display for ValidatorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidFen { game, fen, error } => {
                write!(f, "invalid fen tag in game {}: {} ({})", game, fen, error)
            }
            Self::IllegalFen { game, fen, error } => {
                write!(f, "illegal fen tag in game {}: {} ({})", game, fen, error)
            }
            Self::IllegalSan { game, san, error } => {
                write!(f, "illegal san in game {}: {} ({})", game, san, error)
            }
        }
    }
}

impl Visitor for Validator {
    type Output = ();
    type Break = ValidatorError;

    fn begin_tags(&mut self) -> ControlFlow<Self::Break> {
        self.games += 1;
        self.pos = Chess::default();

        ControlFlow::Continue(())
    }

    fn tag(&mut self, name: &[u8], value: RawTag<'_>) -> ControlFlow<Self::Break> {
        // Support games from a non-standard starting position.
        if name == b"FEN" {
            let fen = match Fen::from_ascii(value.as_bytes()).map_err(|error| {
                ValidatorError::InvalidFen {
                    game: self.games,
                    fen: String::from_utf8_lossy(value.as_bytes()).into_owned(),
                    error,
                }
            }) {
                Ok(fen) => fen,
                Err(error) => return ControlFlow::Break(error),
            };

            self.pos = match fen
                .clone()
                .into_position(CastlingMode::Chess960)
                .map_err(|error| ValidatorError::IllegalFen {
                    game: self.games,
                    fen,
                    error: Box::new(error),
                }) {
                Ok(pos) => pos,
                Err(error) => return ControlFlow::Break(error),
            };
        }

        ControlFlow::Continue(())
    }

    fn san(&mut self, san_plus: SanPlus) -> ControlFlow<Self::Break> {
        let m = match san_plus
            .san
            .to_move(&self.pos)
            .map_err(|error| ValidatorError::IllegalSan {
                game: self.games,
                san: san_plus.san.to_string(),
                error,
            }) {
            Ok(m) => m,
            Err(error) => return ControlFlow::Break(error),
        };

        self.pos.play_unchecked(m);

        ControlFlow::Continue(())
    }

    fn begin_variation(&mut self) -> ControlFlow<Self::Break, Skip> {
        ControlFlow::Continue(Skip(true)) // stay in the mainline
    }

    fn end_game(&mut self) -> Self::Output {}
}

fn main() -> io::Result<()> {
    let mut all_ok = true;

    for arg in env::args().skip(1) {
        let mut file_ok = true;

        let file = File::open(&arg)?;

        let uncompressed: Box<dyn io::Read> = if arg.ends_with(".zst") {
            Box::new(zstd::Decoder::new(file)?)
        } else if arg.ends_with(".bz2") {
            Box::new(bzip2::read::MultiBzDecoder::new(file))
        } else if arg.ends_with(".xz") {
            Box::new(xz2::read::XzDecoder::new(file))
        } else if arg.ends_with(".gz") {
            Box::new(flate2::read::GzDecoder::new(file))
        } else if arg.ends_with(".lz4") {
            Box::new(lz4::Decoder::new(file)?)
        } else {
            Box::new(file)
        };

        let mut reader = BufferedReader::new(uncompressed);

        let mut validator = Validator::new();

        loop {
            match reader.read_game(&mut validator)? {
                None => break,
                Some(ControlFlow::Continue(())) => (),
                Some(ControlFlow::Break(e)) => {
                    eprintln!("{e}");
                    file_ok = false;
                    all_ok = false;
                }
            }
        }

        println!("{arg}: {}", if file_ok { "success" } else { "errors" });
    }

    if !all_ok {
        process::exit(1);
    }

    Ok(())
}
