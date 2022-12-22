// Validates moves in PGNs.
// Usage: cargo run --release --example validate -- [PGN]...

use std::{env, fs::File, io};

use pgn_reader::{BufferedReader, RawHeader, SanPlus, Skip, Visitor};
use shakmaty::{fen::Fen, CastlingMode, Chess, Position};

struct Validator {
    games: usize,
    pos: Chess,
    success: bool,
}

impl Validator {
    fn new() -> Validator {
        Validator {
            games: 0,
            pos: Chess::default(),
            success: true,
        }
    }
}

impl Visitor for Validator {
    type Result = bool;

    fn begin_game(&mut self) {
        self.games += 1;
        self.pos = Chess::default();
        self.success = true;
    }

    fn header(&mut self, key: &[u8], value: RawHeader<'_>) {
        // Support games from a non-standard starting position.
        if key == b"FEN" {
            let fen = match Fen::from_ascii(value.as_bytes()) {
                Ok(fen) => fen,
                Err(err) => {
                    eprintln!(
                        "invalid fen header in game {}: {} ({:?})",
                        self.games, err, value
                    );
                    self.success = false;
                    return;
                }
            };

            self.pos = match fen.into_position(CastlingMode::Chess960) {
                Ok(pos) => pos,
                Err(err) => {
                    eprintln!(
                        "illegal fen header in game {}: {} ({:?})",
                        self.games, err, value
                    );
                    self.success = false;
                    return;
                }
            };
        }
    }

    fn end_headers(&mut self) -> Skip {
        Skip(!self.success)
    }

    fn begin_variation(&mut self) -> Skip {
        Skip(true) // stay in the mainline
    }

    fn san(&mut self, san_plus: SanPlus) {
        if self.success {
            match san_plus.san.to_move(&self.pos) {
                Ok(m) => self.pos.play_unchecked(&m),
                Err(err) => {
                    eprintln!("error in game {}: {} {}", self.games, err, san_plus);
                    self.success = false;
                }
            }
        }
    }

    fn end_game(&mut self) -> Self::Result {
        self.success
    }
}

fn main() -> io::Result<()> {
    let mut success = true;

    for arg in env::args().skip(1) {
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
        while let Some(ok) = reader.read_game(&mut validator)? {
            success &= ok;
        }

        println!("{}: {}", arg, if success { "success" } else { "errors" });
    }

    if !success {
        ::std::process::exit(1);
    }

    Ok(())
}
