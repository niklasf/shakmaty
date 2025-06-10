// Validates moves in PGNs.
// Usage: cargo run --release --example validate -- [PGN]...

use std::{env, fs::File, io, process};

use pgn_reader::{BufferedReader, RawTag, SanPlus, Skip, Visitor};
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

    fn tag(&mut self, name: &[u8], value: RawTag<'_>) {
        // Support games from a non-standard starting position.
        if name == b"FEN" {
            let fen = match Fen::from_ascii(value.as_bytes()) {
                Ok(fen) => fen,
                Err(err) => {
                    eprintln!(
                        "invalid fen tag in game {}: {} ({:?})",
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
                        "illegal fen tag in game {}: {} ({:?})",
                        self.games, err, value
                    );
                    self.success = false;
                    return;
                }
            };
        }
    }

    fn end_tags(&mut self) -> Skip {
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
        while let Some(ok) = reader.read_game(&mut validator)? {
            file_ok &= ok;
        }

        println!("{}: {}", arg, if file_ok { "success" } else { "errors" });
        all_ok &= file_ok;
    }

    if !all_ok {
        process::exit(1);
    }

    Ok(())
}
