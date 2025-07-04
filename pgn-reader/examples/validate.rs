// Validates moves in PGNs.
// Usage: cargo run --release --example validate -- [PGN]...

use std::{env, fs::File, io, ops::ControlFlow, process};

use pgn_reader::{RawTag, Reader, SanPlus, Skip, Visitor};
use shakmaty::{CastlingMode, Chess, Position, fen::Fen};

struct Validator {
    games: usize,
    pos: Chess,
}

impl Validator {
    fn new() -> Validator {
        Validator {
            games: 0,
            pos: Chess::default(),
        }
    }
}

impl Visitor for Validator {
    type Output = bool;

    fn begin_tags(&mut self) -> ControlFlow<Self::Output> {
        self.games += 1;
        self.pos = Chess::default();
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
                    return ControlFlow::Break(false);
                }
            };

            self.pos = match fen.into_position(CastlingMode::Chess960) {
                Ok(pos) => pos,
                Err(err) => {
                    eprintln!(
                        "illegal fen tag in game {}: {} ({:?})",
                        self.games, err, value
                    );
                    return ControlFlow::Break(false);
                }
            };
        }
        ControlFlow::Continue(())
    }

    fn begin_variation(&mut self) -> ControlFlow<Self::Output, Skip> {
        ControlFlow::Continue(Skip(true))
    }

    fn san(&mut self, san_plus: SanPlus) -> ControlFlow<Self::Output> {
        match san_plus.san.to_move(&self.pos) {
            Ok(m) => self.pos.play_unchecked(m),
            Err(err) => {
                eprintln!("error in game {}: {} {}", self.games, err, san_plus);
                return ControlFlow::Break(false);
            }
        }
        ControlFlow::Continue(())
    }

    fn end_game(&mut self) -> Self::Output {
        true
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

        let mut reader = Reader::new(uncompressed);

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
