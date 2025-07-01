// Prints the FEN of the final position of each PGN.
// Usage: cargo run --release --example extract_fen -- [PGN]...

use std::{env, fs::File, io, ops::ControlFlow};

use pgn_reader::{BufferedReader, RawTag, SanPlus, Skip, Visitor};
use shakmaty::{CastlingMode, Chess, EnPassantMode, Position, fen::Fen};

struct FenExtractor {
    pos: Chess,
}

impl FenExtractor {
    fn new() -> Self {
        Self {
            pos: Chess::default(),
        }
    }
}

impl Visitor for FenExtractor {
    type Output = Fen;
    type Break = anyhow::Error;

    fn begin_tags(&mut self) -> ControlFlow<Self::Break> {
        self.pos = Chess::default();

        ControlFlow::Continue(())
    }

    fn tag(&mut self, name: &[u8], value: RawTag<'_>) -> ControlFlow<Self::Break> {
        // Support games from a non-standard starting position.
        if name == b"FEN" {
            let fen = match Fen::from_ascii(value.as_bytes()) {
                Ok(fen) => fen,
                Err(e) => return ControlFlow::Break(e.into()),
            };

            self.pos = match fen.into_position(CastlingMode::Chess960) {
                Ok(pos) => pos,
                Err(e) => return ControlFlow::Break(e.into()),
            };
        }

        ControlFlow::Continue(())
    }

    fn san(&mut self, san_plus: SanPlus) -> ControlFlow<Self::Break> {
        let m = match san_plus.san.to_move(&self.pos) {
            Ok(m) => m,
            Err(e) => return ControlFlow::Break(e.into()),
        };

        self.pos.play_unchecked(m);

        ControlFlow::Continue(())
    }

    fn begin_variation(&mut self) -> ControlFlow<Self::Break, Skip> {
        ControlFlow::Continue(Skip(true)) // stay in the mainline
    }

    fn end_game(&mut self) -> Self::Output {
        Fen::from_position(&self.pos, EnPassantMode::Always)
    }
}

fn main() -> anyhow::Result<()> {
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

        let mut validator = FenExtractor::new();
        let mut i = 0;

        loop {
            match reader.read_game(&mut validator)? {
                None => break,
                Some(ControlFlow::Continue(fen)) => println!("{arg} #{i}: {fen}"),
                Some(ControlFlow::Break(e)) => eprintln!("{e}"),
            }

            i += 1;
        }
    }

    Ok(())
}
