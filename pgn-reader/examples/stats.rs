// Counts games, moves and other tokens in PGNs.
// Usage: cargo run --release --example stats -- [PGN]...

use std::ops::ControlFlow;
use std::{env, fs::File, io};

use pgn_reader::{Nag, Outcome, RawComment, RawTag, Reader, SanPlus, Visitor};

#[derive(Debug, Default)]
struct Stats {
    games: usize,
    tags: usize,
    sans: usize,
    nags: usize,
    comments: usize,
    variations: usize,
    outcomes: usize,
}

impl Stats {
    fn new() -> Stats {
        Stats::default()
    }
}

impl Visitor for Stats {
    type Output = ();

    fn tag(&mut self, _name: &[u8], _value: RawTag<'_>) -> ControlFlow<()> {
        self.tags += 1;
        ControlFlow::Continue(())
    }

    fn san(&mut self, _san: SanPlus) -> ControlFlow<()> {
        self.sans += 1;
        ControlFlow::Continue(())
    }

    fn nag(&mut self, _nag: Nag) -> ControlFlow<()> {
        self.nags += 1;
        ControlFlow::Continue(())
    }

    fn comment(&mut self, _comment: RawComment<'_>) -> ControlFlow<()> {
        self.comments += 1;
        ControlFlow::Continue(())
    }

    fn end_variation(&mut self) -> ControlFlow<()> {
        self.variations += 1;
        ControlFlow::Continue(())
    }

    fn outcome(&mut self, _outcome: Outcome) -> ControlFlow<()> {
        self.outcomes += 1;
        ControlFlow::Continue(())
    }

    fn end_game(&mut self) {
        self.games += 1;
    }
}

fn main() -> Result<(), io::Error> {
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

        let mut reader = Reader::new(uncompressed);

        let mut stats = Stats::new();
        reader.read_all(&mut stats)?;
        println!("{arg}: {stats:?}");
    }

    Ok(())
}
