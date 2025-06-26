// Counts games, moves and other tokens in PGNs.
// Usage: cargo run --release --example stats -- [PGN]...

use std::{env, fs::File, io};

use pgn_reader::{BufferedReader, Nag, Outcome, RawComment, RawTag, SanPlus, Visitor};

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
    type Result = ();

    fn tag(&mut self, _name: &[u8], _value: RawTag<'_>) {
        self.tags += 1;
    }

    fn san(&mut self, _san: SanPlus) {
        self.sans += 1;
    }

    fn nag(&mut self, _nag: Nag) {
        self.nags += 1;
    }

    fn comment(&mut self, _comment: RawComment<'_>) {
        self.comments += 1;
    }

    fn end_variation(&mut self) {
        self.variations += 1;
    }

    fn outcome(&mut self, _outcome: Outcome) {
        self.outcomes += 1;
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

        let mut reader = BufferedReader::new(uncompressed);

        let mut stats = Stats::new();
        reader.read_all(&mut stats)?;
        println!("{arg}: {stats:?}");
    }

    Ok(())
}
