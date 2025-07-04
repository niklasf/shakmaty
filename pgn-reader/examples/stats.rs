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
    type Tags = ();
    type Output = ();
    type Movetext = ();

    fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
        ControlFlow::Continue(())
    }

    fn tag(
        &mut self,
        _tags: &mut Self::Tags,
        _name: &[u8],
        _value: RawTag<'_>,
    ) -> ControlFlow<Self::Output> {
        self.tags += 1;
        ControlFlow::Continue(())
    }

    fn san(&mut self, _movetext: &mut Self::Movetext, _san: SanPlus) -> ControlFlow<Self::Output> {
        self.sans += 1;
        ControlFlow::Continue(())
    }

    fn nag(&mut self, _movetext: &mut Self::Movetext, _nag: Nag) -> ControlFlow<Self::Output> {
        self.nags += 1;
        ControlFlow::Continue(())
    }

    fn begin_movetext(&mut self, _tags: Self::Tags) -> ControlFlow<Self::Output, Self::Movetext> {
        ControlFlow::Continue(())
    }

    fn comment(
        &mut self,
        _movetext: &mut Self::Movetext,
        _comment: RawComment<'_>,
    ) -> ControlFlow<Self::Output> {
        self.comments += 1;
        ControlFlow::Continue(())
    }

    fn end_variation(&mut self, _movetext: &mut Self::Movetext) -> ControlFlow<Self::Output> {
        self.variations += 1;
        ControlFlow::Continue(())
    }

    fn outcome(
        &mut self,
        _movetext: &mut Self::Movetext,
        _outcome: Outcome,
    ) -> ControlFlow<Self::Output> {
        self.outcomes += 1;
        ControlFlow::Continue(())
    }

    fn end_game(&mut self, _movetext: Self::Movetext) -> Self::Output {
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
        reader.visit_all_games(&mut stats)?;
        println!("{arg}: {stats:?}");
    }

    Ok(())
}
