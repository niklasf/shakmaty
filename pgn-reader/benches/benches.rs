use std::{convert::Infallible, fs::File};

use criterion::{Criterion, criterion_group, criterion_main};
use pgn_reader::{Nag, Outcome, RawComment, RawTag, Reader, SanPlus, Skip, Visitor};

const FIXTURES: [&str; 6] = [
    "lichess_db_10k.pgn",
    "lichess_db_100k.pgn",
    "lichess_db_1000k.pgn",
    "twic1599_10k.pgn",
    "twic1599_100k.pgn",
    "twic1599_1000k.pgn",
];

fn bench_stats(c: &mut Criterion) {
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

    impl Visitor for Stats {
        type Output = ();
        type Error = Infallible;

        fn tag(&mut self, _name: &[u8], _value: RawTag<'_>) -> Result<(), Self::Error> {
            self.tags += 1;

            Ok(())
        }

        fn san(&mut self, _san: SanPlus) -> Result<(), Self::Error> {
            self.sans += 1;

            Ok(())
        }

        fn nag(&mut self, _nag: Nag) -> Result<(), Self::Error> {
            self.nags += 1;

            Ok(())
        }

        fn comment(&mut self, _comment: RawComment<'_>) -> Result<(), Self::Error> {
            self.comments += 1;

            Ok(())
        }

        fn end_variation(&mut self) -> Result<(), Self::Error> {
            self.variations += 1;

            Ok(())
        }

        fn outcome(&mut self, _outcome: Outcome) -> Result<(), Self::Error> {
            self.outcomes += 1;

            Ok(())
        }

        fn end_game(&mut self) {
            self.games += 1;
        }
    }

    for fixture in FIXTURES {
        c.bench_function(&format!("stats {fixture}"), |b| {
            b.iter(|| {
                let mut stats = Stats::default();
                Reader::new(File::open(format!("benches/{fixture}")).expect("open"))
                    .read_all(&mut stats)
                    .expect("read all");
                stats
            })
        });
    }
}

fn bench_skip_all(c: &mut Criterion) {
    struct SkipAll;

    impl Visitor for SkipAll {
        type Output = ();
        type Error = Infallible;

        fn end_game(&mut self) {}

        fn begin_movetext(&mut self) -> Result<Skip, Self::Error> {
            Ok(Skip(true))
        }
    }

    for fixture in FIXTURES {
        c.bench_function(&format!("skip all {fixture}"), |b| {
            b.iter(|| {
                let mut skip_all = SkipAll;
                Reader::new(File::open(format!("benches/{fixture}")).expect("open"))
                    .read_all(&mut skip_all)
                    .expect("read all");
            })
        });
    }
}

criterion_group!(benches, bench_stats, bench_skip_all);
criterion_main!(benches);
