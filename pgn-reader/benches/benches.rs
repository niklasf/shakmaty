use std::{fs::File, ops::ControlFlow};

use criterion::{Criterion, criterion_group, criterion_main};
use pgn_reader::{Nag, Outcome, RawComment, RawTag, Reader, SanPlus, Visitor};

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
    for fixture in FIXTURES {
        c.bench_function(&format!("skip all {fixture}"), |b| {
            b.iter(|| {
                let mut reader =
                    Reader::new(File::open(format!("benches/{fixture}")).expect("open"));
                while reader.skip_game().expect("skip game") {}
            })
        });
    }
}

criterion_group!(benches, bench_stats, bench_skip_all);
criterion_main!(benches);
