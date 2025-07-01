use std::fs::File;

use criterion::{Criterion, criterion_group, criterion_main};
use pgn_reader::{Nag, Outcome, RawComment, RawTag, Reader, SanPlus, Skip, Visitor};

const FIXTURES: [&str; 3] = [
    "lichess_db_10k.pgn",
    "lichess_db_100k.pgn",
    "kasparov_deep_blue_1997.pgn",
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
        type Result = ();

        fn end_game(&mut self) {}

        fn begin_movetext(&mut self) -> Skip {
            Skip(true)
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
