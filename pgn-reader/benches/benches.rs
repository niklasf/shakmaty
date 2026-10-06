use std::{fs::File, hint::black_box, ops::ControlFlow};

use criterion::{Criterion, criterion_group, criterion_main};
use pgn_reader::{Nag, Outcome, RawComment, RawTag, Reader, SanPlus, Visitor};
use shakmaty::{CastlingMode, Chess, Position, fen::Fen, san::San};

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
        type Tags = ();
        type Movetext = ();
        type Output = ();

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

        fn begin_movetext(
            &mut self,
            _tags: Self::Tags,
        ) -> ControlFlow<Self::Output, Self::Movetext> {
            ControlFlow::Continue(())
        }

        fn san(
            &mut self,
            _movetext: &mut Self::Movetext,
            _san: SanPlus,
        ) -> ControlFlow<Self::Output> {
            self.sans += 1;
            ControlFlow::Continue(())
        }

        fn nag(&mut self, _movetext: &mut Self::Movetext, _nag: Nag) -> ControlFlow<Self::Output> {
            self.nags += 1;
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

    for fixture in FIXTURES {
        c.bench_function(&format!("stats {fixture}"), |b| {
            b.iter(|| {
                let mut stats = Stats::default();
                Reader::new(File::open(format!("benches/{fixture}")).expect("open"))
                    .visit_all_games(&mut stats)
                    .expect("visit all");
                stats
            })
        });
    }
}

fn bench_collect(c: &mut Criterion) {
    #[derive(Default)]
    struct Collector {
        sans: Vec<San>,
        total: usize,
    }

    impl Visitor for Collector {
        type Tags = ();
        type Movetext = ();
        type Output = ();

        fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
            ControlFlow::Continue(())
        }

        fn begin_movetext(
            &mut self,
            _tags: Self::Tags,
        ) -> ControlFlow<Self::Output, Self::Movetext> {
            self.sans.clear();
            ControlFlow::Continue(())
        }

        fn san(
            &mut self,
            _movetext: &mut Self::Movetext,
            san_plus: SanPlus,
        ) -> ControlFlow<Self::Output> {
            self.sans.push(san_plus.san);
            ControlFlow::Continue(())
        }

        fn end_game(&mut self, _movetext: Self::Movetext) -> Self::Output {
            self.total += black_box(&self.sans).len();
        }
    }

    for fixture in FIXTURES {
        c.bench_function(&format!("collect {fixture}"), |b| {
            b.iter(|| {
                let mut collector = Collector::default();
                Reader::new(File::open(format!("benches/{fixture}")).expect("open"))
                    .visit_all_games(&mut collector)
                    .expect("visit all");
                collector.total
            })
        });
    }
}

fn bench_validate(c: &mut Criterion) {
    struct Validator;

    impl Visitor for Validator {
        type Tags = Option<Chess>;
        type Movetext = Chess;
        type Output = bool;

        fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
            ControlFlow::Continue(None)
        }

        fn tag(
            &mut self,
            tags: &mut Self::Tags,
            name: &[u8],
            value: RawTag<'_>,
        ) -> ControlFlow<Self::Output> {
            if name == b"FEN" {
                let Ok(fen) = Fen::from_ascii(value.as_bytes()) else {
                    return ControlFlow::Break(false);
                };
                let Ok(pos) = fen.into_position(CastlingMode::Chess960) else {
                    return ControlFlow::Break(false);
                };
                tags.replace(pos);
            }
            ControlFlow::Continue(())
        }

        fn begin_movetext(
            &mut self,
            tags: Self::Tags,
        ) -> ControlFlow<Self::Output, Self::Movetext> {
            ControlFlow::Continue(tags.unwrap_or_default())
        }

        fn san(
            &mut self,
            movetext: &mut Self::Movetext,
            san_plus: SanPlus,
        ) -> ControlFlow<Self::Output> {
            match san_plus.san.to_move(movetext) {
                Ok(m) => {
                    movetext.play_unchecked(m);
                    ControlFlow::Continue(())
                }
                Err(_) => ControlFlow::Break(false),
            }
        }

        fn end_game(&mut self, _movetext: Self::Movetext) -> Self::Output {
            true
        }
    }

    for fixture in FIXTURES {
        c.bench_function(&format!("validate {fixture}"), |b| {
            b.iter(|| {
                let mut reader =
                    Reader::new(File::open(format!("benches/{fixture}")).expect("open"));
                for game in reader.read_games(&mut Validator) {
                    assert!(game.expect("read game"), "invalid game in {fixture}");
                }
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

criterion_group!(
    benches,
    bench_stats,    // count but discard moves
    bench_collect,  // keep moves
    bench_validate, // process moves
    bench_skip_all,
);
criterion_main!(benches);
