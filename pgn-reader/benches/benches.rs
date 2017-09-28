#[macro_use]
extern crate bencher;
extern crate pgn_reader;

use bencher::{Bencher, black_box};
use pgn_reader::{Reader, Visitor, Nag};

#[derive(Default)]
struct NagVisitor {
    nag: Option<Nag>,
}

impl Visitor for NagVisitor {
    type Result = Option<Nag>;

    fn nag(&mut self, nag: Nag) {
        self.nag = Some(nag);
    }

    fn end_game(&mut self, _game: &[u8]) -> Self::Result {
        self.nag.take()
    }
}

fn parse_nag(b: &mut Bencher) {
    b.iter(|| {
        let mut visitor = NagVisitor::default();
        let nag = Reader::new(&mut visitor, black_box(b"$42")).read_game().unwrap();
        assert_eq!(nag, Some(Nag(42)));
    });
}

benchmark_group!(benches, parse_nag);
benchmark_main!(benches);
