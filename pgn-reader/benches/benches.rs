#[macro_use]
extern crate bencher;
extern crate pgn_reader;

use std::collections::HashMap;

use bencher::{Bencher, black_box};
use pgn_reader::{Reader, Visitor, Skip, Nag};

const FIXTURE: &[u8] = include_bytes!("fixture.pgn");

#[derive(Default)]
struct NagVisitor {
    nag: Option<Nag>,
}

impl<'pgn> Visitor<'pgn> for NagVisitor {
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
        let nag = Reader::new(&mut visitor, black_box(b"$42"))
            .read_game()
            .unwrap();

        assert_eq!(nag, Some(Nag(42)));
    });
}

struct HeaderVisitor<'a> {
    headers: HashMap<&'a [u8], &'a [u8]>,
}

impl<'a> HeaderVisitor<'a> {
    fn new() -> HeaderVisitor<'a> {
        HeaderVisitor { headers: HashMap::with_capacity(7) }
    }
}

impl<'pgn> Visitor<'pgn> for HeaderVisitor<'pgn> {
    type Result = HashMap<&'pgn [u8], &'pgn [u8]>;

    fn header(&mut self, key: &'pgn [u8], value: &'pgn [u8]) {
        self.headers.insert(key, value);
    }

    fn end_headers(&mut self) -> Skip {
        Skip(true)
    }

    fn end_game(&mut self, _game: &[u8]) -> Self::Result {
        ::std::mem::replace(&mut self.headers, HashMap::with_capacity(7))
    }
}

fn parse_headers(b: &mut Bencher) {
    b.iter(|| {
        let mut visitor = HeaderVisitor::new();
        let headers = Reader::new(&mut visitor, black_box(FIXTURE))
            .read_game()
            .unwrap();

        assert_eq!(headers[&b"White"[..]], b"revoof");
        assert_eq!(headers[&b"Black"[..]], b"Servasky");
    });
}

benchmark_group!(benches, parse_nag, parse_headers);
benchmark_main!(benches);
