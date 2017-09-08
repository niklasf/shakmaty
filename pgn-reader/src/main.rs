#![feature(core_intrinsics)]

extern crate memmap;
extern crate memchr;
extern crate atoi;
extern crate madvise;
extern crate shakmaty;

use std::str;
use std::env;

use memmap::{Mmap, Protection};
use madvise::{AccessPattern, AdviseMemory};
use atoi::atoi;

use shakmaty::{Color, CastlingSide, Outcome};
use shakmaty::{Chess, Setup, Position};
use shakmaty::san::San;

#[derive(Debug)]
struct Skip(bool);

#[derive(Debug)]
struct Nag(u8);

impl Nag {
    fn from_bytes(s: &[u8]) -> Option<Nag> {
        if s == b"?!" {
            Some(Nag(6))
        } else if s == b"?" {
            Some(Nag(2))
        } else if s == b"??" {
            Some(Nag(4))
        } else if s == b"!" {
            Some(Nag(1))
        } else if s == b"!!" {
            Some(Nag(3))
        } else if s == b"!?" {
            Some(Nag(5))
        } else if s.len() > 1 && s[0] == b'$' {
            atoi(&s[1..]).map(Nag)
        } else {
            None
        }
    }
}

trait Consumer {
    type Item;

    fn begin_game(&mut self) { }
    fn end_headers(&mut self) -> Skip { Skip(false) }
    fn end_game(&mut self, game: &[u8]) -> Self::Item;

    fn header(&mut self, _key: &[u8], _value: &[u8]) { }
    fn comment(&mut self, _comment: &[u8]) { }
    fn begin_variation(&mut self) { }
    fn end_variation(&mut self) { }
    fn outcome(&mut self, _outcome: Outcome) { }
    fn san(&mut self, _san: San) { }
    fn nag(&mut self, _nag: Nag) { }
}

#[inline(always)]
fn unlikely(expr: bool) -> bool {
    unsafe { ::std::intrinsics::unlikely(expr) }
}

fn is_space(b: u8) -> bool {
    match b {
        b' ' | b'\t' | b'\n' | b'\r' => true,
        _ => false
    }
}

fn split_after_pgn_space(pgn: &[u8], mut pos: usize) -> (&[u8], &[u8]) {
    while pos < pgn.len() {
        match pgn[pos] {
            b' ' | b'\t' | b'\r' => pos += 1,
            b'\n' => {
                // Also skip % comments.
                pos += 1;
                if unlikely(pos < pgn.len() && pgn[pos] == b'%') {
                    pos += 1;
                    pos = memchr::memchr(b'\n', &pgn[pos..]).map_or_else(|| pgn.len(), |p| pos + p + 1);
                }
            },
            _ => break
        }
    }

    pgn.split_at(pos)
}

struct Scanner<'a, C: Consumer> where C: 'a {
    consumer: &'a mut C,
    pgn: &'a[u8],
}

impl<'a, C: Consumer> Scanner<'a, C> {
    fn new(consumer: &'a mut C, pgn: &'a[u8]) -> Scanner<'a, C> {
        // Skip BOM.
        let pos = if pgn.starts_with(b"\xef\xbb\xbf") { 3 } else { 0 };

        // Skip leading whitespace.
        let (_, pgn) = split_after_pgn_space(pgn, pos);
        Scanner { consumer, pgn }
    }

    fn scan_headers(&mut self) -> usize {
        let mut pos = 0;

        while pos < self.pgn.len() {
            match self.pgn[pos] {
                b'[' => {
                    pos += 1;
                    let key_pos = pos;
                    match memchr::memchr2(b'"', b'\n', &self.pgn[pos..]) {
                        Some(delta) if self.pgn[pos + delta] == b'"' => {
                            pos += delta;
                            let key_end_pos = if self.pgn[pos - 1] == b' ' {
                                pos - 1
                            } else {
                                pos
                            };
                            pos += 1;
                            let value_pos = pos;
                            pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p + 1);
                            if self.pgn[pos - 1] == b'\n' && self.pgn[pos - 2] == b']' && self.pgn[pos - 3] == b'"' {
                                let value_end_pos = pos - 3;
                                self.consumer.header(&self.pgn[key_pos..key_end_pos],
                                                     &self.pgn[value_pos..value_end_pos]);
                            }
                        },
                        Some(delta) => pos += delta + 1,
                        None => pos = self.pgn.len(),
                    }
                },
                b'%' => {
                    pos += 1;
                    pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p + 1);
                },
                _ => break
            }
        }

        pos
    }

    fn skip_movetext(&mut self, mut pos: usize) -> usize {
        while pos < self.pgn.len() {
            match self.pgn[pos] {
                b'{' => {
                    pos += 1;
                    pos = memchr::memchr(b'}', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p + 1);
                },
                b'\n' => {
                    pos += 1;
                    if pos >= self.pgn.len() {
                        break;
                    }
                    match self.pgn[pos] {
                        b'%' => {
                            pos += 1;
                            pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p);
                        },
                        b'[' => {
                            break
                        },
                        b'\n' => {
                            break
                        }
                        _ => continue,
                    }
                },
                _ => {
                    pos += 1;
                    pos = memchr::memchr2(b'\n', b'{', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p);
                },
            }
        }

        pos
    }

    fn skip_token(&mut self, mut pos: usize) -> usize {
        while pos < self.pgn.len() {
            match self.pgn[pos] {
                b' ' | b'\t' | b'\n' | b'{' | b'}' | b'(' | b')' | b'!' | b'?' | b'$' => break,
                _ => pos += 1,
            }
        }

        pos
    }

    fn scan_movetext(&mut self, mut pos: usize) -> usize {
        while pos < self.pgn.len() {
            match self.pgn[pos] {
                b'{' => {
                    pos += 1;
                    pos = if let Some(delta) = memchr::memchr(b'}', &self.pgn[pos..]) {
                        let end = pos + delta;
                        self.consumer.comment(&self.pgn[pos..end]);
                        end + 1
                    } else {
                        self.consumer.comment(&self.pgn[pos..]);
                        self.pgn.len()
                    };
                },
                b'\n' => {
                    pos += 1;
                    if pos >= self.pgn.len() {
                        break;
                    }
                    match self.pgn[pos] {
                        b'%' => {
                            pos += 1;
                            pos = memchr::memchr(b'\n', &self.pgn[pos..]).map_or_else(|| self.pgn.len(), |p| pos + p);
                        },
                        b'[' => {
                            break
                        },
                        b'\n' => {
                            break
                        }
                        _ => continue,
                    }
                },
                b'1' => {
                    pos += 1;
                    if self.pgn[pos..].starts_with(b"-0") {
                        pos += 2;
                        self.consumer.outcome(Outcome::Decisive { winner: Color::White });
                    } else if self.pgn[pos..].starts_with(b"/2-1/2") {
                        pos += 6;
                        self.consumer.outcome(Outcome::Draw);
                    } else {
                        pos = self.skip_token(pos);
                    }
                },
                b'0' => {
                    pos += 1;
                    if self.pgn[pos..].starts_with(b"-1") {
                        pos += 2;
                        self.consumer.outcome(Outcome::Decisive { winner: Color::Black });
                    } else if self.pgn[pos..].starts_with(b"-0-0") {
                        pos += 4;
                        self.consumer.san(San::Castle(CastlingSide::QueenSide));
                    } else if self.pgn[pos..].starts_with(b"-0") {
                        pos += 2;
                        self.consumer.san(San::Castle(CastlingSide::KingSide));
                    } else {
                        pos = self.skip_token(pos);
                    }
                },
                b'(' => {
                    pos += 1;
                    self.consumer.begin_variation();
                },
                b')' => {
                    pos += 1;
                    self.consumer.end_variation();
                },
                b'!' | b'?' | b'$' => {
                    let start = pos;
                    pos = self.skip_token(pos + 1);
                    if let Some(nag) = Nag::from_bytes(&self.pgn[start..pos]) {
                        self.consumer.nag(nag);
                    }
                },
                b' ' | b'\t' | b'P' => {
                    pos += 1;
                },
                _ => {
                    let end = self.skip_token(pos + 1);
                    if self.pgn[pos] > b'9' {
                        //self.consumer.san(San::Null);
                        if let Ok(san) = San::from_bytes(&self.pgn[pos..end]) {
                            self.consumer.san(san);
                        }
                    }
                    pos = end;
                },
            }
        }

        pos
    }
}

impl<'a, C: Consumer> Iterator for Scanner<'a, C> {
    type Item = C::Item;

    fn next(&mut self) -> Option<Self::Item> {
        // Scan game.
        self.consumer.begin_game();
        let pos = self.scan_headers();
        let pos = if let Skip(false) = self.consumer.end_headers() {
            self.scan_movetext(pos)
        } else {
            self.skip_movetext(pos)
        };

        // Skip trailing whitespace.
        let (head, tail) = split_after_pgn_space(self.pgn, pos);
        self.pgn = tail;

        // Check for any content.
        if head.iter().all(|c| is_space(*c)) {
            self.consumer.end_game(head);
            None
        } else {
            Some(self.consumer.end_game(head))
        }
    }
}

struct Parser {
    moves: usize,
    pos: Chess,
}

impl Parser {
    fn new() -> Parser {
        Parser {
            moves: 0,
            pos: Chess::default(),
        }
    }
}

impl Consumer for Parser {
    type Item = ();

    fn begin_game(&mut self) {
        self.pos = Chess::default();
    }

    fn san(&mut self, san: San) {
        self.moves += 1;
        let m = san.to_move(&self.pos).expect("legal");
        self.pos.play_unchecked(&m);
    }

    fn end_game(&mut self, _: &[u8]) { }

    fn header(&mut self, _key: &[u8], _value: &[u8]) { }
}

fn parse(pgn: &[u8]) {
    let mut consumer = Parser::new();
    {
        let scanner = Scanner::new(&mut consumer, pgn);
        eprintln!("% games: {}", scanner.count());
    }
    eprintln!("% moves: {}", consumer.moves);
}

fn main() {
    for arg in env::args().skip(1) {
        let mmap = Mmap::open_path(&arg, Protection::Read).expect("mmap");
        let bytes = unsafe { mmap.as_slice() };
        bytes.advise_memory_access(AccessPattern::Sequential).expect("madvise");
        parse(bytes);
        eprintln!("% file completed: {}", &arg);
    }
}
