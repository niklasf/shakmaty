extern crate memchr;
extern crate atoi;
extern crate shakmaty;

pub use shakmaty::san::San;
pub use shakmaty::{Color, CastlingSide, Outcome};

use atoi::atoi;

/// Tell the reader to skip over a game over variation.
#[derive(Debug)]
pub struct Skip(pub bool);

/// A numeric annotation glyph like `?`, `!!` or `$42`.
#[derive(Debug)]
pub struct Nag(pub u8);

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

/// Consumes games from a reader.
///
/// ![Flow](https://github.com/niklasf/rust-pgn-reader/blob/master/docs/visitor.png?raw=true)
pub trait Visitor {
    type Result;

    fn begin_game(&mut self) { }

    fn begin_headers(&mut self) { }
    fn header(&mut self, _key: &[u8], _value: &[u8]) { }
    fn end_headers(&mut self) -> Skip { Skip(false) }

    fn san(&mut self, _san: San) { }
    fn nag(&mut self, _nag: Nag) { }
    fn comment(&mut self, _comment: &[u8]) { }
    fn begin_variation(&mut self) { }
    fn end_variation(&mut self) { }
    fn outcome(&mut self, _outcome: Outcome) { }

    fn end_game(&mut self, game: &[u8]) -> Self::Result;
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
                if pos < pgn.len() && pgn[pos] == b'%' {
                    pos += 1;
                    pos = memchr::memchr(b'\n', &pgn[pos..]).map_or_else(|| pgn.len(), |p| pos + p + 1);
                }
            },
            _ => break
        }
    }

    pgn.split_at(pos)
}

pub struct Reader<'a, V: Visitor> where V: 'a {
    visitor: &'a mut V,
    pgn: &'a[u8],
}

impl<'a, V: Visitor> Reader<'a, V> {
    pub fn new(visitor: &'a mut V, pgn: &'a[u8]) -> Reader<'a, V> {
        // Skip BOM.
        let pos = if pgn.starts_with(b"\xef\xbb\xbf") { 3 } else { 0 };

        // Skip leading whitespace.
        let (_, pgn) = split_after_pgn_space(pgn, pos);
        Reader { visitor, pgn }
    }

    pub fn read_game(&mut self) -> Option<V::Result> {
        // Scan game.
        self.visitor.begin_game();
        let pos = self.scan_headers();
        let pos = if let Skip(false) = self.visitor.end_headers() {
            self.scan_movetext(pos)
        } else {
            self.skip_movetext(pos)
        };

        // Skip trailing whitespace.
        let (head, tail) = split_after_pgn_space(self.pgn, pos);
        self.pgn = tail;

        // Check for any content.
        if head.iter().all(|c| is_space(*c)) {
            self.visitor.end_game(head);
            None
        } else {
            Some(self.visitor.end_game(head))
        }
    }

    pub fn read_all(&mut self) {
        while let Some(_) = self.read_game() { }
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
                                self.visitor.header(&self.pgn[key_pos..key_end_pos],
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
                        self.visitor.comment(&self.pgn[pos..end]);
                        end + 1
                    } else {
                        self.visitor.comment(&self.pgn[pos..]);
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
                        self.visitor.outcome(Outcome::Decisive { winner: Color::White });
                    } else if self.pgn[pos..].starts_with(b"/2-1/2") {
                        pos += 6;
                        self.visitor.outcome(Outcome::Draw);
                    } else {
                        pos = self.skip_token(pos);
                    }
                },
                b'0' => {
                    pos += 1;
                    if self.pgn[pos..].starts_with(b"-1") {
                        pos += 2;
                        self.visitor.outcome(Outcome::Decisive { winner: Color::Black });
                    } else if self.pgn[pos..].starts_with(b"-0-0") {
                        pos += 4;
                        self.visitor.san(San::Castle(CastlingSide::QueenSide));
                    } else if self.pgn[pos..].starts_with(b"-0") {
                        pos += 2;
                        self.visitor.san(San::Castle(CastlingSide::KingSide));
                    } else {
                        pos = self.skip_token(pos);
                    }
                },
                b'(' => {
                    pos += 1;
                    self.visitor.begin_variation();
                },
                b')' => {
                    pos += 1;
                    self.visitor.end_variation();
                },
                b'!' | b'?' | b'$' => {
                    let start = pos;
                    pos = self.skip_token(pos + 1);
                    if let Some(nag) = Nag::from_bytes(&self.pgn[start..pos]) {
                        self.visitor.nag(nag);
                    }
                },
                b' ' | b'\t' | b'P' => {
                    pos += 1;
                },
                _ => {
                    let end = self.skip_token(pos + 1);
                    if self.pgn[pos] > b'9' {
                        if let Ok(san) = San::from_bytes(&self.pgn[pos..end]) {
                            self.visitor.san(san);
                        }
                    }
                    pos = end;
                },
            }
        }

        pos
    }
}

impl<'a, V: Visitor> Iterator for Reader<'a, V> {
    type Item = V::Result;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_game()
    }
}
