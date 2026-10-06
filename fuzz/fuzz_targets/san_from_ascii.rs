#![no_main]

use libfuzzer_sys::fuzz_target;
use shakmaty::san::{San, SanPlus};

fuzz_target!(|data: &[u8]| {
    // Differential: the optimized parser must agree with the straightforward
    // reference implementation on every input, valid or not.
    assert_eq!(
        SanPlus::from_ascii_prefix(data).ok(),
        reference::san_plus_from_ascii_prefix(data)
    );
    assert_eq!(
        San::from_ascii_prefix(data).ok(),
        reference::san_from_ascii_prefix(data)
    );
    assert_eq!(
        SanPlus::from_ascii(data).ok(),
        reference::san_plus_from_ascii(data)
    );
    assert_eq!(San::from_ascii(data).ok(), reference::san_from_ascii(data));

    // Roundtrip.
    let Ok(san) = SanPlus::from_ascii(data) else {
        return;
    };
    let roundtripped = SanPlus::from_ascii(san.to_string().as_bytes()).expect("roundtrip");
    assert_eq!(san, roundtripped);
});

/// Straightforward SAN parser, reading one byte at a time. Every byte that
/// might continue a SAN is eagerly consumed without backtracking.
mod reference {
    use shakmaty::{
        CastlingSide, File, Rank, Role, Square,
        san::{San, SanPlus, Suffix},
    };

    pub fn san_from_ascii(ascii: &[u8]) -> Option<San> {
        let mut reader = Reader { bytes: ascii };
        let san = reader.read_san()?;
        let _ = reader.eat(b'+') || reader.eat(b'#');
        reader.bytes.is_empty().then_some(san)
    }

    pub fn san_from_ascii_prefix(ascii: &[u8]) -> Option<(San, usize)> {
        let mut reader = Reader { bytes: ascii };
        let san = reader.read_san()?;
        Some((san, ascii.len() - reader.bytes.len()))
    }

    pub fn san_plus_from_ascii(ascii: &[u8]) -> Option<SanPlus> {
        let mut reader = Reader { bytes: ascii };
        let san_plus = reader.read_san_plus()?;
        reader.bytes.is_empty().then_some(san_plus)
    }

    pub fn san_plus_from_ascii_prefix(ascii: &[u8]) -> Option<(SanPlus, usize)> {
        let mut reader = Reader { bytes: ascii };
        let san_plus = reader.read_san_plus()?;
        Some((san_plus, ascii.len() - reader.bytes.len()))
    }

    struct Reader<'a> {
        bytes: &'a [u8],
    }

    impl Reader<'_> {
        fn peek(&self) -> Option<u8> {
            self.bytes.first().copied()
        }

        fn bump(&mut self) {
            self.bytes = &self.bytes[1..];
        }

        fn eat(&mut self, byte: u8) -> bool {
            if self.peek() == Some(byte) {
                self.bump();
                true
            } else {
                false
            }
        }

        fn next(&mut self) -> Option<u8> {
            let byte = self.peek()?;
            self.bump();
            Some(byte)
        }

        fn read_square(&mut self) -> Option<Square> {
            let (head, tail) = self.bytes.split_at_checked(2)?;
            self.bytes = tail;
            Square::from_ascii(head).ok()
        }

        fn read_san(&mut self) -> Option<San> {
            let role = match self.peek()? {
                b'N' => Role::Knight,
                b'B' => Role::Bishop,
                b'R' => Role::Rook,
                b'Q' => Role::Queen,
                b'K' => Role::King,
                b'P' => Role::Pawn,
                b'O' => {
                    self.bump();
                    if !self.eat(b'-') || !self.eat(b'O') {
                        return None;
                    }
                    if !self.eat(b'-') {
                        return Some(San::Castle(CastlingSide::KingSide));
                    }
                    if !self.eat(b'O') {
                        return None;
                    }
                    return Some(San::Castle(CastlingSide::QueenSide));
                }
                b'-' => {
                    self.bump();
                    return self.eat(b'-').then_some(San::Null);
                }
                b'Z' => {
                    self.bump();
                    return self.eat(b'0').then_some(San::Null);
                }
                _ => {
                    return self.read_normal(Role::Pawn);
                }
            };
            self.bump();
            self.read_normal(role)
        }

        fn read_normal(&mut self, role: Role) -> Option<San> {
            if self.eat(b'@') {
                return Some(San::Put {
                    role,
                    to: self.read_square()?,
                });
            }

            let file = File::from_char(char::from(self.peek()?));
            if file.is_some() {
                self.bump();
            }

            let rank = Rank::from_char(char::from(self.peek()?));
            if rank.is_some() {
                self.bump();
            }

            let (file, rank, capture, to) = if self.eat(b'x') {
                (file, rank, true, self.read_square()?)
            } else if let Some(to_file) = self.peek().and_then(|ch| File::from_char(char::from(ch)))
            {
                self.bump();
                let to_rank = Rank::from_char(char::from(self.next()?))?;
                (file, rank, false, Square::from_coords(to_file, to_rank))
            } else {
                // What looked like disambiguation is the destination.
                (None, None, false, Square::from_coords(file?, rank?))
            };

            let promotion = if self.eat(b'=') {
                Some(Role::from_char(char::from(self.next()?))?)
            } else {
                None
            };

            Some(San::Normal {
                role,
                file,
                rank,
                capture,
                to,
                promotion,
            })
        }

        fn read_san_plus(&mut self) -> Option<SanPlus> {
            let san = self.read_san()?;
            let suffix = if self.eat(b'+') {
                Some(Suffix::Check)
            } else if self.eat(b'#') {
                Some(Suffix::Checkmate)
            } else {
                None
            };
            Some(SanPlus { san, suffix })
        }
    }
}
