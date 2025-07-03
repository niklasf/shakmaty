use std::{
    cmp::min,
    io::{self, Chain, Cursor, Read, Seek, SeekFrom},
};

use shakmaty::{
    CastlingSide, Color, KnownOutcome, Outcome,
    san::{San, SanPlus, Suffix},
};

use crate::{
    buffer,
    buffer::Buffer,
    types::{Nag, RawComment, RawTag, Skip},
    visitor::{SkipVisitor, Visitor},
};

const MAX_TAG_LINE_LENGTH: usize = 1024;
const MAX_COMMENT_LENGTH: usize = 4096;
const _: () = {
    assert!(MAX_TAG_LINE_LENGTH <= buffer::CAPACITY);
    assert!(MAX_COMMENT_LENGTH <= buffer::CAPACITY);
};

/// A buffered PGN reader.
///
/// It's redundant and discouraged to wrap this in a [`BufReader`](std::io::BufReader).
#[derive(Debug, Clone)]
pub struct Reader<R> {
    buffer: Buffer,
    reader: R,
}

impl<R: Read> Reader<R> {
    pub fn new(reader: R) -> Reader<R> {
        Reader {
            buffer: Buffer::new(),
            reader,
        }
    }

    /// Converts a [`Read`] value along with the internal [`Buffer`] to a [`Reader`].
    ///
    /// Since [`Buffer`] is private, you can only use use this to create a [`Reader`]
    /// from [`Reader::into_inner`].
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use std::io::Cursor;
    /// # use shakmaty::san::SanPlus;
    /// # use pgn_reader::{Reader, Visitor};
    /// let mut reader = Reader::new(Cursor::new("1. e4 e5\n\n1. d4"));
    /// reader.skip_game().unwrap();
    /// let inner = reader.into_inner();
    /// // do something with inner
    /// let (buffer, r) = inner.into_inner();
    /// reader = Reader::from_buffer(buffer.into_inner(), r);
    ///
    /// #[derive(Default)]
    /// struct LastMove(Option<SanPlus>);
    ///
    /// impl Visitor for LastMove {
    ///     type Result = Option<SanPlus>;
    ///
    ///     fn san(&mut self, san_plus: SanPlus) {
    ///         self.0 = Some(san_plus);
    ///     }
    ///
    ///     fn end_game(&mut self) -> Self::Result {
    ///         std::mem::replace(&mut self.0, None)
    ///     }
    /// }
    ///
    /// assert_eq!(reader.read_game(&mut LastMove::default()).unwrap().unwrap(), Some(SanPlus::from_ascii(b"d4").unwrap()));
    /// ```
    pub fn from_buffer(buffer: Buffer, reader: R) -> Reader<R> {
        Reader { buffer, reader }
    }

    fn skip_bom(&mut self) -> io::Result<()> {
        if self
            .buffer
            .ensure_bytes::<3>(&mut self.reader)?
            .starts_with(b"\xef\xbb\xbf")
        {
            self.buffer.consume(3);
        }
        Ok(())
    }

    fn skip_until(&mut self, needle: u8) -> io::Result<()> {
        while !self.buffer.ensure_bytes::<1>(&mut self.reader)?.is_empty() {
            if let Some(pos) = memchr::memchr(needle, self.buffer.data()) {
                self.buffer.consume(pos);
                return Ok(());
            } else {
                self.buffer.clear();
            }
        }
        Ok(())
    }

    fn skip_line(&mut self) -> io::Result<()> {
        self.skip_until(b'\n')?;
        self.buffer.bump();
        Ok(())
    }

    fn skip_whitespace(&mut self) -> io::Result<()> {
        while let &[ch, ..] = self.buffer.ensure_bytes::<1>(&mut self.reader)? {
            match ch {
                b' ' | b'\t' | b'\r' | b'\n' => {
                    self.buffer.bump();
                }
                b'%' => {
                    self.buffer.bump();
                    self.skip_line()?;
                }
                _ => return Ok(()),
            }
        }
        Ok(())
    }

    fn skip_ket(&mut self) -> io::Result<()> {
        while let &[ch, ..] = self.buffer.ensure_bytes::<1>(&mut self.reader)? {
            match ch {
                b' ' | b'\t' | b'\r' | b']' => {
                    self.buffer.bump();
                }
                b'%' => {
                    self.buffer.bump();
                    self.skip_line()?;
                    return Ok(());
                }
                b'\n' => {
                    self.buffer.bump();
                    return Ok(());
                }
                _ => {
                    return Ok(());
                }
            }
        }

        Ok(())
    }

    fn read_tags<V: Visitor>(&mut self, visitor: &mut V) -> io::Result<()> {
        while let &[ch, ..] = self
            .buffer
            .ensure_bytes::<MAX_TAG_LINE_LENGTH>(&mut self.reader)?
        {
            match ch {
                b'[' => {
                    self.buffer.bump();

                    let left_quote = match memchr::memchr3(b'"', b'\n', b']', self.buffer.data()) {
                        Some(left_quote) if self.buffer.data()[left_quote] == b'"' => left_quote,
                        Some(eol) => {
                            self.buffer.consume(eol + 1);
                            self.skip_ket()?;
                            continue;
                        }
                        None => {
                            self.buffer.clear();
                            self.skip_line()?;
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "unterminated tag",
                            ));
                        }
                    };

                    let space = if left_quote > 0 && self.buffer.data()[left_quote - 1] == b' ' {
                        left_quote - 1
                    } else {
                        left_quote
                    };

                    let value_start = left_quote + 1;
                    let mut right_quote = value_start;
                    let consumed = loop {
                        match memchr::memchr3(
                            b'\\',
                            b'"',
                            b'\n',
                            &self.buffer.data()[right_quote..],
                        ) {
                            Some(delta) if self.buffer.data()[right_quote + delta] == b'"' => {
                                right_quote += delta;
                                break right_quote + 1;
                            }
                            Some(delta) if self.buffer.data()[right_quote + delta] == b'\n' => {
                                right_quote += delta;
                                break right_quote;
                            }
                            Some(delta) => {
                                // Skip escaped character.
                                right_quote =
                                    min(right_quote + delta + 2, self.buffer.data().len());
                            }
                            None => {
                                self.buffer.clear();
                                self.skip_line()?;
                                return Err(io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    "unterminated tag",
                                ));
                            }
                        }
                    };

                    visitor.tag(
                        &self.buffer.data()[..space],
                        RawTag(&self.buffer.data()[value_start..right_quote]),
                    );
                    self.buffer.consume(consumed);
                    self.skip_ket()?;
                }
                b'%' => self.skip_line()?,
                _ => return Ok(()),
            }
        }
        Ok(())
    }

    fn skip_movetext(&mut self) -> io::Result<()> {
        while let &[ch, ..] = self.buffer.ensure_bytes::<3>(&mut self.reader)? {
            self.buffer.bump();

            match ch {
                b'{' => {
                    self.skip_until(b'}')?;
                    self.buffer.bump();
                }
                b';' => {
                    self.skip_until(b'\n')?;
                }
                b'\n' => match self.buffer.peek() {
                    Some(b'%') => self.skip_until(b'\n')?,
                    Some(b'\n' | b'[') => break,
                    Some(b'\r') => {
                        self.buffer.bump();
                        if let Some(b'\n') = self.buffer.peek() {
                            break;
                        }
                    }
                    _ => continue,
                },
                _ => {
                    if let Some(consumed) = memchr::memchr3(b'\n', b'{', b';', self.buffer.data()) {
                        self.buffer.consume(consumed);
                    } else {
                        self.buffer.clear();
                    }
                }
            }
        }

        Ok(())
    }

    fn find_token_end(&self) -> usize {
        self.buffer
            .data()
            .iter()
            .copied()
            .position(is_token_end)
            .unwrap_or(self.buffer.data().len())
    }

    fn read_movetext<V: Visitor>(&mut self, visitor: &mut V) -> io::Result<()> {
        while let &[ch, ..] = self
            .buffer
            .ensure_bytes::<MAX_COMMENT_LENGTH>(&mut self.reader)?
        {
            match ch {
                b'{' => {
                    self.buffer.bump();

                    let right_brace =
                        if let Some(right_brace) = memchr::memchr(b'}', self.buffer.data()) {
                            right_brace
                        } else {
                            self.buffer.clear();
                            self.skip_until(b'}')?;
                            self.buffer.bump();
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "unterminated comment",
                            ));
                        };

                    visitor.comment(RawComment(&self.buffer.data()[..right_brace]));
                    self.buffer.consume(right_brace + 1);
                }
                b'\n' => {
                    self.buffer.bump();

                    match self.buffer.peek() {
                        Some(b'%') => {
                            self.buffer.bump();
                            self.skip_line()?;
                        }
                        Some(b'[' | b'\n') => {
                            break;
                        }
                        Some(b'\r') => {
                            self.buffer.bump();
                            if self.buffer.peek() == Some(b'\n') {
                                break;
                            }
                        }
                        _ => continue,
                    }
                }
                b';' => {
                    self.buffer.bump();
                    self.skip_until(b'\n')?;
                }
                b'0' => {
                    self.buffer.bump();
                    if self.buffer.data().starts_with(b"-1") {
                        self.buffer.consume(2);
                        visitor.outcome(Outcome::Known(KnownOutcome::Decisive {
                            winner: Color::Black,
                        }));
                    } else if self.buffer.data().starts_with(b"-0") {
                        // Castling notation with zeros.
                        self.buffer.consume(2);
                        let side = if self.buffer.data().starts_with(b"-0") {
                            self.buffer.consume(2);
                            CastlingSide::QueenSide
                        } else {
                            CastlingSide::KingSide
                        };
                        let suffix = match self.buffer.peek() {
                            Some(b'+') => Some(Suffix::Check),
                            Some(b'#') => Some(Suffix::Checkmate),
                            _ => None,
                        };
                        visitor.san(SanPlus {
                            san: San::Castle(side),
                            suffix,
                        });
                    } else {
                        self.buffer.consume(self.find_token_end());
                    }
                }
                b'1' => {
                    self.buffer.bump();
                    if self.buffer.data().starts_with(b"-0") {
                        self.buffer.consume(2);
                        visitor.outcome(Outcome::Known(KnownOutcome::Decisive {
                            winner: Color::White,
                        }));
                    } else if self.buffer.data().starts_with(b"/2-1/2") {
                        self.buffer.consume(6);
                        visitor.outcome(Outcome::Known(KnownOutcome::Draw));
                    } else {
                        self.buffer.bump();
                        while let Some(b'0'..=b'9') = self.buffer.peek() {
                            self.buffer.bump();
                        }
                        while let Some(b'.' | b' ') = self.buffer.peek() {
                            self.buffer.bump();
                        }
                    }
                }
                b'2'..=b'9' => {
                    self.buffer.bump();
                    while let Some(b'0'..=b'9') = self.buffer.peek() {
                        self.buffer.bump();
                    }
                    while let Some(b'.' | b' ') = self.buffer.peek() {
                        self.buffer.bump();
                    }
                }
                b'(' => {
                    self.buffer.bump();
                    if let Skip(true) = visitor.begin_variation() {
                        self.skip_variation()?;
                    }
                }
                b')' => {
                    self.buffer.bump();
                    visitor.end_variation();
                }
                b'$' => {
                    self.buffer.bump();
                    let token_end = self.find_token_end();
                    if let Ok(nag) = btoi::btou(&self.buffer.data()[..token_end]) {
                        visitor.nag(Nag(nag));
                    }
                    self.buffer.consume(token_end);
                }
                b'!' => {
                    self.buffer.bump();
                    match self.buffer.peek() {
                        Some(b'!') => {
                            self.buffer.bump();
                            visitor.nag(Nag::BRILLIANT_MOVE);
                        }
                        Some(b'?') => {
                            self.buffer.bump();
                            visitor.nag(Nag::SPECULATIVE_MOVE);
                        }
                        _ => {
                            visitor.nag(Nag::GOOD_MOVE);
                        }
                    }
                }
                b'?' => {
                    self.buffer.bump();
                    match self.buffer.peek() {
                        Some(b'!') => {
                            self.buffer.bump();
                            visitor.nag(Nag::DUBIOUS_MOVE);
                        }
                        Some(b'?') => {
                            self.buffer.bump();
                            visitor.nag(Nag::BLUNDER);
                        }
                        _ => {
                            visitor.nag(Nag::MISTAKE);
                        }
                    }
                }
                b'*' => {
                    visitor.outcome(Outcome::Unknown);
                    self.buffer.bump();
                }
                b' ' | b'\t' | b'\r' | b'.' => {
                    self.buffer.bump();
                }
                _ => {
                    if let Ok((san, bytes)) = SanPlus::from_ascii_prefix(self.buffer.data()) {
                        self.buffer.consume(bytes);
                        if self.buffer.peek().is_none_or(is_token_end) {
                            visitor.san(san);
                        }
                    } else {
                        self.buffer.bump();
                        self.buffer.consume(self.find_token_end());
                    }
                }
            }
        }

        Ok(())
    }

    fn skip_variation(&mut self) -> io::Result<()> {
        let mut depth = 0usize;

        while let &[ch, ..] = self.buffer.ensure_bytes::<3>(&mut self.reader)? {
            match ch {
                b'(' => {
                    depth += 1;
                    self.buffer.bump();
                }
                b')' => {
                    if let Some(d) = depth.checked_sub(1) {
                        self.buffer.bump();
                        depth = d;
                    } else {
                        break;
                    }
                }
                b'{' => {
                    self.buffer.bump();
                    self.skip_until(b'}')?;
                    self.buffer.bump();
                }
                b';' => {
                    self.buffer.bump();
                    self.skip_until(b'\n')?;
                }
                b'\n' => {
                    match self.buffer.data().get(1).copied() {
                        Some(b'%') => {
                            self.buffer.consume(2);
                            self.skip_until(b'\n')?;
                        }
                        Some(b'[' | b'\n') => {
                            // Do not consume the first or second line break.
                            break;
                        }
                        Some(b'\r') => {
                            // Do not consume the first or second line break.
                            if self.buffer.data().get(2).copied() == Some(b'\n') {
                                break;
                            }
                        }
                        _ => {
                            self.buffer.bump();
                        }
                    }
                }
                _ => {
                    self.buffer.bump();
                }
            }
        }

        Ok(())
    }

    /// Read a single game, if any, and returns the result produced by the
    /// visitor. Returns `Ok(None)` if the underlying reader is empty.
    ///
    /// # Errors
    ///
    /// * I/O error from the underlying reader.
    /// * Irrecoverable parser errors.
    pub fn read_game<V: Visitor>(&mut self, visitor: &mut V) -> io::Result<Option<V::Result>> {
        self.skip_bom()?;
        self.skip_whitespace()?;

        if self.buffer.ensure_bytes::<1>(&mut self.reader)?.is_empty() {
            return Ok(None);
        }

        visitor.begin_tags();
        self.read_tags(visitor)?;
        if let Skip(false) = visitor.begin_movetext() {
            self.read_movetext(visitor)?;
        } else {
            self.skip_movetext()?;
        }

        self.skip_whitespace()?;
        Ok(Some(visitor.end_game()))
    }

    /// Skip a single game, if any.
    ///
    /// # Errors
    ///
    /// * I/O error from the underlying reader.
    /// * Irrecoverable parser errors.
    pub fn skip_game(&mut self) -> io::Result<bool> {
        self.read_game(&mut SkipVisitor).map(|r| r.is_some())
    }

    /// Read all games.
    ///
    /// # Errors
    ///
    /// * I/O error from the underlying reader.
    /// * Irrecoverable parser errors.
    pub fn read_all<V: Visitor>(&mut self, visitor: &mut V) -> io::Result<()> {
        while self.read_game(visitor)?.is_some() {}
        Ok(())
    }

    /// Create an iterator over all games.
    ///
    /// # Errors
    ///
    /// * I/O error from the underlying reader.
    /// * Irrecoverable parser errors.
    pub fn into_iter<V: Visitor>(self, visitor: &mut V) -> IntoIter<'_, V, R> {
        IntoIter {
            reader: self,
            visitor,
        }
    }

    /// Gets the remaining bytes in the buffer and the underlying reader.
    pub fn into_inner(self) -> Chain<Cursor<Buffer>, R> {
        Cursor::new(self.buffer).chain(self.reader)
    }

    /// Returns whether the reader has another game to parse, but does not
    /// actually parse it.
    ///
    /// # Errors
    ///
    /// * I/O error from the underlying reader.
    pub fn has_more(&mut self) -> io::Result<bool> {
        self.skip_bom()?;
        self.skip_whitespace()?;
        Ok(!self.buffer.ensure_bytes::<1>(&mut self.reader)?.is_empty())
    }
}

/// Iterator returned by [`Reader::into_iter()`].
#[derive(Debug)]
#[must_use]
pub struct IntoIter<'a, V: 'a, R> {
    visitor: &'a mut V,
    reader: Reader<R>,
}

impl<V: Visitor, R: Read> Iterator for IntoIter<'_, V, R> {
    type Item = Result<V::Result, io::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.reader.read_game(self.visitor) {
            Ok(Some(result)) => Some(Ok(result)),
            Ok(None) => None,
            Err(err) => Some(Err(err)),
        }
    }
}

#[inline]
fn is_token_end(byte: u8) -> bool {
    matches!(
        byte,
        b' ' | b'\t'
            | b'\n'
            | b'\r'
            | b'{'
            | b'}'
            | b'('
            | b')'
            | b'!'
            | b'?'
            | b'$'
            | b';'
            | b'.'
            | b'*'
    )
}

impl<R: Seek> Seek for Reader<R> {
    fn seek(&mut self, pos: SeekFrom) -> io::Result<u64> {
        let result = if let SeekFrom::Current(offset) = pos {
            let buffered = self.buffer.data_len() as i64;
            if let Some(offset) = offset.checked_sub(buffered) {
                self.reader.seek(SeekFrom::Current(offset))?
            } else {
                self.reader.seek_relative(-buffered)?;
                self.buffer.clear();
                self.reader.seek(SeekFrom::Current(offset))?
            }
        } else {
            self.reader.seek(pos)?
        };
        self.buffer.clear();
        Ok(result)
    }

    fn seek_relative(&mut self, offset: i64) -> io::Result<()> {
        let buffered = self.buffer.data_len() as i64;
        if let Some(offset) = offset.checked_sub(buffered) {
            self.reader.seek_relative(offset)?;
            self.buffer.clear();
            Ok(())
        } else {
            self.reader.seek_relative(-buffered)?;
            self.buffer.clear();
            self.reader.seek_relative(offset)
        }
    }

    fn stream_position(&mut self) -> io::Result<u64> {
        let buffered = self.buffer.data_len() as u64;
        self.reader.stream_position().map(|pos| {
            pos.checked_sub(buffered)
                .expect("consistent stream position")
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct _AssertObjectSafe<R>(Box<Reader<R>>);

    #[derive(Default)]
    struct GameCounter {
        count: usize,
    }

    impl Visitor for GameCounter {
        type Result = ();

        fn end_game(&mut self) {
            self.count += 1;
        }
    }

    #[test]
    fn test_empty_game() -> Result<(), io::Error> {
        let mut counter = GameCounter::default();
        let mut reader = Reader::new(io::Cursor::new(b"  "));
        reader.read_game(&mut counter)?;
        assert_eq!(counter.count, 0);
        Ok(())
    }

    #[test]
    fn test_trailing_space() -> Result<(), io::Error> {
        let mut counter = GameCounter::default();
        let mut reader = Reader::new(io::Cursor::new(b"1. e4 1-0\n\n\n\n\n  \n"));
        reader.read_game(&mut counter)?;
        assert_eq!(counter.count, 1);
        reader.read_game(&mut counter)?;
        assert_eq!(counter.count, 1);
        Ok(())
    }

    #[test]
    fn test_nag() -> Result<(), io::Error> {
        struct NagCollector {
            nags: Vec<Nag>,
        }

        impl Visitor for NagCollector {
            type Result = ();

            fn nag(&mut self, nag: Nag) {
                self.nags.push(nag);
            }

            fn end_game(&mut self) {}
        }

        let mut collector = NagCollector { nags: Vec::new() };
        let mut reader = Reader::new(io::Cursor::new(b"1.f3! e5$71 2.g4?? Qh4#!?"));
        reader.read_game(&mut collector)?;
        assert_eq!(
            collector.nags,
            vec![Nag::GOOD_MOVE, Nag(71), Nag::BLUNDER, Nag::SPECULATIVE_MOVE]
        );
        Ok(())
    }

    #[test]
    fn test_null_moves() -> Result<(), io::Error> {
        struct SanCollector {
            sans: Vec<San>,
        }

        impl Visitor for SanCollector {
            type Result = ();

            fn san(&mut self, san: SanPlus) {
                self.sans.push(san.san);
            }

            fn end_game(&mut self) {}
        }

        let mut collector = SanCollector { sans: Vec::new() };
        let mut reader = Reader::new(io::Cursor::new(b"1. e4 -- 2. Nf3 -- 3. -- e5"));
        reader.read_game(&mut collector)?;
        assert_eq!(collector.sans.len(), 6);
        assert_ne!(collector.sans[0], San::Null);
        assert_eq!(collector.sans[1], San::Null);
        assert_ne!(collector.sans[2], San::Null);
        assert_eq!(collector.sans[3], San::Null);
        assert_eq!(collector.sans[4], San::Null);
        assert_ne!(collector.sans[5], San::Null);
        Ok(())
    }
}
