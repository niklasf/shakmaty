use std::{
    cmp::min,
    fmt::Debug,
    io::{self, Chain, Cursor, Read},
    ops::ControlFlow,
};

use shakmaty::{
    CastlingSide, Color, KnownOutcome, Outcome,
    san::{San, SanPlus, Suffix},
};

// use slice_deque::SliceDeque;
use crate::{
    buffer::Buffer,
    types::{Nag, RawComment, RawTag, Skip},
    visitor::{SkipVisitor, Visitor},
};

#[derive(Debug, Clone)]
pub struct BufferedReader<R> {
    reader: R,
    buffer: Buffer,
    max_tag_line_length: usize,
    max_comment_length: usize,
}

impl<R: Read> BufferedReader<R> {
    pub fn new(reader: R) -> BufferedReader<R> {
        BufferedReader {
            reader,
            buffer: Buffer::with_capacity(1 << 14),
            max_tag_line_length: 1024,
            max_comment_length: 4096,
        }
    }

    /// Converts a [`Read`] value along with the internal [`Buffer`] to a [`BufferedReader`].
    ///
    /// Since [`Buffer`] is private, you can only use use this to create a [`BufferedReader`]
    /// from [`BufferedReader::into_inner`].
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use std::convert::Infallible;
    /// # use std::io::Cursor;
    /// # use shakmaty::san::SanPlus;
    /// # use pgn_reader::{BufferedReader, Visitor};
    /// let mut reader = BufferedReader::new(Cursor::new("1. e4 e5\n\n1. d4"));
    /// reader.skip_game().unwrap();
    /// let inner = reader.into_inner();
    /// // do something with inner
    /// let (buffer, r) = inner.into_inner();
    /// reader = BufferedReader::from_buffer(buffer.into_inner(), r);
    ///
    /// #[derive(Default)]
    /// struct LastMove(Option<SanPlus>);
    ///
    /// impl Visitor for LastMove {
    ///     type Output = Option<SanPlus>;
    ///     type Error = Infallible;
    ///
    ///     fn san(&mut self, san_plus: SanPlus) -> Result<(), Self::Break> {
    ///         self.0 = Some(san_plus);
    ///
    ///         Ok(())
    ///     }
    ///
    ///     fn end_game(&mut self) -> Self::Output {
    ///         std::mem::replace(&mut self.0, None)
    ///     }
    /// }
    ///
    /// assert_eq!(reader.read_game(&mut LastMove::default()).unwrap().unwrap().unwrap(), Some(SanPlus::from_ascii(b"d4").unwrap()));
    /// ```
    pub fn from_buffer(buffer: Buffer, reader: R) -> BufferedReader<R> {
        BufferedReader {
            reader,
            buffer,
            max_tag_line_length: 1024,
            max_comment_length: 4096,
        }
    }

    fn skip_bom(&mut self) -> io::Result<()> {
        if self
            .buffer
            .ensure_bytes(3, &mut self.reader)?
            .starts_with(b"\xef\xbb\xbf")
        {
            self.buffer.consume(3);
        }
        Ok(())
    }

    fn skip_until(&mut self, needle: u8) -> io::Result<()> {
        while !self.buffer.ensure_bytes(1, &mut self.reader)?.is_empty() {
            if let Some(pos) = memchr::memchr(needle, self.buffer.data()) {
                self.buffer.consume(pos);
                return Ok(());
            } else {
                self.buffer.discard_data();
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
        while let &[ch, ..] = self.buffer.ensure_bytes(1, &mut self.reader)? {
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
        while let &[ch, ..] = self.buffer.ensure_bytes(1, &mut self.reader)? {
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

    fn read_tags<V: Visitor>(
        &mut self,
        visitor: &mut ControlFlow<V::Break, &mut V>,
    ) -> io::Result<()> {
        while let &[ch, ..] = self
            .buffer
            .ensure_bytes(self.max_tag_line_length, &mut self.reader)?
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
                            self.buffer.discard_data();
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
                                self.buffer.discard_data();
                                self.skip_line()?;
                                return Err(io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    "unterminated tag",
                                ));
                            }
                        }
                    };

                    if let ControlFlow::Continue(visitor_continue) = visitor {
                        visitor_continue
                            .tag(
                                &self.buffer.data()[..space],
                                RawTag(&self.buffer.data()[value_start..right_quote]),
                            )
                            .map_break(|b| *visitor = ControlFlow::Break(b));
                    }

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
        while let &[ch, ..] = self.buffer.ensure_bytes(3, &mut self.reader)? {
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
                    Some(b'\n') | Some(b'[') => break,
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
                        self.buffer.discard_data();
                    }
                }
            }
        }

        Ok(())
    }

    fn find_token_end(&mut self, start: usize) -> usize {
        let mut end = start;
        for &ch in &self.buffer.data()[start..] {
            match ch {
                b' ' | b'\t' | b'\n' | b'\r' | b'{' | b'}' | b'(' | b')' | b'!' | b'?' | b'$'
                | b';' | b'.' => break,
                _ => end += 1,
            }
        }
        end
    }

    fn read_movetext<V: Visitor>(
        &mut self,
        visitor: &mut ControlFlow<V::Break, &mut V>,
    ) -> io::Result<()> {
        while let &[ch, ..] = self
            .buffer
            .ensure_bytes(self.max_comment_length, &mut self.reader)?
        {
            match ch {
                b'{' => {
                    self.buffer.bump();

                    let right_brace =
                        if let Some(right_brace) = memchr::memchr(b'}', self.buffer.data()) {
                            right_brace
                        } else {
                            self.buffer.discard_data();
                            self.skip_until(b'}')?;
                            self.buffer.bump();
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "unterminated comment",
                            ));
                        };

                    if let ControlFlow::Continue(visitor_continue) = visitor {
                        visitor_continue
                            .comment(RawComment(&self.buffer.data()[..right_brace]))
                            .map_break(|b| *visitor = ControlFlow::Break(b));
                    }

                    self.buffer.consume(right_brace + 1);
                }
                b'\n' => {
                    self.buffer.bump();

                    match self.buffer.peek() {
                        Some(b'%') => {
                            self.buffer.bump();
                            self.skip_line()?;
                        }
                        Some(b'[') | Some(b'\n') => {
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

                        if let ControlFlow::Continue(visitor_continue) = visitor {
                            visitor_continue
                                .outcome(Outcome::Known(KnownOutcome::Decisive {
                                    winner: Color::Black,
                                }))
                                .map_break(|b| *visitor = ControlFlow::Break(b));
                        }
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

                        if let ControlFlow::Continue(visitor_continue) = visitor {
                            visitor_continue
                                .san(SanPlus {
                                    san: San::Castle(side),
                                    suffix,
                                })
                                .map_break(|b| *visitor = ControlFlow::Break(b));
                        }
                    } else {
                        let token_end = self.find_token_end(0);
                        self.buffer.consume(token_end);
                    }
                }
                b'1' => {
                    self.buffer.bump();
                    if self.buffer.data().starts_with(b"-0") {
                        self.buffer.consume(2);

                        if let ControlFlow::Continue(visitor_continue) = visitor {
                            visitor_continue
                                .outcome(Outcome::Known(KnownOutcome::Decisive {
                                    winner: Color::White,
                                }))
                                .map_break(|b| *visitor = ControlFlow::Break(b));
                        }
                    } else if self.buffer.data().starts_with(b"/2-1/2") {
                        self.buffer.consume(6);

                        if let ControlFlow::Continue(visitor_continue) = visitor {
                            visitor_continue
                                .outcome(Outcome::Known(KnownOutcome::Draw))
                                .map_break(|b| *visitor = ControlFlow::Break(b));
                        }
                    } else {
                        self.buffer.bump();
                        while let Some(ch) = self.buffer.peek() {
                            if b'0' <= ch && ch <= b'9' {
                                self.buffer.bump();
                            } else {
                                break;
                            }
                        }
                        while let Some(b'.') = self.buffer.peek() {
                            self.buffer.bump();
                        }
                    }
                }
                b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' => {
                    self.buffer.bump();
                    while let Some(ch) = self.buffer.peek() {
                        if b'0' <= ch && ch <= b'9' {
                            self.buffer.bump();
                        } else {
                            break;
                        }
                    }
                    while let Some(b'.') = self.buffer.peek() {
                        self.buffer.bump();
                    }
                }
                b'(' => {
                    self.buffer.bump();

                    if let ControlFlow::Continue(visitor_continue) = visitor {
                        if let ControlFlow::Continue(Skip(true)) = visitor_continue
                            .begin_variation()
                            .map_break(|b| *visitor = ControlFlow::Break(b))
                        {
                            self.skip_variation()?;
                        }
                    }
                }
                b')' => {
                    self.buffer.bump();

                    if let ControlFlow::Continue(visitor_continue) = visitor {
                        visitor_continue
                            .end_variation()
                            .map_break(|b| *visitor = ControlFlow::Break(b));
                    }
                }
                b'$' => {
                    self.buffer.bump();
                    let token_end = self.find_token_end(0);
                    if let Ok(nag) = btoi::btou(&self.buffer.data()[..token_end]) {
                        if let ControlFlow::Continue(visitor_continue) = visitor {
                            visitor_continue
                                .nag(Nag(nag))
                                .map_break(|b| *visitor = ControlFlow::Break(b));
                        }
                    }
                    self.buffer.consume(token_end);
                }
                b'!' => {
                    self.buffer.bump();
                    match self.buffer.peek() {
                        Some(b'!') => {
                            self.buffer.bump();

                            if let ControlFlow::Continue(visitor_continue) = visitor {
                                visitor_continue
                                    .nag(Nag::BRILLIANT_MOVE)
                                    .map_break(|b| *visitor = ControlFlow::Break(b));
                            }
                        }
                        Some(b'?') => {
                            self.buffer.bump();

                            if let ControlFlow::Continue(visitor_continue) = visitor {
                                visitor_continue
                                    .nag(Nag::SPECULATIVE_MOVE)
                                    .map_break(|b| *visitor = ControlFlow::Break(b));
                            }
                        }
                        _ => {
                            if let ControlFlow::Continue(visitor_continue) = visitor {
                                visitor_continue
                                    .nag(Nag::GOOD_MOVE)
                                    .map_break(|b| *visitor = ControlFlow::Break(b));
                            }
                        }
                    }
                }
                b'?' => {
                    self.buffer.bump();
                    match self.buffer.peek() {
                        Some(b'!') => {
                            self.buffer.bump();

                            if let ControlFlow::Continue(visitor_continue) = visitor {
                                visitor_continue
                                    .nag(Nag::DUBIOUS_MOVE)
                                    .map_break(|b| *visitor = ControlFlow::Break(b));
                            }
                        }
                        Some(b'?') => {
                            self.buffer.bump();

                            if let ControlFlow::Continue(visitor_continue) = visitor {
                                visitor_continue
                                    .nag(Nag::BLUNDER)
                                    .map_break(|b| *visitor = ControlFlow::Break(b));
                            }
                        }
                        _ => {
                            if let ControlFlow::Continue(visitor_continue) = visitor {
                                visitor_continue
                                    .nag(Nag::MISTAKE)
                                    .map_break(|b| *visitor = ControlFlow::Break(b));
                            }
                        }
                    }
                }
                b'*' => {
                    if let ControlFlow::Continue(visitor_continue) = visitor {
                        visitor_continue
                            .outcome(Outcome::Unknown)
                            .map_break(|b| *visitor = ControlFlow::Break(b));
                    }

                    self.buffer.bump();
                }
                b'a' | b'b' | b'c' | b'd' | b'e' | b'f' | b'g' | b'h' | b'N' | b'B' | b'R'
                | b'Q' | b'K' | b'@' | b'-' | b'O' => {
                    let token_end = self.find_token_end(1);
                    if let Ok(san) = SanPlus::from_ascii(&self.buffer.data()[..token_end]) {
                        if let ControlFlow::Continue(visitor_continue) = visitor {
                            visitor_continue
                                .san(san)
                                .map_break(|b| *visitor = ControlFlow::Break(b));
                        }
                    }
                    self.buffer.consume(token_end);
                }
                _ => {
                    self.buffer.bump();
                }
            }
        }

        Ok(())
    }

    fn skip_variation(&mut self) -> io::Result<()> {
        let mut depth = 0usize;

        while let &[ch, ..] = self.buffer.ensure_bytes(3, &mut self.reader)? {
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
                    match self.buffer.data().get(1).cloned() {
                        Some(b'%') => {
                            self.buffer.consume(2);
                            self.skip_until(b'\n')?;
                        }
                        Some(b'[') | Some(b'\n') => {
                            // Do not consume the first or second line break.
                            break;
                        }
                        Some(b'\r') => {
                            // Do not consume the first or second line break.
                            if self.buffer.data().get(2).cloned() == Some(b'\n') {
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
    /// visitor, with a possible error. Returns `Ok(None)` if the underlying reader is empty.
    ///
    /// # Errors
    ///
    /// * I/O error from the underlying reader.
    /// * Irrecoverable parser errors.
    /// * Visitor errors. Note that the rest of the game is consumed even if the visitor errors.
    pub fn read_game<V: Visitor>(
        &mut self,
        visitor: &mut V,
    ) -> io::Result<Option<ControlFlow<V::Break, V::Output>>> {
        self.skip_bom()?;
        self.skip_whitespace()?;

        if self.buffer.ensure_bytes(1, &mut self.reader)?.is_empty() {
            return Ok(None);
        }

        let mut visitor = visitor.begin_tags().map_continue(|()| visitor);

        self.read_tags(&mut visitor)?;

        let skip = if let ControlFlow::Continue(ref mut visitor_continue) = visitor {
            match visitor_continue.begin_movetext() {
                ControlFlow::Continue(skip) => skip,
                ControlFlow::Break(b) => {
                    visitor = ControlFlow::Break(b);
                    Skip(true)
                }
            }
        } else {
            Skip(true)
        };

        if skip.0 {
            self.skip_movetext()?;
        } else {
            self.read_movetext(&mut visitor)?;
        }

        self.skip_whitespace()?;
        Ok(Some(visitor.map_continue(|v| v.end_game())))
    }

    /// Skip a single game, returns `true` if there was one, `false` otherwise.
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
    /// See [`Self::read_game`].
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
        Ok(!self.buffer.ensure_bytes(1, &mut self.reader)?.is_empty())
    }
}

/// Iterator returned by
/// [`BufferedReader::into_iter()`](struct.BufferedReader.html#method.into_iter).
#[derive(Debug)]
#[must_use]
pub struct IntoIter<'a, V: 'a, R> {
    visitor: &'a mut V,
    reader: BufferedReader<R>,
}

impl<'a, V: Visitor, R: Read> Iterator for IntoIter<'a, V, R> {
    type Item = io::Result<ControlFlow<V::Break, V::Output>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.reader.read_game(self.visitor) {
            Ok(Some(result)) => Some(Ok(result)),
            Ok(None) => None,
            Err(err) => Some(Err(err)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::convert::Infallible;

    use super::*;

    struct _AssertObjectSafe<R>(Box<BufferedReader<R>>);

    #[derive(Default)]
    struct GameCounter {
        count: usize,
    }

    impl Visitor for GameCounter {
        type Output = ();
        type Break = Infallible;

        fn end_game(&mut self) -> Self::Output {
            self.count += 1;
        }
    }

    #[test]
    fn test_empty_game() -> io::Result<()> {
        let mut counter = GameCounter::default();
        let mut reader = BufferedReader::new(io::Cursor::new(b"  "));
        reader.read_game(&mut counter)?;
        assert_eq!(counter.count, 0);
        Ok(())
    }

    #[test]
    fn test_trailing_space() -> io::Result<()> {
        let mut counter = GameCounter::default();
        let mut reader = BufferedReader::new(io::Cursor::new(b"1. e4 1-0\n\n\n\n\n  \n"));
        reader.read_game(&mut counter)?;
        assert_eq!(counter.count, 1);
        reader.read_game(&mut counter)?;
        assert_eq!(counter.count, 1);
        Ok(())
    }

    #[test]
    fn test_nag() -> io::Result<()> {
        struct NagCollector {
            nags: Vec<Nag>,
        }

        impl Visitor for NagCollector {
            type Output = ();
            type Break = Infallible;

            fn nag(&mut self, nag: Nag) -> ControlFlow<Self::Break> {
                self.nags.push(nag);

                ControlFlow::Continue(())
            }

            fn end_game(&mut self) -> Self::Output {}
        }

        let mut collector = NagCollector { nags: Vec::new() };
        let mut reader = BufferedReader::new(io::Cursor::new(b"1.f3! e5$71 2.g4?? Qh4#!?"));
        reader.read_game(&mut collector)?;
        assert_eq!(
            collector.nags,
            vec![Nag::GOOD_MOVE, Nag(71), Nag::BLUNDER, Nag::SPECULATIVE_MOVE]
        );
        Ok(())
    }

    #[test]
    fn test_null_moves() -> io::Result<()> {
        struct SanCollector {
            sans: Vec<San>,
        }

        impl Visitor for SanCollector {
            type Output = ();
            type Break = Infallible;

            fn san(&mut self, san: SanPlus) -> ControlFlow<Self::Break> {
                self.sans.push(san.san);

                ControlFlow::Continue(())
            }

            fn end_game(&mut self) -> Self::Output {}
        }

        let mut collector = SanCollector { sans: Vec::new() };
        let mut reader = BufferedReader::new(io::Cursor::new(b"1. e4 -- 2. Nf3 -- 3. -- e5"));
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

    #[test]
    fn test_error_in_tags() {
        struct TagErrorer {
            error_when: usize,
            now: usize,
        }

        impl Visitor for TagErrorer {
            type Output = ();
            type Break = String;

            fn begin_tags(&mut self) -> ControlFlow<Self::Break> {
                self.now = 0;

                ControlFlow::Continue(())
            }

            fn tag(&mut self, _name: &[u8], value: RawTag<'_>) -> ControlFlow<Self::Break> {
                if self.now == self.error_when {
                    return ControlFlow::Break(value.decode_utf8_lossy().to_string());
                }

                self.now += 1;

                ControlFlow::Continue(())
            }

            fn end_game(&mut self) -> Self::Output {
                ()
            }
        }

        let mut errorer = TagErrorer {
            error_when: 2,
            now: 0,
        };

        let mut reader = BufferedReader::new(io::Cursor::new(
            r#"[Foo "f"]
[Bar "\"bar"]
[Err "err"]
[Qux "q\"\"\"\\ux"]
        "#,
        ));

        // reads the whole game even if error occurs
        assert_eq!(
            ControlFlow::Break("err".to_string()),
            reader.read_game(&mut errorer).unwrap().unwrap()
        );

        errorer.error_when = 0;

        // no game left
        assert_eq!(None, reader.read_game(&mut errorer).unwrap());
    }
}
