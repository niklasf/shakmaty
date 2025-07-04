use std::{
    cmp::{max, min},
    convert::Infallible,
    io::{self, Read, Seek, SeekFrom},
    mem,
    ops::ControlFlow,
};

use shakmaty::{
    CastlingSide, Color, KnownOutcome, Outcome,
    san::{San, SanPlus, Suffix},
};

use crate::{RawTag, Skip, Visitor, buffer::Buffer, comment::RawComment, nag::Nag};

/// Build a [`Reader`] with custom settings.
#[derive(Debug, Clone)]
pub struct ReaderBuilder<R> {
    reader: R,
    tag_line_bytes: usize,
    movetext_token_bytes: usize,
}

impl<R: Read> ReaderBuilder<R> {
    /// Create a [`ReaderBuilder`] with default settings based on the PGN
    /// standard.
    pub fn new(reader: R) -> Self {
        ReaderBuilder {
            reader,
            tag_line_bytes: 255,
            movetext_token_bytes: 255,
        }
    }

    /// Configure the buffer to support *at least* the given tag line length.
    ///
    /// Defaults to `255` bytes.
    pub fn set_supported_tag_line_length(mut self, bytes: usize) -> Self {
        self.tag_line_bytes = max(255, bytes);
        self
    }

    /// Configure the buffer to support *at least* the given comment length.
    ///
    /// Defaults to `255` bytes.
    pub fn set_supported_comment_length(mut self, bytes: usize) -> Self {
        self.movetext_token_bytes = max(255, bytes) + 2; // Plus '{' and '}'
        self
    }

    /// Finalize and create a [`Reader`].
    pub fn finish(self) -> Reader<R> {
        Reader {
            reader: self.reader,
            tag_line_bytes: self.tag_line_bytes,
            movetext_token_bytes: self.movetext_token_bytes,
            buffer: Buffer::with_capacity(max(
                1 << 14,
                max(self.tag_line_bytes, self.movetext_token_bytes).next_power_of_two() * 2,
            )),
            pending_skip_tags: false,
            pending_skip_movetext: false,
        }
    }
}

/// Reads a PGN and calls [`Visitor`] methods.
///
/// Buffers the underlying reader with an appropriate strategy, so it's *not*
/// recommended to add an additional layer of buffering like
/// [`BufReader`](std::io::BufReader).
#[derive(Debug, Clone)]
pub struct Reader<R> {
    buffer: Buffer,
    reader: R,
    tag_line_bytes: usize,
    movetext_token_bytes: usize,
    pending_skip_tags: bool,
    pending_skip_movetext: bool,
}

impl<R: Read> Reader<R> {
    /// Create a reader with default settings based on the PGN standard.
    pub fn new(reader: R) -> Reader<R> {
        ReaderBuilder::new(reader).finish()
    }

    /// Build a reader with custom settings.
    ///
    /// ```no_run
    /// use std::fs::File;
    /// use pgn_reader::Reader;
    ///
    /// let reader = Reader::build(File::open("example.pgn")?)
    ///     .set_supported_tag_line_length(1000)
    ///     .set_supported_comment_length(4000)
    ///     .finish();
    /// # Ok::<_, std::io::Error>(())
    /// ```
    pub fn build(reader: R) -> ReaderBuilder<R> {
        ReaderBuilder::new(reader)
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
                self.buffer.clear();
            }
        }
        Ok(())
    }

    fn skip_until_after(&mut self, needle: u8) -> io::Result<()> {
        while !self.buffer.ensure_bytes(1, &mut self.reader)?.is_empty() {
            if let Some(pos) = memchr::memchr(needle, self.buffer.data()) {
                self.buffer.consume(pos + 1);
                return Ok(());
            } else {
                self.buffer.clear();
            }
        }
        Ok(())
    }

    fn skip_line(&mut self) -> io::Result<()> {
        self.skip_until_after(b'\n')
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

    fn skip_token(&mut self) -> io::Result<()> {
        while let &[_, ..] = self.buffer.ensure_bytes(1, &mut self.reader)? {
            if let Some(end) = self.find_token_end() {
                self.buffer.consume(end);
                break;
            } else {
                self.buffer.clear();
            }
        }
        Ok(())
    }

    fn find_token_end(&self) -> Option<usize> {
        self.buffer.data().iter().copied().position(is_token_end)
    }

    fn skip_tags(&mut self) -> io::Result<()> {
        struct IgnoreTagsVisitor;

        impl Visitor for IgnoreTagsVisitor {
            type Tags = ();
            type Movetext = Infallible;
            type Output = Infallible;

            fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
                ControlFlow::Continue(())
            }

            fn begin_movetext(
                &mut self,
                _tags: Self::Tags,
            ) -> ControlFlow<Self::Output, Self::Movetext> {
                unreachable!()
            }

            fn end_game(&mut self, _movetext: Self::Movetext) -> Self::Output {
                unreachable!()
            }
        }

        let _ = self.read_tags(&mut IgnoreTagsVisitor, &mut ())?;
        Ok(())
    }

    fn read_tags<V: Visitor>(
        &mut self,
        visitor: &mut V,
        tags: &mut V::Tags,
    ) -> io::Result<ControlFlow<V::Output>> {
        while let &[ch, ..] = self
            .buffer
            .ensure_bytes(self.tag_line_bytes, &mut self.reader)?
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
                                right_quote = min(right_quote + delta + 2, self.buffer.len());
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

                    let cf = visitor.tag(
                        tags,
                        &self.buffer.data()[..space],
                        RawTag(&self.buffer.data()[value_start..right_quote]),
                    );
                    self.buffer.consume(consumed);
                    self.skip_ket()?;
                    if cf.is_break() {
                        return Ok(cf);
                    }
                }
                b'%' => self.skip_line()?,
                _ => return Ok(ControlFlow::Continue(())),
            }
        }
        Ok(ControlFlow::Continue(()))
    }

    fn skip_movetext(&mut self) -> io::Result<()> {
        while let &[ch, ..] = self.buffer.ensure_bytes(3, &mut self.reader)? {
            self.buffer.bump();

            match ch {
                b'{' => {
                    self.skip_until_after(b'}')?;
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

    fn read_movetext<V: Visitor>(
        &mut self,
        visitor: &mut V,
        movetext: &mut V::Movetext,
    ) -> io::Result<ControlFlow<V::Output>> {
        while let &[ch, ..] = self
            .buffer
            .ensure_bytes(self.movetext_token_bytes, &mut self.reader)?
        {
            match ch {
                b'{' => {
                    self.buffer.bump();

                    let right_brace =
                        if let Some(right_brace) = memchr::memchr(b'}', self.buffer.data()) {
                            right_brace
                        } else {
                            self.buffer.clear();
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "unterminated comment",
                            ));
                        };

                    let cf =
                        visitor.comment(movetext, RawComment(&self.buffer.data()[..right_brace]));
                    self.buffer.consume(right_brace + 1);
                    if cf.is_break() {
                        return Ok(cf);
                    }
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
                        let cf = visitor.outcome(
                            movetext,
                            Outcome::Known(KnownOutcome::Decisive {
                                winner: Color::Black,
                            }),
                        );
                        if cf.is_break() {
                            return Ok(cf);
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
                        let cf = visitor.san(
                            movetext,
                            SanPlus {
                                san: San::Castle(side),
                                suffix,
                            },
                        );
                        if cf.is_break() {
                            return Ok(cf);
                        }
                    } else {
                        self.skip_token()?;
                    }
                }
                b'1' => {
                    self.buffer.bump();
                    if self.buffer.data().starts_with(b"-0") {
                        self.buffer.consume(2);
                        let cf = visitor.outcome(
                            movetext,
                            Outcome::Known(KnownOutcome::Decisive {
                                winner: Color::White,
                            }),
                        );
                        if cf.is_break() {
                            return Ok(cf);
                        }
                    } else if self.buffer.data().starts_with(b"/2-1/2") {
                        self.buffer.consume(6);
                        let cf = visitor.outcome(movetext, Outcome::Known(KnownOutcome::Draw));
                        if cf.is_break() {
                            return Ok(cf);
                        }
                    } else {
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
                    match visitor.begin_variation(movetext) {
                        ControlFlow::Continue(Skip(true)) => self.skip_variation()?,
                        ControlFlow::Continue(Skip(false)) => (),
                        ControlFlow::Break(output) => {
                            return Ok(ControlFlow::Break(output));
                        }
                    }
                }
                b')' => {
                    self.buffer.bump();
                    let cf = visitor.end_variation(movetext);
                    if cf.is_break() {
                        return Ok(cf);
                    }
                }
                b'$' => {
                    self.buffer.bump();
                    if let Some(token_end) = self.find_token_end() {
                        if let Ok(nag) = btoi::btou(&self.buffer.data()[..token_end]) {
                            let cf = visitor.nag(movetext, Nag(nag));
                            if cf.is_break() {
                                return Ok(cf);
                            }
                        }
                        self.buffer.consume(token_end);
                    } else {
                        self.buffer.clear();
                        self.skip_token()?;
                    }
                }
                b'!' => {
                    self.buffer.bump();
                    let cf = match self.buffer.peek() {
                        Some(b'!') => {
                            self.buffer.bump();
                            visitor.nag(movetext, Nag::BRILLIANT_MOVE)
                        }
                        Some(b'?') => {
                            self.buffer.bump();
                            visitor.nag(movetext, Nag::SPECULATIVE_MOVE)
                        }
                        _ => visitor.nag(movetext, Nag::GOOD_MOVE),
                    };
                    if cf.is_break() {
                        return Ok(cf);
                    }
                }
                b'?' => {
                    self.buffer.bump();
                    let cf = match self.buffer.peek() {
                        Some(b'!') => {
                            self.buffer.bump();
                            visitor.nag(movetext, Nag::DUBIOUS_MOVE)
                        }
                        Some(b'?') => {
                            self.buffer.bump();
                            visitor.nag(movetext, Nag::BLUNDER)
                        }
                        _ => visitor.nag(movetext, Nag::MISTAKE),
                    };
                    if cf.is_break() {
                        return Ok(cf);
                    }
                }
                b'*' => {
                    self.buffer.bump();
                    let cf = visitor.outcome(movetext, Outcome::Unknown);
                    if cf.is_break() {
                        return Ok(cf);
                    }
                }
                b' ' | b'\t' | b'\r' | b'.' => {
                    self.buffer.bump();
                }
                _ => {
                    if let Ok((san, bytes)) = SanPlus::from_ascii_prefix(self.buffer.data()) {
                        self.buffer.consume(bytes);
                        if self.buffer.peek().is_none_or(is_token_end) {
                            let cf = visitor.san(movetext, san);
                            if cf.is_break() {
                                return Ok(cf);
                            }
                        }
                    } else {
                        self.buffer.bump();
                        self.skip_token()?;
                    }
                }
            }
        }

        Ok(ControlFlow::Continue(()))
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
                    self.skip_until_after(b'}')?;
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

    fn before_game(&mut self) -> io::Result<()> {
        if mem::take(&mut self.pending_skip_tags) {
            self.skip_tags()?;
        }
        if mem::take(&mut self.pending_skip_movetext) {
            self.skip_movetext()?;
        }
        self.skip_bom()?;
        self.skip_whitespace()
    }

    /// Read a single game, if any, and return the result produced by the
    /// visitor.
    ///
    /// Returns `Ok(None)` if the underlying reader is empty.
    ///
    /// # Errors
    ///
    /// * I/O error from the underlying reader.
    /// * Irrecoverable parser errors.
    pub fn read_game<V: Visitor>(&mut self, visitor: &mut V) -> io::Result<Option<V::Output>> {
        self.before_game()?;

        if self.buffer.ensure_bytes(1, &mut self.reader)?.is_empty() {
            return Ok(None);
        }

        let mut tags = match visitor.begin_tags() {
            ControlFlow::Break(output) => {
                self.pending_skip_tags = true;
                self.pending_skip_movetext = true;
                return Ok(Some(output));
            }
            ControlFlow::Continue(tags) => tags,
        };
        if let ControlFlow::Break(output) = self.read_tags(visitor, &mut tags)? {
            self.pending_skip_tags = true;
            self.pending_skip_movetext = true;
            return Ok(Some(output));
        }
        let mut movetext = match visitor.begin_movetext(tags) {
            ControlFlow::Break(output) => {
                self.pending_skip_movetext = true;
                return Ok(Some(output));
            }
            ControlFlow::Continue(movetext) => movetext,
        };
        if let ControlFlow::Break(output) = self.read_movetext(visitor, &mut movetext)? {
            self.pending_skip_movetext = true;
            return Ok(Some(output));
        };
        Ok(Some(visitor.end_game(movetext)))
    }

    /// Returns whether the reader has another game to parse, but does not
    /// actually parse it.
    ///
    /// # Errors
    ///
    /// * I/O error from the underlying reader.
    pub fn has_more(&mut self) -> io::Result<bool> {
        self.before_game()?;
        Ok(!self.buffer.ensure_bytes(1, &mut self.reader)?.is_empty())
    }

    /// Skip a single game, if any.
    ///
    /// Returns `Ok(true)` if a game found and skipped.
    ///
    /// # Errors
    ///
    /// * I/O error from the underlying reader.
    /// * Irrecoverable parser errors.
    pub fn skip_game(&mut self) -> io::Result<bool> {
        let has_more = self.has_more()?;
        self.skip_tags()?;
        self.skip_movetext()?;
        Ok(has_more)
    }

    /// Iterate over all games, yielding the visitor outputs.
    #[must_use = "iterator is lazy"]
    pub fn read_games<'a, V: Visitor>(&'a mut self, visitor: &'a mut V) -> ReadGames<'a, R, V> {
        ReadGames {
            reader: self,
            visitor,
        }
    }

    /// Visit all games, ignoring the visitor outputs.
    ///
    /// # Errors
    ///
    /// * I/O error from the underlying reader.
    /// * Irrecoverable parser errors.
    pub fn visit_all_games<V: Visitor>(&mut self, visitor: &mut V) -> io::Result<()> {
        while self.read_game(visitor)?.is_some() {}
        Ok(())
    }

    /// The currently buffered bytes.
    pub fn buffer(&self) -> &[u8] {
        self.buffer.data()
    }

    /// Discard the remaining bytes in the buffer ([`Reader::buffer()`]) and
    /// get the underlying reader.
    pub fn into_inner(self) -> R {
        self.reader
    }
}

/// Iterator returned by [`Reader::read_games()`].
#[derive(Debug)]
#[must_use]
pub struct ReadGames<'a, R, V> {
    reader: &'a mut Reader<R>,
    visitor: &'a mut V,
}

impl<R: Read, V: Visitor> Iterator for ReadGames<'_, R, V> {
    type Item = Result<V::Output, io::Error>;

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
            let buffered = self.buffer.len() as i64;
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
        let buffered = self.buffer.len() as i64;
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
        let buffered = self.buffer.len() as u64;
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
        type Movetext = ();
        type Tags = ();
        type Output = ();

        fn begin_tags(&mut self) -> ControlFlow<(), Self::Tags> {
            ControlFlow::Continue(())
        }

        fn begin_movetext(&mut self, _tags: Self::Tags) -> ControlFlow<(), Self::Movetext> {
            ControlFlow::Continue(())
        }

        fn end_game(&mut self, _movetext: Self::Movetext) {
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
        struct NagCollector;

        impl Visitor for NagCollector {
            type Tags = ();
            type Movetext = Vec<Nag>;
            type Output = Vec<Nag>;

            fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
                ControlFlow::Continue(())
            }

            fn begin_movetext(
                &mut self,
                _tags: Self::Tags,
            ) -> ControlFlow<Self::Output, Self::Movetext> {
                ControlFlow::Continue(Vec::new())
            }

            fn nag(
                &mut self,
                movetext: &mut Self::Movetext,
                nag: Nag,
            ) -> ControlFlow<Self::Output> {
                movetext.push(nag);
                ControlFlow::Continue(())
            }

            fn end_game(&mut self, movetext: Self::Movetext) -> Self::Output {
                movetext
            }
        }

        let mut reader = Reader::new(io::Cursor::new(b"1.f3! e5$71 2.g4?? Qh4#!?"));
        let nags = reader.read_game(&mut NagCollector)?;
        assert_eq!(
            nags,
            Some(vec![
                Nag::GOOD_MOVE,
                Nag(71),
                Nag::BLUNDER,
                Nag::SPECULATIVE_MOVE
            ])
        );
        Ok(())
    }

    #[test]
    fn test_null_moves() -> Result<(), io::Error> {
        struct SanCollector;

        impl Visitor for SanCollector {
            type Tags = ();
            type Movetext = Vec<San>;
            type Output = Vec<San>;

            fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
                ControlFlow::Continue(())
            }

            fn begin_movetext(
                &mut self,
                _tags: Self::Tags,
            ) -> ControlFlow<Self::Output, Self::Movetext> {
                ControlFlow::Continue(Vec::new())
            }

            fn san(
                &mut self,
                movetext: &mut Self::Movetext,
                san_plus: SanPlus,
            ) -> ControlFlow<Self::Output> {
                movetext.push(san_plus.san);
                ControlFlow::Continue(())
            }

            fn end_game(&mut self, movetext: Self::Movetext) -> Self::Output {
                movetext
            }
        }

        let mut reader = Reader::new(io::Cursor::new(b"1. e4 -- 2. Nf3 -- 3. -- e5"));
        let sans = reader.read_game(&mut SanCollector)?.expect("game found");
        assert_eq!(sans.len(), 6);
        assert_ne!(sans[0], San::Null);
        assert_eq!(sans[1], San::Null);
        assert_ne!(sans[2], San::Null);
        assert_eq!(sans[3], San::Null);
        assert_eq!(sans[4], San::Null);
        assert_ne!(sans[5], San::Null);
        Ok(())
    }
}
