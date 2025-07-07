use std::{io, io::Write, num::NonZeroUsize, ops::ControlFlow};

use shakmaty::{Outcome, san::SanPlus};

use crate::{Nag, RawComment, RawTag, Skip, Visitor};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Config {
    /// Whether to skip variations.
    /// This only dictates what the [`Writer`] returns in [`Visitor::begin_variation`].
    ///
    /// Defaults to `false`.
    pub skip_variations: bool,
    /// Defaults to `1`.
    pub starting_move_number: NonZeroUsize,
    /// Whether to always include move numbers,
    /// even if the parity does not match the starting move number parity.
    ///
    /// For example, if the starting move number is 1 (odd), a move number is even, and
    /// this is set to `true`, that move number will be included.
    ///
    /// Defaults to `false`.
    pub always_include_move_number: bool,
    /// Whether to include a space after a move number (`"1. e4"` if `true`, `"1.e4"` otherwise).
    ///
    /// Defaults to `true`.
    pub space_after_move_number: bool,
    /// Whether to include spaces around variation parentheses (`"( "` and `") "` if `true`, `"("` and `")"` otherwise).
    ///
    /// Defaults to `true`.
    pub space_around_variation: bool,
    /// Whether to include spaces around comment braces (`"{ "` and `" } "` if `true`, `"{"` and `"}"` otherwise).
    ///
    /// Defaults to `true`.
    pub space_around_comments: bool,
}

impl Config {
    /// A space optimized [`Config`]. Not recommended as it won't make much of a difference
    /// and might break certain parsers.
    pub const COMPACT: Self = Self {
        skip_variations: false,
        starting_move_number: NonZeroUsize::new(1).unwrap(),
        always_include_move_number: false,
        space_after_move_number: false,
        space_around_variation: false,
        space_around_comments: false,
    };
}

impl Default for Config {
    /// Optimized for compatibility.
    fn default() -> Self {
        Self {
            skip_variations: false,
            starting_move_number: NonZeroUsize::MIN,
            always_include_move_number: false,
            space_after_move_number: true,
            space_around_variation: true,
            space_around_comments: true,
        }
    }
}

#[derive(Debug)]
pub struct Writer<W> {
    pub writer: W,
    config: Config,
    scheduled_config: Config,
    /// Resets to `0` every [`Visitor::begin_tags`], `total_bytes_written` doesn't.
    bytes_written: usize,
    total_bytes_written: usize,
    buffer: Vec<u8>,
    /// Buffer for making an ASCII usize.
    usize_buffer: [u8; 20],
    /// The move number of the last move.
    move_number: usize,
    /// A stack of move numbers in variations parent to the current one.
    ///
    /// If `self.move_number` is `7` and `self.move_numbers_below` is `[4, 9]`, this means that:
    /// - There are 3 variations.
    /// - The current variation is at move `7`.
    /// - The parent variation of the current variation is at move `9`.
    /// - The root variation is at move `4`.
    move_numbers_below: Vec<usize>,
}

impl<W> Writer<W> {
    pub fn new(writer: W) -> Self {
        let config = Config::default();

        Self {
            writer,
            config,
            scheduled_config: config,
            bytes_written: 0,
            total_bytes_written: 0,
            // the longest thing this will be used for is a tag line, which is likely not going
            // to exceed this
            buffer: Vec::with_capacity(200),
            usize_buffer: [0; 20],
            move_number: config.starting_move_number.get(),
            move_numbers_below: Vec::new(),
        }
    }

    /// The config currently in use.
    pub fn config(&self) -> &Config {
        &self.config
    }

    /// The config to be used next time [`Visitor::begin_tags`] is called; see also [`Self::config`].
    ///
    /// The reason it's like this is to prevent changing the config while visiting a game, which
    /// could damage the output.
    pub fn scheduled_config(&self) -> &Config {
        &self.scheduled_config
    }

    /// See [`Self::scheduled_config`].
    pub fn scheduled_config_mut(&mut self) -> &mut Config {
        &mut self.scheduled_config
    }

    fn increment_bytes_written(&mut self, bytes_written: usize) {
        self.bytes_written += bytes_written;
        self.total_bytes_written += bytes_written;
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TagWriter(());

impl TagWriter {
    /// Writes a tag to the specifier writer and updates the state.
    ///
    /// Includes a newline (`\n`) character.
    fn write_tag(
        &mut self,
        name: &[u8],
        value: RawTag<'_>,
        writer: &mut Writer<impl Write>,
    ) -> io::Result<()> {
        writer.buffer.clear();

        // maybe use a fixed size buffer but I'm not sure it would be much faster
        writer.buffer.push(b'[');
        writer.buffer.extend(name);
        writer.buffer.extend(b" \"");
        writer.buffer.extend(value.encode().as_ref());
        writer.buffer.extend(b"\"]\n");

        writer
            .writer
            .write(&writer.buffer)
            .map(|n| writer.increment_bytes_written(n))
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MovetextWriter(());

impl MovetextWriter {
    /// Writes a SAN to the specified writer and updates the state.
    fn write_san_plus(
        &mut self,
        san_plus: SanPlus,
        writer: &mut Writer<impl Write>,
    ) -> io::Result<()> {
        writer.buffer.clear();

        if writer.config.always_include_move_number
            || (writer.move_number % 2 == writer.config.starting_move_number.get() % 2)
        {
            let mut pos = 20;
            let mut n = writer.move_number;

            while n > 0 {
                pos -= 1;
                writer.usize_buffer[pos] = b'0' + (n % 10) as u8;
                n /= 10;
            }

            writer.buffer.extend(&writer.usize_buffer[pos..]);
            writer
                .buffer
                .extend(if writer.config.space_after_move_number {
                    b". ".as_slice()
                } else {
                    b"."
                });
        }

        san_plus.append_ascii_to(&mut writer.buffer);
        writer.buffer.push(b' ');

        writer
            .writer
            .write(&writer.buffer)
            .map(|n| writer.increment_bytes_written(n))?;

        writer.move_number += 1;

        Ok(())
    }

    /// Writes a NAG and updates the state.
    fn write_nag(&mut self, nag: Nag, writer: &mut Writer<impl Write>) -> io::Result<()> {
        writer.buffer.clear();

        nag.append_ascii_to(&mut writer.buffer);
        writer.buffer.push(b' ');

        writer
            .writer
            .write(&writer.buffer)
            .map(|n| writer.increment_bytes_written(n))
    }

    /// Writes a comment and updates the state.
    fn write_comment(
        &mut self,
        comment: RawComment,
        writer: &mut Writer<impl Write>,
    ) -> io::Result<()> {
        writer.buffer.clear();

        writer
            .buffer
            .extend(if writer.config.space_around_comments {
                b"{ ".as_slice()
            } else {
                b"{"
            });
        writer.buffer.extend(comment.0);
        writer
            .buffer
            .extend(if writer.config.space_around_comments {
                b" } ".as_slice()
            } else {
                b"}"
            });

        writer
            .writer
            .write(&writer.buffer)
            .map(|n| writer.increment_bytes_written(n))
    }

    /// Writes an outcome and updates the state.
    fn write_outcome(
        &mut self,
        outcome: Outcome,
        writer: &mut Writer<impl Write>,
    ) -> io::Result<()> {
        writer
            .writer
            .write(outcome.as_str().as_bytes())
            .map(|n| writer.increment_bytes_written(n))
    }

    /// Writes an opening parenthesis (`(`) and updates the state.
    fn write_begin_variation(&mut self, writer: &mut Writer<impl Write>) -> io::Result<()> {
        writer
            .writer
            .write(if writer.config.space_around_variation {
                b"( ".as_slice()
            } else {
                b"("
            })
            .map(|n| writer.increment_bytes_written(n))?;

        writer.move_number = writer
            .config
            .starting_move_number
            .get()
            .max(writer.move_number.saturating_sub(1));
        writer.move_numbers_below.push(writer.move_number);

        Ok(())
    }

    /// Writes a closed parenthesis (`)`) if there was a previous open parenthesis (`(`)
    /// and updates the state.
    ///
    /// Returns `true` if there was a previous open parenthesis (`(`).
    fn write_end_variation(&mut self, writer: &mut Writer<impl Write>) -> io::Result<bool> {
        if writer.move_numbers_below.is_empty() {
            return Ok(false);
        }

        writer
            .writer
            .write(if writer.config.space_around_variation {
                b") ".as_slice()
            } else {
                b")"
            })
            .map(|n| writer.increment_bytes_written(n))?;

        if let Some(move_number) = writer.move_numbers_below.pop() {
            writer.move_number = move_number;
        }

        Ok(true)
    }
}

impl<W> Visitor for Writer<W>
where
    W: Write,
{
    type Tags = TagWriter;
    type Movetext = MovetextWriter;
    /// How many bytes were written.
    ///
    /// # Errors
    ///
    /// The only errors are from [`W::write`].
    type Output = io::Result<usize>;

    /// Guaranteed to continue.
    fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
        self.config = self.scheduled_config;
        self.bytes_written = 0;
        self.move_number = self.config.starting_move_number.get();
        self.move_numbers_below.clear();

        ControlFlow::Continue(TagWriter(()))
    }

    fn tag(
        &mut self,
        tags: &mut Self::Tags,
        name: &[u8],
        value: RawTag<'_>,
    ) -> ControlFlow<Self::Output> {
        match tags.write_tag(name, value, self) {
            Ok(()) => ControlFlow::Continue(()),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    /// Writes a newline (`\n`) if no bytes were written; that is, if no tags were written.
    fn begin_movetext(&mut self, _: Self::Tags) -> ControlFlow<Self::Output, Self::Movetext> {
        if self.bytes_written == 0 {
            return ControlFlow::Continue(MovetextWriter(()));
        }

        match self.writer.write(b"\n") {
            Ok(bytes_written) => {
                self.increment_bytes_written(bytes_written);

                ControlFlow::Continue(MovetextWriter(()))
            }
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    fn san(
        &mut self,
        movetext: &mut Self::Movetext,
        san_plus: SanPlus,
    ) -> ControlFlow<Self::Output> {
        match movetext.write_san_plus(san_plus, self) {
            Ok(()) => ControlFlow::Continue(()),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    fn nag(&mut self, movetext: &mut Self::Movetext, nag: Nag) -> ControlFlow<Self::Output> {
        match movetext.write_nag(nag, self) {
            Ok(()) => ControlFlow::Continue(()),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    fn comment(
        &mut self,
        movetext: &mut Self::Movetext,
        comment: RawComment,
    ) -> ControlFlow<Self::Output> {
        match movetext.write_comment(comment, self) {
            Ok(()) => ControlFlow::Continue(()),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    fn outcome(
        &mut self,
        movetext: &mut Self::Movetext,
        outcome: Outcome,
    ) -> ControlFlow<Self::Output> {
        match movetext.write_outcome(outcome, self) {
            Ok(()) => ControlFlow::Continue(()),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    fn begin_variation(
        &mut self,
        movetext: &mut Self::Movetext,
    ) -> ControlFlow<Self::Output, Skip> {
        if self.config.skip_variations {
            return ControlFlow::Continue(Skip(true));
        }

        match movetext.write_begin_variation(self) {
            Ok(()) => ControlFlow::Continue(Skip(false)),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    fn end_variation(&mut self, movetext: &mut Self::Movetext) -> ControlFlow<Self::Output> {
        match movetext.write_end_variation(self) {
            Ok(_) => ControlFlow::Continue(()),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    /// Closes all variations and writes two newlines (`\n`).
    fn end_game(&mut self, mut movetext: Self::Movetext) -> Self::Output {
        let mut unclosed_parenthesis = movetext.write_end_variation(self)?;

        while unclosed_parenthesis {
            unclosed_parenthesis = movetext.write_end_variation(self)?;
        }

        self.writer
            .write(b"\n\n")
            .map(|n| self.increment_bytes_written(n))?;

        Ok(self.bytes_written)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tag() {
        let mut writer = Writer::new(Vec::new());
        let mut tags = TagWriter(());

        tags.write_tag(b"Event", RawTag(b"Unit testing \"\\ \""), &mut writer)
            .unwrap();
        assert_eq!(writer.writer, b"[Event \"Unit testing \\\"\\ \\\"\"]\n");

        writer.writer.clear();

        tags.write_tag(b"", RawTag(b""), &mut writer).unwrap();
        assert_eq!(writer.writer, b"[ \"\"]\n");
    }

    #[test]
    fn san_plus() {
        // this API is not available to users
        let mut writer = Writer::new(Vec::new());
        let mut movetext = MovetextWriter(());

        movetext
            .write_san_plus(SanPlus::from_ascii(b"e4").unwrap(), &mut writer)
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ");

        movetext
            .write_san_plus(SanPlus::from_ascii(b"Nd2xf3#").unwrap(), &mut writer)
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 Nd2xf3# ");

        writer.writer.clear();

        let mut writer = Writer::new(Vec::new());
        writer.config.always_include_move_number = true;
        let mut movetext = MovetextWriter(());

        movetext
            .write_san_plus(SanPlus::from_ascii(b"e4").unwrap(), &mut writer)
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ");

        movetext
            .write_san_plus(SanPlus::from_ascii(b"Nd2xf3#").unwrap(), &mut writer)
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 2. Nd2xf3# ");
    }

    #[test]
    fn variation() {
        let mut writer = Writer::new(Vec::new());
        writer.scheduled_config_mut().always_include_move_number = true;
        let tags = writer.begin_tags().continue_value().unwrap();
        let mut movetext = writer.begin_movetext(tags).continue_value().unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"e4").unwrap())
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ");

        let _ = writer
            .begin_variation(&mut movetext)
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ( ");

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"d4").unwrap())
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ( 1. d4 ");

        let _ = writer
            .begin_variation(&mut movetext)
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ( 1. d4 ( ");

        let _ = writer
            .begin_variation(&mut movetext)
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ( 1. d4 ( ( ");

        let _ = writer
            .begin_variation(&mut movetext)
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ( 1. d4 ( ( ( ");

        writer
            .end_variation(&mut movetext)
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ( 1. d4 ( ( ( ) ");

        writer.end_game(movetext).unwrap();
        assert_eq!(writer.writer, b"1. e4 ( 1. d4 ( ( ( ) ) ) ) \n\n");
    }
}
