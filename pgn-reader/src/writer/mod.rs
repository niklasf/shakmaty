mod config;

use std::{io, io::Write, ops::ControlFlow};

pub use config::Config;
use shakmaty::{Outcome, san::SanPlus};

use crate::{Nag, RawComment, RawTag, Skip, Visitor};

/// Write a PGN using the [`Visitor`] implementation.
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
    /// The move *index* of the last move.
    /// Starts at `starting_move_number - 1`.
    move_index: usize,
    /// A stack of move indices in variations parent to the current one.
    ///
    /// If `self.move_index` is `7` and `self.move_indices_below` is `[4, 9]`, this means that:
    /// - There are 3 variations.
    /// - The current variation is at move `7`.
    /// - The parent variation of the current variation is at move `9`.
    /// - The root variation is at move `4`.
    move_indices_below: Vec<usize>,
    /// Whether at least one SAN was written.
    /// Necessary in order to prevent starting a variation before any SAN is written.
    written_san: bool,
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
            // to exceed 200 bytes
            buffer: Vec::with_capacity(200),
            usize_buffer: [0; 20],
            move_index: config.starting_move_number.get() - 1,
            move_indices_below: Vec::new(),
            written_san: false,
        }
    }

    /// Gets the amount of bytes written this game. 0 if no game is being visited.
    pub fn bytes_written(&self) -> usize {
        self.bytes_written
    }

    /// Gets the total amount of bytes written.
    pub fn total_bytes_written(&self) -> usize {
        self.total_bytes_written
    }

    /// Sets [`Self::total_bytes_written`] to 0.
    pub fn reset_total_bytes_written(&mut self) {
        self.total_bytes_written = 0;
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

impl<W> Writer<W>
where
    W: Write,
{
    /// Writes `self.buffer` to `self.writer` and updates bytes written.
    fn write_buffer(&mut self) -> io::Result<()> {
        self.writer
            .write(&self.buffer)
            .map(|n| self.increment_bytes_written(n))
    }

    /// Writes a closed parenthesis (`)`) if there was a previous open parenthesis (`(`)
    /// and updates the state.
    ///
    /// Returns `true` if there was a previous open parenthesis (`(`).
    fn write_end_variation(&mut self) -> io::Result<bool> {
        if self.move_indices_below.is_empty() {
            return Ok(false);
        }

        self.writer
            .write(if self.config.space_around_variation {
                b") ".as_slice()
            } else {
                b")"
            })
            .map(|n| self.increment_bytes_written(n))?;

        if let Some(move_index) = self.move_indices_below.pop() {
            self.move_index = move_index;
        }

        Ok(true)
    }
}

/// You can only get this with [`Writer::begin_tags`] to prevent misuse.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TagWriter(());

/// You can only get this with [`Writer::begin_movetext`] to prevent misuse.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MovetextWriter(());

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

    /// Doesn't write anything, only resets the state. Guaranteed to continue.
    fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
        self.config = self.scheduled_config;
        self.bytes_written = 0;
        self.move_index = self.config.starting_move_number.get() - 1;
        self.move_indices_below.clear();

        ControlFlow::Continue(TagWriter(()))
    }

    /// Writes a tag like `[White "Garry Kasparov"]`.
    ///
    /// Includes a newline (`\n`) character.
    fn tag(
        &mut self,
        _: &mut Self::Tags,
        name: &[u8],
        value: RawTag<'_>,
    ) -> ControlFlow<Self::Output> {
        self.buffer.clear();

        self.buffer.push(b'[');
        self.buffer.extend(name);
        self.buffer.extend(b" \"");
        self.buffer.extend(value.encode().as_ref());
        self.buffer.extend(b"\"]\n");

        match self.write_buffer() {
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

    /// Writes a [`SanPlus`].
    fn san(&mut self, _: &mut Self::Movetext, san_plus: SanPlus) -> ControlFlow<Self::Output> {
        self.buffer.clear();

        if self.config.always_include_move_number
            || (self.move_index % 2 == (self.config.starting_move_number.get() - 1) % 2)
        {
            let mut pos = 20;
            let mut n = 1 + (self.move_index / 2);

            while n > 0 {
                pos -= 1;
                self.usize_buffer[pos] = b'0' + (n % 10) as u8;
                n /= 10;
            }

            self.buffer.extend(&self.usize_buffer[pos..]);
            self.buffer.extend(if self.config.space_after_move_number {
                b". ".as_slice()
            } else {
                b"."
            });
        }

        san_plus.append_ascii_to(&mut self.buffer);
        self.buffer.push(b' ');

        match self.write_buffer() {
            Ok(()) => {
                self.move_index += 1;
                self.written_san = true;
                ControlFlow::Continue(())
            }
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    /// Writes a [`Nag`].
    fn nag(&mut self, _: &mut Self::Movetext, nag: Nag) -> ControlFlow<Self::Output> {
        self.buffer.clear();

        nag.append_ascii_to(&mut self.buffer);
        self.buffer.push(b' ');

        match self.write_buffer() {
            Ok(()) => ControlFlow::Continue(()),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    /// Writes a [`RawComment`] surrounded by braces.
    fn comment(
        &mut self,
        _: &mut Self::Movetext,
        comment: RawComment,
    ) -> ControlFlow<Self::Output> {
        self.buffer.clear();

        self.buffer.extend(if self.config.space_around_comments {
            b"{ ".as_slice()
        } else {
            b"{"
        });
        self.buffer.extend(comment.0);
        self.buffer.extend(if self.config.space_around_comments {
            b" } ".as_slice()
        } else {
            b"}"
        });

        match self.write_buffer() {
            Ok(()) => ControlFlow::Continue(()),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    // TODO: allow only one last marker per variation
    fn outcome(&mut self, _: &mut Self::Movetext, outcome: Outcome) -> ControlFlow<Self::Output> {
        self.buffer.clear();

        self.buffer.extend(outcome.as_str().as_bytes());
        self.buffer.push(b' ');

        match self.write_buffer() {
            Ok(()) => ControlFlow::Continue(()),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    /// Writes an open parenthesis (`(`).
    /// Errors with [`io::ErrorKind::InvalidData`] if no SAN was previously written.
    /// Variations have to come after SANs.
    fn begin_variation(&mut self, _: &mut Self::Movetext) -> ControlFlow<Self::Output, Skip> {
        if !self.written_san {
            return ControlFlow::Break(Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "no SAN written before opening a variation",
            )));
        }

        if self.config.skip_variations {
            return ControlFlow::Continue(Skip(true));
        }

        match self
            .writer
            .write(if self.config.space_around_variation {
                b"( ".as_slice()
            } else {
                b"("
            })
            .map(|n| self.increment_bytes_written(n))
        {
            Ok(()) => {
                self.move_indices_below.push(self.move_index);
                self.move_index = self
                    .config
                    .starting_move_number
                    .get()
                    .max(self.move_index.saturating_sub(1));

                ControlFlow::Continue(Skip(false))
            }
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    /// Writes a closed parenthesis (`(`) if there was a previous open parenthesis (`(`).
    fn end_variation(&mut self, _: &mut Self::Movetext) -> ControlFlow<Self::Output> {
        match self.write_end_variation() {
            Ok(_) => ControlFlow::Continue(()),
            Err(e) => ControlFlow::Break(Err(e)),
        }
    }

    /// Closes all variations (see [`Self::end_variation`]) and writes two newlines (`\n`).
    fn end_game(&mut self, _: Self::Movetext) -> Self::Output {
        let mut unclosed_parenthesis = self.write_end_variation()?;

        while unclosed_parenthesis {
            unclosed_parenthesis = self.write_end_variation()?;
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
        let mut tags = writer.begin_tags().continue_value().unwrap();

        writer
            .tag(&mut tags, b"Event", RawTag(b"Unit testing \"\\ \""))
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"[Event \"Unit testing \\\"\\ \\\"\"]\n");

        writer.writer.clear();

        writer
            .tag(&mut tags, b"", RawTag(b""))
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"[ \"\"]\n");
    }

    #[test]
    fn san_plus() {
        let mut writer = Writer::new(Vec::new());
        let tags = writer.begin_tags().continue_value().unwrap();
        let mut movetext = writer.begin_movetext(tags).continue_value().unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"e4").unwrap())
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ");

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"Nd2xf3#").unwrap())
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 Nd2xf3# ");

        writer.writer.clear();

        let mut writer = Writer::new(Vec::new());
        writer.config.always_include_move_number = true;
        let mut movetext = MovetextWriter(());

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"e4").unwrap())
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ");

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"Nd2xf3#").unwrap())
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 1. Nd2xf3# ");
    }

    #[test]
    fn longer() {
        let mut writer = Writer::new(Vec::new());
        let tags = writer.begin_tags().continue_value().unwrap();
        let mut movetext = writer.begin_movetext(tags).continue_value().unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"e4").unwrap())
            .continue_value()
            .unwrap();

        let _ = writer
            .begin_variation(&mut movetext)
            .continue_value()
            .unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"d4").unwrap())
            .continue_value()
            .unwrap();

        writer
            .end_variation(&mut movetext)
            .continue_value()
            .unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"e5").unwrap())
            .continue_value()
            .unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"Nf3").unwrap())
            .continue_value()
            .unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"Nc6").unwrap())
            .continue_value()
            .unwrap();

        writer.end_game(movetext).unwrap();
        assert_eq!(
            str::from_utf8(&writer.writer).unwrap(),
            "1. e4 ( d4 ) e5 2. Nf3 Nc6 \n\n"
        );
    }

    #[test]
    fn longer_variation_later() {
        let mut writer = Writer::new(Vec::new());
        let tags = writer.begin_tags().continue_value().unwrap();
        let mut movetext = writer.begin_movetext(tags).continue_value().unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"e4").unwrap())
            .continue_value()
            .unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"e5").unwrap())
            .continue_value()
            .unwrap();

        let _ = writer
            .begin_variation(&mut movetext)
            .continue_value()
            .unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"d5").unwrap())
            .continue_value()
            .unwrap();

        writer
            .end_variation(&mut movetext)
            .continue_value()
            .unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"Nf3").unwrap())
            .continue_value()
            .unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"Nc6").unwrap())
            .continue_value()
            .unwrap();

        writer.end_game(movetext).unwrap();
        assert_eq!(
            str::from_utf8(&writer.writer).unwrap(),
            "1. e4 e5 ( d5 ) 2. Nf3 Nc6 \n\n"
        );
    }

    #[test]
    fn variation_immediately() {
        let mut writer = Writer::new(Vec::new());
        let tags = writer.begin_tags().continue_value().unwrap();
        let mut movetext = writer.begin_movetext(tags).continue_value().unwrap();

        assert!(matches!(
            writer
                .begin_variation(&mut movetext),
            ControlFlow::Break(Err(err)) if err.kind() == io::ErrorKind::InvalidData && err.to_string() == "no SAN written before opening a variation"
        ));
    }

    #[test]
    fn deep_variation() {
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
