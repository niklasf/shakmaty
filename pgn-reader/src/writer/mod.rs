mod config;
mod error;

use std::{io, io::Write, ops::ControlFlow};

use bitflags::bitflags;
pub use config::Config;
pub use error::Error;
use shakmaty::{Outcome, san::SanPlus};

use crate::{Nag, RawComment, RawTag, Skip, Visitor};

bitflags! {
    /// Used to check which movetext tokens ([`Visitor`] methods) are allowed.
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub struct MovetextToken: u8 {
        const SAN = 1 << 0;
        const NAG = 1 << 1;
        const COMMENT = 1 << 2;
        const BEGIN_VARIATION = 1 << 3;
        const END_VARIATION = 1 << 4;
        const OUTCOME = 1 << 5;
        /// Always allowed.
        const END_GAME = 1 << 6;
    }
}

/// Write a PGN using the [`Visitor`] implementation.
#[derive(Debug)]
pub struct Writer<W, Skip> {
    pub writer: W,
    config: Config<Skip>,
    scheduled_config: Config<Skip>,
    /// Resets to `0` every [`Visitor::begin_tags`], `total_bytes_written` doesn't.
    bytes_written: usize,
    total_bytes_written: usize,
    buffer: Vec<u8>,
    /// Buffer for making an ASCII usize.
    usize_buffer: [u8; 20],
    /// The move *index* of the last move.
    /// Starts at `starting_move_number - 1`.
    move_index: usize,
    /// Move indices of each parent variation.
    ///
    /// The first one is the root variation, last one is the parent variation of the current variation.
    parent_variation_move_indices: Vec<usize>,
    /// What token (visitor method) is allowed next.
    allowed_tokens: MovetextToken,
    /// What move index is the last NAG attached to.
    /// Used for checking for two NAGs in a row, even if they are separated by other tokens.
    last_nag_move_index: Option<usize>,
}

impl<W, Skip> Writer<W, Skip>
where
    Skip: Copy,
{
    pub fn new(writer: W, config: Config<Skip>) -> Self {
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
            parent_variation_move_indices: Vec::new(),
            allowed_tokens: MovetextToken::SAN | MovetextToken::COMMENT | MovetextToken::END_GAME,
            last_nag_move_index: None,
        }
    }
}

impl<W, Skip> Writer<W, Skip> {
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
    pub fn config(&self) -> &Config<Skip> {
        &self.config
    }

    /// The config to be used next time [`Visitor::begin_tags`] is called; see also [`Self::config`].
    ///
    /// The reason it's like this is to prevent changing the config while visiting a game, which
    /// could damage the output.
    pub fn scheduled_config(&self) -> &Config<Skip> {
        &self.scheduled_config
    }

    /// See [`Self::scheduled_config`].
    pub fn scheduled_config_mut(&mut self) -> &mut Config<Skip> {
        &mut self.scheduled_config
    }

    fn increment_bytes_written(&mut self, bytes_written: usize) {
        self.bytes_written += bytes_written;
        self.total_bytes_written += bytes_written;
    }
}

impl<W, Skip> Writer<W, Skip>
where
    W: Write,
{
    /// Writes `self.buffer` to `self.writer` and updates bytes written.
    fn write_buffer(&mut self) -> io::Result<()> {
        self.writer
            .write(&self.buffer)
            .map(|n| self.increment_bytes_written(n))
    }
}

/// You can only get this with [`Writer::begin_tags`] to prevent misuse.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TagWriter(());

/// You can only get this with [`Writer::begin_movetext`] to prevent misuse.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MovetextWriter(());

impl<W, Skip> Visitor for Writer<W, Skip>
where
    W: Write,
    Skip: Copy + Fn() -> bool,
{
    type Tags = TagWriter;
    type Movetext = MovetextWriter;
    /// How many bytes were written.
    ///
    /// Equivalent to calling [`Writer::bytes_written`] right after [`Writer::end_game`].
    type Output = Result<usize, Error>;

    /// Doesn't write anything, only resets the state. Guaranteed to continue.
    fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
        self.config = self.scheduled_config;
        self.bytes_written = 0;
        self.move_index = self.config.starting_move_number.get() - 1;
        self.parent_variation_move_indices.clear();
        self.allowed_tokens = MovetextToken::SAN | MovetextToken::COMMENT | MovetextToken::END_GAME;

        ControlFlow::Continue(TagWriter(()))
    }

    /// Writes a tag line like `[White "Garry Kasparov"]`.
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
            Err(e) => ControlFlow::Break(Err(Error::Io(e))),
        }
    }

    /// Writes a newline (`\n`) if any tags were written.
    ///
    /// # Methods allowed afterwards
    ///
    /// - [`Self::san`]
    /// - [`Self::comment`]
    /// - [`Self::end_game`]
    fn begin_movetext(&mut self, _: Self::Tags) -> ControlFlow<Self::Output, Self::Movetext> {
        if self.bytes_written == 0 {
            return ControlFlow::Continue(MovetextWriter(()));
        }

        match self.writer.write(b"\n") {
            Ok(bytes_written) => {
                self.increment_bytes_written(bytes_written);

                ControlFlow::Continue(MovetextWriter(()))
            }
            Err(e) => ControlFlow::Break(Err(Error::Io(e))),
        }
    }

    /// Writes a [`SanPlus`].
    ///
    /// # Methods allowed afterwards
    ///
    /// - [`Self::san`]
    /// - [`Self::nag`]
    /// - [`Self::comment`]
    /// - [`Self::begin_variation`]
    /// - [`Self::end_variation`]
    /// - [`Self::outcome`]
    /// - [`Self::end_game`]
    fn san(&mut self, _: &mut Self::Movetext, san_plus: SanPlus) -> ControlFlow<Self::Output> {
        if !self.allowed_tokens.contains(MovetextToken::SAN) {
            return ControlFlow::Break(Err(Error::InvalidToken {
                token: MovetextToken::SAN,
                allowed: self.allowed_tokens,
            }));
        }

        self.buffer.clear();

        if self.move_index % 2 == (self.config.starting_move_number.get() - 1) % 2 {
            let mut pos = 20;
            let mut n = 1 + (self.move_index / 2);

            while n > 0 {
                pos -= 1;
                self.usize_buffer[pos] = b'0' + (n % 10) as u8;
                n /= 10;
            }

            self.buffer.extend(&self.usize_buffer[pos..]);
            self.buffer.extend(b". ");
        }

        san_plus.append_ascii_to(&mut self.buffer);
        self.buffer.push(b' ');

        match self.write_buffer() {
            Ok(()) => {
                self.move_index += 1;
                self.allowed_tokens = MovetextToken::all();

                ControlFlow::Continue(())
            }
            Err(e) => ControlFlow::Break(Err(Error::Io(e))),
        }
    }

    /// Writes a [`Nag`].
    ///
    /// # Errors
    ///
    /// Apart from the usual (see [`Error`]), also errors if the last [`Self::nag`] was called on the same
    /// move as this call.
    ///
    /// # Methods allowed afterwards
    ///
    /// - [`Self::san`]
    /// - [`Self::comment`]
    /// - [`Self::begin_variation`]
    /// - [`Self::end_variation`]
    /// - [`Self::outcome`]
    /// - [`Self::end_game`]
    fn nag(&mut self, _: &mut Self::Movetext, nag: Nag) -> ControlFlow<Self::Output> {
        if self
            .last_nag_move_index
            .is_some_and(|nag_move_index| nag_move_index == self.move_index)
        {
            self.allowed_tokens.remove(MovetextToken::NAG);

            return ControlFlow::Break(Err(Error::InvalidToken {
                token: MovetextToken::NAG,
                allowed: self.allowed_tokens,
            }));
        }

        if !self.allowed_tokens.contains(MovetextToken::NAG) {
            return ControlFlow::Break(Err(Error::InvalidToken {
                token: MovetextToken::NAG,
                allowed: self.allowed_tokens,
            }));
        }

        self.buffer.clear();

        nag.append_ascii_to(&mut self.buffer);
        self.buffer.push(b' ');

        match self.write_buffer() {
            Ok(()) => {
                self.allowed_tokens = MovetextToken::all();
                self.allowed_tokens.remove(MovetextToken::NAG);
                self.last_nag_move_index = Some(self.move_index);

                ControlFlow::Continue(())
            }
            Err(e) => ControlFlow::Break(Err(Error::Io(e))),
        }
    }

    /// Writes a [`RawComment`] surrounded by braces.
    ///
    /// # Methods allowed afterwards
    ///
    /// - [`Self::san`]
    /// - [`Self::nag`]
    /// - [`Self::comment`]
    /// - [`Self::begin_variation`]
    /// - [`Self::end_variation`]
    /// - [`Self::outcome`]
    /// - [`Self::end_game`]
    fn comment(
        &mut self,
        _: &mut Self::Movetext,
        comment: RawComment,
    ) -> ControlFlow<Self::Output> {
        if !self.allowed_tokens.contains(MovetextToken::COMMENT) {
            return ControlFlow::Break(Err(Error::InvalidToken {
                token: MovetextToken::COMMENT,
                allowed: self.allowed_tokens,
            }));
        }

        self.buffer.clear();

        self.buffer.extend(b"{ ");
        self.buffer.extend(comment.0);
        self.buffer.extend(b" } ");

        match self.write_buffer() {
            Ok(()) => {
                self.allowed_tokens = MovetextToken::all();

                ControlFlow::Continue(())
            }
            Err(e) => ControlFlow::Break(Err(Error::Io(e))),
        }
    }

    /// Writes an [`Outcome`].
    ///
    /// # Methods allowed afterwards
    ///
    /// - [`Self::end_variation`]
    /// - [`Self::end_game`]
    fn outcome(&mut self, _: &mut Self::Movetext, outcome: Outcome) -> ControlFlow<Self::Output> {
        if !self.allowed_tokens.contains(MovetextToken::OUTCOME) {
            return ControlFlow::Break(Err(Error::InvalidToken {
                token: MovetextToken::OUTCOME,
                allowed: self.allowed_tokens,
            }));
        }

        self.buffer.clear();

        self.buffer.extend(outcome.as_str().as_bytes());
        self.buffer.push(b' ');

        match self.write_buffer() {
            Ok(()) => {
                // per the PGN standard, the termination marker is the last element in the movetext
                // while variations aren't mentioned, it's common to include an outcome in a variation
                self.allowed_tokens = MovetextToken::END_VARIATION | MovetextToken::END_GAME;

                ControlFlow::Continue(())
            }
            Err(e) => ControlFlow::Break(Err(Error::Io(e))),
        }
    }

    /// Writes an open parenthesis (`(`).
    ///
    /// # Methods allowed afterwards
    ///
    /// - [`Self::san`]
    /// - [`Self::comment`]
    /// - [`Self::begin_variation`]
    /// - [`Self::end_variation`]
    /// - [`Self::end_game`]
    fn begin_variation(
        &mut self,
        _: &mut Self::Movetext,
    ) -> ControlFlow<Self::Output, crate::Skip> {
        if !self.allowed_tokens.contains(MovetextToken::BEGIN_VARIATION) {
            return ControlFlow::Break(Err(Error::InvalidToken {
                token: MovetextToken::BEGIN_VARIATION,
                allowed: self.allowed_tokens,
            }));
        }

        if (self.config.skip_variations)() {
            return ControlFlow::Continue(Skip(true));
        }

        match self
            .writer
            .write(b"( ")
            .map(|n| self.increment_bytes_written(n))
        {
            Ok(()) => {
                self.parent_variation_move_indices.push(self.move_index);
                self.move_index = self
                    .config
                    .starting_move_number
                    .get()
                    .max(self.move_index.saturating_sub(1));
                self.allowed_tokens = MovetextToken::all();
                self.allowed_tokens.remove(MovetextToken::NAG);
                self.allowed_tokens.remove(MovetextToken::OUTCOME);

                ControlFlow::Continue(Skip(false))
            }
            Err(e) => ControlFlow::Break(Err(Error::Io(e))),
        }
    }

    /// Writes a closed parenthesis (`)`).
    ///
    /// # Errors
    ///
    /// Apart from the usual (see [`Error`]), also errors if there is no corresponding open variation.
    ///
    /// # Methods allowed afterwards
    ///
    /// - [`Self::san`]
    /// - [`Self::nag`]
    /// - [`Self::comment`]
    /// - [`Self::begin_variation`]
    /// - [`Self::end_variation`]
    /// - [`Self::outcome`]
    /// - [`Self::end_game`]
    fn end_variation(&mut self, _: &mut Self::Movetext) -> ControlFlow<Self::Output> {
        if self.parent_variation_move_indices.is_empty() {
            self.allowed_tokens.remove(MovetextToken::END_VARIATION);
            return ControlFlow::Break(Err(Error::InvalidToken {
                token: MovetextToken::END_VARIATION,
                allowed: self.allowed_tokens,
            }));
        }

        if !self.allowed_tokens.contains(MovetextToken::END_VARIATION) {
            return ControlFlow::Break(Err(Error::InvalidToken {
                token: MovetextToken::END_VARIATION,
                allowed: self.allowed_tokens,
            }));
        }

        match self
            .writer
            .write(b") ")
            .map(|n| self.increment_bytes_written(n))
        {
            Ok(_) => {
                self.allowed_tokens = MovetextToken::all();
                self.move_index = self.parent_variation_move_indices.pop().unwrap();

                ControlFlow::Continue(())
            }
            Err(e) => ControlFlow::Break(Err(Error::Io(e))),
        }
    }

    /// Writes two newlines (`\n`).
    fn end_game(&mut self, _: Self::Movetext) -> Self::Output {
        if !self.allowed_tokens.contains(MovetextToken::END_GAME) {
            return Err(Error::InvalidToken {
                token: MovetextToken::END_GAME,
                allowed: self.allowed_tokens,
            });
        }

        self.writer
            .write(b"\n\n")
            .map(|n| self.increment_bytes_written(n))
            .map_err(Error::Io)?;

        Ok(self.bytes_written)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tag() {
        let mut writer = Writer::new(Vec::new(), Config::default());
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
        let mut writer = Writer::new(Vec::new(), Config::default());
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

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"Ke2#").unwrap())
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 Nd2xf3# 2. Ke2# ");
    }

    #[test]
    fn nag() {
        let mut writer = Writer::new(Vec::new(), Config::default());
        let tags = writer.begin_tags().continue_value().unwrap();
        let mut movetext = writer.begin_movetext(tags).continue_value().unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"e4").unwrap())
            .continue_value()
            .unwrap();

        writer
            .nag(&mut movetext, Nag::BRILLIANT_MOVE)
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
            .comment(
                &mut movetext,
                RawComment(b"hmm yes that variation was very positional and sophisticated"),
            )
            .continue_value()
            .unwrap();

        assert_eq!(
            writer.writer,
            b"1. e4 $3 ( d4 ) { hmm yes that variation was very positional and sophisticated } "
        );

        assert!(matches!(writer
            .nag(&mut movetext, Nag::BLUNDER), // now e4 just seems primitive
            // oh no, we already used a NAG!
            ControlFlow::Break(Err(Error::InvalidToken {
                token,
                allowed
            })) if token == MovetextToken::NAG && allowed == MovetextToken::SAN | MovetextToken::COMMENT | MovetextToken::BEGIN_VARIATION | MovetextToken::END_VARIATION | MovetextToken::OUTCOME | MovetextToken::END_GAME
        ));
    }

    #[test]
    fn longer() {
        let mut writer = Writer::new(Vec::new(), Config::default());
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
        let mut writer = Writer::new(Vec::new(), Config::default());
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
        let mut writer = Writer::new(Vec::new(), Config::default());
        let tags = writer.begin_tags().continue_value().unwrap();
        let mut movetext = writer.begin_movetext(tags).continue_value().unwrap();

        assert!(matches!(
            writer.begin_variation(&mut movetext),
            ControlFlow::Break(Err(Error::InvalidToken {
                token,
                allowed
            })) if token == MovetextToken::BEGIN_VARIATION && allowed == MovetextToken::SAN | MovetextToken::COMMENT | MovetextToken::END_GAME
        ));
    }

    #[test]
    fn writing_after_outcome() {
        let mut writer = Writer::new(Vec::new(), Config::default());
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
            .outcome(&mut movetext, Outcome::Unknown)
            .continue_value()
            .unwrap();

        assert!(matches!(
            writer.san(&mut movetext, SanPlus::from_ascii(b"d5").unwrap()),
            ControlFlow::Break(Err(Error::InvalidToken {
                token,
                allowed
            })) if token == MovetextToken::SAN && allowed == MovetextToken::END_VARIATION | MovetextToken::END_GAME
        ));

        assert!(matches!(
            writer.nag(&mut movetext, Nag::BLUNDER),
            ControlFlow::Break(Err(Error::InvalidToken {
                token,
                allowed
            })) if token == MovetextToken::NAG && allowed == MovetextToken::END_VARIATION | MovetextToken::END_GAME
        ));

        assert!(matches!(
            writer.begin_variation(&mut movetext),
            ControlFlow::Break(Err(Error::InvalidToken {
                token,
                allowed
            })) if token == MovetextToken::BEGIN_VARIATION && allowed == MovetextToken::END_VARIATION | MovetextToken::END_GAME
        ));

        assert!(matches!(
            writer.comment(&mut movetext, RawComment(b"")),
            ControlFlow::Break(Err(Error::InvalidToken {
                token,
                allowed
            })) if token == MovetextToken::COMMENT && allowed == MovetextToken::END_VARIATION | MovetextToken::END_GAME
        ));

        writer
            .end_variation(&mut movetext)
            .continue_value()
            .unwrap();

        writer
            .san(&mut movetext, SanPlus::from_ascii(b"e5").unwrap())
            .continue_value()
            .unwrap();

        writer
            .outcome(&mut movetext, Outcome::Unknown)
            .continue_value()
            .unwrap();

        assert!(matches!(
            writer.san(&mut movetext, SanPlus::from_ascii(b"d5").unwrap()),
            ControlFlow::Break(Err(Error::InvalidToken {
                token,
                allowed
            })) if token == MovetextToken::SAN && allowed == MovetextToken::END_VARIATION | MovetextToken::END_GAME
        ));

        assert!(matches!(
            writer.nag(&mut movetext, Nag::BLUNDER),
            ControlFlow::Break(Err(Error::InvalidToken {
                token,
                allowed
            })) if token == MovetextToken::NAG && allowed == MovetextToken::END_VARIATION | MovetextToken::END_GAME
        ));

        assert!(matches!(
            writer.begin_variation(&mut movetext),
            ControlFlow::Break(Err(Error::InvalidToken {
                token,
                allowed
            })) if token == MovetextToken::BEGIN_VARIATION && allowed == MovetextToken::END_VARIATION | MovetextToken::END_GAME
        ));

        writer.end_game(movetext).unwrap();
    }

    #[test]
    fn deep_variation() {
        let mut writer = Writer::new(Vec::new(), Config::default());
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
        assert_eq!(writer.writer, b"1. e4 ( d4 ");

        let _ = writer
            .begin_variation(&mut movetext)
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ( d4 ( ");

        let _ = writer
            .begin_variation(&mut movetext)
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ( d4 ( ( ");

        let _ = writer
            .begin_variation(&mut movetext)
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ( d4 ( ( ( ");

        writer
            .end_variation(&mut movetext)
            .continue_value()
            .unwrap();
        assert_eq!(writer.writer, b"1. e4 ( d4 ( ( ( ) ");

        writer.end_game(movetext).unwrap();
        assert_eq!(writer.writer, b"1. e4 ( d4 ( ( ( ) \n\n");
    }
}
