use std::ops::ControlFlow;

use crate::{Nag, Outcome, RawComment, RawTag, SanPlus};

/// Tell the reader to skip over a variation.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[must_use]
pub struct Skip(pub bool);

/// Visits games from a [`Reader`](crate::Reader).
///
/// Visitor methods are called in these phases:
///
/// 1. [`Visitor::begin_tags()`]
///    - [`Visitor::tag()`]
/// 2. [`Visitor::begin_movetext()`]
///    - [`Visitor::san()`]
///    - [`Visitor::nag()`]
///    - [`Visitor::partial_comment()`]
///    - [`Visitor::comment()`]
///    - [`Visitor::begin_variation()`] or skip
///    - [`Visitor::end_variation()`]
///    - [`Visitor::outcome()`]
/// 3. [`Visitor::end_game()`]
pub trait Visitor {
    /// Produced by [`Visitor::begin_tags()`].
    type Tags;
    /// Produced by [`Visitor::begin_movetext()`].
    type Movetext;
    /// Produced by [`Visitor::end_game()`] or a short-circuiting
    /// `ControlFlow::Break(_)` returned from any of the other methods.
    type Output;

    /// Called at the start of the game, directly before reading game tags.
    fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags>;

    /// Called when parsing a game tag pair, like `[White "Deep Blue"]`.
    fn tag(
        &mut self,
        tags: &mut Self::Tags,
        name: &[u8],
        value: RawTag<'_>,
    ) -> ControlFlow<Self::Output> {
        let _tags = tags;
        let _name = name;
        let _value = value;
        ControlFlow::Continue(())
    }

    /// Called after reading the tags of a game, before reading the movetext.
    fn begin_movetext(&mut self, tags: Self::Tags) -> ControlFlow<Self::Output, Self::Movetext>;

    /// Called for each move, like `Nf3+`.
    fn san(
        &mut self,
        movetext: &mut Self::Movetext,
        san_plus: SanPlus,
    ) -> ControlFlow<Self::Output> {
        let _movetext = movetext;
        let _san_plus = san_plus;
        ControlFlow::Continue(())
    }

    /// Called for each numeric annotation glyph, like `!?` or `$7`.
    fn nag(&mut self, movetext: &mut Self::Movetext, nag: Nag) -> ControlFlow<Self::Output> {
        let _movetext = movetext;
        let _nag = nag;
        ControlFlow::Continue(())
    }

    /// Called for `{ comment }`s which exceed the size of the reader's movetext
    /// token buffer.
    ///
    /// This method will be called once for every buffer-sized chunk of the
    /// comment, except for the last one in which the closing `}` is finally
    /// encountered, for which [`comment`](Visitor::comment) will be called
    /// instead. This method's default implementation simply forwards its
    /// arguments to `comment`, so if you don't override it, long comments will
    /// look like a series of several short ones.
    ///
    /// The reader will avoid splitting chunks in the middle of a multibyte
    /// UTF-8 sequence, therefore it's guaranteed that if the comment is valid
    /// UTF-8, then so is every chunk.
    ///
    /// Buffer size is configurable via
    /// [`ReaderBuilder::set_supported_comment_length`](crate::reader::ReaderBuilder::set_supported_comment_length)),
    fn partial_comment(
        &mut self,
        movetext: &mut Self::Movetext,
        comment: RawComment<'_>,
    ) -> ControlFlow<Self::Output> {
        self.comment(movetext, comment)
    }

    /// Called for each `{ comment }`.
    fn comment(
        &mut self,
        movetext: &mut Self::Movetext,
        comment: RawComment<'_>,
    ) -> ControlFlow<Self::Output> {
        let _movetext = movetext;
        let _comment = comment;
        ControlFlow::Continue(())
    }

    /// Called for each `(`.
    ///
    /// Defaults to skip over the following variation directly to
    /// [`Visitor::end_variation()`] (or to [`Visitor::end_game()`] if no
    /// matching `)` follows before the end of the game.
    fn begin_variation(
        &mut self,
        movetext: &mut Self::Movetext,
    ) -> ControlFlow<Self::Output, Skip> {
        let _movetext = movetext;
        ControlFlow::Continue(Skip(true))
    }

    /// Called for each `)`. It is *not* guaranteed that there was a
    /// matching `(`.
    fn end_variation(&mut self, movetext: &mut Self::Movetext) -> ControlFlow<Self::Output> {
        let _movetext = movetext;
        ControlFlow::Continue(())
    }

    /// Called for each game termination, like `*` or `1-0`.
    fn outcome(
        &mut self,
        movetext: &mut Self::Movetext,
        outcome: Outcome,
    ) -> ControlFlow<Self::Output> {
        let _movetext = movetext;
        let _outcome = outcome;
        ControlFlow::Continue(())
    }

    /// Called after parsing a game.
    fn end_game(&mut self, movetext: Self::Movetext) -> Self::Output;
}
