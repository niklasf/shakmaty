use crate::{Nag, Outcome, RawComment, RawTag, SanPlus};
use std::ops::ControlFlow;

/// Tell the reader to skip over a variation.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[must_use]
pub struct Skip(pub bool);

/// Consumes games from a reader.
///
/// ![Flow](https://github.com/niklasf/shakmaty/blob/master/pgn-reader/docs/visitor.png?raw=true)
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
    /// May skip over the following movetext directly to
    /// [`Visitor::end_game()`].
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

    /// Called for each `(`. May skip over the following variation directly
    /// to [`Visitor::end_variation()`] (or to
    /// [`Visitor::end_game()`] if no matching `)`
    /// follows before the end of the game.
    fn begin_variation(
        &mut self,
        movetext: &mut Self::Movetext,
    ) -> ControlFlow<Self::Output, Skip> {
        let _movetext = movetext;
        ControlFlow::Continue(Skip(false))
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
