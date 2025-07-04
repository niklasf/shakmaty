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
    /// Output that is produced from visiting a game.
    type Output;

    /// Called at the start of the game, directly before reading game tags.
    fn begin_tags(&mut self) -> ControlFlow<Self::Output> {
        ControlFlow::Continue(())
    }

    /// Called when parsing a game tag pair, like `[White "Deep Blue"]`.
    fn tag(&mut self, name: &[u8], value: RawTag<'_>) -> ControlFlow<Self::Output> {
        let _name = name;
        let _value = value;
        ControlFlow::Continue(())
    }

    /// Called after reading the tags of a game, before reading the movetext.
    /// May skip over the following movetext directly to
    /// [`Visitor::end_game()`].
    fn begin_movetext(&mut self) -> ControlFlow<Self::Output> {
        ControlFlow::Continue(())
    }

    /// Called for each move, like `Nf3+`.
    fn san(&mut self, san_plus: SanPlus) -> ControlFlow<Self::Output> {
        let _san_plus = san_plus;
        ControlFlow::Continue(())
    }

    /// Called for each numeric annotation glyph, like `!?` or `$7`.
    fn nag(&mut self, nag: Nag) -> ControlFlow<Self::Output> {
        let _nag = nag;
        ControlFlow::Continue(())
    }

    /// Called for each `{ comment }`.
    fn comment(&mut self, comment: RawComment<'_>) -> ControlFlow<Self::Output> {
        let _comment = comment;
        ControlFlow::Continue(())
    }

    /// Called for each `(`. May skip over the following variation directly
    /// to [`Visitor::end_variation()`] (or to
    /// [`Visitor::end_game()`] if no matching `)`
    /// follows before the end of the game.
    fn begin_variation(&mut self) -> ControlFlow<Self::Output, Skip> {
        ControlFlow::Continue(Skip(false))
    }

    /// Called for each `)`. It is *not* guaranteed that there was a
    /// matching `(`.
    fn end_variation(&mut self) -> ControlFlow<Self::Output> {
        ControlFlow::Continue(())
    }

    /// Called for each game termination, like `*` or `1-0`.
    fn outcome(&mut self, outcome: Outcome) -> ControlFlow<Self::Output> {
        let _outcome = outcome;
        ControlFlow::Continue(())
    }

    /// Called after parsing a game.
    fn end_game(&mut self) -> Self::Output;
}
