use std::{convert::Infallible, ops::ControlFlow};

use shakmaty::{Outcome, san::SanPlus};

use crate::types::{Nag, RawComment, RawTag, Skip};

/// Consumes games from a reader.
///
/// ![Flow](https://github.com/niklasf/rust-pgn-reader/blob/master/docs/visitor.png?raw=true)
pub trait Visitor {
    /// Value produced by the visitor after reading a game.
    type Output;
    /// Break type produced by each method, except [`end_game`](Visitor::end_game).
    type Break;

    /// Called at the start of the game; directly before reading game tags.
    fn begin_tags(&mut self) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }
    /// Called when parsing a game tag pair like `[White "Deep Blue"]`.
    fn tag(&mut self, _name: &[u8], _value: RawTag<'_>) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }
    /// Called after reading the tags of a game; before reading the movetext.
    /// May skip over the following movetext directly to
    /// [`end_game()`](trait.Visitor.html#tymethod.end_game).
    ///
    /// Doesn't skip by default.
    fn begin_movetext(&mut self) -> ControlFlow<Self::Break, Skip> {
        ControlFlow::Continue(Skip(false))
    }

    /// Called for each move, like `Nf3+`.
    fn san(&mut self, _san_plus: SanPlus) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }
    /// Called for each numeric annotation glyph like `!?` or `$7`.
    fn nag(&mut self, _nag: Nag) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }
    /// Called for each `{ comment }`.
    fn comment(&mut self, _comment: RawComment<'_>) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }
    /// Called for each `(`. May skip over the following variation directly
    /// to [`end_variation()`](trait.Visitor.html#method.end_variation) (or to
    /// [`end_game()`](trait.Visitor.html#tymethod.end_game) if no matching `)`
    /// follows before the end of the game.
    ///
    /// Doesn't skip by default.
    fn begin_variation(&mut self) -> ControlFlow<Self::Break, Skip> {
        ControlFlow::Continue(Skip(false))
    }
    /// Called for each `)`. It is *not* guaranteed that there was a
    /// matching `(`.
    fn end_variation(&mut self) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }
    /// Called for each game termination, like `*` or `1-0`.
    fn outcome(&mut self, _outcome: Outcome) -> ControlFlow<Self::Break> {
        ControlFlow::Continue(())
    }

    /// Called after parsing a game. Can produce a custom result.
    fn end_game(&mut self) -> Self::Output;
}

pub(crate) struct SkipVisitor;

impl Visitor for SkipVisitor {
    type Output = ();
    type Break = Infallible;

    fn begin_movetext(&mut self) -> ControlFlow<Self::Break, Skip> {
        ControlFlow::Continue(Skip(true))
    }
    fn begin_variation(&mut self) -> ControlFlow<Self::Break, Skip> {
        ControlFlow::Continue(Skip(true))
    }
    fn end_game(&mut self) -> Self::Output {}
}
