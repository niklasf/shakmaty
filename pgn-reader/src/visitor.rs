use std::convert::Infallible;

use shakmaty::{Outcome, san::SanPlus};

use crate::types::{Nag, RawComment, RawTag, Skip};

/// Consumes games from a reader.
///
/// ![Flow](https://github.com/niklasf/rust-pgn-reader/blob/master/docs/visitor.png?raw=true)
pub trait Visitor {
    /// Value produced by the visitor after reading a game.
    type Output;
    /// Error produced by each method, except [`end_game`](Visitor::end_game).
    type Error;

    /// Called at the start of the game; directly before reading game tags.
    fn begin_tags(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
    /// Called when parsing a game tag pair like `[White "Deep Blue"]`.
    fn tag(&mut self, _name: &[u8], _value: RawTag<'_>) -> Result<(), Self::Error> {
        Ok(())
    }
    /// Called after reading the tags of a game; before reading the movetext.
    /// May skip over the following movetext directly to
    /// [`end_game()`](trait.Visitor.html#tymethod.end_game).
    ///
    /// Doesn't skip by default.
    fn begin_movetext(&mut self) -> Result<Skip, Self::Error> {
        Ok(Skip(false))
    }

    /// Called for each move, like `Nf3+`.
    fn san(&mut self, _san_plus: SanPlus) -> Result<(), Self::Error> {
        Ok(())
    }
    /// Called for each numeric annotation glyph like `!?` or `$7`.
    fn nag(&mut self, _nag: Nag) -> Result<(), Self::Error> {
        Ok(())
    }
    /// Called for each `{ comment }`.
    fn comment(&mut self, _comment: RawComment<'_>) -> Result<(), Self::Error> {
        Ok(())
    }
    /// Called for each `(`. May skip over the following variation directly
    /// to [`end_variation()`](trait.Visitor.html#method.end_variation) (or to
    /// [`end_game()`](trait.Visitor.html#tymethod.end_game) if no matching `)`
    /// follows before the end of the game.
    ///
    /// Doesn't skip by default.
    fn begin_variation(&mut self) -> Result<Skip, Self::Error> {
        Ok(Skip(false))
    }
    /// Called for each `)`. It is *not* guaranteed that there was a
    /// matching `(`.
    fn end_variation(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
    /// Called for each game termination, like `*` or `1-0`.
    fn outcome(&mut self, _outcome: Outcome) -> Result<(), Self::Error> {
        Ok(())
    }

    /// Called after parsing a game. Can produce a custom result.
    fn end_game(&mut self) -> Self::Output;
}

pub(crate) struct SkipVisitor;

impl Visitor for SkipVisitor {
    type Output = ();
    type Error = Infallible;

    fn begin_movetext(&mut self) -> Result<Skip, Self::Error> {
        Ok(Skip(true))
    }
    fn begin_variation(&mut self) -> Result<Skip, Self::Error> {
        Ok(Skip(true))
    }
    fn end_game(&mut self) -> Self::Output {}
}
