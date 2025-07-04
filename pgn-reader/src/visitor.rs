use crate::{Nag, Outcome, RawComment, RawTag, SanPlus};

/// Tell the reader to skip over a game or variation.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[must_use]
pub struct Skip(pub bool);

/// Consumes games from a reader.
///
/// ![Flow](https://github.com/niklasf/shakmaty/blob/master/pgn-reader/docs/visitor.png?raw=true)
pub trait Visitor {
    /// Value produced by the visitor after reading a game.
    type Result;

    /// Called at the start of the game, directly before reading game tags.
    fn begin_tags(&mut self) {}
    /// Called when parsing a game tag pair, like `[White "Deep Blue"]`.
    fn tag(&mut self, name: &[u8], value: RawTag<'_>) {
        let _name = name;
        let _value = value;
    }
    /// Called after reading the tags of a game, before reading the movetext.
    /// May skip over the following movetext directly to
    /// [`Visitor::end_game()`].
    fn begin_movetext(&mut self) -> Skip {
        Skip(false)
    }

    /// Called for each move, like `Nf3+`.
    fn san(&mut self, san_plus: SanPlus) {
        let _san_plus = san_plus;
    }
    /// Called for each numeric annotation glyph, like `!?` or `$7`.
    fn nag(&mut self, nag: Nag) {
        let _nag = nag;
    }
    /// Called for each `{ comment }`.
    fn comment(&mut self, comment: RawComment<'_>) {
        let _comment = comment;
    }
    /// Called for each `(`. May skip over the following variation directly
    /// to [`Visitor::end_variation()`] (or to
    /// [`Visitor::end_game()`] if no matching `)`
    /// follows before the end of the game.
    fn begin_variation(&mut self) -> Skip {
        Skip(false)
    }
    /// Called for each `)`. It is *not* guaranteed that there was a
    /// matching `(`.
    fn end_variation(&mut self) {}
    /// Called for each game termination, like `*` or `1-0`.
    fn outcome(&mut self, outcome: Outcome) {
        let _outcome = outcome;
    }

    /// Called after parsing a game. Can produce a custom result.
    fn end_game(&mut self) -> Self::Result;
}

pub(crate) struct SkipVisitor;

impl Visitor for SkipVisitor {
    type Result = ();

    fn begin_movetext(&mut self) -> Skip {
        Skip(true)
    }
    fn begin_variation(&mut self) -> Skip {
        Skip(true)
    }
    fn end_game(&mut self) {}
}
