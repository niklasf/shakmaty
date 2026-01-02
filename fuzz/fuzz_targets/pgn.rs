#![no_main]

use std::{fmt, io, io::Seek as _, mem, ops::ControlFlow};

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use pgn_reader::{Nag, Outcome, RawComment, RawTag, Reader, SanPlus, Skip, Visitor};

#[derive(Debug, Eq, PartialEq)]
enum Token {
    BeginTags,
    Tag(Vec<u8>, Vec<u8>),
    BeginMovetext,
    San(SanPlus),
    Nag(Nag),
    PartialComment(Vec<u8>),
    Comment(Vec<u8>),
    BeginVariation,
    EndVariation,
    Outcome(Outcome),
    EndGame,
}

#[derive(Debug, Arbitrary, Clone)]
struct BreakOnToken {
    begin_tags: bool,
    tag: bool,
    begin_movetext: bool,
    san: bool,
    nag: bool,
    partial_comment: bool,
    comment: bool,
    begin_variation: bool,
    end_variation: bool,
    outcome: bool,

    skip_variation: bool,
}

impl Visitor for BreakOnToken {
    type Tags = Vec<Token>;
    type Movetext = Vec<Token>;
    type Output = Vec<Token>;

    fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
        let tags = vec![Token::BeginTags];
        if self.begin_tags {
            ControlFlow::Break(tags)
        } else {
            ControlFlow::Continue(tags)
        }
    }

    fn tag(
        &mut self,
        tags: &mut Self::Tags,
        name: &[u8],
        value: RawTag<'_>,
    ) -> ControlFlow<Self::Output> {
        tags.push(Token::Tag(name.to_owned(), value.decode().into_owned()));
        if self.tag {
            ControlFlow::Break(mem::take(tags))
        } else {
            ControlFlow::Continue(())
        }
    }

    fn begin_movetext(
        &mut self,
        mut tags: Self::Tags,
    ) -> ControlFlow<Self::Output, Self::Movetext> {
        tags.push(Token::BeginMovetext);
        if self.begin_movetext {
            ControlFlow::Break(tags)
        } else {
            ControlFlow::Continue(tags)
        }
    }

    fn san(
        &mut self,
        movetext: &mut Self::Movetext,
        san_plus: SanPlus,
    ) -> ControlFlow<Self::Output> {
        movetext.push(Token::San(san_plus));
        if self.san {
            ControlFlow::Break(mem::take(movetext))
        } else {
            ControlFlow::Continue(())
        }
    }

    fn nag(&mut self, movetext: &mut Self::Movetext, nag: Nag) -> ControlFlow<Self::Output> {
        movetext.push(Token::Nag(nag));
        if self.nag {
            ControlFlow::Break(mem::take(movetext))
        } else {
            ControlFlow::Continue(())
        }
    }

    fn partial_comment(
        &mut self,
        movetext: &mut Self::Movetext,
        comment: RawComment<'_>,
    ) -> ControlFlow<Self::Output> {
        movetext.push(Token::PartialComment(comment.as_bytes().to_owned()));
        if self.partial_comment {
            ControlFlow::Break(mem::take(movetext))
        } else {
            ControlFlow::Continue(())
        }
    }

    fn comment(
        &mut self,
        movetext: &mut Self::Movetext,
        comment: RawComment<'_>,
    ) -> ControlFlow<Self::Output> {
        movetext.push(Token::Comment(comment.as_bytes().to_owned()));
        if self.comment {
            ControlFlow::Break(mem::take(movetext))
        } else {
            ControlFlow::Continue(())
        }
    }

    fn begin_variation(
        &mut self,
        movetext: &mut Self::Movetext,
    ) -> ControlFlow<Self::Output, Skip> {
        movetext.push(Token::BeginVariation);
        if self.begin_variation {
            ControlFlow::Break(mem::take(movetext))
        } else {
            ControlFlow::Continue(Skip(self.skip_variation))
        }
    }

    fn end_variation(&mut self, movetext: &mut Self::Movetext) -> ControlFlow<Self::Output> {
        movetext.push(Token::EndVariation);
        if self.end_variation {
            ControlFlow::Break(mem::take(movetext))
        } else {
            ControlFlow::Continue(())
        }
    }

    fn outcome(
        &mut self,
        movetext: &mut Self::Movetext,
        outcome: Outcome,
    ) -> ControlFlow<Self::Output> {
        movetext.push(Token::Outcome(outcome));
        if self.outcome {
            ControlFlow::Break(mem::take(movetext))
        } else {
            ControlFlow::Continue(())
        }
    }

    fn end_game(&mut self, mut movetext: Self::Movetext) -> Self::Output {
        movetext.push(Token::EndGame);
        movetext
    }
}

#[derive(Arbitrary)]
struct TestCase {
    pgn: Vec<u8>,
    left_visitor: BreakOnToken,
    right_visitor: BreakOnToken,
}

impl fmt::Debug for TestCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TestCase")
            .field("pgn", &String::from_utf8_lossy(&self.pgn))
            .field("left_visitor", &self.left_visitor)
            .field("right_visitor", &self.right_visitor)
            .finish()
    }
}

fuzz_target!(|data: TestCase| {
    let mut data = data;
    let mut left_reader = Reader::new(io::Cursor::new(&data.pgn));
    let mut right_reader = Reader::new(io::Cursor::new(&data.pgn));

    // Read first game with different visitors.
    if left_reader.read_game(&mut data.left_visitor).is_err() {
        return;
    }
    if right_reader.read_game(&mut data.right_visitor).is_err() {
        return;
    }

    // Process any pending skips. Then the positions in the stream must be
    // equal.
    assert_eq!(left_reader.has_more().ok(), right_reader.has_more().ok());
    assert_eq!(
        left_reader.stream_position().expect("cursor i/o"),
        right_reader.stream_position().expect("cursor i/o")
    );

    // Results must be equal when continuing with just one of the visitors,
    // unless there was an I/O error (io::ErrorKind::InvalidData may go
    // unnoticed depending on the buffer state).
    let mut visitor = data.left_visitor;
    loop {
        let Ok(Some(left_game)) = left_reader.read_game(&mut visitor) else {
            break;
        };
        let Ok(Some(right_game)) = right_reader.read_game(&mut visitor) else {
            break;
        };
        assert_eq!(left_game, right_game);
    }
});
