use std::ops::ControlFlow;

use pgn_reader::{Nag, RawComment, RawTag, Visitor, Writer, writer, writer::MovetextToken};
use shakmaty::{Color, KnownOutcome, Outcome, san::SanPlus};

fn main() {
    let mut writer = Writer::new(Vec::new(), writer::Config::default());
    // guaranteed to continue
    let mut tags = writer.begin_tags().continue_value().unwrap();

    writer
        .tag(&mut tags, b"Event", RawTag(b"Annual Horde Championship"))
        .continue_value()
        .unwrap();
    writer
        .tag(&mut tags, b"Site", RawTag(b"Mongolian Steppe"))
        .continue_value()
        .unwrap();
    writer
        .tag(&mut tags, b"Annotator", RawTag(b"Ben Finegold"))
        .continue_value()
        .unwrap();

    let mut movetext = writer.begin_movetext(tags).continue_value().unwrap();

    // stockfish line for horde
    writer
        .san(&mut movetext, SanPlus::from_ascii(b"h5").unwrap())
        .continue_value()
        .unwrap();
    writer
        .san(&mut movetext, SanPlus::from_ascii(b"e6").unwrap())
        .continue_value()
        .unwrap();

    // variation for black
    let _ = writer
        .begin_variation(&mut movetext)
        .continue_value()
        .unwrap();

    writer
        .san(&mut movetext, SanPlus::from_ascii(b"g6").unwrap())
        .continue_value()
        .unwrap();

    // end it
    writer
        .end_variation(&mut movetext)
        .continue_value()
        .unwrap();

    // back to mainline
    // stop using stockfish
    writer
        .san(&mut movetext, SanPlus::from_ascii(b"f6").unwrap())
        .continue_value()
        .unwrap();

    // now you upset ben
    writer
        .comment(&mut movetext, RawComment(b"never play f6!"))
        .continue_value()
        .unwrap();

    writer
        .nag(&mut movetext, Nag::BLUNDER) // the truth hurts
        .continue_value()
        .unwrap();

    // forced resignation
    writer
        .outcome(
            &mut movetext,
            Outcome::Known(KnownOutcome::Decisive {
                winner: Color::Black,
            }),
        )
        .continue_value()
        .unwrap();

    // we can't write after an outcome! only end_variation and end_game
    assert!(matches!(
        writer.comment(&mut movetext, RawComment(b"sneaky")),
        ControlFlow::Break(Err(writer::Error::InvalidToken {
            token,
            allowed
        })) if token == MovetextToken::COMMENT && allowed == MovetextToken::END_VARIATION | MovetextToken::END_GAME
    ));

    writer.end_game(movetext).unwrap();

    let target = "[Event \"Annual Horde Championship\"]\n\
[Site \"Mongolian Steppe\"]\n\
[Annotator \"Ben Finegold\"]\n\n\
1. h5 e6 ( g6 ) 2. f6 { never play f6! } $4 0-1 \n\n";

    assert_eq!(str::from_utf8(&writer.writer).unwrap(), target);

    // this count resets every begin_tags
    assert_eq!(writer.bytes_written(), target.len());

    // this count does not
    assert_eq!(writer.total_bytes_written(), target.len());
}
