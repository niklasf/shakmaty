use pgn_reader::{Nag, RawComment, RawTag, Visitor, Writer, writer};
use shakmaty::{Color, KnownOutcome, Outcome, san::SanPlus};

/// Normal usage of the writer.
#[test]
fn normal() {
    let mut writer = Writer::new(Vec::new(), writer::Config::default());
    let mut tags = writer.begin_tags().continue_value().unwrap();

    writer
        .tag(&mut tags, b"Event", RawTag(b"Testing\""))
        .continue_value()
        .unwrap();
    writer
        .tag(&mut tags, b"Site", RawTag(b"#[test]"))
        .continue_value()
        .unwrap();

    let mut movetext = writer.begin_movetext(tags).continue_value().unwrap();

    writer
        .san(&mut movetext, SanPlus::from_ascii(b"exd4").unwrap())
        .continue_value()
        .unwrap();
    writer
        .san(&mut movetext, SanPlus::from_ascii(b"b2").unwrap())
        .continue_value()
        .unwrap();
    let _ = writer
        .begin_variation(&mut movetext)
        .continue_value()
        .unwrap();
    writer
        .san(&mut movetext, SanPlus::from_ascii(b"Qxg1").unwrap())
        .continue_value()
        .unwrap();
    writer
        .san(&mut movetext, SanPlus::from_ascii(b"Ke1#").unwrap())
        .continue_value()
        .unwrap();
    writer
        .comment(&mut movetext, RawComment(b"brilliant discovered mate"))
        .continue_value()
        .unwrap();
    writer
        .nag(&mut movetext, Nag::BRILLIANT_MOVE)
        .continue_value()
        .unwrap();
    writer
        .end_variation(&mut movetext)
        .continue_value()
        .unwrap();
    writer
        .outcome(
            &mut movetext,
            Outcome::Known(KnownOutcome::Decisive {
                winner: Color::White,
            }),
        )
        .continue_value()
        .unwrap();

    let bytes_written = writer.end_game(movetext).unwrap();
    assert_eq!(bytes_written, writer.bytes_written());

    let correct = "[Event \"Testing\\\"\"]\n[Site \"#[test]\"]\n\n1. exd4 b2 ( Qxg1 2. Ke1# { brilliant discovered mate } $3 ) 1-0 \n\n";

    assert_eq!(str::from_utf8(&writer.writer).unwrap(), correct);
    assert_eq!(writer.bytes_written(), correct.len());
    assert_eq!(writer.total_bytes_written(), correct.len());
}

/// User tries to break the writer.
#[test]
fn attempt_breakage() {
    let mut writer = Writer::new(Vec::new(), writer::Config::default());
    let tags = writer.begin_tags().continue_value().unwrap();
    let mut movetext = writer.begin_movetext(tags).continue_value().unwrap();

    writer
        .san(&mut movetext, SanPlus::from_ascii(b"Nc3").unwrap())
        .continue_value()
        .unwrap();

    // unclosed variations
    let _ = writer
        .begin_variation(&mut movetext)
        .continue_value()
        .unwrap();

    let _ = writer
        .begin_variation(&mut movetext)
        .continue_value()
        .unwrap();

    let _ = writer
        .begin_variation(&mut movetext)
        .continue_value()
        .unwrap();

    let bytes_written = writer.end_game(movetext).unwrap();

    // TODO: should unclosed variations be allowed?
    let correct1 = "1. Nc3 ( ( ( ) ) ) \n\n";

    assert_eq!(str::from_utf8(&writer.writer).unwrap(), correct1);
    assert_eq!(bytes_written, correct1.len());
    assert_eq!(writer.bytes_written(), correct1.len());
    assert_eq!(writer.total_bytes_written(), correct1.len());

    writer.writer.clear();

    let tags = writer.begin_tags().continue_value().unwrap();
    let mut movetext = writer.begin_movetext(tags).continue_value().unwrap();

    writer
        .end_variation(&mut movetext)
        .continue_value()
        .unwrap();

    writer
        .end_variation(&mut movetext)
        .continue_value()
        .unwrap();

    let bytes_written = writer.end_game(movetext).unwrap();

    let correct2 = r##"

"##;

    assert_eq!(str::from_utf8(&writer.writer).unwrap(), correct2);
    assert_eq!(bytes_written, correct2.len());
    assert_eq!(writer.bytes_written(), correct2.len());
    assert_eq!(
        writer.total_bytes_written(),
        correct1.len() + correct2.len()
    );
}
