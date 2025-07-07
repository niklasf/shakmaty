use pgn_reader::{Nag, RawComment, RawTag, Visitor, Writer, writer};
use shakmaty::{Color, KnownOutcome, Outcome, san::SanPlus};

/// Normal usage of the writer.
#[test]
fn default() {
    fn routine(mut writer: Writer<Vec<u8>>) -> Writer<Vec<u8>> {
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
        writer.end_game(movetext).unwrap();

        writer
    }

    let mut writer = Writer::new(Vec::new());
    writer = routine(writer);

    assert_eq!(
        str::from_utf8(&writer.writer).unwrap(),
        r##"[Event "Testing\""]
[Site "#[test]"]

1. exd4 b2 ( Qxg1 3. Ke1# { brilliant discovered mate } $3 ) 1-0

"##
    );

    writer.writer.clear();
    *writer.scheduled_config_mut() = writer::Config::COMPACT;

    writer = routine(writer);

    // space after certain tokens like moves and nags is necessary
    assert_eq!(
        str::from_utf8(&writer.writer).unwrap(),
        r##"[Event "Testing\""]
[Site "#[test]"]

1.exd4 b2 (Qxg1 3.Ke1# {brilliant discovered mate}$3 )1-0

"##
    );
}

/// User tries to break the writer.
#[test]
fn attempt_breakage() {
    let mut writer = Writer::new(Vec::new());
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
    writer.end_game(movetext).unwrap();

    assert_eq!(
        str::from_utf8(&writer.writer).unwrap(),
        r##"[Event "Testing\""]
[Site "#[test]"]

1. exd4 b2 ( Qxg1 3. Ke1# { brilliant discovered mate } $3 ) 1-0

"##
    );
}
