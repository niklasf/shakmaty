# Changelog for pgn-reader

## v0.29.0

- Breaking: Default implementation of `Visitor::begin_variation()` changed to
  skip variations.
- Comments longer than the configured limit (default 255 bytes) are no longer an
  error: A new method `Visitor::partial_comment()` is now called with all
  except the last chunk of very long comments.
  The default implementation forwards them to `Visitor::comment()`.
- Now accepting `Z0` as notation for null moves.
- Update shakmaty `0.30`.
