# Changelog for pgn-reader

## v0.29.1

- Performance improvements on well-formatted PGN files. Stacks with
  shakmaty 0.30.2 to ~1.8x speedup on stats, ~1.2x on validate,
  ~1.7x on parallel validate.
- MSRV is now `1.97`.

## v0.29.0

- Breaking: Default implementation of `Visitor::begin_variation()` changed to
  skip variations.
- Comments longer than the configured limit (default 255 bytes) are no longer an
  error: A new method `Visitor::partial_comment()` is now called with all
  except the last chunk of very long comments.
  The default implementation forwards them to `Visitor::comment()`.
- Now accepting `Z0` as notation for null moves.
- Update shakmaty `0.30`.
