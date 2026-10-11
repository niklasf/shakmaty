# Changelog for pgn-reader

## Unreleased

- Major peformance improvements for `Reader::skip_game()`.
  The `parallel_validate` example now shows how skipping can be used to
  correctly chunk at game boundaries and offload parsing to multiple threads.
- Add `Reader::{get_ref,get_mut}()` for access to the underlying reader.
- Add `ReaderBuilder::with_minimum_buffer_capacity()` and `Reader::capacity()`.

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
