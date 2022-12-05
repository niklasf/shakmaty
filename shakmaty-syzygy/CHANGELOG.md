# Changelog for shakmaty-syzygy

## v0.21.0

- Update shakmaty to `0.23`.

## v0.20.0

- Update shakmaty to `0.22`.
- Remove the `backtrace` feature. `ProbeError::CorruptedTable` now always
  captures a backtrace when the `RUST_BACKTRACE` or `RUST_LIB_BACKTRACE`
  environment variables are set.
- Fix compilation on 32-bit Unix systems.

## v0.19.1

*yanked*

## v0.19.0

- Update shakmaty to `0.21`.

## v0.18.0

- Introduce `MaybeRounded` and `AmbiguousWdl`, ensuring that DTZ rounding
  is handled.
- Add `Tablebase::max_pieces()`.
- `Dtz::before_zeroing()` is no longer generic.
- `Dtz::add_plies()` now takes `u32`. Add checked and saturating versions.
- Fix `ProbeError::CorruptedTable` should be `#[non_exhaustive]` when
  `backtrace` feature not enabled.
- Optimize initialization of internal constants, no longer depending on
  on `lazy_static`.

## v0.17.0

- Update shakmaty to `0.20`.

## v0.16.1

- Update shakmaty to `0.19`.

## v0.15.0

- Update shakmaty to `0.18`.


## v0.14.0

- Update shakmaty to `0.17`.
- Fix panic when lead pawns are corrupted.

## v0.13.1

- Explicit `#![forbid(unsafe_code)]`.
- Update dependencies.

## v0.13.0

- Update shakmaty to `0.16`.
- No longer depend on `failure`. Instead, nightly Rust and the `backtrace`
  feature are required to get backtraces for corrupted tables.
- Validate file size when opening table files.

## v0.12.1

- Update dependencies.

## v0.12.0

- Update shakmaty to `0.15`.

## v0.11.1

- Update dependencies.

## v0.11.0

- Update shakmaty to `0.14`.
- Add reimplementation of [Fathom](https://github.com/basil00/Fathom) as example.

## v0.10.0

- Update shakmaty to `0.13`.
- Use `FADV_RANDOM` on tablebase files.

## v0.9.0

- Update shakmaty to `0.12`.
- `Tablebase::add_directory()` now returns the number of added files.
- Added `Tablebase::add_file()`.
- `ProbeError` and `Metric` are now public. Other previously hidden APIs
  are no longer exposed.
- Various refactorings, performance improvements and dependency updates.

## v0.8.1

- Optimize reading block lengths.
- More internal refactorings.
- Add benchmark.

## v0.8.0

- Update shakmaty to 0.11.x.
- Add `Syzygy::MAX_PIECES`.
- More efficient `Tablebase::best_move()`.
- Internal refactorings following tablebase rewrite in Cfish.

## v0.7.0

- Update shakmaty to 0.10.x.

## v0.6.0

- Update shakmaty to 0.9.x. Minimum Rust version is now 1.27.0.

## v0.5.2

- Fix `Wdl::from_dtz_after_zeroing(Dtz(100))`.

## v0.5.1

- Fix out of bounds panic when probing KPPPPPvK.
- Fix error when first group in giveaway should have had more than two
  identical pieces.
- Only 6 pieces supported in antichess variants. Return
  `SyzygyError::TooManyPieces` instead of potential panic.

## v0.5.0

- More contextual information for errors (table, backtrace,
  invalid magic header bytes).
- Update to shakmaty 0.8.x.
- Provide a target for `cargo fuzz`. Fix several panics caused by
  initializing and probing corrupted tables:
  - Panic when reference pawn is missing.
  - Panic when subtable material is not consistent with first file.
  - Integer overflow when reading `symlen`.
  - Integer overflow when `min_symlen` or `max_symlen` are out of bounds.
  - Integer overflow when computing `block_length_size`.
  - Stack overflow when recursively reading `symlen`.

## v0.4.5

- Make better use of `positioned-io` (cursor, remove `RandomAccessFile`).

## v0.4.4

- Remove work around from 0.3.1.
- Minor internal optimizations and cleanups.

## v0.4.3

- Fix DTZ off-by-one in some positions when there is mate in 1.
- Fix DTZ off-by-one in antichess endgames, when there is a threat to force
  a capture leading to a blessed loss.
- Add `Tablebase::best_move()`.

## v0.4.2

- Fix DTZ in en passant positions.
- Minimum Rust version 1.26.0.
- Use guaranteed lossless versions from `u16` to `usize`.
- Provide i128 conversions for `Wdl` and `Dtz`.

## v0.4.1

- Fix debug assertion for `Dtz(0).add_plies()`.

## v0.4.0

- Update to shakmaty 0.7.x, which has bugfixes with regard to insufficient
  material.

## v0.3.1

- Work around compiler bug in release mode.

## v0.3.0

- Rename `Tablebases` to `Tablebase`.
- Group file extension and magic as `TableType` in `Syzygy` trait.

## v0.2.2

- Implement support for wide DTZ values. These are required for some long
  7 piece endgames.
- Fix panic on corrupted table.

## v0.2.1

- Should not panic on corrupted tables (even in debug mode).
- Switch from fnv to fxhash.

## v0.2.0

- Replace `Wdl::from(dtz)` with `Wdl::from_dtz_after_zeroing(dtz)`.
- Remove `isize` conversions of `Dtz` and `Wdl`.
- Fix WDL of lone king in atomic chess.
- Support stable rust.

## v0.1.4

- Fix ordering of lead pawns.

## v0.1.3

- Add support for Atomic chess and Giveaway.

## v0.1.2

- Cosmetic tweak to DTZ in case of mate in 1.

## v0.1.1

- Optional `serde-1` feature.
- Add `Dtz.add_plies()`.

## v0.1.0

- First release.
