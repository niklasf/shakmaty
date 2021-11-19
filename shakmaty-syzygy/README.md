shakmaty-syzygy
===============

A Rust library to probe Syzygy endgame tablebases, based on [shakmaty](https://github.com/niklasf/shakmaty).

[![Build Status](https://travis-ci.org/niklasf/shakmaty-syzygy.svg?branch=master)](https://travis-ci.org/niklasf/shakmaty-syzygy)
[![crates.io](https://img.shields.io/crates/v/shakmaty-syzygy.svg)](https://crates.io/crates/shakmaty-syzygy)
[![docs.rs](https://docs.rs/shakmaty-syzygy/badge.svg)](https://docs.rs/shakmaty-syzygy)

[Syzygy tables](https://syzygy-tables.info/#syzygy) allow optimal play under
the 50-move rule. Tables are available for positions with up to 7 pieces.

Example: Usage
--------------

```rust
use shakmaty::{CastlingMode, Chess, fen::Fen};
use shakmaty_syzygy::{Tablebase, MaybeRounded, Wdl, Dtz, Syzygy};

let mut tables = Tablebase::new();
tables.add_directory("tables/chess")?;

let pos: Chess = "8/8/8/8/B7/N7/K2k4/8 b - - 0 1"
    .parse::<Fen>()?
    .position(CastlingMode::Standard)?;

let wdl = tables.probe_wdl_after_zeroing(&pos)?;
assert_eq!(wdl, Wdl::Loss);

let dtz = tables.probe_dtz(&pos)?;
assert!(matches!(dtz, MaybeRounded::Rounded(Dtz(-59))));
```

Example: Command line tool
--------------------------

A command line tool similar to [Fathom](https://github.com/basil00/Fathom):

```
$ cargo run --example fathom -- --path tables/chess -- "3qk3/8/8/8/8/8/8/4K3 w - - 0 1"
[Event "KvKQ"]
[Site ""]
[Date "????.??.??"]
[Round "-"]
[White "Syzygy"]
[Black "Syzygy"]
[Result "0-1"]
[FEN "3qk3/8/8/8/8/8/8/4K3 w - - 0 1"]
[Annotator "shakmaty-syzygy"]
[DTZ "-16 or -17"]

{ KvKQ with DTZ -16 or -17 } 1. Ke2 Kd7 2. Kd1 Ke6+ 3. Kc1 Qd3 4. Kb2 Qd2+ 5. Ka1 Kd5 6. Kb1 Kc4 7. Ka1 Kb3 8. Kb1 Qd1# { Checkmate } 0-1
```

Documentation
-------------

[Read the documentation](https://docs.rs/shakmaty-syzygy)

Changelog
---------

* 0.17.0
  - Update shakmaty to `0.20`.
* 0.16.1
  - Update shakmaty to `0.19`.
* 0.15.0
  - Update shakmaty to `0.18`.
* 0.14.0
  - Update shakmaty to `0.17`.
  - Fix panic when lead pawns are corrupted.
* 0.13.1
  - Explicit `#![forbid(unsafe_code)]`.
  - Update dependencies.
* 0.13.0
  - Update shakmaty to `0.16`.
  - No longer depend on `failure`. Instead, nightly Rust and the `backtrace`
    feature are required to get backtraces for corrupted tables.
  - Validate file size when opening table files.
* 0.12.1
  - Update dependencies.
* 0.12.0
  - Update shakmaty to `0.15`.
* 0.11.1
  - Update dependencies.
* 0.11.0
  - Update shakmaty to `0.14`.
  - Add reimplementation of [Fathom](https://github.com/basil00/Fathom) as example.
* 0.10.0
  - Update shakmaty to `0.13`.
  - Use `FADV_RANDOM` on tablebase files.
* 0.9.0
  - Update shakmaty to `0.12`.
  - `Tablebase::add_directory()` now returns the number of added files.
  - Added `Tablebase::add_file()`.
  - `ProbeError` and `Metric` are now public. Other previously hidden APIs
    are no longer exposed.
  - Various refactorings, performance improvements and dependency updates.
* 0.8.1
  - Optimize reading block lengths.
  - More internal refactorings.
  - Add benchmark.
* 0.8.0
  - Update shakmaty to 0.11.x.
  - Add `Syzygy::MAX_PIECES`.
  - More efficient `Tablebase::best_move()`.
  - Internal refactorings following tablebase rewrite in Cfish.
* 0.7.0
  - Update shakmaty to 0.10.x.
* 0.6.0
  - Update shakmaty to 0.9.x. Minimum Rust version is now 1.27.0.
* 0.5.2
  - Fix `Wdl::from_dtz_after_zeroing(Dtz(100))`.
* 0.5.1
  - Fix out of bounds panic when probing KPPPPPvK.
  - Fix error when first group in giveaway should have had more than two
    identical pieces.
  - Only 6 pieces supported in antichess variants. Return
    `SyzygyError::TooManyPieces` instead of potential panic.
* 0.5.0
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
* 0.4.5
  - Make better use of `positioned-io` (cursor, remove `RandomAccessFile`).
* 0.4.4
  - Remove work around from 0.3.1.
  - Minor internal optimizations and cleanups.
* 0.4.3
  - Fix DTZ off-by-one in some positions when there is mate in 1.
  - Fix DTZ off-by-one in antichess endgames, when there is a threat to force
    a capture leading to a blessed loss.
  - Add `Tablebase::best_move()`.
* 0.4.2
  - Fix DTZ in en passant positions.
  - Minimum Rust version 1.26.0.
  - Use guaranteed lossless versions from `u16` to `usize`.
  - Provide i128 conversions for `Wdl` and `Dtz`.
* 0.4.1
  - Fix debug assertion for `Dtz(0).add_plies()`.
* 0.4.0
  - Update to shakmaty 0.7.x, which has bugfixes with regard to insufficient
    material.
* 0.3.1
  - Work around compiler bug in release mode.
* 0.3.0
  - Rename `Tablebases` to `Tablebase`.
  - Group file extension and magic as `TableType` in `Syzygy` trait.
* 0.2.2
  - Implement support for wide DTZ values. These are required for some long
    7 piece endgames.
  - Fix panic on corrupted table.
* 0.2.1
  - Should not panic on corrupted tables (even in debug mode).
  - Switch from fnv to fxhash.
* 0.2.0
  - Replace `Wdl::from(dtz)` with `Wdl::from_dtz_after_zeroing(dtz)`.
  - Remove `isize` conversions of `Dtz` and `Wdl`.
  - Fix WDL of lone king in atomic chess.
  - Support stable rust.
* 0.1.4
  - Fix ordering of lead pawns.
* 0.1.3
  - Add support for Atomic chess and Giveaway.
* 0.1.2
  - Cosmetic tweak to DTZ in case of mate in 1.
* 0.1.1
  - Optional `serde-1` feature.
  - Add `Dtz.add_plies()`.
* 0.1.0
  - First release.

Acknowledgement
---------------

Thanks to Ronald de Man for his [Syzygy tablebases](https://github.com/syzygy1/tb).
The probing code is closely based on his implementation for Stockfish.

License
-------

shakmaty-syzygy is licensed under the GPL-3.0 (or any later version at your
option). See the COPYING file for the full license text.
