shakmaty
========

A Rust library for chess move generation

[![Build Status](https://travis-ci.org/niklasf/shakmaty.svg?branch=master)](https://travis-ci.org/niklasf/shakmaty)
[![crates.io](https://img.shields.io/crates/v/shakmaty.svg)](https://crates.io/crates/shakmaty)
[![docs.rs](https://docs.rs/shakmaty/badge.svg)](https://docs.rs/shakmaty)

Features
--------

* Generate legal moves:

  ```rust
  use shakmaty::{Chess, Position};

  let pos = Chess::default();
  let legals = pos.legals();
  assert_eq!(legals.len(), 20);
  ```

* Play moves:

  ```rust
  use shakmaty::{Square, Move, Role};

  // 1. e4
  let pos = pos.play(&Move::Normal {
      role: Role::Pawn,
      from: Square::E2,
      to: Square::E4,
      capture: None,
      promotion: None,
  })?;
  ```

* Detect game end conditions: `pos.is_checkmate()`, `pos.is_stalemate()`,
  `pos.is_insufficient_material()`, `pos.outcome()`.

* Read and write FENs, SANs and UCIs.

* Supports standard chess, Chess960, Giveaway, Atomic, King Of The Hill,
  Three-Check, Crazyhouse, Racing Kings and Horde. Provides vocabulary
  to implement other variants.

* Bitboards and compact fixed shift magic attack tables.

* Probe Syzygy tablebases with [shakmaty-syzygy](https://crates.io/crates/shakmaty-syzygy).

Documentation
-------------

[Read the documentation](https://docs.rs/shakmaty)

Benchmarks
----------

Simple [perft](https://www.chessprogramming.org/Perft) of the initial
position. No hashtables. i7-6850K CPU @ 3.60GHz.

perft | 4 | 5
--- | --- | ---
shakmaty 0.16.0 | 1.0 ms | 24.1 ms
[jordanbray/chess](https://crates.io/crates/chess) 3.1.1 | 0.8 ms | 18.6 ms
Stockfish 8 (x86-64-bmi2) | 4 ms | 33 ms

It should be noted that Stockfish is not optimized for perft speed and also
maintains additional data structures for evaluation. Newer versions of
Stockfish put even less emphasis on this.

Changelog
---------

* 0.16.2
  - Fix Racing Kings game end detection: Black cannot catch up if their own
    pieces block the goal.
  - Pawn drops in Crazyhouse are now considered zeroing.
* 0.16.1
  - Fix (impossible) Crazyhouse insufficient material.
  - Fix (impossible) castling rights of exploded king in Atomic chess.
* 0.16.0
  - Updated `arrayvec` to 0.5.x, which comes with significant performance
    improvements.
  - The default `Giveaway` position now has castling rights.
* 0.15.3
  - Follow FICS rules in Atomic castling edge cases.
  - Use `#[repr(align)] enum` and `reverse_bits()` stabilized in Rust 1.37.
* 0.15.2
  - In Horde chess, allow double pawn moves from the first rank.
  - Added `{Square,Bitboard}::{flip_anti_diagonal,rotate_90,rotate_180,rotate_270}()`.
* 0.15.1
  - FEN parser was expecting `~` before promoted pieces, but it should be
    after.
* 0.15.0
  - Moved `Position::from_setup()` to a seperate new trait `FromSetup`.
  - Square and file/rank index calculations are now performed with `u32` and
    `i32`, which is more performant than using the narrower types `u8` and
    `i8`. The types are still `#[repr(u8)]`.
  - Renamed `Square::combine()` to `Square::with_rank_of()`.
  - Added `Fen::from_setup()`.
  - Added `FenOpts::scid()` for Scid/Lichess compatible FEN formatting.
    Of course the parser also accepts these formats.
  - Added `variant::Variant` and `VariantPosition` for runtime selected
    chess variants. Limited to the built in variants.
  - Added `{Material,MaterialSide}::is_empty()`.
* 0.14.1
  - `TryFrom` now available on stable.
* 0.14.0
  - `SanPlus::from_move()` no longer requires the move to be legal, which
    was an undocumented requirement. The new
    `SanPlus::from_move_and_play_unchecked()` is closest to the previous
    behavior.
  - Added `San::disambiguate()` and `SanSuffix::from_position()`.
  - Implement `TryFrom` for various types on nightly.
  - Implement `Add`, `AddAssign`, `Sub`, `SubAssign` for `Material` and
    `MaterialSide`.
  - Added `CastlingSide::from_{king|queen}_side()`.
  - Use `u32` for `depth` argument of `perft()`.
* 0.13.1
  - Performance improvements on nightly (aligned `Move` enum).
* 0.13.0
  - Replaced `SanPlus::check` and `SanPlus::checkmate` with `san::Suffix` enum.
  - Renamed `{Rank,File}::rotate()` to `flip_diagonal()`.
  - Renamed `Bitboard::flip()` to `toggle()`.
  - Added `Square::coords()`.
  - Added `Bitboard::flip_{vertical,horizontal,diagonal}()`.
  - Added `Position::promotion_moves()`.
  - Derive `Hash` on `Board`, `FenOpts`, `San`, `SanPlus`, `Suffix`, `Color`,
    `Role`, `Piece`, `RemainingChecks`, `CastlingSide`, and `Uci`.
  - Minimum Rust version 1.31.0.
* 0.12.0
  - `Setup::halfmove_clock()` is now `Setup::halfmoves()`.
  - New conversions: `f32::from(file)`, `f64::from(rank)`.
  - Replaced `Pockets` and `Pocket` by more general `Material` and
    `MaterialSide`. Note that their `Display` and `FromStr` implementations
    differ.
  - Unified naming of error types.
* 0.11.2
  - Fix Atomic insufficient material: KQN can mate.
* 0.11.1
  - Fix Chess960 should not allow a-side castling over a blocking h-side rook.
* 0.11.0
  - `Outcome` is now `Copy`.
  - `Position::castling_uncovers_rank_attack()` is no longer public.
* 0.10.0
  - Added `File`, `Rank`. These are now returned by `Square::{file, rank}`.
    `Square::from_coords(file, rank)` takes the new types and is no longer
    fallible.
  - `Bitboard` is now `IntoIterator` instead of `Iterator` itself.
    Methods `Bitboard::{first, last, count, is_empty}` remain.
  - `Bitboard::{add, flip, discard, with, without}` are now generic over
     `Into<Bitboard>`. Alternative methods `*_all` have been removed.
  - `Bitboard::relative_shift(color, shift: u8)` now takes `u32`.
  - `shakmaty::CarryRippler` is now `shakmaty::bitboard::CarryRippler`.
  - Add new methods:
    `Bitboard::{pop_front, pop_back, is_disjoint, is_subset, is_superset}`.
  - Add `Position::has_insufficient_material(color)`.
* 0.9.0
  - Remove `uci`, `chess960_uci`, `san` and `san_plus` that were deprecated
    in 0.8.1.
  - Renamed `*::from_bytes()` to `*::from_ascii()`.
  - Take small copy types by value: `Piece::char()`, `CastlingSide::*`.
  - Added `Castles::has(color, side)`.
  - `fen::{board_fen, epd, fen}` are now shortcuts for formatting with
    default options.
  - Minimum rust version 1.27.0 (i128 conversions again, fused iterators again,
    `Option::filter()`, `dyn Trait`).
  - Eliminate many uses of unsafe, including `TrustedLen` impls, at minor cost.
* 0.8.1
  - Make `Role` discriminants public.
  - Put `San`, `SanPlus` and `Uci` constructors on `San`, `SanPlus` and `Uci`
    respectively.
* 0.8.0
  - Revert fused iterators and u128. Minimum Rust version back to 1.23.0.
  - Changed `Role` discriminants. Now starting with `Pawn = 1`.
  - Performance improvements (~12% faster perft).
  - Switch benchmarks to `criterion.rs`.
* 0.7.2
  - Add `Outcome.winner()`.
* 0.7.1
  - Minimum Rust version 1.26.0 (fused iterators, u128 conversions).
* 0.7.0
  - Reintroduce the remaining Lichess variants: `Crazyhouse`, `KingOfTheHill`,
    `ThreeCheck`, `Horde`, `RacingKings`.
  - Expose `Position.castles()` and replace `Position.is_chess960()` with
    `Position.castles().is_chess960()`.
  - Fix insufficient material: KNvK was not given as drawn due to a typo.
  - Fix insufficient material in `Atomic`: Two knights of different colors
    are not safe.
  - Let `Pockets.count()` and `Pocket.count()` return `usize`.
* 0.6.7
  - Giveaway starting position should have no castling rights.
* 0.6.6
  - Fix missing king promotions in Giveaway.
* 0.6.5
  - Validate color of missing king in atomic chess.
  - Clear move buffers before generating variant moves.
* 0.6.4
  - Reintroduce `Giveaway` and `Atomic` chess variants.
* 0.6.3
  - New method `Move.is_zeroing()`.
* 0.6.2
  - Make unit error types (`InvalidSquareName`, `InvalidUci`, `InvalidSan`)
    completely public.
  - Documentation, coding style and debugging tweaks.
* 0.6.1
  - Expose `attacks::bishop_mask` and `attacks::rook_mask`.
  - Eliminate almost all unchecked indexing.
* 0.6.0
  - Split `impl From<Move> for Uci` into `uci()` and `chess960_uci()`.
  - Fix display of pawn drops.
  - Move generating methods clear the move buffer (and therefor no longer panic on too full buffers).
  - Added `Position.is_chess960()`, `Bitboard.without_all()`, `Role.upper_char()`, `Board.stepper()`.
* 0.5.1
  - Fix `Uci::to_move()` for en passant moves. Thanks zxqfl.
* 0.5.0
  - Use `u64` instead of `usize` for `perft()`.
  - Export error type `InvalidSquareName`.
  - New methods: `CastlingSide.is_{queen|king}_side()`, `San.matches()`, `Move.is_capture()`, `Move.is_promotion()`, `Move.castling_side()`, `Position.is_check()`.
  - Derive `Ord` and `PartialOrd` for `Role`.
  - Support running benchmarks on stable.
* 0.4.2
  - Fix build error on beta due to the new nightly `option_filter` feature.
  - Fix unterminated code block in documentation.
* 0.4.1
  - Fix build error due to the new nightly
    [`option_filter`](https://github.com/rust-lang/rust/issues/45860) feature.
* 0.4.0
  - Rename `Color::from_bool()` to `Color::from_white()`,
    add `Color::from_black()`.
  - Add `Move::role()`, `Move::is_en_passant()` and `Move::is_castle()`.
  - Add `Position::en_passant_moves()` and `Position::capture_moves()`.
  - Implement `BitXor<bool>` for `Color`.
  - Implement `FusedIterator` and `TrustedLen` on `Bitboard`.
* 0.3.0
  - Switch to `#[repr(i8)]` for `Square`. Implement all lossless integer
    conversions `From<Square>`.
  - Add `Square::flip_horizontal()`, `flip_vertical()` and `flip_diagonal()`.
  - Efficiently implement `CarryRippler::last()` by @nvzqz.
  - Eliminate some unchecked indexing by @nvzqz.
  - Faster ASCII case conversions and tests by @nvzqz.
* 0.2.0
  - `Square` is now a `#[repr(u8)]` enum.
  - Use `bitflags` for `PositionError`.
  - Rename `RemainingChecks::subtract()` to `decrement()`.
  - Add `Position::swap_turn()`.
* 0.1.0
  - First release with support for stable Rust.

License
-------

Shakmaty is licensed under the GPL-3.0 (or any later version at your option).
See the COPYING file for the full license text.
