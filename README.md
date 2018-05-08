shakmaty
========

A Rust library for chess move generation

[![Build Status](https://travis-ci.org/niklasf/shakmaty.svg?branch=master)](https://travis-ci.org/niklasf/shakmaty)
[![crates.io](https://img.shields.io/crates/v/shakmaty.svg)](https://crates.io/crates/shakmaty)

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

* Supports Standard chess, Chess960, Giveaway and Atomic. Provides vocabulary
  to implement other variants.

* Bitboards and compact fixed shift magic attack tables.

* Probe Syzygy tablebases with [shakmaty-syzygy](https://crates.io/crates/shakmaty-syzygy).

Documentation
-------------

[Read the documentation](https://docs.rs/shakmaty)

Changelog
---------

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
