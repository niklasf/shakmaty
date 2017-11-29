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

* Supports Standard chess and Chess960. Provides vocabulary to implement
  other variants.

* Bitboards and compact fixed shift magic attack tables.

Documentation
-------------

[Read the documentation](https://docs.rs/shakmaty)

Changelog
---------

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
