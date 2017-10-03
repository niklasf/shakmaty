shakmaty
========

A Rust library for chess move generation

[![Build Status](https://travis-ci.org/niklasf/shakmaty.svg?branch=master)](https://travis-ci.org/niklasf/shakmaty)
[![crates.io](https://img.shields.io/crates/v/shakmaty.svg)](https://crates.io/crates/shakmaty)

Features
--------

* Generate legal moves:

  ```rust
  use shakmaty::{Chess, Position, MoveList};

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

* Upcoming
  - `Square` is now a `#[repr(u8)]` enum
  - Use `bitflags` for `PositionError`
  - Renamed `RemainingChecks::subtract()` to `decrement`
  - Added `Position::swap_turn()`
* 0.1.0
  - First release with support for stable Rust.

License
-------

Shakmaty is licensed under the GPL-3.0 (or any later version at your option).
See the COPYING file for the full license text.
