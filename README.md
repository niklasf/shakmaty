shakmaty
========

A Rust library for chess move generation

[![Build Status](https://travis-ci.org/niklasf/shakmaty.svg?branch=master)](https://travis-ci.org/niklasf/shakmaty)
[![crates.io](https://img.shields.io/crates/v/shakmaty.svg)](https://crates.io/crates/shakmaty)

Features
--------

* Generate legal moves:

  ```rust
  use shakmaty::Chess;
  use shakmaty::Position;
  use shakmaty::MoveList;

  let pos = Chess::default(); // starting position

  let mut legals = MoveList::new();
  pos.legal_moves(&mut legals);
  assert_eq!(legals.len(), 20);
  ```

* Play moves:

  ```rust
  use shakmaty::Move;
  use shakmaty::Role;
  use shakmaty::square;

  let pos = pos.play(&Move::Normal {
      role: Role::Pawn,
      from: square::E2,
      to: square::E4,
      capture: None,
      promotion: None,
  }).expect("1. e4 is legal");
  ```

* Detect game end conditions: `pos.is_checkmate()`, `pos.is_stalemate()`,
  `pos.is_insufficient_material()`, `pos.outcome()`.

* Read and write FENs, SANs and UCIs.

* Supports Standard chess and these variants: Crazyhouse, King Of The Hill,
  Giveaway, ThreeCheck, Horde, Atomic, and RacingKings.

* [PDEP](https://chessprogramming.wikispaces.com/BMI2#Instructions-PDEP)
  attack tables.

[Read the documentation](https://docs.rs/shakmaty/0.0.1/shakmaty/)

License
-------

Shakmaty is licensed under the GPL3. See the COPYING file for the full
license text.
