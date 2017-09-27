pgn-reader
==========

A fast non-allocating reader chess games in PGN notation, as a Rust library.

[![Build Status](https://travis-ci.org/niklasf/rust-pgn-reader.svg?branch=master)](https://travis-ci.org/niklasf/rust-pgn-reader)
[![crates.io](https://img.shields.io/crates/v/pgn-reader.svg)](https://crates.io/crates/pgn-reader)

Introduction
------------

`Reader` parses games and calls methods of a user provided `Visitor`.
Implementing custom visitors allows for maximum flexibility:

* The reader itself does not allocate.
  The visitor can decide if and how to represent games in memory.
* The reader does not validate move legality.
  This allows implementing support for custom chess variants,
  or delaying move validation.
* The visitor can signal to the reader that it does not care about a game or
  variation.

Example
-------

A visitor that counts the number of syntactically valid moves in each game.

```rust
extern crate pgn_reader;

use pgn_reader::{Visitor, Reader, San};

struct MoveCounter {
    moves: usize,
}

impl MoveCounter {
    fn new() -> MoveCounter {
        MoveCounter { moves: 0 }
    }
}

impl Visitor for MoveCounter {
    type Result = usize;

    fn begin_game(&mut self) {
        self.moves = 0;
    }

    fn san(&mut self, _san: San) {
        self.moves += 1;
    }

    fn end_game(&mut self, _game: &[u8]) -> Self::Result {
        self.moves
    }
}

fn main() {
    let pgn = b"1. e4 e5 2. Nf3
                { game paused due to bad weather }
                2... Nf6 *";

    let mut counter = MoveCounter::new();
    let reader = Reader::new(&mut counter, pgn);

    let moves: usize = reader.into_iter().sum();
    assert_eq!(moves, 4);
}
```

Documentation
-------------

[Read the documentation](https://docs.rs/pgn-reader)

License
-------

pgn-reader is licensed under the GPL-3.0 (or any later version at your option).
See the COPYING file for the full license text.
