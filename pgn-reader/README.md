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

A visitor that counts the number of syntactically valid moves in the
mainline of each game.

```rust
extern crate pgn_reader;

use pgn_reader::{Visitor, Skip, Reader, San};

struct MoveCounter {
    moves: usize,
}

impl MoveCounter {
    fn new() -> MoveCounter {
        MoveCounter { moves: 0 }
    }
}

impl<'pgn> Visitor<'pgn> for MoveCounter {
    type Result = usize;

    fn begin_game(&mut self) {
        self.moves = 0;
    }

    fn san(&mut self, _san: San) {
        self.moves += 1;
    }

    fn begin_variation(&mut self) -> Skip {
        Skip(true) // stay in the mainline
    }

    fn end_game(&mut self, _game: &'pgn [u8]) -> Self::Result {
        self.moves
    }
}

fn main() {
    let pgn = b"1. e4 e5 2. Nf3 (2. f4)
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

Bechmarks
---------

Run with [lichess_db_standard_rated_2014-07.pgn](https://database.lichess.org/standard/lichess_db_standard_rated_2014-07.pgn.bz2) (1,048,440 games, 1000 MB uncompressed) on an SSD, Intel i7-5500U CPU @ 2.40GHz.

Benchmark | Time | Throughput
--- | --- | ---
examples/stats.rs | 4.0s | 249.4 MB/s
examples/validate.rs | 10.2s | 98.0 MB/s
examples/lichess4545.rs | 1.6s | 625.0 MB/s
[`scoutfish make`](https://github.com/mcostalba/scoutfish) | 10.9s | 96.1 MB/s
`grep -F "[Event " -c` | 1.1s | 909.1 MB/s

License
-------

pgn-reader is licensed under the GPL-3.0 (or any later version at your option).
See the COPYING file for the full license text.
