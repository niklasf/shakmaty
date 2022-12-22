pgn-reader
==========

A fast non-allocating and streaming reader for chess games in PGN notation,
as a Rust library.

[![Build Status](https://travis-ci.org/niklasf/rust-pgn-reader.svg?branch=master)](https://travis-ci.org/niklasf/rust-pgn-reader)
[![crates.io](https://img.shields.io/crates/v/pgn-reader.svg)](https://crates.io/crates/pgn-reader)
[![docs.rs](https://docs.rs/pgn-reader/badge.svg)](https://docs.rs/pgn-reader)

Introduction
------------

`Reader` parses games and calls methods of a user provided `Visitor`.
Implementing custom visitors allows for maximum flexibility:

* The reader itself does not allocate (besides a single fixed-size buffer).
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
use std::io;
use pgn_reader::{Visitor, Skip, BufferedReader, SanPlus};

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

    fn san(&mut self, _san_plus: SanPlus) {
        self.moves += 1;
    }

    fn begin_variation(&mut self) -> Skip {
        Skip(true) // stay in the mainline
    }

    fn end_game(&mut self) -> Self::Result {
        self.moves
    }
}

fn main() -> io::Result<()> {
    let pgn = b"1. e4 e5 2. Nf3 (2. f4)
                { game paused due to bad weather }
                2... Nf6 *";

    let mut reader = BufferedReader::new_cursor(&pgn[..]);

    let mut counter = MoveCounter::new();
    let moves = reader.read_game(&mut counter)?;

    assert_eq!(moves, Some(4));
    Ok(())
}
```

Documentation
-------------

[Read the documentation](https://docs.rs/pgn-reader)

State of the library
--------------------

The API could be cleaner and performance may have regressed slightly compared
to the `mmap` based approach from old versions
([#12](https://github.com/niklasf/rust-pgn-reader/issues/12)).
This needs some attention. Until I get around to it, I am doing only
minimal maintenance, following `shakmaty` as required.

Nonetheless, it is probably still one of the fastest PGN parsers around.

Benchmarks (v0.12.0)
--------------------

Run with [lichess_db_standard_rated_2018-10.pgn](https://database.lichess.org/standard/lichess_db_standard_rated_2018-10.pgn.bz2) (24,784,600 games, 52,750 MB uncompressed) on an SSD (Samsung 850), Intel i7-6850K CPU @ 3.60 GHz:

Benchmark | Time | Throughput
--- | --- | ---
examples/stats.rs | 111.9s | 471.4 MB/s
examples/validate.rs | 237.1s | 222.5 MB/s
examples/parallel_validate.rs | 148.6s | 355.0 MB/s
[`scoutfish make`](https://github.com/mcostalba/scoutfish) | 269.2s | 196.0 MB/s
`grep -F "[Event " -c` | 39.2s | 1345.7 MB/s

`examples/stats.rs` with compressed files:

Compression | File size | Time | Throughput
--- | --- | --- | ---
*none* | 52,750 MB | 111.9s | 471.4 MB/s
bz2 | 6,226 MB | 1263.1s | 4.9 MB/s
xz | 6,989 MB | 495.9s | 14.1 MB/s
gz | 10,627 MB | 335.7s | 31.7 MB/s
lz4 | 16,428 MB | 180.0s | 91.3 MB/s

License
-------

pgn-reader is licensed under the GPL-3.0 (or any later version at your option).
See the COPYING file for the full license text.
