pgn-reader
==========

A fast non-allocating and streaming reader for chess games in PGN notation.
Experimental.

[![crates.io](https://img.shields.io/crates/v/pgn-reader.svg)](https://crates.io/crates/pgn-reader)
[![docs.rs](https://docs.rs/pgn-reader/badge.svg)](https://docs.rs/pgn-reader)

Design priorities
-----------------

In this order:

1. Safe to run on untrusted inputs. No panics, no denial of service
   (linear time complexity).
2. Correct on valid PGNs.
3. Performance. One of the fastest PGN parsers in any language.
4. Reasonable behavior on invalid PGNs.
5. Usability. Barrier to entry can be quite high when you have to bring your
   own in-memory representation for games.

Introduction
------------

`Reader` parses games and calls methods of a user provided `Visitor`.
Implementing custom visitors allows for maximum flexibility:

* The reader itself does not allocate (besides a single fixed-size buffer).
  The visitor can decide if and how to represent games in memory.
* The reader does not validate move legality.
  This allows implementing support for custom chess variants,
  or delaying move validation.
* The visitor can short-circuit and let the reader use a fast path for
  skipping games or variations.

Example
-------

A visitor that counts the number of syntactically valid moves in the
mainline of each game.

```rust
use std::{io, ops::ControlFlow};
use pgn_reader::{Visitor, Skip, Reader, SanPlus};

struct MoveCounter;

impl Visitor for MoveCounter {
    type Tags = ();
    type Movetext = usize;
    type Output = usize;

    fn begin_tags(&mut self) -> ControlFlow<Self::Output, Self::Tags> {
        ControlFlow::Continue(())
    }

    fn begin_movetext(&mut self, _tags: Self::Tags) -> ControlFlow<Self::Output, Self::Movetext> {
        ControlFlow::Continue(0)
    }

    fn san(&mut self, movetext: &mut Self::Movetext, _san_plus: SanPlus) -> ControlFlow<Self::Output> {
        *movetext += 1;
        ControlFlow::Continue(())
    }

    fn begin_variation(&mut self, _movetext: &mut Self::Movetext) -> ControlFlow<Self::Output, Skip> {
        ControlFlow::Continue(Skip(true)) // stay in the mainline
    }

    fn end_game(&mut self, movetext: Self::Movetext) -> Self::Output {
        movetext
    }
}

fn main() -> io::Result<()> {
    let pgn = b"1. e4 e5 2. Nf3 (2. f4)
                { game paused due to bad weather }
                2... Nf6 *";

    let mut reader = Reader::new(io::Cursor::new(&pgn));

    let moves = reader.read_game(&mut MoveCounter)?;
    assert_eq!(moves, Some(4));

    Ok(())
}
```

Documentation
-------------

[Read the documentation](https://docs.rs/pgn-reader)

Benchmarks (v0.28.0)
--------------------

Run with [lichess_db_standard_rated_2018-10.pgn](https://database.lichess.org/standard/lichess_db_standard_rated_2018-10.pgn.zst),
a very orderly PGN file with additional headers and many small comments
for evaluations and clock times,
containing 24,784,600 games, 50,307 MiB uncompressed on tmpfs,
AMD Ryzen 9 9950X @ 4.3 GHz,
compiled with Rust 1.88.0:

Benchmark | Time | Throuhput (games) | Throughput (data)
--- | ---: | ---: | ---:
examples/stats.rs | 50.6 | 489,814 /s | 994 MiB/s
examples/validate.rs | 116.8 | 212,197 /s | 431 MiB/s
examples/parallel_validate.rs (1 + 3 threads) | 62.5 | 396,554 /s | 805 MiB/s
`grep -F "[Event " -c` | 24.0 | 1,032,691 /s | 2,096 MiB/s

License
-------

pgn-reader is licensed under the GPL-3.0 (or any later version at your option).
See the COPYING file for the full license text.
