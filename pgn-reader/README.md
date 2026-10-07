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
4. Reasonable behavior on invalid PGNs. Common quirks may be supported, but
   only if there is essentially no performance cost.
5. Usability. Basic operation can be quite verbose and users need to bring
   their own in-memory representation for games.

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
use pgn_reader::{Visitor, Reader, SanPlus};

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

Benchmarks (v0.29.1)
--------------------

Run with [lichess_db_standard_rated_2018-10.pgn](https://database.lichess.org/standard/lichess_db_standard_rated_2018-10.pgn.zst),
a very orderly PGN file with additional headers and many small comments
for evaluations and clock times,
containing 24,784,600 games, 50,307 MiB uncompressed on tmpfs,
AMD Ryzen 9 9950X @ 4.3 GHz,
compiled with Rust 1.99.0:

Benchmark | Time | Throuhput (games) | Throughput (data)
--- | ---: | ---: | ---:
examples/stats.rs | 26.2 s | 945,905 /s | 1,920 MiB/s
examples/validate.rs | 84.5 s | 293,392 /s | 596 MiB/s
examples/parallel_validate.rs (1 + 3 threads) | 35.3 s | 702,571 /s | 1,426 MiB/s
`grep -F "[Event " -c` (GNU grep 3.12) | 24.8 s | 998,413 /s | 2,027 MiB/s
`rg -F "[Event " -c` (ripgrep 15.2.0) | 4.6 s | 5,340,358 /s | 10,840 MiB/s

License
-------

pgn-reader is licensed under the GPL-3.0 (or any later version at your option).
