shakmaty-syzygy
===============

A Rust library to probe Syzygy endgame tablebases, based on [shakmaty](https://github.com/niklasf/shakmaty).

[![crates.io](https://img.shields.io/crates/v/shakmaty-syzygy.svg)](https://crates.io/crates/shakmaty-syzygy)
[![docs.rs](https://docs.rs/shakmaty-syzygy/badge.svg)](https://docs.rs/shakmaty-syzygy)

[Syzygy tables](https://syzygy-tables.info/#syzygy) allow optimal play under
the 50-move rule. Tables are available for positions with up to 7 pieces.

Example: Usage
--------------

```rust
use shakmaty::{CastlingMode, Chess, fen::Fen};
use shakmaty_syzygy::{Tablebase, MaybeRounded, Wdl, Dtz, Syzygy};

let mut tables = Tablebase::new();
tables.add_directory("tables/chess")?;

let pos: Chess = "8/8/8/8/B7/N7/K2k4/8 b - - 0 1"
    .parse::<Fen>()?
    .into_position(CastlingMode::Standard)?;

let wdl = tables.probe_wdl_after_zeroing(&pos)?;
assert_eq!(wdl, Wdl::Loss);

let dtz = tables.probe_dtz(&pos)?;
assert!(matches!(dtz, MaybeRounded::Rounded(Dtz(-59))));
```

Example: Command line tool
--------------------------

A command line tool similar to [Fathom](https://github.com/basil00/Fathom):

```
$ cargo run --example fathom -- --path tables/chess -- "3qk3/8/8/8/8/8/8/4K3 w - - 0 1"
[Event "KvKQ"]
[Site ""]
[Date "????.??.??"]
[Round "-"]
[White "Syzygy"]
[Black "Syzygy"]
[Result "0-1"]
[FEN "3qk3/8/8/8/8/8/8/4K3 w - - 0 1"]
[Annotator "shakmaty-syzygy"]
[DTZ "-16 or -17"]

{ KvKQ with DTZ -16 or -17 } 1. Ke2 Kd7 2. Kd1 Ke6+ 3. Kc1 Qd3 4. Kb2 Qd2+ 5. Ka1 Kd5 6. Kb1 Kc4 7. Ka1 Kb3 8. Kb1 Qd1# { Checkmate } 0-1
```

Documentation
-------------

[Read the documentation](https://docs.rs/shakmaty-syzygy)

Acknowledgement
---------------

Thanks to Ronald de Man for his [Syzygy tablebases](https://github.com/syzygy1/tb).
The probing code is closely based on his implementation for Stockfish.

License
-------

shakmaty-syzygy is licensed under the GPL-3.0 (or any later version at your
option). See the COPYING file for the full license text.
