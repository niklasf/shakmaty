# shakmaty

A Rust library for chess move generation

[![crates.io](https://img.shields.io/crates/v/shakmaty.svg)](https://crates.io/crates/shakmaty)
[![docs.rs](https://docs.rs/shakmaty/badge.svg)](https://docs.rs/shakmaty)

## Features

- Generate legal moves:

  ```rust
  use shakmaty::{Chess, Position};

  let pos = Chess::default();
  let legals = pos.legal_moves();
  assert_eq!(legals.len(), 20);
  ```

- Play moves:

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

- Detect game end conditions: `pos.is_checkmate()`, `pos.is_stalemate()`,
  `pos.is_insufficient_material()`, `pos.outcome()`.

- Read and write FEN, SAN and UCI notation.

- Supports all Lichess variants: Standard chess, Chess960, Antichess, Atomic,
  King of the Hill, Three-Check, Crazyhouse, Racing Kings and Horde. Provides
  vocabulary to implement other variants.

- Bitboards and compact fixed shift magic attack tables.

- Zobrist hash positions.

- Probe Syzygy tablebases with [shakmaty-syzygy](https://crates.io/crates/shakmaty-syzygy).

## Documentation

[Read the documentation](https://docs.rs/shakmaty)

## Benchmarks

Simple [perft](https://www.chessprogramming.org/Perft) of the initial
position. No hashtables. i7-6850K CPU @ 3.60GHz.

| perft                                                    | 4      | 5       |
| -------------------------------------------------------- | ------ | ------- |
| shakmaty 0.16.0                                          | 1.0 ms | 24.1 ms |
| [jordanbray/chess](https://crates.io/crates/chess) 3.1.1 | 0.8 ms | 18.6 ms |
| Stockfish 8 (x86-64-bmi2)                                | 4 ms   | 33 ms   |

It should be noted that Stockfish is not optimized for perft speed and also
maintains additional data structures for evaluation. Newer versions of
Stockfish put even less emphasis on this.

## License

Shakmaty is licensed under the GPL-3.0 (or any later version at your option).
See the COPYING file for the full license text.
