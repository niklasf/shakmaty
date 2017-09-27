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

Usage and documentation
-----------------------

[Read the documentation](https://docs.rs/pgn-reader)

License
-------

pgn-reader is licensed under the GPL-3.0 (or any later version at your option).
See the COPYING file for the full license text.
