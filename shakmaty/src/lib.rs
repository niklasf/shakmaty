//! A library for chess vocabulary and move generation.
//!
//! # Examples
//!
//! Generate legal moves in the starting position:
//!
//! ```
//! use shakmaty::{Chess, Position};
//!
//! let pos = Chess::default();
//! let legals = pos.legal_moves();
//! assert_eq!(legals.len(), 20);
//! ```
//!
//! Play moves:
//!
//! ```
//! # use shakmaty::{Chess, Position};
//! use shakmaty::{Square, Move, Role};
//! #
//! # let pos = Chess::default();
//!
//! // 1. e4
//! let pos = pos.play(Move::Normal {
//!     role: Role::Pawn,
//!     from: Square::E2,
//!     to: Square::E4,
//!     capture: None,
//!     promotion: None,
//! })?;
//! # Ok::<_, shakmaty::PlayError<_>>(())
//! ```
//!
//! Detect game end conditions:
//!
//! ```
//! # use shakmaty::{Chess, Position, Outcome};
//! # let pos = Chess::default();
//! assert!(!pos.is_checkmate());
//! assert!(!pos.is_stalemate());
//! assert!(!pos.is_insufficient_material());
//! assert_eq!(pos.outcome(), Outcome::Unknown); // no winner yet
//! ```
//!
//! Also supports [FEN](fen), [SAN](san) and
//! [UCI](uci) formats for positions and moves.
//!
//! # Feature flags
//!
//! * `alloc`: Enables APIs which require the
//!   [`alloc`](https://doc.rust-lang.org/stable/alloc/index.html) crate
//!   (e.g. FEN string rendering).
//! * `std`: Implies `alloc`. Enabled by default.
//!   For `no_std` environments, this must be disabled with `default-features = false`.
//! * `variant`: Enables support for all Lichess variants.
//! * `arbitrary`: Implements [`arbitrary::Arbitrary`](https://docs.rs/arbitrary/1/arbitrary/trait.Arbitrary.html)
//!   for vocabulary types.
//! * `proptest`: Implements [`proptest`](https://docs.rs/proptest/1)
//!   strategies for vocabulary types.
//! * `bincode`: Implements [`bincode`](https://docs.rs/bincode/2)
//!   encoding/decoding for vocabulary types.
//!
//!   Changing encodings is considered a semver breaking change and will be
//!   noted in the changelog. But note that in such a case a migration
//!   by unpacking with the previous version may be required.
//! * `serde`: Implements [`serde`](https://docs.rs/serde/1)
//!   serialization/deserialization for types with unique natural
//!   representations.
//! * `nohash-hasher`: Implements
//!   [`nohash_hasher::IsEnabled`](https://docs.rs/nohash-hasher/0.2/nohash_hasher/trait.IsEnabled.html)
//!   for sensible types.
//!
//! # Related crates
//!
//! * [shakmaty-syzygy](https://docs.rs/shakmaty-syzygy)
//! * [pgn-reader](https://docs.rs/pgn-reader)

#![no_std]
#![warn(missing_debug_implementations)]
#![cfg_attr(docs_rs, feature(doc_cfg))]
#![allow(clippy::too_many_arguments)]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

#[macro_use]
mod util;
mod castling_side;
mod color;
mod m;
mod perft;
mod position;
mod role;
mod setup;
mod square;
mod types;

pub mod attacks;
pub mod bitboard;
pub mod board;
pub mod fen;
pub mod packed;
pub mod san;
pub mod uci;
pub mod zobrist;

#[cfg(feature = "variant")]
pub mod variant;

pub use bitboard::Bitboard;
pub use board::Board;
pub use castling_side::{ByCastlingSide, CastlingSide};
pub use color::{ByColor, Color, ParseColorError};
pub use m::{Move, MoveList};
pub use perft::perft;
pub use position::{
    Chess, FromSetup, KnownOutcome, Outcome, ParseOutcomeError, PlayError, Position, PositionError,
    PositionErrorKinds,
};
pub use role::{ByRole, Role};
pub use setup::{Castles, Setup};
pub use square::{File, ParseSquareError, Rank, Square};
pub use types::{CastlingMode, EnPassantMode, Piece, RemainingChecks};

#[cfg(feature = "nohash-hasher")]
impl nohash_hasher::IsEnabled for File {}

#[cfg(feature = "nohash-hasher")]
impl nohash_hasher::IsEnabled for Rank {}

#[cfg(feature = "nohash-hasher")]
impl nohash_hasher::IsEnabled for Square {}

#[cfg(feature = "nohash-hasher")]
impl nohash_hasher::IsEnabled for Role {}

#[cfg(feature = "nohash-hasher")]
impl nohash_hasher::IsEnabled for Color {}
