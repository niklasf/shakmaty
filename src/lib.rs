//! A library for chess move generation.
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
//! let pos = pos.play(&Move::Normal {
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
//! # use shakmaty::{Chess, Position};
//! # let pos = Chess::default();
//! assert!(!pos.is_checkmate());
//! assert!(!pos.is_stalemate());
//! assert!(!pos.is_insufficient_material());
//! assert_eq!(pos.outcome(), None); // no winner yet
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
//! * `std`: Implements the
//!   [`std::error::Error`](https://doc.rust-lang.org/stable/std/error/trait.Error.html)
//!   trait for various errors in the crate.
//!   Implies the `alloc` feature (since `std` depends on `alloc` anyway).
//!   Enabled by default for convenience. For `no_std` environments, this must
//!   be disabled with `default-features = false`.
//! * `variant`: Enables support for all Lichess variants.
//! * `nohash-hasher`: Implements
//!   [`nohash_hasher::IsEnabled`](https://docs.rs/nohash-hasher/0.2/nohash_hasher/trait.IsEnabled.html)
//!   for sensible types.

#![no_std]
#![doc(html_root_url = "https://docs.rs/shakmaty/0.26.0")]
#![forbid(unsafe_op_in_unsafe_fn)]
#![warn(missing_debug_implementations)]
#![cfg_attr(docs_rs, feature(doc_auto_cfg))]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

mod bootstrap;
mod color;
mod magics;
mod movelist;
mod perft;
mod position;
mod role;
mod setup;
mod square;
mod types;
mod util;

pub mod attacks;
pub mod bitboard;
pub mod board;
pub mod fen;
pub mod san;
pub mod uci;
pub mod zobrist;

#[cfg(feature = "variant")]
pub mod variant;

pub use bitboard::Bitboard;
pub use board::Board;
pub use color::{ByColor, Color, ParseColorError};
pub use movelist::MoveList;
pub use perft::perft;
pub use position::{
    Chess, FromSetup, Outcome, ParseOutcomeError, PlayError, Position, PositionError,
    PositionErrorKinds,
};
pub use role::{ByRole, Role};
pub use setup::{Castles, Setup};
pub use square::{File, ParseSquareError, Rank, Square};
pub use types::{CastlingMode, CastlingSide, EnPassantMode, Move, Piece, RemainingChecks};

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
