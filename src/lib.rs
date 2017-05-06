//! A library for chess move generation.
//!
//! # Examples
//!
//! Generate legal moves in the starting position:
//!
//! ```
//! use shakmaty::Chess;
//! use shakmaty::Position;
//! use shakmaty::MoveList;
//!
//! let pos = Chess::default();
//!
//! let mut legals = MoveList::new();
//! pos.legal_moves(&mut legals);
//! assert_eq!(legals.len(), 20);
//! ```
//!
//! Play moves:
//!
//! ```
//! use shakmaty::Move;
//! use shakmaty::Role;
//! use shakmaty::square;
//! # use shakmaty::{Chess, Position};
//! # let pos = Chess::default();
//!
//! let pos = pos.play(&Move::Normal {
//!     role: Role::Pawn,
//!     from: square::E2,
//!     to: square::E4,
//!     capture: None,
//!     promotion: None,
//! }).expect("1. e4 is legal");
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
//!
//! assert_eq!(pos.outcome(), None); // no winner yet
//! ```
//!
//! Also supports FEN, SAN and UCI formats for positions and moves.
#![feature(test)]
#![feature(associated_consts)]
#![feature(cfg_target_feature)]
#![feature(platform_intrinsics)]
#![feature(const_fn)]

extern crate test;

#[macro_use]
extern crate lazy_static;
extern crate arrayvec;
extern crate option_filter;

mod types;
mod bitboard;
mod board;
mod position;
mod setup;

pub mod square;
pub mod attacks;
pub mod perft;
pub mod fen;
pub mod uci;
pub mod san;

pub use square::Square;
pub use types::{Color, Role, Piece, Move, Pockets, Pocket, RemainingChecks};
pub use bitboard::{Bitboard, CarryRippler};
pub use board::Board;
pub use setup::Setup;
pub use position::{MoveList, Outcome, Position, PositionError, Chess};

pub mod variants {
    //! Chess variants.
    pub use Chess;
    pub use position::Crazyhouse;
    pub use position::KingOfTheHill;
    pub use position::Giveaway;
    pub use position::ThreeCheck;
    pub use position::Horde;
    pub use position::Atomic;
    pub use position::RacingKings;
}
