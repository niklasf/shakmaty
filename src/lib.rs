#![feature(test)]
#![feature(associated_consts)]
#![feature(cfg_target_feature)]
#![feature(platform_intrinsics)]

extern crate test;

#[macro_use]
extern crate lazy_static;

extern crate arrayvec;

mod types;
mod bitboard;
mod board;
mod position;
mod setup;

pub mod square;
pub mod attacks;
pub mod perft;
pub mod fen;

pub use square::Square;
pub use types::{Color, Role, Piece, Move, Pockets, Pocket, RemainingChecks};
pub use bitboard::{Bitboard, CarryRippler};
pub use board::Board;
pub use setup::Setup;
pub use position::{PositionError, MoveList, Position, Chess};
