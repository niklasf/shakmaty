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

pub use square::Square;
pub use types::{Color, Role, Piece, Move, Pockets, Pocket, RemainingChecks};
pub use bitboard::{Bitboard, CarryRippler};
pub use board::Board;
pub use setup::Setup;
pub use position::{MoveList, Position, PositionError, Chess};

pub mod variants {
    //! Chess variants.
    pub use Chess;
    pub use position::Crazyhouse;
    pub use position::KingOfTheHill;
    pub use position::Giveaway;
    pub use position::ThreeCheck;
}
