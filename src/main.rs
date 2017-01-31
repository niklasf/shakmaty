#![feature(associated_consts)]
#![feature(cfg_target_feature)]
#![feature(asm)]

#![allow(dead_code)]  // Remove

mod square;
mod bitboard;
mod attacks;
mod board;

use board::Board;

fn main() {
    let precomp = attacks::Precomp::new();

    let board = Board::new();
    board.pseudo_legal_moves(&precomp);
}
