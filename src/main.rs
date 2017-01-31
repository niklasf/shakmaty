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
    let board = Board::new();

    let bb = board.select()
        .side(board::Color::White)
        .get();

    println!("{:?}", bb);

    println!("---");

    println!("{:?}", board.by_type(board::Role::Knight));
}
