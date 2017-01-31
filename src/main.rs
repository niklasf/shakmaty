#![feature(associated_consts)]
#![feature(cfg_target_feature)]
#![feature(asm)]
#![feature(field_init_shorthand)]

#![allow(dead_code)]  // Remove

mod square;
mod bitboard;
mod attacks;
mod board;

use board::Board;
use board::Move;
use attacks::Precomp;

fn perft(board: Board, depth: i8, precomp: &Precomp) -> usize {
    if depth < 1 {
        1
    } else {
        let mut moves: Vec<Move> = Vec::new();
        board.pseudo_legal_moves(&mut moves, precomp);

        moves.iter().map(|m| {
            let mut child = board.clone();
            child.do_move(m);
            perft(child, depth - 1, precomp)
        }).sum()
    }
}

fn main() {
    let precomp = attacks::Precomp::new();
    println!("{:}", perft(Board::new(), 2, &precomp));
}
