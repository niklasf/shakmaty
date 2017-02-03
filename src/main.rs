#![feature(associated_consts)]
#![feature(cfg_target_feature)]
#![feature(asm)]
#![feature(test)]
#![feature(field_init_shorthand)]

#![allow(dead_code)]  // TODO: Remove

extern crate rayon;
extern crate test;

mod square;
mod types;
mod bitboard;
mod attacks;
mod board;

use rayon::prelude::*;

use types::Move;
use attacks::Precomp;
use board::Board;

fn perft_inner(board: &Board, depth: i8, precomp: &Precomp) -> usize {
    if depth < 1 {
        1
    } else {
        let mut moves: Vec<Move> = Vec::new();
        board.legal_moves(&mut moves, precomp);

        if depth == 1 {
            moves.len()
        } else {
            moves.iter().map(|m| {
                let mut child = board.clone();
                child.do_move(m);
                perft_inner(&child, depth - 1, precomp)
            }).sum()
        }
    }
}

fn perft(board: &Board, depth: i8, precomp: &Precomp) -> usize {
    if depth < 1 {
        1
    } else {
        let mut moves: Vec<Move> = Vec::new();
        board.legal_moves(&mut moves, precomp);

        moves.par_iter().map(|m| {
            let mut child = board.clone();
            child.do_move(m);
            let p = perft_inner(&child, depth - 1, precomp);
            println!("{} {} {}", depth, m, p);
            p
        }).sum()
    }
}

fn main() {
    let precomp = attacks::Precomp::new();
    let mut board = Board::new();

    /* assert_eq!(perft(&board, 1, &precomp), 20);
    assert_eq!(perft(&board, 2, &precomp), 400);
    assert_eq!(perft(&board, 3, &precomp), 8902);
    assert_eq!(perft(&board, 4, &precomp), 197281);
    assert_eq!(perft(&board, 5, &precomp), 4865609); */
    assert_eq!(perft(&board, 6, &precomp), 119060324);
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn test_perft() {
        let precomp = attacks::Precomp::new();
        let mut board = Board::new();

        board.do_move(&Move::from_uci("h2h4").unwrap());
        board.do_move(&Move::from_uci("g7g5").unwrap());
        board.do_move(&Move::from_uci("e2e4").unwrap());
        assert_eq!(perft_inner(&board, 2, &precomp), 699);

        board.do_move(&Move::from_uci("g5h4").unwrap());
        assert_eq!(perft_inner(&board, 1, &precomp), 31);
    }

    #[test]
    fn test_duplicate_evasions() {
        let precomp = attacks::Precomp::new();
        let mut board = Board::new();

        board.do_move(&Move::from_uci("b2b3").unwrap());
        board.do_move(&Move::from_uci("e7e6").unwrap());
        assert_eq!(perft_inner(&board, 4, &precomp), 438837);

        board.do_move(&Move::from_uci("c1b2").unwrap());
        assert_eq!(perft_inner(&board, 3, &precomp), 24465);

        board.do_move(&Move::from_uci("e8e7").unwrap());
        assert_eq!(perft_inner(&board, 2, &precomp), 560);

        board.do_move(&Move::from_uci("b2f6").unwrap());
        assert_eq!(perft_inner(&board, 1, &precomp), 5);
    }

    #[bench]
    fn bench_perft(b: &mut Bencher) {
        let precomp = attacks::Precomp::new();
        let board = Board::new();
        b.iter(|| assert_eq!(perft_inner(&board, 4, &precomp), 197281));
    }
}
