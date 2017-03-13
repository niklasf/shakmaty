#![feature(test)]
#![feature(associated_consts)]
#![feature(cfg_target_feature)]
#![feature(platform_intrinsics)]

extern crate test;

pub mod square;
pub mod types;
mod bitboard;
mod attacks;
pub mod board;
pub mod position;

pub use bitboard::{Bitboard, CarryRippler};
pub use position::{Position, Standard};
use types::Move;
pub use attacks::Precomp;

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    fn perft<P: Position>(pos: &P, depth: i8, precomp: &Precomp) -> usize {
        if depth < 1 {
            1
        } else {
            let mut moves: Vec<Move> = Vec::new();
            pos.legal_moves(&mut moves, precomp);

            if depth == 1 {
                moves.len()
            } else {
                moves.iter().map(|m| {
                    let child = pos.clone().do_move(m);
                    perft(&child, depth - 1, precomp)
                }).sum()
            }
        }
    }

    #[test]
    fn test_perft() {
        let precomp = attacks::Precomp::new();

        let pos = Standard::default()
            .do_move(&Move::from_uci("h2h4").unwrap())
            .do_move(&Move::from_uci("g7g5").unwrap())
            .do_move(&Move::from_uci("e2e4").unwrap());

        assert_eq!(perft(&pos, 2, &precomp), 699);

        let pos = pos.do_move(&Move::from_uci("g5h4").unwrap());
        assert_eq!(perft(&pos, 1, &precomp), 31);
    }

    #[test]
    fn test_duplicate_evasions() {
        let precomp = attacks::Precomp::new();

        let pos = Standard::default()
            .do_move(&Move::from_uci("b2b3").unwrap())
            .do_move(&Move::from_uci("e7e6").unwrap());

        assert_eq!(perft(&pos, 4, &precomp), 438837);

        let pos = pos.do_move(&Move::from_uci("c1b2").unwrap());
        assert_eq!(perft(&pos, 3, &precomp), 24465);

        let pos = pos.do_move(&Move::from_uci("e8e7").unwrap());
        assert_eq!(perft(&pos, 2, &precomp), 560);

        let pos = pos.do_move(&Move::from_uci("b2f6").unwrap());
        assert_eq!(perft(&pos, 1, &precomp), 5);
    }

    #[test]
    fn test_en_passant() {
        let precomp = attacks::Precomp::new();

        let pos = Standard::default()
            .do_move(&Move::from_uci("a2a4").unwrap())
            .do_move(&Move::from_uci("h7h6").unwrap());

        assert_eq!(perft(&pos, 4, &precomp), 199242);

        let pos = pos.do_move(&Move::from_uci("a4a5").unwrap());
        assert_eq!(perft(&pos, 3, &precomp), 8189);

        let pos = pos.do_move(&Move::from_uci("b7b5").unwrap());
        assert_eq!(perft(&pos, 2, &precomp), 439);

        let pos = pos.do_move(&Move::from_uci("a5b6").unwrap());
        assert_eq!(perft(&pos, 1, &precomp), 21);
    }

    #[test]
    fn test_prevented_castling() {
        let precomp = attacks::Precomp::new();

        let pos = Standard::default()
            .do_move(&Move::from_uci("g1f3").unwrap())
            .do_move(&Move::from_uci("a7a5").unwrap())
            .do_move(&Move::from_uci("g2g3").unwrap());

        assert_eq!(perft(&pos, 4, &precomp), 282514);

        let pos = pos.do_move(&Move::from_uci("d7d6").unwrap());
        assert_eq!(perft(&pos, 3, &precomp), 16080);

        let pos = pos.do_move(&Move::from_uci("f1h3").unwrap());
        assert_eq!(perft(&pos, 2, &precomp), 755);

        let pos = pos.do_move(&Move::from_uci("c8h3").unwrap());
        assert_eq!(perft(&pos, 1, &precomp), 20);
    }

    #[test]
    fn test_forfeit_castling_rights() {
        let precomp = attacks::Precomp::new();

        let pos = Standard::default()
            .do_move(&Move::from_uci("b2b3").unwrap())
            .do_move(&Move::from_uci("g8h6").unwrap())
            .do_move(&Move::from_uci("c1a3").unwrap())
            .do_move(&Move::from_uci("e7e6").unwrap());

        assert_eq!(perft(&pos, 4, &precomp), 482138);

        let pos = pos.do_move(&Move::from_uci("a3f8").unwrap());
        assert_eq!(perft(&pos, 3, &precomp), 16924);

        let pos = pos.do_move(&Move::from_uci("e8f8").unwrap());
        assert_eq!(perft(&pos, 2, &precomp), 540);

        let pos = pos.do_move(&Move::from_uci("a2a3").unwrap());
        assert_eq!(perft(&pos, 1, &precomp), 27);
    }

    #[test]
    fn test_en_passant_evasion() {
        let precomp = attacks::Precomp::new();
        let pos_a = Standard::from_fen("rb6/5b2/1p2r3/p1k1P3/PpP1p3/2R4P/3P4/1N1K2R1 w - -").unwrap();

        assert_eq!(perft(&pos_a, 2, &precomp), 601);

        let pos_a = pos_a.do_move(&Move::from_uci("d2d4").unwrap());
        assert_eq!(perft(&pos_a, 1, &precomp), 3);

        let pos_b = Standard::from_fen("4k3/1p6/5R1n/4rBp1/K3b3/2pp2P1/7P/1R4N1 b - -").unwrap();
        assert_eq!(perft(&pos_b, 2, &precomp), 762);
    }

    #[bench]
    fn bench_perft(b: &mut Bencher) {
        let precomp = attacks::Precomp::new();
        let pos = Standard::default();
        b.iter(|| assert_eq!(perft(&pos, 4, &precomp), 197281));
    }
}
