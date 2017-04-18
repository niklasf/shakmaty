use attacks::Precomp;
use types::Move;
use position::Position;

/// Counts legal move paths of a given length.
///
/// Paths with mate or stalemate are not counted unless it occurs in the final
/// position. Useful for comparing, testing and debugging move generation
/// correctness and performance.
pub fn perft<P: Position>(pos: &P, depth: u8, precomp: &Precomp) -> usize {
    if depth < 1 {
        1
    } else {
        let mut moves: Vec<Move> = Vec::with_capacity(P::MAX_LEGAL_MOVES);
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

pub fn debug_perft<P: Position>(pos: &P, depth: u8, precomp: &Precomp) -> usize {
    if depth < 1 {
        1
    } else {
        let mut moves: Vec<Move> = Vec::with_capacity(P::MAX_LEGAL_MOVES);
        pos.legal_moves(&mut moves, precomp);

        moves.iter().map(|m| {
            let child = pos.clone().do_move(m);
            let nodes = perft(&child, depth - 1, precomp);
            println!("{} {} {}: {}", m.to_uci(), m, depth - 1, nodes);
            nodes
        }).sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    use position::Standard;
    use types::Uci;
    use std::str::FromStr;

    fn do_uci<P: Position>(pos: P, uci: &str, precomp: &Precomp) -> Option<P> {
        Uci::from_str(uci).ok()
            .and_then(|u| pos.validate(&u, precomp))
            .map(|m| pos.do_move(&m))
    }

    #[test]
    fn test_prevented_castling() {
        let precomp = Precomp::new();

        let pos = do_uci(Standard::default(), "g1f3", &precomp)
            .and_then(|p| do_uci(p, "a7a5", &precomp))
            .and_then(|p| do_uci(p, "g2g3", &precomp))
            .unwrap();

        assert_eq!(perft(&pos, 4, &precomp), 282514);

        let pos = do_uci(pos, "d7d6", &precomp).unwrap();
        assert_eq!(perft(&pos, 3, &precomp), 16080);

        let pos = do_uci(pos, "f1h3", &precomp).unwrap();
        assert_eq!(perft(&pos, 2, &precomp), 755);

        let pos = do_uci(pos, "c8h3", &precomp).unwrap();
        assert_eq!(perft(&pos, 1, &precomp), 20);
    }

    #[test]
    fn test_forfeit_castling_rights() {
        let precomp = Precomp::new();

        let pos = do_uci(Standard::default(), "b2b3", &precomp)
            .and_then(|p| do_uci(p, "g8h6", &precomp))
            .and_then(|p| do_uci(p, "c1a3", &precomp))
            .and_then(|p| do_uci(p, "e7e6", &precomp))
            .unwrap();

        assert_eq!(perft(&pos, 4, &precomp), 482138);

        let pos = do_uci(pos, "a3f8", &precomp).unwrap();
        assert_eq!(perft(&pos, 3, &precomp), 16924);

        let pos = do_uci(pos, "e8f8", &precomp).unwrap();
        assert_eq!(perft(&pos, 2, &precomp), 540);

        let pos = do_uci(pos, "a2a3", &precomp).unwrap();
        assert_eq!(perft(&pos, 1, &precomp), 27);
    }

    #[test]
    fn test_en_passant_evasion() {
        let precomp = Precomp::new();
        let pos_a = Standard::from_fen("rb6/5b2/1p2r3/p1k1P3/PpP1p3/2R4P/3P4/1N1K2R1 w - -").unwrap();

        assert_eq!(perft(&pos_a, 1, &precomp), 26);
        assert_eq!(perft(&pos_a, 2, &precomp), 601);

        let pos_a = do_uci(pos_a, "d2d4", &precomp).unwrap();
        assert_eq!(perft(&pos_a, 1, &precomp), 3);

        let pos_b = Standard::from_fen("4k3/1p6/5R1n/4rBp1/K3b3/2pp2P1/7P/1R4N1 b - -").unwrap();
        assert_eq!(perft(&pos_b, 2, &precomp), 762);
    }

    #[bench]
    fn bench_perft(b: &mut Bencher) {
        let precomp = Precomp::new();
        let pos = Standard::default();
        b.iter(|| assert_eq!(perft(&pos, 4, &precomp), 197281));
    }
}
