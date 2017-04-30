//! Count legal move paths.

use types::Move;
use position::Position;
use variant::Variant;

const LEGAL_MOVES_HINT: usize = 255;

/// Counts legal move paths of a given length.
///
/// Paths with mate or stalemate are not counted unless it occurs in the final
/// position. Useful for comparing, testing and debugging move generation
/// correctness and performance.
pub fn perft<V: Variant>(pos: &V, depth: u8) -> usize {
    if depth < 1 {
        1
    } else {
        let mut moves: Vec<Move> = Vec::with_capacity(LEGAL_MOVES_HINT);
        pos.legal_moves(&mut moves);

        if depth == 1 {
            moves.len()
        } else {
            moves.iter().map(|m| {
                let child = pos.clone().do_move(m);
                perft(&child, depth - 1)
            }).sum()
        }
    }
}

/* pub fn debug_perft(pos: &Position, depth: u8) -> usize {
    if depth < 1 {
        1
    } else {
        let mut moves: Vec<Move> = Vec::with_capacity(LEGAL_MOVES_HINT);
        pos.legal_moves(&mut moves);

        moves.iter().map(|m| {
            let child = pos.clone().do_move(m);
            let nodes = perft(&child, depth - 1);
            println!("{} {} {}: {}", m.to_uci(), m, depth - 1, nodes);
            nodes
        }).sum()
    }
} */

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    use types::Uci;
    use std::str::FromStr;
    use variant::Standard;

    /* fn do_uci(pos: Position, uci: &str) -> Option<Position> {
        Uci::from_str(uci).ok()
            .and_then(|u| pos.validate(&u))
            .map(|m| pos.do_move(&m))
    }

    #[test]
    fn test_prevented_castling() {
        let pos = do_uci(Position::default(), "g1f3")
            .and_then(|p| do_uci(p, "a7a5"))
            .and_then(|p| do_uci(p, "g2g3"))
            .unwrap();

        assert_eq!(perft(&pos, 4), 282514);

        let pos = do_uci(pos, "d7d6").unwrap();
        assert_eq!(perft(&pos, 3), 16080);

        let pos = do_uci(pos, "f1h3").unwrap();
        assert_eq!(perft(&pos, 2), 755);

        let pos = do_uci(pos, "c8h3").unwrap();
        assert_eq!(perft(&pos, 1), 20);
    }

    #[test]
    fn test_forfeit_castling_rights() {
        let pos = do_uci(Position::default(), "b2b3")
            .and_then(|p| do_uci(p, "g8h6"))
            .and_then(|p| do_uci(p, "c1a3"))
            .and_then(|p| do_uci(p, "e7e6"))
            .unwrap();

        assert_eq!(perft(&pos, 4), 482138);

        let pos = do_uci(pos, "a3f8").unwrap();
        assert_eq!(perft(&pos, 3), 16924);

        let pos = do_uci(pos, "e8f8").unwrap();
        assert_eq!(perft(&pos, 2), 540);

        let pos = do_uci(pos, "a2a3").unwrap();
        assert_eq!(perft(&pos, 1), 27);
    }

    #[test]
    fn test_en_passant_evasion() {
        let pos_a = Position::from_fen("rb6/5b2/1p2r3/p1k1P3/PpP1p3/2R4P/3P4/1N1K2R1 w - -").unwrap();

        assert_eq!(perft(&pos_a, 1), 26);
        assert_eq!(perft(&pos_a, 2), 601);

        let pos_a = do_uci(pos_a, "d2d4").unwrap();
        assert_eq!(perft(&pos_a, 1), 3);

        let pos_b = Position::from_fen("4k3/1p6/5R1n/4rBp1/K3b3/2pp2P1/7P/1R4N1 b - -").unwrap();
        assert_eq!(perft(&pos_b, 2), 762);
    } */

    #[bench]
    fn bench_perft(b: &mut Bencher) {
        let pos = Standard::default();
        b.iter(|| assert_eq!(perft(&pos, 4), 197281));
    }
}
