//! Count legal move paths.

use types::Move;
use position::Position;

/// Counts legal move paths of a given length.
///
/// Paths with mate or stalemate are not counted unless it occurs in the final
/// position. Useful for comparing, testing and debugging move generation
/// correctness and performance.
pub fn perft<P: Position>(pos: &P, depth: u8) -> usize {
    if depth < 1 {
        1
    } else {
        let mut moves: Vec<Move> = Vec::with_capacity(P::MAX_LEGAL_MOVES);
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

/// Like `perft()`, but also prints the perft of each child for debugging.
pub fn debug_perft<P: Position>(pos: &P, depth: u8) -> usize {
    if depth < 1 {
        1
    } else {
        let mut moves: Vec<Move> = Vec::with_capacity(P::MAX_LEGAL_MOVES);
        pos.legal_moves(&mut moves);

        moves.iter().map(|m| {
            let child = pos.clone().do_move(m);
            let nodes = perft(&child, depth - 1);
            println!("{} {} {}: {}", m.to_uci(), m, depth - 1, nodes);
            nodes
        }).sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;
    use position::Chess;

    #[bench]
    fn bench_perft(b: &mut Bencher) {
        let pos = Chess::default();
        b.iter(|| assert_eq!(perft(&pos, 4), 197281));
    }
}
