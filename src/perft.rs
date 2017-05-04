//! Count legal move paths.
//!
//! # Examples
//!
//! ```
//! use shakmaty::Chess;
//! use shakmaty::perft::perft;
//!
//! let pos = Chess::default();
//! assert_eq!(perft(&pos, 1), 20);
//! assert_eq!(perft(&pos, 2), 400);
//! assert_eq!(perft(&pos, 3), 8902);
//! ```

use position::{Position, MoveList};
use uci::Uci;

/// Counts legal move paths of a given length.
///
/// Paths with mate or stalemate are not counted unless it occurs in the final
/// position. Useful for comparing, testing and debugging move generation
/// correctness and performance.
pub fn perft<P: Position>(pos: &P, depth: u8) -> usize {
    if depth < 1 {
        1
    } else {
        let mut moves = MoveList::new();
        pos.legal_moves(&mut moves);

        if depth == 1 {
            moves.len()
        } else {
            moves.drain(..).map(|ref m| {
                let child = pos.clone().play_unchecked(m);
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
        let mut moves = MoveList::new();
        pos.legal_moves(&mut moves);

        moves.iter().map(|m| {
            let child = pos.clone().play(m).expect("legal move");
            let nodes = perft(&child, depth - 1);
            let uci: Uci = m.into();
            println!("{} {} {}: {}", uci, m, depth - 1, nodes);
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
    fn bench_shallow_perft(b: &mut Bencher) {
        let pos = Chess::default();
        b.iter(|| assert_eq!(perft(&pos, 4), 197281));
    }

    #[bench]
    fn bench_deep_perft(b: &mut Bencher) {
        let pos = Chess::default();
        b.iter(|| assert_eq!(perft(&pos, 5), 4865609));
    }
}
