// This file is part of the shakmaty library.
// Copyright (C) 2017 Niklas Fiekas <niklas.fiekas@backscattering.de>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.

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
    use position::{Chess, Atomic, Giveaway};
    use fen::Fen;

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

    #[bench]
    fn bench_atomic(b: &mut Bencher) {
        let fen = "rn2kb1r/1pp1p2p/p2q1pp1/3P4/2P3b1/4PN2/PP3PPP/R2QKB1R b KQkq -";

        let pos: Atomic = fen
            .parse::<Fen>().expect("valid fen")
            .position().expect("legal atomic position");

        b.iter(|| {
            assert_eq!(perft(&pos, 1), 40);
            assert_eq!(perft(&pos, 2), 1238);
            assert_eq!(perft(&pos, 3), 45237);
        });
    }

    #[bench]
    fn bench_giveaway(b: &mut Bencher) {
        let fen = "rnbqk2r/pppppp1p/6p1/8/6B1/3P2P1/PPP1PP1P/RN1QK1NR b - -";

        let pos: Giveaway = fen
            .parse::<Fen>().expect("valid fen")
            .position().expect("legal giveaway position");

        b.iter(|| {
            assert_eq!(perft(&pos, 1), 20);
            assert_eq!(perft(&pos, 2), 21);
            assert_eq!(perft(&pos, 3), 68);
            assert_eq!(perft(&pos, 4), 1564);
            assert_eq!(perft(&pos, 5), 26823);
            assert_eq!(perft(&pos, 6), 582484);
        });
    }
}
