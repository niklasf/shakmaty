#[macro_use]
extern crate bencher;
extern crate shakmaty;

use bencher::{black_box, Bencher};
use shakmaty::{perft, Chess, Color, Move, MoveList, Position, Role, Setup, Square};
use shakmaty::san::San;
use shakmaty::fen::Fen;

fn bench_shallow_perft(b: &mut Bencher) {
    let pos = Chess::default();
    b.iter(|| assert_eq!(perft(&pos, black_box(4)), 197_281));
}

fn bench_deep_perft(b: &mut Bencher) {
    let pos = Chess::default();
    b.iter(|| assert_eq!(perft(&pos, black_box(5)), 4_865_609));
}

fn bench_parse_san_move_complicated(b: &mut Bencher) {
    b.iter(|| {
        assert_eq!(San::from_bytes(black_box(b"bxc1=R+")), Ok(San::Normal {
            role: Role::Pawn,
            file: Some(1),
            rank: None,
            capture: true,
            to: Square::C1,
            promotion: Some(Role::Rook),
        }));
    });
}

fn bench_generate_moves(b: &mut Bencher) {
    let fen = "rn1qkb1r/pbp2ppp/1p2p3/3n4/8/2N2NP1/PP1PPPBP/R1BQ1RK1 b kq -";
    let pos: Chess = fen.parse::<Fen>()
        .expect("valid fen")
        .position()
        .expect("legal position");

    b.iter(|| {
        let mut moves = MoveList::new();
        black_box(&pos).legal_moves(&mut moves);
        assert_eq!(moves.len(), 39);
    });
}

fn bench_play_unchecked(b: &mut Bencher) {
    let fen = "rn1qkb1r/pbp2ppp/1p2p3/3n4/8/2N2NP1/PP1PPPBP/R1BQ1RK1 b kq -";
    let pos: Chess = fen.parse::<Fen>()
        .expect("valid fen")
        .position()
        .expect("legal position");

    let m = Move::Normal {
        role: Role::Bishop,
        from: Square::F8,
        capture: None,
        to: Square::E7,
        promotion: None,
    };

    b.iter(|| {
        let mut pos = black_box(pos.clone());
        pos.play_unchecked(&m);
        assert_eq!(pos.turn(), Color::White);
    });
}

fn bench_san_candidates(b: &mut Bencher) {
    let fen = "r2q1rk1/pb1nbppp/5n2/1p2p3/3NP3/P1NB4/1P2QPPP/R1BR2K1 w - -";
    let pos: Chess = fen.parse::<Fen>()
        .expect("valid fen")
        .position()
        .expect("legal position");

    b.iter(|| {
        let mut moves = MoveList::new();
        black_box(&pos).san_candidates(Role::Knight, Square::B5, &mut moves);
        assert_eq!(moves.len(), 2);
    });
}

benchmark_group!(benches,
    bench_shallow_perft,
    bench_deep_perft,
    bench_parse_san_move_complicated,
    bench_generate_moves,
    bench_play_unchecked,
    bench_san_candidates);

benchmark_main!(benches);
