use iai::black_box;
use shakmaty::{
    fen::Fen,
    perft,
    san::San,
    zobrist::{Zobrist64, ZobristHash},
    CastlingMode, Chess, EnPassantMode, Move, Position, Role, Square,
};

fn bench_shallow_perft() {
    let pos = Chess::default();
    assert_eq!(black_box(perft(black_box(&pos), 4)), 197_281);
}

fn bench_deep_perft() {
    let pos = Chess::default();
    assert_eq!(perft(black_box(&pos), 5), 4_865_609);
}

fn bench_kiwipete() {
    let fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ";
    let pos: Chess = fen
        .parse::<Fen>()
        .expect("valid fen")
        .into_position(CastlingMode::Standard)
        .expect("legal position");
    assert_eq!(perft(black_box(&pos), 4), 4_085_603);
}

fn bench_generate_moves() {
    let fen = "rn1qkb1r/pbp2ppp/1p2p3/3n4/8/2N2NP1/PP1PPPBP/R1BQ1RK1 b kq -";
    let pos: Chess = fen
        .parse::<Fen>()
        .expect("valid fen")
        .into_position(CastlingMode::Chess960)
        .expect("legal position");

    assert_eq!(black_box(&pos).legal_moves().len(), 39);
}

fn bench_play_unchecked() -> Chess {
    let fen = "rn1qkb1r/pbp2ppp/1p2p3/3n4/8/2N2NP1/PP1PPPBP/R1BQ1RK1 b kq -";
    let pos: Chess = fen
        .parse::<Fen>()
        .expect("valid fen")
        .into_position(CastlingMode::Chess960)
        .expect("legal position");

    let m = Move::Normal {
        role: Role::Bishop,
        from: Square::F8,
        capture: None,
        to: Square::E7,
        promotion: None,
    };

    let mut pos = black_box(pos);
    pos.play_unchecked(&m);
    pos
}

fn bench_san_candidates() {
    let fen = "r2q1rk1/pb1nbppp/5n2/1p2p3/3NP3/P1NB4/1P2QPPP/R1BR2K1 w - -";
    let pos: Chess = fen
        .parse::<Fen>()
        .expect("valid fen")
        .into_position(CastlingMode::Chess960)
        .expect("legal position");

    let moves = black_box(&pos).san_candidates(Role::Knight, Square::B5);
    assert_eq!(moves.len(), 2);
}

fn bench_play_sans() -> Chess {
    let pgn = [
        "e4", "e5", "Nf3", "Nc6", "Bc4", "Nf6", "Ng5", "d5", "exd5", "Na5", "Bb5+", "c6", "dxc6",
        "bxc6", "Ba4", "Ba6", "d3", "Bc5", "O-O", "O-O", "Nc3", "Qc7", "Nge4", "Be7", "Nxf6+",
        "Bxf6", "Ne4", "Be7", "Re1", "Rad8", "f3", "c5", "Be3", "c4", "Qc1", "cxd3", "cxd3", "Qb8",
        "Nf2", "Bxd3", "Nxd3", "Rxd3", "Qc2", "Rxe3", "Rxe3", "Qb6", "Re1", "Bc5", "Qe4", "f5",
        "Qxe5", "f4", "Qd5+", "Kh8", "Kh1", "Bxe3", "b3", "Qd8", "Rd1", "Qxd5", "Rxd5", "Nb7",
        "b4", "Rd8", "Rxd8+", "Nxd8", "Bd7", "Kg8", "a4", "Kf8", "g4", "Ke7", "Bf5", "h6", "h4",
        "Nf7", "h5", "Nd6", "Bd3", "Ke6", "Kg2", "Kd5", "Kh3", "Nf7", "b5", "Bb6", "Kg2", "Kc5",
        "Kf1", "Ne5", "Be2", "Kb4", "Bd1", "Nc4", "Ke2", "Ne3", "g5", "hxg5", "Kd2", "Nxd1",
        "Kxd1", "Kxa4", "Kd2", "Kxb5", "Kd3", "a5", "Ke4", "a4", "Kf5", "a3", "h6", "gxh6",
    ];

    let mut pos = black_box(Chess::default());
    for san in black_box(pgn).iter() {
        let m = san
            .parse::<San>()
            .expect("valid san")
            .to_move(&pos)
            .expect("legal move");

        pos.play_unchecked(&m);
    }
    pos
}

fn bench_zobrist_hash() -> Zobrist64 {
    black_box(Chess::default()).zobrist_hash(EnPassantMode::Legal)
}

iai::main!(
    bench_shallow_perft,
    bench_deep_perft,
    bench_kiwipete,
    bench_generate_moves,
    bench_play_unchecked,
    bench_san_candidates,
    bench_play_sans,
    bench_zobrist_hash,
);
