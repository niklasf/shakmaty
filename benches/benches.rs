use iai::black_box;
use shakmaty::{
    fen::Fen,
    packed::{PackedSetup, PackedUciMove},
    perft,
    san::San,
    san::SanPlus,
    uci::UciMove,
    zobrist::{Zobrist64, ZobristHash},
    CastlingMode, Chess, EnPassantMode, Move, Position, Role, Setup, Square,
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

fn bench_play_unchecked() -> Chess {
    let m = black_box(Move::Normal {
        role: Role::Knight,
        from: Square::G1,
        capture: None,
        to: Square::F3,
        promotion: None,
    });

    let mut pos = black_box(Chess::default());
    pos.play_unchecked(m);
    pos
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
    for san in black_box(pgn) {
        let m = san
            .parse::<San>()
            .expect("valid san")
            .to_move(&pos)
            .expect("legal move");

        pos.play_unchecked(m);
    }
    pos
}

fn bench_san_roundtrip() {
    let pgn = [
        "Nf3", "g6", "d4", "d5", "c4", "c6", "cxd5", "cxd5", "Nc3", "Nf6", "Bf4", "Nc6", "e3",
        "Bg7", "Be2", "O-O", "Ne5", "Ne4", "Nxe4", "dxe4", "Nxc6", "bxc6", "O-O", "b3", "c5",
        "Rc1", "cxd4", "exd4", "Qxd4", "Be3", "Qxd1", "Rfxd1", "a5", "Rc7", "a4", "Rxe7", "axb3",
        "axb3", "Bxb3", "Re1", "Be6", "h3", "Rfe8", "Rc7", "Rec8", "Rcc1", "Rxc1", "Rxc1", "Ra1",
        "Bd2", "Rxe1+", "Bxe1", "Bd4", "Kf1", "f5", "f3", "e3", "f4", "Kg7", "g3", "Bd5", "h4",
        "Kf6", "Bb4", "h5", "Be1", "Ke6", "Bb4", "Be4", "Ba5", "Kd5", "Bb4", "Bc5", "Be1", "Kd4",
        "Ba5", "Bd3", "Be1", "Ke4", "Bc3", "Bd4", "Be1", "Bxe2+", "Kxe2", "Bc5", "Bc3", "Bd4",
        "Be1", "Bb4", "Be7",
    ];
    for san in black_box(pgn) {
        assert_eq!(san.parse::<SanPlus>().expect("valid san").to_string(), san);
    }
}

fn bench_zobrist_hash() -> Zobrist64 {
    black_box(Chess::default()).zobrist_hash(EnPassantMode::Legal)
}

fn bench_fen_roundtrip() -> String {
    let mut buffer = String::new();
    black_box("rnbqkb1r/1p3ppp/p2p1n2/4p3/3NP3/2N1B3/PPP2PPP/R2QKB1R w KQkq - 0 7")
        .parse::<Fen>()
        .expect("valid fen")
        .append_to_string(&mut buffer);
    buffer
}

fn bench_packed_setup_roundtrip() {
    let setup = black_box(Setup::default());
    let packed = PackedSetup::pack_standard(&setup).expect("representable");
    let repacked = PackedSetup::try_from_bytes(packed.as_bytes()).expect("repacked");
    assert_eq!(repacked.unpack_standard().expect("roundtrip"), setup);
}

fn bench_packed_uci_roundtrip() {
    for from in Square::ALL {
        for to in Square::ALL {
            let uci = UciMove::Normal {
                from,
                to,
                promotion: None,
            };
            assert_eq!(
                black_box(black_box(PackedUciMove::pack(black_box(uci))).unpack()),
                uci
            );
        }
    }
}

iai::main!(
    bench_shallow_perft,
    bench_deep_perft,
    bench_kiwipete,
    bench_play_unchecked,
    bench_play_sans,
    bench_zobrist_hash,
    bench_san_roundtrip,
    bench_fen_roundtrip,
    bench_packed_setup_roundtrip,
    bench_packed_uci_roundtrip,
);
