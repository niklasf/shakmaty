use bencher::{benchmark_group, benchmark_main, black_box, Bencher};
use shakmaty::{fen::Fen, CastlingMode, Chess};
use shakmaty_syzygy::{Tablebase, Wdl};

fn bench_add_directory(bench: &mut Bencher) {
    bench.iter(|| {
        let mut tablebase = Tablebase::<Chess>::new();
        tablebase
            .add_directory("tables/chess")
            .expect("readable directory");
        tablebase
    });
}

fn bench_probe_wdl(bench: &mut Bencher) {
    let mut tb = Tablebase::new();
    tb.add_directory("tables/chess")
        .expect("readable directory");

    let pos = "2q5/6NR/8/8/8/8/5k2/K6Q b - - 0 1"
        .parse::<Fen>()
        .expect("valid fen")
        .position::<Chess>(CastlingMode::Chess960)
        .expect("legal position");

    bench.iter(|| {
        assert!(matches!(
            tb.probe_wdl(black_box(&pos)),
            Ok(Wdl::BlessedLoss)
        ));
    });
}

benchmark_group!(benches, bench_add_directory, bench_probe_wdl);

benchmark_main!(benches);
