use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use shakmaty::{CastlingMode, Chess, fen::Fen};
use shakmaty_syzygy::{AmbiguousWdl, Tablebase};

fn bench_add_directory(c: &mut Criterion) {
    c.bench_function("add_directory", |b| {
        b.iter(|| {
            let mut tablebase = Tablebase::<Chess>::new();
            tablebase
                .add_directory("tables/chess")
                .expect("readable directory");
            tablebase
        })
    });
}

fn bench_probe_wdl(c: &mut Criterion) {
    let mut tb = Tablebase::new();

    tb.add_directory("tables/chess")
        .expect("readable directory");

    let pos = "2q5/6NR/8/8/8/8/5k2/K6Q b - - 0 1"
        .parse::<Fen>()
        .expect("valid fen")
        .into_position::<Chess>(CastlingMode::Chess960)
        .expect("legal position");

    c.bench_function("probe_wdl", |b| {
        b.iter(|| {
            assert!(matches!(
                tb.probe_wdl(black_box(&pos)),
                Ok(AmbiguousWdl::BlessedLoss)
            ));
        })
    });
}

#[cfg(all(feature = "mmap", target_pointer_width = "64"))]
fn bench_probe_wdl_mmap(c: &mut Criterion) {
    // Safety: No modifications to table files and I/O errors please.
    // Fingers crossed.
    let mut tb = unsafe { Tablebase::with_mmap_filesystem() };

    tb.add_directory("tables/chess")
        .expect("readable directory");

    let pos = "2q5/6NR/8/8/8/8/5k2/K6Q b - - 0 1"
        .parse::<Fen>()
        .expect("valid fen")
        .into_position::<Chess>(CastlingMode::Chess960)
        .expect("legal position");

    c.bench_function("probe_wdl_mmap", |b| {
        b.iter(|| {
            assert!(matches!(
                tb.probe_wdl(black_box(&pos)),
                Ok(AmbiguousWdl::BlessedLoss)
            ));
        })
    });
}

#[cfg(not(all(feature = "mmap", target_pointer_width = "64")))]
criterion_group!(benches, bench_add_directory, bench_probe_wdl);
#[cfg(all(feature = "mmap", target_pointer_width = "64"))]
criterion_group!(
    benches,
    bench_add_directory,
    bench_probe_wdl,
    bench_probe_wdl_mmap
);

criterion_main!(benches);
