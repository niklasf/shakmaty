#[macro_use]
extern crate bencher;
extern crate shakmaty;
extern crate shakmaty_syzygy;

use bencher::Bencher;
use shakmaty::Chess;
use shakmaty_syzygy::Tablebase;

fn bench_add_directory(bench: &mut Bencher) {
    bench.iter(|| {
        let mut tablebase = Tablebase::<Chess>::new();
        tablebase.add_directory("tables/regular").expect("readable directory");
        tablebase
    });
}

benchmark_group!(benches,
    bench_add_directory);

benchmark_main!(benches);
