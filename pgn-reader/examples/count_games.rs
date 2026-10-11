// Counts games in PGNs, skipping over their contents.
// Usage: cargo run --release --example count_games -- [PGN]...

use std::{env, fs::File, io};

use pgn_reader::Reader;

fn main() -> Result<(), io::Error> {
    for arg in env::args().skip(1) {
        let file = File::open(&arg)?;

        let uncompressed: Box<dyn io::Read> = if arg.ends_with(".zst") {
            Box::new(zstd::Decoder::new(file)?)
        } else {
            Box::new(file)
        };

        let mut reader = Reader::new(uncompressed);

        let mut games = 0;
        while reader.skip_game()? {
            games += 1;
        }
        println!("{arg}: {games}");
    }

    Ok(())
}
