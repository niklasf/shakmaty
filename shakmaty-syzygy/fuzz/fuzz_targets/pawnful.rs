#![no_main]

use libfuzzer_sys::fuzz_target;
use shakmaty::{fen::Fen, CastlingMode, Chess, Position};
use shakmaty_syzygy::{Material, WdlTable};

fuzz_target!(|data: &[u8]| {
    let pos: Chess = "8/2K5/8/8/8/8/3p4/1k2N3 b - - 0 1" // KNvKP
        .parse::<Fen>()
        .expect("valid fen")
        .into_position(CastlingMode::Standard)
        .expect("valid position");

    if let Ok(table) = WdlTable::new(data, &Material::from_board(pos.board())) {
        let _ = table.probe_wdl(&pos);
    }
});
