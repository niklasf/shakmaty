#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate shakmaty;
extern crate shakmaty_syzygy;

use shakmaty::{Chess, Setup};
use shakmaty::fen::Fen;
use shakmaty_syzygy::{WdlTable, Material};

fuzz_target!(|data: &[u8]| {
    let pos: Chess = "8/2K5/8/8/8/8/3p4/1k2N3 b - - 0 1" // KNvKP
        .parse::<Fen>()
        .expect("valid fen")
        .position()
        .expect("valid position");

    if let Ok(table) = WdlTable::new(data, &Material::from_board(pos.board())) {
        let _ = table.probe_wdl_table(&pos);
    }
});
