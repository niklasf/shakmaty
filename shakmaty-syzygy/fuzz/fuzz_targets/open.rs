#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate shakmaty;
extern crate shakmaty_syzygy;
extern crate tempfile;

use std::fs::File;
use std::io::Write;

use shakmaty::Chess;
use shakmaty::fen::Fen;
use shakmaty_syzygy::{Tablebase, Syzygy};

fuzz_target!(|data: &[u8]| {
    let dir = tempfile::tempdir().expect("tempdir");
    let table_path = dir.path().join("KQvK.rtbw");

    let mut table_file = File::create(table_path).expect("create file");
    //table_file.write_all(&Chess::TBW.magic).expect("write magic");
    table_file.write_all(data).expect("write");

    let mut tablebase = Tablebase::new();
    tablebase.add_directory(dir.path()).expect("add directory");

    let pos: Chess = "8/2K5/8/8/8/8/3p4/1k2N3 b - - 0 1"
        .parse::<Fen>()
        .expect("valid fen")
        .position()
        .expect("valid position");

    let _ = tablebase.probe_wdl(&pos);
});
