#![no_main]

use std::{
    io,
    path::{Path, PathBuf},
};

use futures_util::future::FutureExt as _;
use libfuzzer_sys::fuzz_target;
use shakmaty::{CastlingMode, Chess, fen::Fen};
use shakmaty_syzygy::aio::{Filesystem, RandomAccessFile, ReadHint, Tablebase};

struct FakeFilesystem<'a> {
    data: &'a [u8],
}

impl<'a> Filesystem for FakeFilesystem<'a> {
    type RandomAccessFile = FakeFile<'a>;

    async fn read_dir(&self, _path: &Path) -> io::Result<Vec<PathBuf>> {
        Ok(vec!["KNvKP.rtbw".into()])
    }

    async fn regular_file_size(&self, _path: &Path) -> io::Result<u64> {
        Ok(148048)
    }

    async fn open(&self, _path: &Path) -> io::Result<FakeFile<'a>> {
        Ok(FakeFile { data: self.data })
    }
}

struct FakeFile<'a> {
    data: &'a [u8],
}

impl RandomAccessFile for FakeFile<'_> {
    async fn read_at(&self, buf: &mut [u8], offset: u64, _hint: ReadHint) -> io::Result<usize> {
        let offset = offset as usize;
        let end = offset + buf.len();
        buf.copy_from_slice(
            self.data
                .get(offset..end)
                .ok_or(io::ErrorKind::UnexpectedEof)?,
        );
        Ok(buf.len())
    }
}

fuzz_target!(|data: &[u8]| {
    let pos: Chess = "8/2K5/8/8/8/8/3p4/1k2N3 b - - 0 1" // KNvKP
        .parse::<Fen>()
        .expect("valid fen")
        .into_position(CastlingMode::Standard)
        .expect("valid position");

    let mut tables = Tablebase::with_filesystem(FakeFilesystem { data });

    assert_eq!(
        tables
            .add_directory("fake")
            .now_or_never()
            .expect("blocking")
            .expect("add directory"),
        1
    );

    let _ = tables.probe_wdl(&pos).now_or_never().expect("blocking");
});
