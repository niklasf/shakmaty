use std::{
    fs, io,
    path::{Path, PathBuf},
};

use positioned_io::{RandomAccessFile, ReadAt as _};

pub trait Filesystem: Send + Sync {
    fn regular_file_size(&self, path: &Path) -> io::Result<u64>;
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>>;
    fn open(&self, path: &Path) -> io::Result<Box<dyn File>>;
}

#[derive(Debug, Copy, Clone)]
pub enum ReadHint {
    Header,
    SparseIndex,
    BlockLengths,
    Data,
    DtzMap,
}

pub trait File: Send + Sync {
    fn read_at(&self, hint: ReadHint, pos: u64, buf: &mut [u8]) -> io::Result<usize>;

    fn read_exact_at(&self, hint: ReadHint, mut pos: u64, mut buf: &mut [u8]) -> io::Result<()> {
        while !buf.is_empty() {
            match self.read_at(hint, pos, buf) {
                Ok(0) => break,
                Ok(n) => {
                    let tmp = buf;
                    buf = &mut tmp[n..];
                    pos += n as u64;
                }
                Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
                Err(e) => return Err(e),
            }
        }
        if !buf.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "failed to fill whole buffer",
            ));
        }
        Ok(())
    }

    fn read_u8_at(&self, hint: ReadHint, pos: u64) -> io::Result<u8> {
        let mut buf = [0];
        self.read_exact_at(hint, pos, &mut buf[..])?;
        Ok(buf[0])
    }

    fn read_u16_le_at(&self, hint: ReadHint, pos: u64) -> io::Result<u16> {
        let mut buf = [0; 2];
        self.read_exact_at(hint, pos, &mut buf[..])?;
        Ok(u16::from_le_bytes(buf))
    }
}

pub struct StdFilesystem;

impl Filesystem for StdFilesystem {
    fn regular_file_size(&self, path: &Path) -> io::Result<u64> {
        let meta = path.metadata()?;
        if !meta.is_file() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "not a regular file",
            ));
        }
        Ok(meta.len())
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        fs::read_dir(path)?
            .into_iter()
            .map(|maybe_entry| maybe_entry.map(|entry| entry.path().to_owned()))
            .collect()
    }

    fn open(&self, path: &Path) -> io::Result<Box<dyn File>> {
        Ok(Box::new(StdFile {
            file: RandomAccessFile::open(path)?,
        }))
    }
}

pub struct StdFile {
    file: RandomAccessFile,
}

impl File for StdFile {
    fn read_at(&self, _hint: ReadHint, pos: u64, buf: &mut [u8]) -> io::Result<usize> {
        self.file.read_at(pos, buf)
    }
}
