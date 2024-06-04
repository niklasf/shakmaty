//! Traits to provide a custom filesystem implementation.

use std::{
    fs, io,
    path::{Path, PathBuf},
};

use positioned_io::ReadAt as _;

/// An abstract filesystem.
pub trait Filesystem: Send + Sync {
    /// Determines the size in bytes of the given file.
    ///
    /// Follows symbolic links.
    ///
    /// # Errors
    ///
    /// See [`std::fs::metadata()`]. Additionally errors if `path` does not
    /// ultimately point to a regular file.
    fn regular_file_size(&self, path: &Path) -> io::Result<u64>;

    /// Returns a list of files in the given directory.
    ///
    /// # Errors
    ///
    /// See [`std::fs::read_dir()`].
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>>;

    /// Opens the given file, returning a handle for random read requests.
    ///
    /// # Errors
    ///
    /// See [`std::fs::File::open()`].
    fn open(&self, path: &Path) -> io::Result<Box<dyn RandomAccessFile>>;
}

/// The purpose of a read. Advisory only.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[non_exhaustive]
pub enum ReadHint {
    /// Reading metadata from the table file header.
    Header,
    /// Reading to sparse index to jump close to the correct entry in the block
    /// length table.
    SparseIndex,
    /// Reading the block length table.
    BlockLengths,
    /// Reading a compressed block.
    Data,
    /// Reading the DTZ value map.
    DtzMap,
}

/// An abstract randomly readable file.
pub trait RandomAccessFile: Send + Sync {
    /// Reads some bytes starting from a given offset.
    ///
    /// See [`std::os::unix::fs::FileExt::read_at()`] for precise semantics.
    fn read_at(&self, hint: ReadHint, pos: u64, buf: &mut [u8]) -> io::Result<usize>;

    /// Reads the exact number of bytes required to fill `buf` from the given
    /// offset.
    ///
    /// See [`std::os::unix::fs::FileExt::read_exact_at()`] for
    /// precise semantics.
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

    /// Reads the single byte at a given offset.
    fn read_u8_at(&self, hint: ReadHint, pos: u64) -> io::Result<u8> {
        let mut buf = [0];
        self.read_exact_at(hint, pos, &mut buf[..])?;
        Ok(buf[0])
    }

    /// Reads two bytes at a given offset, returning an integer in little
    /// endian.
    fn read_u16_le_at(&self, hint: ReadHint, pos: u64) -> io::Result<u16> {
        let mut buf = [0; 2];
        self.read_exact_at(hint, pos, &mut buf[..])?;
        Ok(u16::from_le_bytes(buf))
    }
}

pub(crate) struct DefaultFilesystem;

impl Filesystem for DefaultFilesystem {
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

    fn open(&self, path: &Path) -> io::Result<Box<dyn RandomAccessFile>> {
        Ok(Box::new(DefaultRandomAccessFile {
            inner: positioned_io::RandomAccessFile::open(path)?,
        }))
    }
}

pub(crate) struct DefaultRandomAccessFile {
    inner: positioned_io::RandomAccessFile,
}

impl RandomAccessFile for DefaultRandomAccessFile {
    fn read_at(&self, _hint: ReadHint, pos: u64, buf: &mut [u8]) -> io::Result<usize> {
        self.inner.read_at(pos, buf)
    }
}
