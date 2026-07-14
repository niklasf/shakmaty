use std::{
    future::Future,
    io,
    path::{Path, PathBuf},
};

/// An abstract filesystem.
pub trait Filesystem {
    /// Type of handle to file opened for random read requests.
    type RandomAccessFile: RandomAccessFile;

    /// Returns a list of files in the given directory. May filter for table
    /// files.
    ///
    /// # Errors
    ///
    /// General I/O errors.
    fn read_dir(&self, path: &Path) -> impl Future<Output = io::Result<Vec<PathBuf>>>;

    /// Checks that `path` points to a regular file and determines its size
    /// in bytes.
    ///
    /// # Errors
    ///
    /// Besides general I/O errors, errors with
    /// [`std::io::ErrorKind::InvalidInput`] if `path` does not
    /// ultimately point to a regular file.
    fn regular_file_size(&self, path: &Path) -> impl Future<Output = io::Result<u64>>;

    /// Opens the given file, returning a handle for random read requests.
    ///
    /// # Errors
    ///
    /// General I/O errors.
    fn open(&self, path: &Path) -> impl Future<Output = io::Result<Self::RandomAccessFile>>;
}

/// The purpose of a read. Advisory only.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ReadHint {
    /// Reading metadata from the table file header.
    Header,
    /// Reading the DTZ value map.
    DtzMap,
    /// Reading the sparse index to jump close to the correct entry in the block
    /// length table.
    SparseIndex,
    /// Reading the block length table.
    BlockLengths,
    /// Reading a compressed block.
    Data,
}

/// An abstract randomly readable file.
pub trait RandomAccessFile {
    /// Reads some bytes starting from a given offset.
    ///
    /// See [`std::os::unix::fs::FileExt::read_at()`] for precise semantics.
    fn read_at(
        &self,
        buf: &mut [u8],
        offset: u64,
        hint: ReadHint,
    ) -> impl Future<Output = io::Result<usize>>;

    /// Reads the exact number of bytes required to fill `buf` from the given
    /// offset.
    ///
    /// See [`std::os::unix::fs::FileExt::read_exact_at()`] for
    /// precise semantics.
    fn read_exact_at(
        &self,
        mut buf: &mut [u8],
        mut offset: u64,
        hint: ReadHint,
    ) -> impl Future<Output = io::Result<()>> {
        async move {
            while !buf.is_empty() {
                match self.read_at(buf, offset, hint).await {
                    Ok(0) => return Err(io::ErrorKind::UnexpectedEof.into()),
                    Ok(n) => {
                        let tmp = buf;
                        buf = &mut tmp[n..];
                        offset += n as u64;
                    }
                    Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
                    Err(e) => return Err(e),
                }
            }
            Ok(())
        }
    }

    /// Reads the single byte at a given offset.
    fn read_u8_at(&self, offset: u64, hint: ReadHint) -> impl Future<Output = io::Result<u8>> {
        async move {
            let mut buf = [0];
            self.read_exact_at(&mut buf[..], offset, hint).await?;
            Ok(buf[0])
        }
    }

    /// Reads two bytes at a given offset, and interprets them as a
    /// little endian integer.
    fn read_u16_le_at(&self, offset: u64, hint: ReadHint) -> impl Future<Output = io::Result<u16>> {
        async move {
            let mut buf = [0; 2];
            self.read_exact_at(&mut buf[..], offset, hint).await?;
            Ok(u16::from_le_bytes(buf))
        }
    }
}
