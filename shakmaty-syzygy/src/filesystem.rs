//! Traits to provide a custom filesystem implementation.

use std::{
    fs, io,
    path::{Path, PathBuf},
};

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

    /// Returns a list of files in the given directory. May filter for table
    /// files.
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
    fn read_at(&self, buf: &mut [u8], offset: u64, hint: ReadHint) -> io::Result<usize>;

    /// Reads the exact number of bytes required to fill `buf` from the given
    /// offset.
    ///
    /// See [`std::os::unix::fs::FileExt::read_exact_at()`] for
    /// precise semantics.
    fn read_exact_at(&self, mut buf: &mut [u8], mut offset: u64, hint: ReadHint) -> io::Result<()> {
        while !buf.is_empty() {
            match self.read_at(buf, offset, hint) {
                Ok(0) => break,
                Ok(n) => {
                    let tmp = buf;
                    buf = &mut tmp[n..];
                    offset += n as u64;
                }
                Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
                Err(e) => return Err(e),
            }
        }
        if !buf.is_empty() {
            return Err(io::ErrorKind::UnexpectedEof.into());
        }
        Ok(())
    }

    /// Reads the single byte at a given offset.
    fn read_u8_at(&self, offset: u64, hint: ReadHint) -> io::Result<u8> {
        let mut buf = [0];
        self.read_exact_at(&mut buf[..], offset, hint)?;
        Ok(buf[0])
    }

    /// Reads two bytes at a given offset, and interprets them as a
    /// little endian integer.
    fn read_u16_le_at(&self, offset: u64, hint: ReadHint) -> io::Result<u16> {
        let mut buf = [0; 2];
        self.read_exact_at(&mut buf[..], offset, hint)?;
        Ok(u16::from_le_bytes(buf))
    }
}

#[cfg(any(unix, windows))]
pub(crate) mod os {
    use super::*;
    pub struct OsFilesystem;

    impl Filesystem for OsFilesystem {
        fn regular_file_size(&self, path: &Path) -> io::Result<u64> {
            regular_file_size_impl(path)
        }

        fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
            read_dir_impl(path)
        }

        fn open(&self, path: &Path) -> io::Result<Box<dyn RandomAccessFile>> {
            let file = fs::File::open(path)?;

            // Safety: No requirements.
            #[cfg(target_os = "linux")]
            unsafe {
                libc::posix_fadvise(
                    std::os::unix::io::AsRawFd::as_raw_fd(&file),
                    0,
                    0,
                    libc::POSIX_FADV_RANDOM,
                );
            }

            Ok(Box::new(OsRandomAccessFile { file }))
        }
    }

    pub struct OsRandomAccessFile {
        file: fs::File,
    }

    impl RandomAccessFile for OsRandomAccessFile {
        #[cfg(unix)]
        fn read_at(&self, buf: &mut [u8], offset: u64, _hint: ReadHint) -> io::Result<usize> {
            std::os::unix::fs::FileExt::read_at(&self.file, buf, offset)
        }
        #[cfg(windows)]
        fn read_at(&self, buf: &mut [u8], offset: u64, _hint: ReadHint) -> io::Result<usize> {
            std::os::windows::fs::FileExt::seek_read(&self.file, buf, offset)
        }
    }
}

#[cfg(feature = "mmap")]
pub(crate) mod mmap {
    use memmap2::{Mmap, MmapOptions};

    use super::*;

    pub struct MmapFilesystem {
        _priv: (),
    }

    impl MmapFilesystem {
        pub unsafe fn new() -> MmapFilesystem {
            MmapFilesystem { _priv: () }
        }
    }

    impl Filesystem for MmapFilesystem {
        fn regular_file_size(&self, path: &Path) -> io::Result<u64> {
            regular_file_size_impl(path)
        }

        fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
            read_dir_impl(path)
        }

        fn open(&self, path: &Path) -> io::Result<Box<dyn RandomAccessFile>> {
            let file = fs::File::open(path)?;

            // Safety: Contract forwarded to MmapFilesystem::new().
            let mmap = unsafe { MmapOptions::new().map(&file)? };

            #[cfg(unix)]
            mmap.advise(memmap2::Advice::Random)?;

            Ok(Box::new(MmapRandomAccessFile { mmap }))
        }
    }

    pub struct MmapRandomAccessFile {
        mmap: Mmap,
    }

    impl RandomAccessFile for MmapRandomAccessFile {
        fn read_at(&self, buf: &mut [u8], offset: u64, _hint: ReadHint) -> io::Result<usize> {
            let offset = offset as usize;
            let end = offset + buf.len();
            buf.copy_from_slice(
                self.mmap
                    .get(offset..end)
                    .ok_or(io::ErrorKind::UnexpectedEof)?,
            );
            Ok(buf.len())
        }
    }
}

#[cfg(any(unix, windows, feature = "mmap"))]
fn regular_file_size_impl(path: &Path) -> io::Result<u64> {
    let meta = path.metadata()?;
    if !meta.is_file() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "not a regular file",
        ));
    }
    Ok(meta.len())
}

#[cfg(any(unix, windows, feature = "mmap"))]
fn read_dir_impl(path: &Path) -> io::Result<Vec<PathBuf>> {
    fs::read_dir(path)?
        .map(|maybe_entry| maybe_entry.map(|entry| entry.path().to_owned()))
        .collect()
}
