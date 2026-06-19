use std::{
    fmt,
    fmt::Debug,
    io,
    path::{Path, PathBuf},
};

use futures_util::future::FutureExt;
use shakmaty::{Move, Position};

use crate::{
    AmbiguousWdl, Dtz, MaybeRounded, Syzygy, Wdl, aio,
    aio::{Filesystem, RandomAccessFile, ReadHint},
    errors::SyzygyError,
};

trait BlockingFilesystem: Debug + Sync + Send {
    fn regular_file_size_blocking(&self, path: &Path) -> io::Result<u64>;
    fn read_dir_blocking(&self, path: &Path) -> io::Result<Vec<PathBuf>>;
    fn open_blocking(&self, path: &Path) -> io::Result<Box<dyn BlockingRandomAccessFile>>;
}

#[doc(hidden)]
impl Filesystem for Box<dyn BlockingFilesystem> {
    type RandomAccessFile = Box<dyn BlockingRandomAccessFile>;

    async fn regular_file_size(&self, path: &Path) -> io::Result<u64> {
        self.regular_file_size_blocking(path)
    }

    async fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        self.read_dir_blocking(path)
    }

    async fn open(&self, path: &Path) -> io::Result<Box<dyn BlockingRandomAccessFile>> {
        self.open_blocking(path)
    }
}

trait BlockingRandomAccessFile: Debug + Sync + Send {
    fn read_at_blocking(&self, buf: &mut [u8], offset: u64, hint: ReadHint) -> io::Result<usize>;
}

#[doc(hidden)]
impl RandomAccessFile for Box<dyn BlockingRandomAccessFile> {
    async fn read_at(&self, buf: &mut [u8], offset: u64, hint: ReadHint) -> io::Result<usize> {
        self.read_at_blocking(buf, offset, hint)
    }
}

/// A collection of tables.
pub struct Tablebase<S> {
    inner: aio::Tablebase<S, Box<dyn BlockingFilesystem>>,
}

#[cfg(any(unix, windows))]
impl Default for Tablebase<()> {
    fn default() -> Self {
        Self::new()
    }
}

impl<S: Syzygy> Debug for Tablebase<S>
where
    S: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Tablebase")
            .field("inner", &self.inner)
            .finish()
    }
}

impl<S> Tablebase<S> {
    /// Creates an empty collection of tables. A safe default filesystem
    /// implementation will be used to read table files.
    #[cfg(any(unix, windows))]
    pub fn new() -> Self {
        Self::with_filesystem(Box::new(os::OsFilesystem))
    }

    /// Creates an empty collection of tables. Memory maps will be used
    /// to read table files.
    ///
    /// Reading from memory maps avoids the significant syscall overhead
    /// of the default implementation.
    ///
    /// # Safety
    ///
    /// * Externally guarantee that table files are not modified after
    ///   they were opened.
    /// * Externally guarantee absence of I/O errors (or live with the
    ///   consequences). For example, I/O errors will generate
    ///   `SIGSEV`/`SIGBUS` on Linux.
    #[cfg(all(feature = "mmap", target_pointer_width = "64"))]
    pub unsafe fn with_mmap_filesystem() -> Self {
        Self::with_filesystem(unsafe { Box::new(mmap::MmapFilesystem::new()) })
    }

    fn with_filesystem(filesystem: Box<dyn BlockingFilesystem>) -> Self {
        Self {
            inner: aio::Tablebase::with_filesystem(filesystem),
        }
    }

    /// Returns the maximum number of pieces over all added tables.
    ///
    /// This number is updated when adding table files and very fast to read.
    #[inline]
    pub fn max_pieces(&self) -> usize {
        self.inner.max_pieces()
    }
}

impl<S: Syzygy> Tablebase<S> {
    /// Add all relevant tables from a directory.
    ///
    /// Tables are selected by filename, e.g., `KQvKP.rtbz`.
    ///
    /// The files are not actually opened. This happens lazily when probing.
    /// Eventually all files may be opened, so configure resource limits like
    /// `RLIMIT_NOFILE` accordingly.
    ///
    /// Probing generally requires tables for the specific material
    /// composition, as well as material compositions that are transitively
    /// reachable by captures and promotions. These are sometimes distributed
    /// separately, so make sure to add tables from all relevant directories.
    ///
    /// Traverses symbolic links.
    ///
    /// Returns the number of added table files.
    ///
    /// # Errors
    ///
    /// Returns an error result when:
    ///
    /// * `path` is not pointing to a directory.
    /// * Listing the directory fails (no permission, ...).
    ///   See [`std::fs::read_dir()`].
    /// * Querying meta data for a table in `path` failed.
    ///   See [`std::fs::metadata()`].
    ///   Some tables maybe have already been added.
    /// * There is a table in `path` where the file size indicates that
    ///   it must be corrupted. Some tables may have already been added.
    pub fn add_directory(&mut self, path: impl AsRef<Path>) -> io::Result<usize> {
        self.inner
            .add_directory(path)
            .now_or_never()
            .expect("blocking impl ready immediately")
    }

    /// Add a table file.
    ///
    /// The file is not actually opened. This happens lazily when probing.
    ///
    /// Traverses symbolic links.
    ///
    /// # Errors
    ///
    /// Despite lazy opening, there are some immediate error conditions:
    ///
    /// * [`std::io::ErrorKind::InvalidInput`] if the filename does not indicate
    ///   that it is a valid table file (e.g., `KQvKP.rtbz`).
    /// * [`std::io::ErrorKind::InvalidInput`] if `path` is not pointing to a
    ///   regular file.
    /// * [`std::io::ErrorKind::InvalidData`] if the file size indicates that
    ///   the table file must be corrupted.
    /// * Querying metadata for the path fails (file does not exist,
    ///   broken symlink, no permission to read metadata, ...).
    ///   See [`std::fs::metadata()`].
    pub fn add_file(&mut self, path: impl AsRef<Path>) -> io::Result<()> {
        self.inner
            .add_file(path.as_ref())
            .now_or_never()
            .expect("blocking impl ready immediately")
    }
}

impl<S: Syzygy + Position + Clone> Tablebase<S> {
    /// Probe tables for the [`Wdl`] value of a position, assuming `pos`
    /// is reached directly after a capture or pawn move.
    ///
    /// Requires only WDL tables.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`] for possible error conditions.
    pub fn probe_wdl_after_zeroing(&self, pos: &S) -> Result<Wdl, SyzygyError> {
        self.inner
            .probe_wdl_after_zeroing(pos)
            .now_or_never()
            .expect("blocking impl ready immediately")
    }

    /// Probe tables for the WDL value of a position, considering also
    /// the halfmove counter of `pos`. The result may be
    /// [ambiguous due to DTZ rounding](MaybeRounded).
    ///
    /// Requires both WDL and DTZ tables.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`] for possible error conditions.
    pub fn probe_wdl(&self, pos: &S) -> Result<AmbiguousWdl, SyzygyError> {
        self.inner
            .probe_wdl(pos)
            .now_or_never()
            .expect("blocking impl ready immediately")
    }

    /// Probe tables for the [`Dtz`] value of a position.
    ///
    /// Requires both WDL and DTZ tables.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`] for possible error conditions.
    pub fn probe_dtz(&self, pos: &S) -> Result<MaybeRounded<Dtz>, SyzygyError> {
        self.inner
            .probe_dtz(pos)
            .now_or_never()
            .expect("blocking impl ready immediately")
    }

    /// Get the recommended tablebase move.
    ///
    /// Following the tablebase mainline *starting from a capture or pawn move*
    /// guarantees achieving the optimal outcome under the 50-move rule.
    ///
    /// Otherwise (i.e., when not immediately following the tablebase mainline
    /// after the capture that crosses into tablebase territory),
    /// [some care needs to be taken due to DTZ rounding](MaybeRounded).
    ///
    /// Requires both WDL and DTZ tables.
    ///
    /// # Errors
    ///
    /// See [`SyzygyError`] for possible error conditions.
    pub fn best_move(&self, pos: &S) -> Result<Option<(Move, MaybeRounded<Dtz>)>, SyzygyError> {
        self.inner
            .best_move(pos)
            .now_or_never()
            .expect("blocking impl ready immediately")
    }
}

#[cfg(any(unix, windows))]
mod os {
    use super::*;

    #[derive(Debug)]
    pub(crate) struct OsFilesystem;

    impl BlockingFilesystem for OsFilesystem {
        fn regular_file_size_blocking(&self, path: &Path) -> io::Result<u64> {
            regular_file_size_impl(path)
        }

        fn read_dir_blocking(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
            read_dir_impl(path)
        }

        fn open_blocking(&self, path: &Path) -> io::Result<Box<dyn BlockingRandomAccessFile>> {
            Ok(Box::new(OsRandomAccessFile {
                file: std::fs::File::open(path)?,
            }))
        }
    }

    #[derive(Debug)]
    struct OsRandomAccessFile {
        file: std::fs::File,
    }

    impl BlockingRandomAccessFile for OsRandomAccessFile {
        #[cfg(unix)]
        fn read_at_blocking(
            &self,
            buf: &mut [u8],
            offset: u64,
            _hint: ReadHint,
        ) -> io::Result<usize> {
            std::os::unix::fs::FileExt::read_at(&self.file, buf, offset)
        }
        #[cfg(windows)]
        fn read_at_blocking(
            &self,
            buf: &mut [u8],
            offset: u64,
            _hint: ReadHint,
        ) -> io::Result<usize> {
            std::os::windows::fs::FileExt::seek_read(&self.file, buf, offset)
        }
    }
}

#[cfg(all(feature = "mmap", target_pointer_width = "64"))]
mod mmap {
    use memmap2::{Mmap, MmapOptions};

    use super::*;

    #[derive(Debug)]
    pub(crate) struct MmapFilesystem {
        _unsafe_priv: (),
    }

    impl MmapFilesystem {
        pub unsafe fn new() -> MmapFilesystem {
            MmapFilesystem { _unsafe_priv: () }
        }
    }

    impl BlockingFilesystem for MmapFilesystem {
        fn regular_file_size_blocking(&self, path: &Path) -> io::Result<u64> {
            regular_file_size_impl(path)
        }

        fn read_dir_blocking(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
            read_dir_impl(path)
        }

        fn open_blocking(&self, path: &Path) -> io::Result<Box<dyn BlockingRandomAccessFile>> {
            let file = std::fs::File::open(path)?;
            let mmap = unsafe { MmapOptions::new().map(&file)? };
            Ok(Box::new(MmapRandomAccessFile { mmap }))
        }
    }

    #[derive(Debug)]
    struct MmapRandomAccessFile {
        mmap: Mmap,
    }

    impl BlockingRandomAccessFile for MmapRandomAccessFile {
        fn read_at_blocking(
            &self,
            buf: &mut [u8],
            offset: u64,
            _hint: ReadHint,
        ) -> io::Result<usize> {
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

#[cfg(any(unix, windows, all(feature = "mmap", target_pointer_width = "64")))]
fn regular_file_size_impl(path: &Path) -> io::Result<u64> {
    let meta = path.metadata()?;
    if !meta.is_file() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "not a regular file",
        ));
    }
    Ok(meta.len())
}

#[cfg(any(unix, windows, all(feature = "mmap", target_pointer_width = "64")))]
fn read_dir_impl(path: &Path) -> io::Result<Vec<PathBuf>> {
    std::fs::read_dir(path)?
        .map(|maybe_entry| maybe_entry.map(|entry| entry.path().to_owned()))
        .collect()
}

#[cfg(test)]
mod tests {
    use shakmaty::{CastlingMode, Chess, Role, Square, fen::Fen};

    use super::*;

    #[test]
    fn test_send_sync() {
        fn assert_send<T: Send>(_: T) {}
        fn assert_sync<T: Sync>(_: T) {}

        assert_send(Tablebase::<Chess>::new());
        assert_sync(Tablebase::<Chess>::new());
    }

    #[test]
    fn test_mating_best_move() {
        let mut tables = Tablebase::new();
        tables
            .add_directory("tables/chess")
            .expect("read directory");

        let pos: Chess = "5BrN/8/8/8/8/2k5/8/2K5 b - -"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position(CastlingMode::Chess960)
            .expect("legal position");

        assert!(matches!(
            tables.best_move(&pos),
            Ok(Some((
                Move::Normal {
                    role: Role::Rook,
                    from: Square::G8,
                    capture: None,
                    to: Square::G1,
                    promotion: None,
                },
                MaybeRounded::Rounded(Dtz(-1))
            )))
        ));
    }

    #[test]
    fn test_black_escapes_via_underpromotion() {
        let mut tables = Tablebase::new();
        tables
            .add_directory("tables/chess")
            .expect("read directory");

        let pos: Chess = "8/6B1/8/8/B7/8/K1pk4/8 b - - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position(CastlingMode::Chess960)
            .expect("legal position");

        assert!(matches!(
            tables.best_move(&pos),
            Ok(Some((
                Move::Normal {
                    role: Role::Pawn,
                    from: Square::C2,
                    to: Square::C1,
                    capture: None,
                    promotion: Some(Role::Knight),
                },
                MaybeRounded::Rounded(Dtz(109))
            )))
        ));
    }

    #[test]
    #[ignore]
    fn test_many_pawns() {
        let mut tables = Tablebase::new();
        tables
            .add_directory("tables/chess")
            .expect("read directory");

        let pos: Chess = "3k4/5P2/8/8/4K3/2P3P1/PP6/8 w - - 0 1"
            .parse::<Fen>()
            .expect("valid fen")
            .into_position(CastlingMode::Chess960)
            .expect("legal position");

        assert!(matches!(
            tables.probe_dtz(&pos),
            Ok(MaybeRounded::Precise(Dtz(1)))
        ));
    }
}
