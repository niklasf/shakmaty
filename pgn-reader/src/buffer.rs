use std::{
    cmp,
    io::{self, Chain, Cursor, Read},
    ops::{Deref, DerefMut, Range},
};

pub const CAPACITY: usize = 1 << 14;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Buffer {
    buffer: Box<[u8]>,
    /// The start of the valid data.
    ///
    /// Never greater than `self.end`.
    start: usize,
    /// The end of the valid data + 1 (not a valid index).
    ///
    /// Never greater than [`CAPACITY`].
    end: usize,
}

impl Buffer {
    /// Creates a new [`Buffer`] that can hold [`CAPACITY`] many elements.
    pub(crate) fn new() -> Self {
        Self {
            buffer: vec![0; CAPACITY].into_boxed_slice(),
            start: 0,
            end: 0,
        }
    }

    /// Equivalent to [`self.data_range().len()`](Self::data_range), but faster.
    #[inline]
    fn data_len(&self) -> usize {
        self.end - self.start
    }

    #[inline]
    /// Range from `self.start` to `self.end`.
    ///
    /// This is where [`Self::data`] lives.
    fn data_range(&self) -> Range<usize> {
        self.start..self.end
    }

    /// Gets the valid data in the buffer.
    #[inline]
    pub(crate) fn data(&self) -> &[u8] {
        debug_assert!(self.start <= self.end && self.end <= CAPACITY);

        // SAFETY: self.start <= self.end <= CAPACITY
        unsafe { self.buffer.get_unchecked(self.data_range()) }
    }

    /// Returns the first item in [`Self::data`].
    #[inline]
    pub(crate) fn peek(&self) -> Option<u8> {
        self.buffer.get(self.start).copied()
    }

    /// Clears the buffer.
    #[inline]
    pub(crate) fn clear(&mut self) {
        self.start = 0;
        self.end = 0;
    }

    /// Discards `n` many bytes at the front of [`Self::data`].
    #[inline]
    pub(crate) fn consume(&mut self, n: usize) {
        self.start = cmp::min(self.start + n, self.end);
    }

    /// Like [`self.consume(1)`](Self::consume).
    #[inline]
    pub(crate) fn bump(&mut self) {
        self.consume(1);
    }

    /// Ensures that `N` amount of bytes are in the buffer and returns the data.
    ///
    /// The only situation where the returned slice does not have `N` elements is if EOF was
    /// encountered.
    pub(crate) fn ensure_bytes<const N: usize>(&mut self, mut r: impl Read) -> io::Result<&[u8]> {
        const {
            debug_assert!(N <= CAPACITY);
        }

        if self.end + N > CAPACITY {
            self.backshift();
        }

        while self.data_len() < N {
            let len = r.read(&mut self.buffer[self.end..])?;

            // EOF
            if len == 0 {
                break;
            }

            self.end += len;
        }

        Ok(self.data())
    }

    /// Moves [`Self::data`] to the beginning.
    fn backshift(&mut self) {
        let data_range = self.data_range();
        self.start = 0;
        self.end = data_range.len();
        self.buffer.copy_within(data_range, 0);
    }
}

impl AsRef<[u8]> for Buffer {
    fn as_ref(&self) -> &[u8] {
        &self.buffer[self.start..self.end]
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BufferWithReader<R> {
    buffer: Buffer,
    reader: R,
}

impl<R: Read> BufferWithReader<R> {
    /// Creates a new [`BufferWithReader`] that can hold [`CAPACITY`] many elements.
    pub fn new(reader: R) -> Self {
        Self {
            buffer: Buffer::new(),
            reader,
        }
    }

    pub fn from_buffer(buffer: Buffer, reader: R) -> Self {
        Self { buffer, reader }
    }

    /// Gets the remaining bytes in the buffer and the underlying reader.
    pub fn into_inner(self) -> Chain<Cursor<Buffer>, R> {
        Cursor::new(self.buffer).chain(self.reader)
    }

    /// Ensures that [`N`] amount of bytes are in the buffer and returns the data.
    ///
    /// The only situation where the returned slice does not have [`N`] elements is if EOF was
    /// encountered.
    pub fn ensure_bytes<const N: usize>(&mut self) -> io::Result<&[u8]> {
        self.buffer.ensure_bytes::<N>(&mut self.reader)
    }
}

impl<R> Deref for BufferWithReader<R> {
    type Target = Buffer;

    fn deref(&self) -> &Buffer {
        &self.buffer
    }
}

impl<R> DerefMut for BufferWithReader<R> {
    fn deref_mut(&mut self) -> &mut Buffer {
        &mut self.buffer
    }
}
