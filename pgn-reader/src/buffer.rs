use std::{
    cmp::min,
    io::{self, Read},
};

#[derive(Debug, Clone)]
pub(crate) struct Buffer {
    buffer: Vec<u8>,
    start: usize,
    end: usize,
}

impl Buffer {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buffer: vec![0; capacity],
            start: 0,
            end: 0,
        }
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.buffer.len()
    }

    #[inline]
    pub fn data(&self) -> &[u8] {
        debug_assert!(self.start <= self.end && self.end <= self.capacity());

        // SAFETY: self.start <= self.end <= self.capacity()
        unsafe { self.buffer.get_unchecked(self.start..self.end) }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.data().len()
    }

    #[inline]
    pub fn peek(&self) -> Option<u8> {
        self.data().first().copied()
    }

    #[inline]
    pub fn clear(&mut self) {
        self.start = 0;
        self.end = 0;
    }

    #[inline]
    pub fn consume(&mut self, n: usize) {
        // Cap at self.end to maintain safety invariant
        self.start = min(self.start + n, self.end);
    }

    #[inline]
    pub fn bump(&mut self) {
        self.consume(1);
    }

    /// Ensures that `n` bytes are in the buffer and returns the data.
    ///
    /// The only situation where the returned slice has less than `n` bytes is if
    /// EOF was reached.
    pub fn ensure_bytes(&mut self, n: usize, mut r: impl Read) -> io::Result<&[u8]> {
        debug_assert!(n <= self.capacity() / 2);

        while self.len() < n {
            self.backshift();
            let len = r.read(&mut self.buffer[self.end..])?;

            // EOF
            if len == 0 {
                break;
            }

            self.end += len;
        }

        Ok(self.data())
    }

    fn backshift(&mut self) {
        let data_range = self.start..self.end;
        self.start = 0;
        self.end = data_range.len();
        self.buffer.copy_within(data_range, 0);
    }
}
