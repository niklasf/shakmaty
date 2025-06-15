use std::cmp;
use std::io::{self, Read};

#[derive(Debug, Clone)]
pub struct Buffer {
    buf: Box<[u8]>,
    pos: usize,
    filled: usize,
}

impl Buffer {
    pub fn with_capacity(capacity: usize) -> Buffer {
        Buffer {
            buf: vec![0; capacity].into_boxed_slice(),
            pos: 0,
            filled: 0,
        }
    }

    #[inline]
    pub fn data(&self) -> &[u8] {
        // SAFETY: self.pos <= self.filled <= self.buf.len()
        unsafe { self.buf.get_unchecked(self.pos..self.filled) }
    }

    #[inline]
    pub fn discard_data(&mut self) {
        self.pos = 0;
        self.filled = 0;
    }

    #[inline]
    pub fn consume(&mut self, n: usize) {
        self.pos = cmp::min(self.pos + n, self.filled);
    }

    #[inline]
    pub fn bump(&mut self) {
        self.consume(1);
    }

    #[inline]
    pub fn peek(&self) -> Option<u8> {
        self.data().first().copied()
    }

    pub fn ensure_bytes(&mut self, n: usize, mut reader: impl Read) -> io::Result<&[u8]> {
        debug_assert!(n < self.buf.len());

        while self.data().len() < n {
            if self.pos > 0 {
                self.backshift();
            }

            let len = reader.read(&mut self.buf[self.filled..])?;
            if len == 0 {
                break;
            }

            self.filled += len;
        }
        Ok(self.data())
    }

    pub fn backshift(&mut self) {
        let range = self.pos..self.filled;
        self.pos = 0;
        self.filled = range.len();
        self.buf.copy_within(range, 0);
    }
}

impl AsRef<[u8]> for Buffer {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.data()
    }
}
