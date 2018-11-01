use super::{Nag, Outcome, RawHeader, Skip, San};
use std::io;
use std::io::Read;
use std::ptr;
use slice_deque::SliceDeque;

pub trait Visitor {
    type Result;

    fn begin_game(&mut self) { }

    fn begin_headers(&mut self) { }
    fn header(&mut self, _key: &[u8], _value: RawHeader<'_>) { }
    fn end_headers(&mut self) -> Skip { Skip(false) }

    fn san(&mut self, _san: San) { }
    fn nag(&mut self, _nag: Nag) { }
    fn comment(&mut self, _comment: &[u8]) { }
    fn begin_variation(&mut self) -> Skip { Skip(false) }
    fn end_variation(&mut self) { }
    fn outcome(&mut self, _outcome: Outcome) { }

    fn end_game(&mut self) -> Self::Result;
}

#[derive(Debug)]
pub struct PgnReader<R> {
    inner: R,
    buffer: SliceDeque<u8>,
}

const BUFFER_SIZE: usize = 8192;

impl<R: Read> PgnReader<R> {
    pub fn new(inner: R) -> PgnReader<R> {
        PgnReader {
            inner,
            buffer: SliceDeque::with_capacity(BUFFER_SIZE * 2)
        }
    }

    pub fn into_inner(self) -> R {
        self.inner
    }

    fn fill_buffer(&mut self) -> io::Result<()> {
        while self.buffer.len() < BUFFER_SIZE {
            unsafe {
                let size = {
                    let remainder = self.buffer.tail_head_slice();
                    ptr::write_bytes(remainder.as_mut_ptr(), 0, remainder.len()); // TODO
                    self.inner.read(remainder)?
                };

                if size == 0 {
                    break;
                }

                self.buffer.move_tail(size as isize);
            }
        }

        Ok(())
    }

    fn skip_bom(&mut self) -> io::Result<()> {
        self.fill_buffer()?;
        if self.buffer.starts_with(b"\xef\xbb\xbf") {
            unsafe { self.buffer.move_head(3); }
        }
        Ok(())
    }

    fn skip_line(&mut self) -> io::Result<()> {
        loop {
            if let Some(pos) = memchr::memchr(b'\n', self.buffer.as_slice()) {
                unsafe { self.buffer.move_head(pos as isize + 1); }
                return Ok(());
            } else {
                self.fill_buffer()?;
                if self.buffer.is_empty() {
                    return Ok(());
                }
            }
        }
    }

    fn skip_whitespace_after_newline(&mut self) -> io::Result<()> {
        let mut after_newline = true;

        loop {
            while let Some(ch) = self.buffer.pop_front() {
                match ch {
                    b' ' | b'\t' | b'\r' => after_newline = false,
                    b'\n' => after_newline = true,
                    b'%' if after_newline => self.skip_line()?,
                    _ => {
                        self.buffer.push_front(ch);
                        return Ok(());
                    }
                }
            }

            self.fill_buffer()?;
            if self.buffer.is_empty() {
                return Ok(());
            }
        }
    }

    pub fn read_game<V: Visitor>(&mut self, visitor: &mut V) -> io::Result<Option<V::Result>> {
        self.skip_bom()?;
        self.skip_whitespace_after_newline()?;

        self.fill_buffer()?;
        if self.buffer.is_empty() {
            return Ok(None);
        }

        visitor.begin_game();
        visitor.begin_headers();
        visitor.end_headers();

        self.skip_whitespace_after_newline()?;

        Ok(Some(visitor.end_game()))
    }
}
