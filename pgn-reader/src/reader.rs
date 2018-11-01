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

    fn fill_buffer(&mut self) -> io::Result<bool> {
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

        Ok(!self.buffer.is_empty())
    }

    fn skip_bom(&mut self) -> io::Result<()> {
        self.fill_buffer()?;
        if self.buffer.starts_with(b"\xef\xbb\xbf") {
            unsafe { self.buffer.move_head(3); }
        }
        Ok(())
    }

    fn skip_line(&mut self) -> io::Result<()> {
        while self.fill_buffer()? {
            if let Some(pos) = memchr::memchr(b'\n', self.buffer.as_slice()) {
                unsafe { self.buffer.move_head(pos as isize + 1); }
                return Ok(());
            } else {
                self.buffer.clear();
            }
        }
        Ok(())
    }

    fn skip_whitespace(&mut self) -> io::Result<()> {
        while self.fill_buffer()? {
            while let Some(ch) = self.buffer.pop_front() {
                match ch {
                    b' ' | b'\t' | b'\r' | b'\n' => (),
                    b'%' => self.skip_line()?,
                    _ => {
                        self.buffer.push_front(ch);
                        return Ok(());
                    }
                }
            }
        }

        Ok(())
    }

    fn read_headers<V: Visitor>(&mut self, visitor: &mut V) -> io::Result<()> {
        println!("read headers");
        while self.fill_buffer()? {
            if let Some(ch) = self.buffer.pop_front() {
                match ch {
                    b'[' => {
                        let left_quote = match memchr::memchr2(b'"', b'\n', self.buffer.as_slice()) {
                            Some(left_quote) if self.buffer[left_quote] == b'"' => left_quote,
                            Some(eol) => {
                                visitor.header(&self.buffer[..eol], RawHeader(b""));
                                unsafe { self.buffer.move_head(eol as isize + 1); }
                                continue;
                            },
                            None => {
                                self.skip_line()?;
                                continue;
                            }
                        };

                        let space = if left_quote > 0 && self.buffer[left_quote - 1] == b' ' {
                            left_quote - 1
                        } else {
                            left_quote
                        };

                        let value_start = left_quote + 1;
                        let mut right_quote = value_start;
                        loop {
                            match memchr::memchr3(b'\\', b'"', b'\n', &self.buffer[right_quote..]) {
                                Some(delta) if self.buffer[right_quote + delta] == b'\\' && right_quote + delta + 1 < self.buffer.len() => {
                                    right_quote += delta + 2;
                                },
                                Some(delta) => {
                                    right_quote += delta;
                                    break;
                                }
                                None => {
                                    right_quote = self.buffer.len();
                                    break;
                                }
                            }
                        }

                        visitor.header(&self.buffer[..space], RawHeader(&self.buffer[value_start..right_quote]));
                        self.skip_line()?;
                    },
                    b'%' => self.skip_line()?,
                    _ => {
                        self.buffer.push_front(ch);
                        return Ok(());
                    }
                }
            }
        }
        Ok(())
    }

    pub fn read_game<V: Visitor>(&mut self, visitor: &mut V) -> io::Result<Option<V::Result>> {
        self.skip_bom()?;
        self.skip_whitespace()?;

        if !self.fill_buffer()? {
            return Ok(None);
        }

        visitor.begin_game();
        visitor.begin_headers();
        self.read_headers(visitor)?;
        visitor.end_headers();

        self.skip_whitespace()?;

        Ok(Some(visitor.end_game()))
    }
}
