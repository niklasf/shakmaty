use super::{Nag, Outcome, RawHeader, Skip, San};
use std::io;
use std::io::Read;
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

    pub fn fill_buffer(&mut self) -> io::Result<()> {
        while self.buffer.len() < BUFFER_SIZE {
            unsafe {
                let size = {
                    let remainder = self.buffer.tail_head_slice();
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

    fn skip_bom(&mut self) {
        if self.buffer.starts_with(b"\xef\xbb\xbf") {
            unsafe { self.buffer.move_head(3); }
        }
    }

    /* fn skip_whitespace(&mut self) {
        while let Some(ch) = self.buffer.pop_front() {
            match ch {
                b' ' | b'\t' | b'\r' => (),
                b'\n' => {
                }
            }
        }
    } */

    pub fn read_game<V: Visitor>(&mut self, mut visitor: V) -> io::Result<Option<V::Result>> {
        self.skip_bom();

        let pgn = self.buffer.as_slice();

        visitor.begin_game();
        visitor.begin_headers();


        Ok(Some(visitor.end_game()))
    }
}
