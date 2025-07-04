use std::{
    borrow::Cow,
    fmt,
    str::{self, Utf8Error},
};

/// A tag value.
///
/// Provides helper methods for decoding [backslash
/// escaped](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c7)
/// values.
///
/// > A quote inside a string is represented by the backslash immediately
/// > followed by a quote. A backslash inside a string is represented by
/// > two adjacent backslashes.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct RawTag<'a>(pub &'a [u8]);

impl<'a> RawTag<'a> {
    /// Returns the raw byte representation of the tag value.
    pub fn as_bytes(&self) -> &[u8] {
        self.0
    }

    /// Decodes escaped quotes and backslashes into bytes. Allocates only when
    /// the value actually contains escape sequences.
    pub fn decode(&self) -> Cow<'a, [u8]> {
        let mut head = 0;
        let mut decoded: Vec<u8> = Vec::new();
        for escape in memchr::memchr_iter(b'\\', self.0) {
            match self.0.get(escape + 1).cloned() {
                Some(ch) if ch == b'\\' || ch == b'"' => {
                    decoded.extend_from_slice(&self.0[head..escape]);
                    head = escape + 1;
                }
                _ => (),
            }
        }
        if head == 0 {
            Cow::Borrowed(self.0)
        } else {
            decoded.extend_from_slice(&self.0[head..]);
            Cow::Owned(decoded)
        }
    }

    /// Tries to decode the tag as UTF-8. This is guaranteed to succeed on
    /// valid PGNs.
    ///
    /// # Errors
    ///
    /// Errors if the tag contains an invalid UTF-8 byte sequence.
    pub fn decode_utf8(&self) -> Result<Cow<'a, str>, Utf8Error> {
        Ok(match self.decode() {
            Cow::Borrowed(borrowed) => Cow::Borrowed(str::from_utf8(borrowed)?),
            Cow::Owned(owned) => Cow::Owned(String::from_utf8(owned).map_err(|e| e.utf8_error())?),
        })
    }

    /// Decodes the tag as UTF-8, replacing any invalid byte sequences with
    /// the placeholder � U+FFFD.
    pub fn decode_utf8_lossy(&self) -> Cow<'a, str> {
        match self.decode() {
            Cow::Borrowed(borrowed) => String::from_utf8_lossy(borrowed),
            Cow::Owned(owned) => Cow::Owned(String::from_utf8_lossy(&owned).into_owned()),
        }
    }
}

impl<'a> fmt::Debug for RawTag<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.decode_utf8_lossy())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_raw_tag() {
        let tag = RawTag(b"Hello world");
        assert_eq!(tag.decode().as_ref(), b"Hello world");

        let tag = RawTag(b"Hello \\world\\");
        assert_eq!(tag.decode().as_ref(), b"Hello \\world\\");

        let tag = RawTag(b"\\Hello \\\"world\\\\");
        assert_eq!(tag.decode().as_ref(), b"\\Hello \"world\\");
    }
}
