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
        let mut pos = 0;
        let mut decoded: Vec<u8> = Vec::new();
        while let Some(delta) = memchr::memchr(b'\\', &self.0[pos..]) {
            let escape = pos + delta;
            match self.0.get(escape + 1) {
                Some(b'\\' | b'"') => {
                    decoded.extend_from_slice(&self.0[head..escape]);
                    head = escape + 1;
                    pos = escape + 2;
                }
                _ => pos = escape + 1,
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
        for (raw, expected) in [
            // No escape sequences.
            (&br""[..], &br""[..]),
            (br"Hello world", br"Hello world"),
            // Stray backslashes that do not start an escape sequence are
            // kept.
            (br"\", br"\"),
            (br"\x", br"\x"),
            (br"Hello \world\", br"Hello \world\"),
            (br"C:\chess", br"C:\chess"),
            // Escaped quotes and backslashes.
            (br#"\""#, br#"""#),
            (br"\\", br"\"),
            (br#"\Hello \"world\\"#, br#"\Hello "world\"#),
            // Consecutive escape sequences.
            (br"\\\\", br"\\"),
            (br"x\\", br"x\"),
            (br#"\\\""#, br#"\""#),
            (br#"\"\""#, br#""""#),
            (br#"a\\"b"#, br#"a\"b"#),
            (br"C:\\games\\", br"C:\games\"),
            // Escape sequence followed by a stray backslash.
            (br"\\\", br"\\"),
            (br"\\\x", br"\\x"),
        ] {
            let decoded = RawTag(raw).decode();
            assert_eq!(
                decoded.as_ref(),
                expected,
                "decoding {:?}",
                String::from_utf8_lossy(raw)
            );
            // Allocates only when there are escape sequences.
            assert_eq!(
                matches!(decoded, Cow::Borrowed(_)),
                raw == expected,
                "borrowing {:?}",
                String::from_utf8_lossy(raw)
            );
        }
    }

    fn naive_decode(raw: &[u8]) -> Vec<u8> {
        let mut decoded = Vec::new();
        let mut i = 0;
        while i < raw.len() {
            if raw[i] == b'\\' && matches!(raw.get(i + 1), Some(b'\\' | b'"')) {
                decoded.push(raw[i + 1]);
                i += 2;
            } else {
                decoded.push(raw[i]);
                i += 1;
            }
        }
        decoded
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_raw_tag_exhaustive() {
        let alphabet = br#"\"x"#;
        for len in 0..=8 {
            for mut n in 0..alphabet.len().pow(len) {
                let mut raw = Vec::new();
                for _ in 0..len {
                    raw.push(alphabet[n % alphabet.len()]);
                    n /= alphabet.len();
                }

                let decoded = RawTag(&raw).decode();
                let expected = naive_decode(&raw);
                assert_eq!(
                    decoded.as_ref(),
                    expected,
                    "decoding {:?}",
                    String::from_utf8_lossy(&raw)
                );
                assert_eq!(
                    matches!(decoded, Cow::Borrowed(_)),
                    expected.len() == raw.len(),
                    "borrowing {:?}",
                    String::from_utf8_lossy(&raw)
                );
            }
        }
    }

    #[test]
    fn test_raw_tag_borrows_without_escapes() {
        assert!(matches!(RawTag(br"C:\chess").decode(), Cow::Borrowed(_)));
        assert!(matches!(RawTag(br#"\""#).decode(), Cow::Owned(_)));
    }
}
