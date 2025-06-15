use std::{
    borrow::Cow,
    error::Error,
    fmt,
    str::{self, FromStr, Utf8Error},
};

/// Tell the reader to skip over a game or variation.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
#[must_use]
pub struct Skip(pub bool);

/// A numeric annotation glyph like `?`, `!!` or `$42`.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct Nag(pub u8);

impl Nag {
    /// Tries to parse a NAG from ASCII.
    ///
    /// # Examples
    ///
    /// ```
    /// use pgn_reader::Nag;
    ///
    /// assert_eq!(Nag::from_ascii(b"??"), Ok(Nag(4)));
    /// assert_eq!(Nag::from_ascii(b"$24"), Ok(Nag(24)));
    /// ```
    ///
    /// # Errors
    ///
    /// Returns an [`InvalidNag`] error if the input is neither a known glyph
    /// (`?!`, `!`, ...) nor a valid numeric annotation (`$0`, ..., `$255`).
    ///
    ///
    /// [`InvalidNag`]: struct.InvalidNag.html
    pub fn from_ascii(s: &[u8]) -> Result<Nag, InvalidNag> {
        if s == b"?!" {
            Ok(Nag::DUBIOUS_MOVE)
        } else if s == b"?" {
            Ok(Nag::MISTAKE)
        } else if s == b"??" {
            Ok(Nag::BLUNDER)
        } else if s == b"!" {
            Ok(Nag::GOOD_MOVE)
        } else if s == b"!!" {
            Ok(Nag::BRILLIANT_MOVE)
        } else if s == b"!?" {
            Ok(Nag::SPECULATIVE_MOVE)
        } else if s.len() > 1 && s[0] == b'$' {
            btoi::btou(&s[1..])
                .ok()
                .map(Nag)
                .ok_or(InvalidNag { _priv: () })
        } else {
            Err(InvalidNag { _priv: () })
        }
    }

    /// A good move (`!`).
    pub const GOOD_MOVE: Nag = Nag(1);

    /// A mistake (`?`).
    pub const MISTAKE: Nag = Nag(2);

    /// A brilliant move (`!!`).
    pub const BRILLIANT_MOVE: Nag = Nag(3);

    /// A blunder (`??`).
    pub const BLUNDER: Nag = Nag(4);

    /// A speculative move (`!?`).
    pub const SPECULATIVE_MOVE: Nag = Nag(5);

    /// A dubious move (`?!`).
    pub const DUBIOUS_MOVE: Nag = Nag(6);
}

impl fmt::Display for Nag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl From<u8> for Nag {
    fn from(nag: u8) -> Nag {
        Nag(nag)
    }
}

/// Error when parsing an invalid NAG.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct InvalidNag {
    _priv: (),
}

impl fmt::Debug for InvalidNag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("InvalidNag").finish()
    }
}

impl fmt::Display for InvalidNag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "invalid nag".fmt(f)
    }
}

impl Error for InvalidNag {
    fn description(&self) -> &str {
        "invalid nag"
    }
}

impl FromStr for Nag {
    type Err = InvalidNag;

    fn from_str(s: &str) -> Result<Nag, InvalidNag> {
        Nag::from_ascii(s.as_bytes())
    }
}

/// A tag value.
///
/// Provides helper methods for decoding [backslash
/// escaped](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c7)
/// values.
///
/// > A quote inside a string is represented by the backslash immediately
/// > followed by a quote. A backslash inside a string is represented by
/// > two adjacent backslashes.
#[derive(Copy, Clone, Eq, PartialEq)]
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

/// A comment, excluding the braces.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct RawComment<'a>(pub &'a [u8]);

impl<'a> RawComment<'a> {
    /// Returns the raw byte representation of the comment.
    pub fn as_bytes(&self) -> &[u8] {
        self.0
    }
}

impl<'a> fmt::Debug for RawComment<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", String::from_utf8_lossy(self.as_bytes()).as_ref())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nag() {
        assert_eq!(Nag::from_ascii(b"$33"), Ok(Nag(33)));
    }

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
