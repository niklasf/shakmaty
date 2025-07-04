use std::{error::Error, fmt, str::FromStr};

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
            btoi::btou(&s[1..]).ok().map(Nag).ok_or(InvalidNag)
        } else {
            Err(InvalidNag)
        }
    }

    /// Appends the NAG to a string.
    ///
    /// ```
    /// use pgn_reader::Nag;
    ///
    /// let mut buf = String::new();
    /// Nag(255).append_to_string(&mut buf);
    /// assert_eq!(buf, "$255");
    /// ```
    pub fn append_to_string(&self, s: &mut String) {
        s.reserve(4);
        s.push('$');
        if self.0 >= 100 {
            s.push((b'0' + (self.0 / 100) % 10) as char);
        }
        if self.0 >= 10 {
            s.push((b'0' + (self.0 / 10) % 10) as char);
        }
        s.push((b'0' + (self.0 % 10)) as char);
    }

    /// Appends the NAG as ASCII to a byte buffer.
    ///
    /// ```
    /// use pgn_reader::Nag;
    ///
    /// let mut buf = Vec::new();
    /// Nag(255).append_ascii_to(&mut buf);
    /// assert_eq!(buf, b"$255");
    /// ```
    pub fn append_ascii_to(&self, buf: &mut Vec<u8>) {
        buf.reserve(4);
        buf.push(b'$');
        if self.0 >= 100 {
            buf.push(b'0' + (self.0 / 100) % 10);
        }
        if self.0 >= 10 {
            buf.push(b'0' + (self.0 / 10) % 10);
        }
        buf.push(b'0' + (self.0 % 10));
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

impl From<Nag> for u8 {
    fn from(Nag(nag): Nag) -> u8 {
        nag
    }
}

impl FromStr for Nag {
    type Err = InvalidNag;

    fn from_str(s: &str) -> Result<Nag, InvalidNag> {
        Nag::from_ascii(s.as_bytes())
    }
}

/// Error when parsing an invalid NAG.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct InvalidNag;

impl fmt::Display for InvalidNag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("invalid nag")
    }
}

impl Error for InvalidNag {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_ascii() {
        assert_eq!(Nag::from_ascii(b"$1"), Ok(Nag(1)));
        assert_eq!(Nag::from_ascii(b"$12"), Ok(Nag(12)));
        assert_eq!(Nag::from_ascii(b"$123"), Ok(Nag(123)));
        assert_eq!(Nag::from_ascii(b"$1234"), Err(InvalidNag));
    }

    #[test]
    fn test_append_to_string() {
        let mut s = String::new();
        Nag(0).append_to_string(&mut s);
        Nag(1).append_to_string(&mut s);
        Nag(12).append_to_string(&mut s);
        Nag(123).append_to_string(&mut s);
        assert_eq!(s, "$0$1$12$123");
    }

    #[test]
    fn test_append_ascii_to() {
        let mut buf = Vec::new();
        Nag(123).append_ascii_to(&mut buf);
        Nag(12).append_ascii_to(&mut buf);
        Nag(1).append_ascii_to(&mut buf);
        Nag(0).append_ascii_to(&mut buf);
        assert_eq!(buf, b"$123$12$1$0");
    }
}
