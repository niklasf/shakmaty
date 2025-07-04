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
    fn test_nag() {
        assert_eq!(Nag::from_ascii(b"$1"), Ok(Nag(1)));
        assert_eq!(Nag::from_ascii(b"$12"), Ok(Nag(12)));
        assert_eq!(Nag::from_ascii(b"$123"), Ok(Nag(123)));
        assert_eq!(Nag::from_ascii(b"$1234"), Err(InvalidNag));
    }
}
