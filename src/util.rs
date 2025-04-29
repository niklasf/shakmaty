use core::{convert::TryFrom as _, fmt, fmt::Write as _, num::TryFromIntError};

use arrayvec::ArrayString;

pub(crate) fn out_of_range_error() -> TryFromIntError {
    // This is a hack to construct TryFromIntError despite its private
    // constructor. The standard library keeps it private intentionally,
    // to be able to provide error details in the future, but it is unlikely
    // that something more specific than "overflow" will be added.
    u32::try_from(u64::MAX).unwrap_err()
}

macro_rules! from_enum_as_int_impl {
    ($from:ty, $($t:ident)+) => {
        impl $from {
            $(
            #[inline]
            pub(crate) const fn $t(self) -> $t {
                self as $t
            }
            )+
        }
        $(impl From<$from> for $t {
            #[inline]
            fn from(value: $from) -> $t {
                value.$t()
            }
        })+
    }
}

pub(crate) trait AppendAscii {
    type Error;

    fn append_ascii(&mut self, ascii_char: char) -> Result<(), Self::Error>;
    fn reserve(&mut self, additional: usize);

    fn append_u32(&mut self, n: u32) -> Result<(), Self::Error> {
        if n >= 1_000_000_000 {
            self.append_ascii(char::from(b'0' + ((n / 1_000_000_000) % 10) as u8))?;
        }
        if n >= 100_000_000 {
            self.append_ascii(char::from(b'0' + ((n / 100_000_000) % 10) as u8))?;
        }
        if n >= 10_000_000 {
            self.append_ascii(char::from(b'0' + ((n / 10_000_000) % 10) as u8))?;
        }
        if n >= 1_000_000 {
            self.append_ascii(char::from(b'0' + ((n / 1_000_000) % 10) as u8))?;
        }
        if n >= 100_000 {
            self.append_ascii(char::from(b'0' + ((n / 100_000) % 10) as u8))?;
        }
        if n >= 10_000 {
            self.append_ascii(char::from(b'0' + ((n / 10_000) % 10) as u8))?;
        }
        if n >= 1_000 {
            self.append_ascii(char::from(b'0' + ((n / 1_000) % 10) as u8))?;
        }
        if n >= 100 {
            self.append_ascii(char::from(b'0' + ((n / 100) % 10) as u8))?;
        }
        if n >= 10 {
            self.append_ascii(char::from(b'0' + ((n / 10) % 10) as u8))?;
        }
        self.append_ascii(char::from(b'0' + (n % 10) as u8))
    }
}

impl AppendAscii for fmt::Formatter<'_> {
    type Error = fmt::Error;

    fn reserve(&mut self, _additional: usize) {}

    fn append_ascii(&mut self, ascii_char: char) -> Result<(), Self::Error> {
        self.write_char(ascii_char)
    }
}

impl<const CAP: usize> AppendAscii for ArrayString<CAP> {
    type Error = core::convert::Infallible;

    fn reserve(&mut self, _additional: usize) {}

    fn append_ascii(&mut self, ascii_char: char) -> Result<(), Self::Error> {
        self.push(ascii_char);
        Ok(())
    }
}

#[cfg(feature = "alloc")]
impl AppendAscii for alloc::string::String {
    type Error = core::convert::Infallible;

    fn reserve(&mut self, additional: usize) {
        self.reserve(additional);
    }

    fn append_ascii(&mut self, ascii_char: char) -> Result<(), Self::Error> {
        self.push(ascii_char);
        Ok(())
    }
}

#[cfg(feature = "alloc")]
impl AppendAscii for alloc::vec::Vec<u8> {
    type Error = core::convert::Infallible;

    fn reserve(&mut self, additional: usize) {
        self.reserve(additional);
    }

    fn append_ascii(&mut self, ascii_char: char) -> Result<(), Self::Error> {
        self.push(ascii_char as u8);
        Ok(())
    }
}

#[cfg(feature = "std")]
pub(crate) struct WriteAscii<W>(pub W);

#[cfg(feature = "std")]
impl<W: std::io::Write> AppendAscii for WriteAscii<W> {
    type Error = std::io::Error;

    fn reserve(&mut self, _additional: usize) {}

    fn append_ascii(&mut self, ascii_char: char) -> Result<(), Self::Error> {
        let buf = [ascii_char as u8];
        self.0.write_all(&buf[..])
    }
}
