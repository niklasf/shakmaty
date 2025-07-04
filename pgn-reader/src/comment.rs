use std::fmt;

/// A comment, excluding the braces.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
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
mod tests {}
