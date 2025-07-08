use std::io;

/// Error when using [`Visitor`](crate::Visitor) methods on a [`Writer`](super::Writer).
#[derive(Debug)]
pub enum Error {
    /// A token ([`Visitor`](crate::Visitor) method) which was not allowed was called.
    InvalidToken {
        token: super::MovetextToken,
        allowed: super::MovetextToken,
    },
    Io(io::Error),
}
