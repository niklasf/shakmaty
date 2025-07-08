use std::io;

/// Error when using [`Visitor`](crate::Visitor) methods on a [`Writer`](super::Writer).
#[derive(Debug)]
pub enum Error {
    /// [`Writer::begin_variation`](super::Writer::begin_variation) was called before
    /// any [`Writer::san`](super::Writer::san) was called.
    ///
    /// A movetext must not begin with a variation.
    ImmediateVariation,
    /// A method other than [`Writer::end_variation`](super::Writer::end_variation)
    /// was called after [`Writer::outcome`](super::Writer::outcome) was called.
    ///
    /// Outcomes are the last thing that can appear in a variation, except for variation closing
    /// parentheses.
    WritingAfterOutcome,
    Io(io::Error),
}
