use std::num::NonZeroUsize;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Config<Skip> {
    /// Whether to skip variations.
    /// This dictates what the [`Writer`](super::Writer) returns in [`Visitor::begin_variation`](crate::Visitor::begin_variation)
    /// and whether it does anything.
    ///
    /// Defaults to `|| false`.
    pub skip_variations: Skip,
    /// Defaults to `1`.
    pub starting_move_number: NonZeroUsize,
}

impl Default for Config<fn() -> bool> {
    fn default() -> Self {
        Self {
            skip_variations: || false,
            starting_move_number: NonZeroUsize::MIN,
        }
    }
}
