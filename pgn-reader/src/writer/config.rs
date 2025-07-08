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
    /// Whether to always include move numbers.
    ///
    /// Defaults to `false`.
    pub always_include_move_number: bool,
    /// Whether to include a space after a move number (`"1. e4"` if `true`, `"1.e4"` otherwise).
    ///
    /// Defaults to `true`.
    pub space_after_move_number: bool,
    /// Whether to include spaces around variation parentheses (`"( "` and `") "` if `true`, `"("` and `")"` otherwise).
    ///
    /// Defaults to `true`.
    pub space_around_variation: bool,
    /// Whether to include spaces around comment braces (`"{ "` and `" } "` if `true`, `"{"` and `"}"` otherwise).
    ///
    /// Defaults to `true`.
    pub space_around_comments: bool,
}

impl Config<fn() -> bool> {
    /// A space optimized [`Config`]. Not recommended as it won't make much of a difference
    /// and might break certain parsers.
    pub const COMPACT: Self = Self {
        skip_variations: || false,
        starting_move_number: NonZeroUsize::new(1).unwrap(),
        always_include_move_number: false,
        space_after_move_number: false,
        space_around_variation: false,
        space_around_comments: false,
    };
}

impl Default for Config<fn() -> bool> {
    /// Optimized for compatibility.
    fn default() -> Self {
        Self {
            skip_variations: || false,
            starting_move_number: NonZeroUsize::MIN,
            always_include_move_number: false,
            space_after_move_number: true,
            space_around_variation: true,
            space_around_comments: true,
        }
    }
}
