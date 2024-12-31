//! Probe Syzygy endgame tablebases.
//!
//! [Syzygy tables](https://syzygy-tables.info/#syzygy) allow optimal play
//! with and without regard to the 50-move rule.
//! Tables are available for positions with up to 7 pieces.
//!
//! # Example
//!
//! ```
//! use shakmaty::{CastlingMode, Chess, fen::Fen};
//! use shakmaty_syzygy::{Tablebase, MaybeRounded, Wdl, Dtz, Syzygy};
//!
//! let mut tables = Tablebase::new();
//! tables.add_directory("tables/chess")?;
//!
//! let pos: Chess = "8/8/8/8/B7/N7/K2k4/8 b - - 0 1"
//!     .parse::<Fen>()?
//!     .into_position(CastlingMode::Standard)?;
//!
//! let wdl = tables.probe_wdl_after_zeroing(&pos)?;
//! assert_eq!(wdl, Wdl::Loss);
//!
//! let dtz = tables.probe_dtz(&pos)?;
//! assert!(matches!(dtz, MaybeRounded::Rounded(Dtz(-59))));
//! # Ok::<_, Box<dyn std::error::Error>>(())
//! ```
//!
//! # Errors
//!
//! See [`SyzygyError`] for possible error
//! conditions.
//!
//! # Cargo features
//!
//! * `mmap`: Enables support for memory-mapped tablebase files
//!   via `Tablebase::with_mmap_filesystem()`.
//! * `variant`: Enables support for Antichess and Atomic chess.

#![doc(html_root_url = "https://docs.rs/shakmaty-syzygy/0.25.2")]
#![warn(missing_debug_implementations)]
#![cfg_attr(docs_rs, feature(doc_auto_cfg))]

#[macro_use]
mod errors;
pub mod filesystem;
mod material;
mod table;
mod tablebase;
mod types;

pub use crate::{
    errors::{ProbeError, SyzygyError},
    material::Material,
    tablebase::Tablebase,
    types::{AmbiguousWdl, Dtz, MaybeRounded, Metric, Syzygy, TableType, Wdl},
};
