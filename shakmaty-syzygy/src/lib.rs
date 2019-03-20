// This file is part of the shakmaty-syzygy library.
// Copyright (C) 2017-2018 Niklas Fiekas <niklas.fiekas@backscattering.de>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.

//! Probe Syzygy endgame tablebases.
//!
//! [Syzygy tables](https://syzygy-tables.info/#syzygy) allow optimal play
//! under the 50-move rule. Tables are available for positions with up to
//! 7 pieces.
//!
//! # Example
//!
//! ```
//! # extern crate failure;
//! # extern crate shakmaty;
//! # extern crate shakmaty_syzygy;
//! #
//! # use failure::Error;
//! use shakmaty::Chess;
//! use shakmaty::fen::Fen;
//! use shakmaty_syzygy::{Tablebase, Wdl, Dtz, Syzygy};
//!
//! # fn try_main() -> Result<(), Error> {
//! let mut tables = Tablebase::new();
//! tables.add_directory("tables/regular")?;
//!
//! let pos: Chess = "8/8/8/8/B7/N7/K2k4/8 b - - 0 1"
//!     .parse::<Fen>()?
//!     .position()?;
//!
//! let wdl = tables.probe_wdl(&pos)?;
//! assert_eq!(wdl, Wdl::Loss);
//!
//! let dtz = tables.probe_dtz(&pos)?;
//! assert_eq!(dtz, Dtz(-59));
//! #
//! #     Ok(())
//! # }
//! #
//! # fn main() {
//! #     try_main().expect("success");
//! # }
//! ```
//!
//! # Errors
//!
//! See [`SyzygyError`](enum.SyzygyError.html) for possible error
//! conditions.

#![doc(html_root_url = "https://docs.rs/shakmaty-syzygy/0.11.0")]

#![warn(missing_debug_implementations)]
#![warn(rust_2018_idioms)]

#[macro_use]
mod errors;
mod material;
mod table;
mod tablebase;
mod types;

pub use crate::errors::{ProbeError, SyzygyError};
pub use crate::tablebase::Tablebase;
pub use crate::types::{Dtz, Metric, Syzygy, TableType, Wdl};

#[cfg(fuzzing)]
pub use crate::table::{DtzTable, WdlTable};
#[cfg(fuzzing)]
pub use crate::types::DecisiveWdl;
