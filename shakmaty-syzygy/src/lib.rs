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
//! Syzygy tables allow optimal play under the 50-move rule. Tables are
//! available for positions with up to 6 (and experimentally 7) pieces.
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

#![doc(html_root_url = "https://docs.rs/shakmaty-syzygy/0.4.1")]
#![warn(missing_debug_implementations)]
#![cfg_attr(feature = "cargo-clippy", allow(needless_range_loop))]
#![cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))]

extern crate arrayvec;
#[macro_use]
extern crate bitflags;
extern crate bit_vec;
extern crate byteorder;
#[macro_use(Fail)]
extern crate failure;
extern crate itertools;
#[macro_use]
extern crate lazy_static;
extern crate double_checked_cell;
extern crate fxhash;
extern crate num_integer;
extern crate positioned_io;
extern crate shakmaty;
#[cfg(feature = "serde-1")]
extern crate serde;

#[macro_use]
mod errors;
mod material;
mod table;
mod tablebase;
mod types;

pub use errors::SyzygyError;
pub use tablebase::Tablebase;
pub use types::{Dtz, Syzygy, TableType, Wdl};
