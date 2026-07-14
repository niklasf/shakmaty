//! Tablebase implementation that allows plugging in custom providers
//! for asynchronous I/O operations.
//!
//! The implementation is runtime-agnostic. No concrete providers are included
//! in this crate.

mod filesystem;
mod table;
mod tablebase;

pub use crate::aio::{
    filesystem::{Filesystem, RandomAccessFile, ReadHint},
    tablebase::Tablebase,
};
