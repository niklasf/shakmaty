use std::{backtrace::Backtrace, error::Error, fmt, io};

use crate::{
    material::{Material, NormalizedMaterial},
    types::Metric,
};

pub type SyzygyResult<T> = Result<T, SyzygyError>;

pub type ProbeResult<T> = Result<T, ProbeError>;

/// Error when probing tablebase.
#[derive(Debug)]
pub enum SyzygyError {
    /// Position has castling rights, but Syzygy tables do not contain
    /// positions with castling rights.
    Castling,
    /// Position has too many pieces, i.e., more pieces than any opened table.
    /// Syzygy tables for standard chess support up to 7 pieces.
    TooManyPieces,
    /// Missing table.
    MissingTable {
        #[allow(missing_docs)]
        metric: Metric,
        #[allow(missing_docs)]
        material: Material,
    },
    /// Probe failed.
    ProbeFailed {
        #[allow(missing_docs)]
        metric: Metric,
        #[allow(missing_docs)]
        material: Material,
        #[allow(missing_docs)]
        error: Box<ProbeError>,
    },
}

impl fmt::Display for SyzygyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyzygyError::Castling => write!(
                f,
                "syzygy tables do not contain position with castling rights"
            ),
            SyzygyError::TooManyPieces => write!(f, "too many pieces"),
            SyzygyError::MissingTable { metric, material } => {
                write!(f, "required {metric} table not found: {material}")
            }
            SyzygyError::ProbeFailed {
                metric,
                material,
                error,
            } => write!(f, "failed to probe {metric} table {material}: {error}"),
        }
    }
}

impl Error for SyzygyError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            SyzygyError::ProbeFailed { error, .. } => Some(error),
            _ => None,
        }
    }
}

/// Error when probing a table.
#[derive(Debug)]
pub enum ProbeError {
    /// I/O error.
    Read {
        #[allow(missing_docs)]
        error: io::Error,
    },
    /// Table file has unexpected magic header bytes.
    Magic {
        #[allow(missing_docs)]
        magic: [u8; 4],
    },
    /// Corrupted table.
    CorruptedTable {
        #[allow(missing_docs)]
        backtrace: Backtrace,
    },
}

impl fmt::Display for ProbeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProbeError::Read { error } => write!(f, "i/o error reading table file: {error}"),
            ProbeError::Magic { magic } => write!(f, "invalid magic header bytes: {magic:x?}"),
            ProbeError::CorruptedTable { backtrace } => write!(f, "corrupted table: {backtrace}"),
        }
    }
}

impl Error for ProbeError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ProbeError::Read { error } => Some(error),
            _ => None,
        }
    }
}

pub trait ProbeResultExt<T> {
    fn ctx(self, metric: Metric, material: &NormalizedMaterial) -> SyzygyResult<T>;
}

impl<T> ProbeResultExt<T> for ProbeResult<T> {
    fn ctx(self, metric: Metric, material: &NormalizedMaterial) -> SyzygyResult<T> {
        self.map_err(|error| SyzygyError::ProbeFailed {
            metric,
            material: material.inner().clone(),
            error: Box::new(error),
        })
    }
}

impl From<io::Error> for ProbeError {
    fn from(error: io::Error) -> ProbeError {
        match error.kind() {
            io::ErrorKind::UnexpectedEof => ProbeError::CorruptedTable {
                backtrace: Backtrace::capture(),
            },
            _ => ProbeError::Read { error },
        }
    }
}

/// Return a `CorruptedTable` error.
macro_rules! throw {
    () => {
        return Err(crate::errors::ProbeError::CorruptedTable {
            backtrace: ::std::backtrace::Backtrace::capture(),
        })
    };
}

/// Unwrap an `Option` or return a `CorruptedTable` error.
macro_rules! u {
    ($e:expr) => {
        match $e {
            Some(ok) => ok,
            None => throw!(),
        }
    };
}

/// Ensure that a condition holds. Otherwise return a `CorruptedTable` error.
macro_rules! ensure {
    ($cond:expr) => {
        if !$cond {
            throw!();
        }
    };
}
