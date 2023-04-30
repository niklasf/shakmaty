use core::{convert::TryFrom as _, num::TryFromIntError};

pub(crate) fn overflow_error() -> TryFromIntError {
    // This is a hack to construct TryFromIntError despite its private
    // constructor. The standard library keeps it private intentionally,
    // to be able to provide error details in the future, but it is unlikely
    // that something more specific than "overflow" will be added.
    u32::try_from(u64::MAX).unwrap_err()
}
