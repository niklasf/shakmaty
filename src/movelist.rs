use types::Move;

use arrayvec::{Array, ArrayVec};

/// A stack allocated container for legal moves.
pub type MoveList = ArrayVec<[Move; 512]>;

pub trait ArrayVecExt {
    type Item;

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all elements `e` such that `f(&e)` returns
    /// `false`.
    ///
    /// Like `ArrayVec::retain()`, but does not preserve order.
    fn swap_retain<F>(&mut self, f: F)
        where F: FnMut(&mut Self::Item) -> bool;
}

impl<A: Array> ArrayVecExt for ArrayVec<A> {
    type Item = A::Item;

    fn swap_retain<F>(&mut self, mut f: F)
        where F: FnMut(&mut Self::Item) -> bool
    {
        let mut i = 0;
        while i < self.len() {
            if f(&mut self[i]) {
                i += 1;
            } else {
                self.swap_remove(i);
            }
        }
    }
}
