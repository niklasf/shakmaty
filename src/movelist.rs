// This file is part of the shakmaty library.
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

use types::Move;

use arrayvec::{Array, ArrayVec};

/// A container for moves that can be stored inline on the stack.
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
    where
        F: FnMut(&mut Self::Item) -> bool;
}

impl<A: Array> ArrayVecExt for ArrayVec<A> {
    type Item = A::Item;

    fn swap_retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Self::Item) -> bool,
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
