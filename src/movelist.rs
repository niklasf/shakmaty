use std::mem;
use std::slice;
use std::ops::{Deref, DerefMut};
use std::ptr;

use types::Move;

pub unsafe trait Array {
    type Item;
    const CAPACITY: usize;

    fn as_ptr(&self) -> *const Self::Item;
    fn as_mut_ptr(&mut self) -> *mut Self::Item;
}

unsafe impl<T> Array for [T; 512] {
    type Item = T;
    const CAPACITY: usize = 512;
    fn as_ptr(&self) -> *const T { self as *const _ as *const _ }
    fn as_mut_ptr(&mut self) -> *mut T { self as *mut _ as *mut _ }
}

pub struct StackVec<A: Array>
    where A::Item: Copy
{
    xs: A,
    len: usize,
}

impl<A: Array> StackVec<A> where A::Item: Copy {
    pub fn new() -> StackVec<A> {
        StackVec {
            xs: unsafe { mem::uninitialized() },
            len: 0,
        }
    }

    #[inline]
    pub fn len(&self) -> usize { self.len }

    #[inline]
    pub fn capacity(&self) -> usize { A::CAPACITY }

    #[inline]
    pub fn push(&mut self, element: A::Item) {
        assert!(self.len() < A::CAPACITY);
        unsafe {
            self.push_unchecked(element);
        }
    }

    #[inline]
    pub unsafe fn push_unchecked(&mut self, element: A::Item) {
        let len = self.len();
        debug_assert!(len < A::CAPACITY);
        ptr::write(self.get_unchecked_mut(len), element);
        self.len = len + 1;
    }

    pub fn retain<F>(&mut self, mut f: F)
        where F: FnMut(&mut A::Item) -> bool
    {
        let base = self.xs.as_mut_ptr();
        let mut cur = 0;

        while cur < self.len {
            if !f(unsafe { self.get_unchecked_mut(cur) }) {
                self.len -= 1;
                unsafe {
                    *base.offset(cur as isize) = *base.offset(self.len as isize);
                }
            } else {
                cur += 1;
            }
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.len = 0;
    }
}

impl<'a, A: Array> IntoIterator for &'a StackVec<A> where A::Item: Copy {
    type Item = &'a A::Item;
    type IntoIter = slice::Iter<'a, A::Item>;

    fn into_iter(self) -> Self::IntoIter { self.iter() }
}

impl<A: Array> Default for StackVec<A> where A::Item: Copy {
    fn default() -> StackVec<A> {
        StackVec::new()
    }
}

impl<A: Array> Deref for StackVec<A> where A::Item: Copy {
    type Target = [A::Item];

    #[inline]
    fn deref(&self) -> &[A::Item] {
        unsafe {
            slice::from_raw_parts(self.xs.as_ptr(), self.len())
        }
    }
}

impl<A: Array> DerefMut for StackVec<A> where A::Item: Copy {
    #[inline]
    fn deref_mut(&mut self) -> &mut [A::Item] {
        unsafe {
            slice::from_raw_parts_mut(self.xs.as_mut_ptr(), self.len())
        }
    }
}

impl<A: Array> AsMut<[A::Item]> for StackVec<A> where A::Item: Copy {
    #[inline]
    fn as_mut(&mut self) -> &mut [A::Item] { self }
}

/// A stack allocated container for moves.
pub type MoveList = StackVec<[Move; 512]>;
