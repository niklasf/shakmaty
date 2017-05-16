use arrayvec::{Array, ArrayVec};

/// Retains only the elements specified by the predicate.
///
/// Unlike `vec.retain()` it does not preserve the order of the retained
/// elements.
pub fn swap_retain<A: Array, F>(vec: &mut ArrayVec<A>, mut f: F)
    where F: FnMut(&mut A::Item) -> bool
{
    let mut i = 0;
    while i < vec.len() {
        if f(&mut vec[i]) {
            i += 1;
        } else {
            vec.swap_remove(i);
        }
    }
}
