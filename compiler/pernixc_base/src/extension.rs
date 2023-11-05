//! Contains extension traits for the standard library.

/// An extension trait with methods for by-reference mutation and inspection of a [`Cell`].
///
/// This is useful when you want to inspect or mutate a value inside a [`Cell`] without making a
/// whole copy of it first.
///
/// # Soundness
///
/// This extension doesn't leak any inner references to the outside world, so it's sound.
pub trait CellExt<T> {
    /// Inspects the value inside the [`Cell`] by reference.
    fn visit<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R;

    /// Mutates the value inside the [`Cell`] by reference.
    fn visit_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R;
}

impl<T> CellExt<T> for std::cell::Cell<T> {
    #[inline]
    fn visit<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        unsafe { f(&*self.as_ptr()) }
    }

    #[inline]
    fn visit_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut T) -> R,
    {
        unsafe { f(&mut *self.as_ptr()) }
    }
}
