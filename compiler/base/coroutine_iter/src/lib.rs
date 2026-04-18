//! This crate provides macros to easily create [`Iterator`]s using the nightly
//! Rust coroutines feature.
//!
//! # Nightly Requirement
//!
//! To use the macros exported by this crate, the user code must enable the
//! `coroutines` and `stmt_expr_attributes` features:
//!
//! ```rust,ignore
//! #![feature(coroutines, stmt_expr_attributes)]
//! ```
//! You might also need the `coroutine_trait` depending on your usage.
//!
//! # Macros
//!
//! - [`coroutine_iter!`]: Creates an `Iterator` from a standard (movable)
//!   coroutine. **Recommendation:** Use this first. As long as your coroutine
//!   logic doesn't hold references to local variables across `yield` points
//!   (i.e., it isn't self-referential), this avoids heap allocation.
//!
//! - [`static_coroutine_iter!`]: Creates an `Iterator` from a `static` (pinned)
//!   coroutine. This creates a heap-allocated `Box::pin(...)` behind the
//!   scenes. Use this when your generator needs to keep internal references
//!   across `yield` points (i.e., self-referential iterators).
//!
//! # Usage
//!
//! ```rust,ignore
//! fn gen_numbers(arr: &[i32]) -> impl Iterator<Item = &i32> {
//!     // No heap allocation needed, as we just iterate over `arr`
//!     coroutine_iter!({
//!         for num in arr {
//!             yield num;
//!         }
//!     })
//! }
//!
//! fn gen_owned_numbers() -> impl Iterator<Item = i32> {
//!     // Heap allocation needed because `num` references `a`, and we yield across that borrow     
//!     static_coroutine_iter!({
//!         let a = vec![1, 2, 3];
//!         for num in &a {
//!             yield *num;
//!         }
//!     })
//! }
//! ```

#![allow(unused_features)]
#![feature(coroutine_trait, coroutines)]

use std::{ops::Coroutine, pin::Pin};

#[derive(Debug)]
pub struct GeneratorCoroutine<T>(pub T);

impl<T: Coroutine + Unpin> Iterator for GeneratorCoroutine<T> {
    type Item = T::Yield;

    fn next(&mut self) -> Option<Self::Item> {
        match Pin::new(&mut self.0).resume(()) {
            std::ops::CoroutineState::Yielded(item) => Some(item),
            std::ops::CoroutineState::Complete(_) => None,
        }
    }
}

impl<T: Coroutine> Iterator for Pin<Box<GeneratorCoroutine<T>>> {
    type Item = T::Yield;

    fn next(&mut self) -> Option<Self::Item> {
        let pin_mut = self.as_mut();

        // SAFETY: Since the coroutine itself is pinned, we can safely create a
        // pinned reference to it.
        let this_pin =
            unsafe { Pin::new_unchecked(&mut pin_mut.get_unchecked_mut().0) };

        match this_pin.resume(()) {
            std::ops::CoroutineState::Yielded(item) => Some(item),
            std::ops::CoroutineState::Complete(_) => None,
        }
    }
}

/// Creates an `Iterator` using a standard (movable) coroutine.
///
/// **Recommendation:** Prefer using this macro over [`static_coroutine_iter!`]
/// if your iteration logic is not self-referential (i.e., it does not hold
/// references to local variables across `yield` points). It avoids
/// heap-allocating the coroutine state.
#[macro_export]
macro_rules! coroutine_iter {
    ($b:block) => {
        $crate::GeneratorCoroutine(
            #[coroutine]
            move || $b,
        )
    };
}

/// Creates an `Iterator` using a `static` (pinned) coroutine.
///
/// Use this macro when your coroutine logic needs to hold references to local
/// variables across `yield` points (making it a self-referential generator).
/// Under the hood, this heap-allocates the state using `Box::pin(...)`.
#[macro_export]
macro_rules! static_coroutine_iter {
    ($b:block) => {
        $crate::GeneratorCoroutine(Box::pin(
            #[coroutine]
            static move || $b,
        ))
    };
}

#[cfg(test)]
mod test {
    #[test]
    fn normal_coroutine() {
        fn gen_numbers(a: &Vec<i32>) -> impl for<'a> Iterator<Item = &i32> {
            super::coroutine_iter!({
                for num in a {
                    yield num;
                }
            })
        }

        let numbers = vec![1, 2, 3];
        for (expect, num) in (1..).zip(gen_numbers(&numbers)) {
            assert_eq!(*num, expect);
        }
    }

    #[test]
    fn self_referential_coroutine() {
        fn gen_numbers() -> impl Iterator<Item = i32> {
            super::static_coroutine_iter!({
                let a = vec![1, 2, 3];

                for num in &a {
                    yield *num;
                }
            })
        }

        for (expect, num) in (1..).zip(gen_numbers()) {
            assert_eq!(num, expect);
        }
    }
}
