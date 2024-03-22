use std::{fmt::Debug, hash::Hash};

use pernixc_base::diagnostic::Handler;

use super::finalizer::Finalizer;
use crate::{arena::ID, error, table::Table};

mod occurrences;

pub use occurrences::Occurrences;

pub(in crate::table::building) mod adt_implementation;
pub(in crate::table::building) mod adt_implementation_constant;
pub(in crate::table::building) mod adt_implementation_function;
pub(in crate::table::building) mod adt_implementation_type;
pub(in crate::table::building) mod constant;
pub(in crate::table::building) mod r#enum;
pub(in crate::table::building) mod function;
pub(in crate::table::building) mod negative_trait_implementation;
pub(in crate::table::building) mod r#struct;
pub(in crate::table::building) mod r#trait;
pub(in crate::table::building) mod trait_constant;
pub(in crate::table::building) mod trait_function;
pub(in crate::table::building) mod trait_implementation;
pub(in crate::table::building) mod trait_implementation_constant;
pub(in crate::table::building) mod trait_implementation_function;
pub(in crate::table::building) mod trait_implementation_type;
pub(in crate::table::building) mod trait_type;
pub(in crate::table::building) mod r#type;
pub(in crate::table::building) mod variant;

/// Trait that all flags must implement.
pub trait Flag:
    Debug
    + Copy
    + Clone
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Send
    + Sync
    + Hash
    + 'static
{
    /// The first state of the flag.
    fn first() -> Self;

    /// The last state of the flag.
    fn last() -> Self;

    /// Increments the flag to the next state.
    fn increment(&mut self);
}

/// A trait for finalizing a symbol.
pub trait Finalize {
    type SyntaxTree: Debug;
    type Flag: Flag;
    type Data: Debug + Send + Sync + Default;

    fn finalize(
        table: &Table<Finalizer>,
        symbol_id: ID<Self>,
        state_flag: Self::Flag,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    );
}

macro_rules! count {
    () => (0u8);
    ( $x:ident $(, $xs:ident)* $(,)? ) => (1u8 + $crate::table::building::finalizing::finalize::count!($($xs, )*));
}

macro_rules! build_flag {
    (
        $vis:vis enum $name:ident {
            $(
                $(#[$meta:meta])?
                $variant:ident,
            )*
        }
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(u8)]
        $vis enum $name {
            $(
                $(#[$meta])?
                $variant,
            )*
        }

        impl $crate::table::building::finalizing::finalize::Flag for $name {
            fn first() -> Self {
                unsafe { std::mem::transmute(0u8) }
            }

            fn last() -> Self {
                unsafe { std::mem::transmute($crate::table::building::finalizing::finalize::count!($($variant),*) - 1) }
            }

            fn increment(&mut self) {
                let last = $crate::table::building::finalizing::finalize::Flag::last();

                if *self == last {
                    return
                }

                *self = unsafe { std::mem::transmute(std::mem::transmute::<Self, u8>(*self) + 1) };
            }
        }
    };
}

use build_flag;
use count;
