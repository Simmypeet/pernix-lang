use std::{fmt::Debug, hash::Hash};

use pernixc_base::diagnostic::Handler;

use super::building::Building;
use crate::{arena::ID, error, table::Table};

mod adt_implementation;
mod adt_implementation_constant;
mod adt_implementation_function;
mod adt_implementation_type;
mod constant;
mod r#enum;
mod function;
mod negative_trait_implementation;
mod r#struct;
mod r#trait;
mod trait_constant;
mod trait_function;
mod trait_implementation;
mod trait_implementation_constant;
mod trait_implementation_function;
mod trait_implementation_type;
mod trait_type;
mod r#type;
mod variant;

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

/// A trait for building a symbol.
pub trait Symbol {
    type SyntaxTree: Debug;
    type Flag: Flag;
    type Data: Debug + Send + Sync + Default;

    fn build(
        table: &Table<Building>,
        symbol_id: ID<Self>,
        state_flag: Self::Flag,
        syntax_tree: &Self::SyntaxTree,
        data: &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    );
}

macro_rules! count {
    () => (0u8);
    ( $x:ident $(, $xs:ident)* $(,)? ) => (1u8 + $crate::table::state::symbol::count!($($xs, )*));
}

macro_rules! build_flag {
    (
        $vis:vis enum $name:ident {
            $(
                $variant:ident,
            )*
        }
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(u8)]
        $vis enum $name {
            $(
                $variant,
            )*
        }

        impl $crate::table::state::symbol::Flag for $name {
            fn first() -> Self {
                unsafe { std::mem::transmute(0u8) }
            }

            fn last() -> Self {
                unsafe { std::mem::transmute($crate::table::state::symbol::count!($($variant),*) - 1) }
            }

            fn increment(&mut self) {
                let last = $crate::table::state::symbol::Flag::last();

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
