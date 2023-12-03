use std::{
    collections::HashMap,
    fmt::Debug,
    sync::{atomic::AtomicBool, Arc},
};

use parking_lot::{Condvar, Mutex, RwLock};

use super::Table;
use crate::{
    arena::{Arena, ID},
    symbol::{
        Constant, Enum, Function, Implementation, ImplementationFunction, Struct, Trait,
        TraitConstant, TraitFunction, TraitType, Type, Variant,
    },
};

mod constant;
mod r#enum;
mod function;
mod implementaiton_function;
mod implementation;
mod implementation_constant;
mod implementation_type;
mod negative_implementation;
mod r#struct;
mod r#trait;
mod trait_constant;
mod trait_function;
mod trait_type;
mod r#type;
mod variant;

/// The trait for flag, indicating the state of a symbol.
trait Flag: Debug + Copy + Clone + PartialEq + Eq + PartialOrd + Ord + Send + Sync + 'static {
    /// The state of a symbol when it is first created.
    fn drafting_state() -> Self;

    /// The state of a symbol when it is fully defined.
    fn final_state() -> Self;

    /// Increments the state of a symnol.
    fn increment(&mut self);
}

/// The trait for a symbol in the symbol table.
trait Symbol: Sized + 'static {
    /// The type of the flag.
    type Flag: Flag;

    /// The type of the syntax tree which the symbol based on.
    type SyntaxTree: Debug;

    /// The type of the data associated while building the symbol.
    type Data: Debug + Send + Sync + Default;

    fn get_arena(table: &Table) -> &Arena<RwLock<Self>, ID<Self>>;
    fn get_arena_mut(table: &mut Table) -> &mut Arena<RwLock<Self>, ID<Self>>;
}

macro_rules! count {
    () => (0u8);
    ( $x:ident $(, $xs:ident)* $(,)? ) => (1u8 + $crate::table::state::count!($($xs, )*));
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

        impl $crate::table::state::Flag for $name {
            fn drafting_state() -> Self {
                unsafe { std::mem::transmute(0u8) }
            }

            fn final_state() -> Self {
                unsafe { std::mem::transmute($crate::table::state::count!($($variant),*)) }
            }

            fn increment(&mut self) {
                let last = $crate::table::state::Flag::final_state();

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

/// The object used to synchronize the building of various interdependent symbols.
#[derive(Debug)]
struct Synchronization<F: Flag> {
    notify: Condvar,
    building: AtomicBool,
    finalize_flag: Mutex<F>,
}

#[derive(Debug)]
pub(super) struct State<S: Symbol> {
    syntax_tree: Arc<S::SyntaxTree>,
    synchronization: Synchronization<S::Flag>,
    data: Arc<Mutex<S::Data>>,
}

#[derive(Debug, Default)]
pub struct Builder {
    states_by_enum_id: HashMap<ID<Enum>, State<Enum>>,
    states_by_variant_id: HashMap<ID<Variant>, State<Variant>>,

    states_by_struct_id: HashMap<ID<Struct>, State<Struct>>,
    states_by_constant_id: HashMap<ID<Constant>, State<Constant>>,
    states_by_type_id: HashMap<ID<Type>, State<Type>>,
    states_by_function_id: HashMap<ID<Function>, State<Function>>,

    states_by_trait_id: HashMap<ID<Trait>, State<Trait>>,
    states_by_trait_constant_id: HashMap<ID<TraitConstant>, State<TraitConstant>>,
    states_by_trait_type_id: HashMap<ID<TraitType>, State<TraitType>>,
    states_by_trait_function_id: HashMap<ID<TraitFunction>, State<TraitFunction>>,

    states_by_negative_implementation_id: HashMap<ID<Implementation>, State<Implementation>>,
    states_by_implementation_id: HashMap<ID<Implementation>, State<Implementation>>,
    states_by_implementation_function_id:
        HashMap<ID<ImplementationFunction>, State<ImplementationFunction>>,
    states_by_implementation_constant_id:
        HashMap<ID<ImplementationFunction>, State<ImplementationFunction>>,
    states_by_implementation_type_id:
        HashMap<ID<ImplementationFunction>, State<ImplementationFunction>>,
}
