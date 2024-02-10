use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    sync::{atomic::AtomicBool, Arc},
};

use parking_lot::{Condvar, Mutex, RwLock};
use paste::paste;

use super::Symbol;
use crate::{
    arena::ID,
    symbol::{
        AdtImplementation, AdtImplementationConstant,
        AdtImplementationFunction, AdtImplementationType, Constant, Enum,
        Function, GlobalID, NegativeTraitImplementation, Struct, Trait,
        TraitConstant, TraitFunction, TraitImplementation,
        TraitImplementationConstant, TraitImplementationFunction,
        TraitImplementationType, TraitType, Type, Variant,
    },
};

#[derive(Debug)]
pub struct Synchronization<F> {
    notify: Condvar,
    building: AtomicBool,
    state_flag: Mutex<F>,
}

#[derive(Debug)]
pub struct State<S: Symbol> {
    syntax_tree: Arc<S::SyntaxTree>,
    synchronization: Arc<Synchronization<S::Flag>>,
    data: Arc<Mutex<S::Data>>,

    /// Maps the state of the symbol to the set of symbols that depends on it.
    ///
    /// This is used to detect cyclic dependency, notifying the symbols that
    /// depends on it when the symbol is finalized, and avoiding deadlock
    /// when building the symbol.
    dependencies: HashMap<S::Flag, HashSet<GlobalID>>,
}

#[derive(Debug, Default, derive_more::Deref, derive_more::DerefMut)]
pub struct Builder(RwLock<Representation>);

#[derive(Debug, Default)]
pub struct Representation {
    adt_implementation_constants: HashMap<
        ID<AdtImplementationConstant>,
        State<AdtImplementationConstant>,
    >,
    adt_implementation_functions: HashMap<
        ID<AdtImplementationFunction>,
        State<AdtImplementationFunction>,
    >,
    adt_implementation_types:
        HashMap<ID<AdtImplementationType>, State<AdtImplementationType>>,
    adt_implementations:
        HashMap<ID<AdtImplementation>, State<AdtImplementation>>,

    constants: HashMap<ID<Constant>, State<Constant>>,
    enums: HashMap<ID<Enum>, State<Enum>>,
    functions: HashMap<ID<Function>, State<Function>>,
    structs: HashMap<ID<Struct>, State<Struct>>,

    trait_constants: HashMap<ID<TraitConstant>, State<TraitConstant>>,
    trait_functions: HashMap<ID<TraitFunction>, State<TraitFunction>>,
    trait_implementation_constants: HashMap<
        ID<TraitImplementationConstant>,
        State<TraitImplementationConstant>,
    >,
    trait_implementation_functions: HashMap<
        ID<TraitImplementationFunction>,
        State<TraitImplementationFunction>,
    >,
    trait_implementation_types:
        HashMap<ID<TraitImplementationType>, State<TraitImplementationType>>,
    trait_implementations:
        HashMap<ID<TraitImplementation>, State<TraitImplementation>>,
    negative_trait_implementations: HashMap<
        ID<NegativeTraitImplementation>,
        State<NegativeTraitImplementation>,
    >,
    trait_types: HashMap<ID<TraitType>, State<TraitType>>,
    traits: HashMap<ID<Trait>, State<Trait>>,
    types: HashMap<ID<Type>, State<Type>>,
    variants: HashMap<ID<Variant>, State<Variant>>,
}

/// The trait used for retrieving the states of a particular symbol in
/// [`Representation`]
pub trait Element: Sized + Symbol {
    fn get_states(
        representation: &Representation,
    ) -> &HashMap<ID<Self>, State<Self>>;
    fn get_states_mut(
        representation: &mut Representation,
    ) -> &mut HashMap<ID<Self>, State<Self>>;
}

macro_rules! implements_element {
    ($symbol:ident) => {
        impl Element for $symbol {
            fn get_states(
                representation: &Representation,
            ) -> &HashMap<ID<Self>, State<Self>> {
                paste! {
                    &representation.[< $symbol:snake s >]
                }
            }

            fn get_states_mut(
                representation: &mut Representation,
            ) -> &mut HashMap<ID<Self>, State<Self>> {
                paste! {
                    &mut representation.[< $symbol:snake s >]
                }
            }
        }
    };
}

implements_element!(AdtImplementationConstant);
implements_element!(AdtImplementationFunction);
implements_element!(AdtImplementationType);
implements_element!(AdtImplementation);
implements_element!(Constant);
implements_element!(Enum);
implements_element!(Function);
implements_element!(Struct);
implements_element!(TraitConstant);
implements_element!(TraitFunction);
implements_element!(TraitImplementationConstant);
implements_element!(TraitImplementationFunction);
implements_element!(TraitImplementationType);
implements_element!(TraitImplementation);
implements_element!(NegativeTraitImplementation);
implements_element!(TraitType);
implements_element!(Trait);
implements_element!(Type);
implements_element!(Variant);

impl Builder {
    /// Adds a new symbol to the builder and sets its state to drafting.
    ///
    /// If there exists a symbol with the same ID, then the symbol is not
    /// added and the function returns `false`. Otherwise, returns `true`.
    #[must_use]
    pub fn draft_symbol<T: Symbol + Element>(
        &self,
        id: ID<T>,
        syntax_tree: T::SyntaxTree,
    ) -> bool {
        let mut self_write = self.write();

        #[allow(clippy::significant_drop_in_scrutinee)]
        match T::get_states_mut(&mut self_write).entry(id) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                entry.insert(State {
                    syntax_tree: Arc::new(syntax_tree),
                    synchronization: Arc::new(Synchronization {
                        notify: Condvar::new(),
                        building: AtomicBool::new(false),
                        state_flag: Mutex::new(T::Flag::default()),
                    }),
                    data: Arc::new(Mutex::new(T::Data::default())),
                    dependencies: HashMap::new(),
                });
                true
            }
        }
    }
}
