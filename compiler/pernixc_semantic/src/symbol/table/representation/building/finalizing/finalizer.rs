//! Contains the definition of the [`Finalizer`] and its related types.

use std::{
    collections::{hash_map::Entry, BTreeSet, HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    sync::{
        atomic::{self, AtomicBool},
        Arc,
    },
};

use parking_lot::{Condvar, Mutex, RwLock};
use paste::paste;
use pernixc_base::handler::Handler;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use super::finalize::{Finalize, StateFlag};
use crate::{
    arena::ID,
    error::{self, CyclicDependency},
    symbol::{
        table::{representation::RwLockContainer, Building, Table},
        AdtImplementation, AdtImplementationFunction, Constant, Enum, Function,
        GlobalID, NegativeTraitImplementation, PositiveTraitImplementation,
        Struct, Trait, TraitConstant, TraitFunction,
        TraitImplementationConstant, TraitImplementationFunction,
        TraitImplementationType, TraitType, Type,
    },
};

pub(in crate::symbol::table) mod builder;

#[derive(Debug)]
pub struct Synchronization {
    notify: Condvar,
    building: AtomicBool,
    state_flag: Mutex<Option<StateFlag>>,
}

#[derive(Debug)]
pub struct State<S: Finalize> {
    syntax_tree: Arc<S::SyntaxTree>,
    synchronization: Arc<Synchronization>,
    data: Arc<Mutex<S::Data>>,

    /// Maps the state of the symbol to the set of symbols that depends on it.
    ///
    /// This is used to detect cyclic dependency, notifying the symbols that
    /// depends on it when the symbol is finalized, and avoiding deadlock
    /// when building the symbol.
    dependants_by_flag: HashMap<StateFlag, HashSet<GlobalID>>,
}

/// Finalizer is the state of the table used for finalizing the symbols.
///
/// This instance is used to synchronize the building of the symbols and
/// dependecies management. This allows the table to build the symbols in
/// parallel.
#[derive(Debug, Default, derive_more::Deref, derive_more::DerefMut)]
pub struct Finalizer(RwLock<Representation>);

#[derive(Debug, Default)]
pub struct Representation {
    adt_implementation_functions: HashMap<
        ID<AdtImplementationFunction>,
        State<AdtImplementationFunction>,
    >,
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
    positive_trait_implementations: HashMap<
        ID<PositiveTraitImplementation>,
        State<PositiveTraitImplementation>,
    >,
    negative_trait_implementations: HashMap<
        ID<NegativeTraitImplementation>,
        State<NegativeTraitImplementation>,
    >,
    trait_types: HashMap<ID<TraitType>, State<TraitType>>,
    traits: HashMap<ID<Trait>, State<Trait>>,
    types: HashMap<ID<Type>, State<Type>>,

    dependencies_by_dependant: HashMap<GlobalID, GlobalID>,
    reported_cyclic_dependencies: HashSet<BTreeSet<GlobalID>>,
}

impl Representation {
    fn should_report_cyclic_error(
        &self,
        required_from: GlobalID,
        target_state_flag: StateFlag,
        dependency_stack: &[(GlobalID, StateFlag)],
    ) -> bool {
        let required_from_is_structural_dependency = {
            match required_from {
                GlobalID::Enum(_) => {
                    target_state_flag
                        == super::finalize::r#enum::PRE_DEFINITION_STATE
                        || target_state_flag
                            == super::finalize::r#enum::DEFINITION_STATE
                }

                GlobalID::Struct(_) => {
                    target_state_flag
                        == super::finalize::r#struct::PRE_DEFINITION_STATE
                        || target_state_flag
                            == super::finalize::r#struct::DEFINITION_STATE
                }

                _ => false,
            }
        };

        let last_is_structural_dependency = {
            match dependency_stack.last() {
                Some((GlobalID::Enum(_), state_flag)) => {
                    *state_flag == super::finalize::r#enum::PRE_DEFINITION_STATE
                        || *state_flag
                            == super::finalize::r#enum::DEFINITION_STATE
                }

                Some((GlobalID::Struct(_), state_flag)) => {
                    *state_flag
                        == super::finalize::r#struct::PRE_DEFINITION_STATE
                        || *state_flag
                            == super::finalize::r#struct::DEFINITION_STATE
                }

                _ => false,
            }
        };

        let allowed = required_from_is_structural_dependency
            && last_is_structural_dependency;

        !allowed
    }

    pub fn get_dependants_by_flag_of(
        &self,
        id: GlobalID,
    ) -> Option<&HashMap<StateFlag, HashSet<GlobalID>>> {
        match id {
            GlobalID::Struct(id) => {
                self.structs.get(&id).map(|x| &x.dependants_by_flag)
            }
            GlobalID::Enum(id) => {
                self.enums.get(&id).map(|x| &x.dependants_by_flag)
            }
            GlobalID::Constant(id) => {
                self.constants.get(&id).map(|x| &x.dependants_by_flag)
            }
            GlobalID::Function(id) => {
                self.functions.get(&id).map(|x| &x.dependants_by_flag)
            }
            GlobalID::Type(id) => {
                self.types.get(&id).map(|x| &x.dependants_by_flag)
            }
            GlobalID::AdtImplementation(id) => {
                self.adt_implementations.get(&id).map(|x| &x.dependants_by_flag)
            }
            GlobalID::AdtImplementationFunction(id) => self
                .adt_implementation_functions
                .get(&id)
                .map(|x| &x.dependants_by_flag),
            GlobalID::Trait(id) => {
                self.traits.get(&id).map(|x| &x.dependants_by_flag)
            }
            GlobalID::TraitConstant(id) => {
                self.trait_constants.get(&id).map(|x| &x.dependants_by_flag)
            }
            GlobalID::TraitFunction(id) => {
                self.trait_functions.get(&id).map(|x| &x.dependants_by_flag)
            }
            GlobalID::PositiveTraitImplementation(id) => self
                .positive_trait_implementations
                .get(&id)
                .map(|x| &x.dependants_by_flag),
            GlobalID::TraitImplementationConstant(id) => self
                .trait_implementation_constants
                .get(&id)
                .map(|x| &x.dependants_by_flag),
            GlobalID::TraitImplementationFunction(id) => self
                .trait_implementation_functions
                .get(&id)
                .map(|x| &x.dependants_by_flag),
            GlobalID::TraitImplementationType(id) => self
                .trait_implementation_types
                .get(&id)
                .map(|x| &x.dependants_by_flag),
            GlobalID::NegativeTraitImplementation(id) => self
                .negative_trait_implementations
                .get(&id)
                .map(|x| &x.dependants_by_flag),
            GlobalID::TraitType(id) => {
                self.trait_types.get(&id).map(|x| &x.dependants_by_flag)
            }

            GlobalID::Variant(_) | GlobalID::Module(_) => None,
        }
    }

    /// Checks if the `dependent` symbol is depending on the `dependency` symbol
    /// to be built to a certain state.
    #[allow(clippy::too_many_lines)]
    pub fn is_depending_on(
        &self,
        dependency: GlobalID,
        dependent: GlobalID,
    ) -> Option<bool> {
        match dependency {
            GlobalID::Struct(id) => self.structs.get(&id).map(|x| {
                x.dependants_by_flag.values().any(|x| x.contains(&dependent))
            }),
            GlobalID::Enum(id) => self.enums.get(&id).map(|x| {
                x.dependants_by_flag.values().any(|x| x.contains(&dependent))
            }),
            GlobalID::Constant(id) => self.constants.get(&id).map(|x| {
                x.dependants_by_flag.values().any(|x| x.contains(&dependent))
            }),
            GlobalID::Function(id) => self.functions.get(&id).map(|x| {
                x.dependants_by_flag.values().any(|x| x.contains(&dependent))
            }),
            GlobalID::Type(id) => self.types.get(&id).map(|x| {
                x.dependants_by_flag.values().any(|x| x.contains(&dependent))
            }),
            GlobalID::AdtImplementation(id) => {
                self.adt_implementations.get(&id).map(|x| {
                    x.dependants_by_flag
                        .values()
                        .any(|x| x.contains(&dependent))
                })
            }
            GlobalID::AdtImplementationFunction(id) => {
                self.adt_implementation_functions.get(&id).map(|x| {
                    x.dependants_by_flag
                        .values()
                        .any(|x| x.contains(&dependent))
                })
            }
            GlobalID::Trait(id) => self.traits.get(&id).map(|x| {
                x.dependants_by_flag.values().any(|x| x.contains(&dependent))
            }),
            GlobalID::TraitConstant(id) => {
                self.trait_constants.get(&id).map(|x| {
                    x.dependants_by_flag
                        .values()
                        .any(|x| x.contains(&dependent))
                })
            }
            GlobalID::TraitFunction(id) => {
                self.trait_functions.get(&id).map(|x| {
                    x.dependants_by_flag
                        .values()
                        .any(|x| x.contains(&dependent))
                })
            }
            GlobalID::PositiveTraitImplementation(id) => {
                self.positive_trait_implementations.get(&id).map(|x| {
                    x.dependants_by_flag
                        .values()
                        .any(|x| x.contains(&dependent))
                })
            }
            GlobalID::TraitImplementationConstant(id) => {
                self.trait_implementation_constants.get(&id).map(|x| {
                    x.dependants_by_flag
                        .values()
                        .any(|x| x.contains(&dependent))
                })
            }
            GlobalID::TraitImplementationFunction(id) => {
                self.trait_implementation_functions.get(&id).map(|x| {
                    x.dependants_by_flag
                        .values()
                        .any(|x| x.contains(&dependent))
                })
            }
            GlobalID::TraitImplementationType(id) => {
                self.trait_implementation_types.get(&id).map(|x| {
                    x.dependants_by_flag
                        .values()
                        .any(|x| x.contains(&dependent))
                })
            }
            GlobalID::NegativeTraitImplementation(id) => {
                self.negative_trait_implementations.get(&id).map(|x| {
                    x.dependants_by_flag
                        .values()
                        .any(|x| x.contains(&dependent))
                })
            }
            GlobalID::TraitType(id) => self.trait_types.get(&id).map(|x| {
                x.dependants_by_flag.values().any(|x| x.contains(&dependent))
            }),

            GlobalID::Variant(_) | GlobalID::Module(_) => None,
        }
    }
}

/// The trait used for retrieving the states of a particular symbol in
/// [`Representation`]
pub trait Element: Sized + Finalize {
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

implements_element!(AdtImplementationFunction);
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
implements_element!(PositiveTraitImplementation);
implements_element!(NegativeTraitImplementation);
implements_element!(TraitType);
implements_element!(Trait);
implements_element!(Type);

impl Finalizer {
    /// Adds a new symbol to the builder and sets its state to drafting.
    ///
    /// If there exists a symbol with the same ID, then the symbol is not
    /// added and the function returns `false`. Otherwise, returns `true`.
    #[must_use]
    pub fn draft_symbol<T: Finalize + Element>(
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
                        state_flag: Mutex::new(None),
                    }),
                    data: Arc::new(Mutex::new(T::Data::default())),
                    dependants_by_flag: HashMap::new(),
                });
                true
            }
        }
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("threre's no state for the given symbol")]
pub struct EntryNotFoundError;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum BuildSymbolError {
    #[error(transparent)]
    EntryNotFound(#[from] EntryNotFoundError),

    #[error("cyclic dependency detected")]
    CyclicDependency,

    #[error(
        "the given state flag is larger than the maximum state flag of the \
         symbol"
    )]
    InvalidStateFlag { found: StateFlag, max: StateFlag },
}

impl Table<Building<RwLockContainer, Finalizer>> {
    fn build_loop<T: Finalize + Element>(
        &self,
        id: ID<T>,
        to_flag: StateFlag,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), EntryNotFoundError>
    where
        ID<T>: Into<GlobalID>,
    {
        let (synchronization, syntax_tree, data) = {
            let builder_read = self.state.read();
            let states = T::get_states(&builder_read);
            let state = states.get(&id).ok_or(EntryNotFoundError)?;

            let result = (
                state.synchronization.clone(),
                state.syntax_tree.clone(),
                state.data.clone(),
            );

            drop(builder_read);

            result
        };

        loop {
            // if the symbol is already finalized beyond the given flag, then we
            // can just return.
            if synchronization.state_flag.lock().map_or(false, |x| x >= to_flag)
            {
                synchronization.building.store(false, atomic::Ordering::SeqCst);
                synchronization.notify.notify_all();
                return Ok(());
            }

            let next_flag =
                synchronization.state_flag.lock().map_or(0, |x| x + 1);

            T::finalize(
                self,
                id,
                next_flag,
                &syntax_tree,
                &mut data.lock(),
                handler,
            );

            // notify all waiting threads
            let mut builder_write = self.state.write();
            let states = T::get_states_mut(&mut builder_write);

            // update the state of the symbol
            synchronization.state_flag.lock().replace(next_flag);

            let dependants = states
                .get_mut(&id)
                .ok_or(EntryNotFoundError)?
                .dependants_by_flag
                .remove(&next_flag);

            for dependant in dependants.into_iter().flatten() {
                // the dependency of the dependant might have been overridden,
                // so we better not mistakenly remove the dependency.
                if builder_write
                    .dependencies_by_dependant
                    .get(&dependant)
                    .copied()
                    == Some(id.into())
                {
                    builder_write.dependencies_by_dependant.remove(&dependant);
                }
            }

            drop(builder_write);
            synchronization.notify.notify_all();
        }
    }

    fn acquire_build_lock<T: Finalize + Element>(
        &self,
        id: ID<T>,
        to_flag: StateFlag,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), EntryNotFoundError>
    where
        ID<T>: Into<GlobalID>,
    {
        loop {
            let synchronization = T::get_states(&self.state.read())
                .get(&id)
                .ok_or(EntryNotFoundError)?
                .synchronization
                .clone();

            // if the symbol is being built by another thread, then this thread
            // will wait until the other thread finishes building
            // the symbol. Otherwise, this thread will build the
            // symbol.

            if synchronization
                .building
                .compare_exchange(
                    false,
                    true,
                    atomic::Ordering::SeqCst,
                    atomic::Ordering::SeqCst,
                )
                .is_ok()
            {
                return self.build_loop(id, to_flag, handler);
            }

            // some other thread is building the symbol, so we wait for it
            // to finish.
            let mut flag = synchronization.state_flag.lock();

            // wait until either the symbol is finalized beyond or equal to
            // the given flag, or the symbol is no longer
            // being built by another thread.
            if let Some(flag_some) = *flag {
                if flag_some < to_flag {
                    synchronization.notify.wait_while(&mut flag, |flag| {
                        flag.map_or(true, |x| x < to_flag)
                            && synchronization
                                .building
                                .load(atomic::Ordering::SeqCst)
                    });
                }

                if let Some(flag_some) = *flag {
                    if flag_some >= to_flag {
                        return Ok(());
                    }
                }
            }
        }
    }

    /// Builds the given symbol id to the specified `to_flag` state.
    ///
    /// # Parameters
    ///
    /// - `id`: The ID of the symbol to build.
    /// - `required_from`: The ID of the symbol that requires the symbol to be
    ///  built to the specified state. This is used to detect cyclic dependency.
    /// - `to_flag`: The state to build the symbol to.
    /// - `handler`: The handler to report the error to.
    ///
    /// # Errors
    ///
    /// See [`BuildSymbolError`] for the list of errors that can be returned.
    #[allow(clippy::significant_drop_tightening)]
    pub fn build_to<T: Finalize + Element>(
        &self,
        id: ID<T>,
        required_from: Option<GlobalID>,
        to_flag: StateFlag,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), BuildSymbolError>
    where
        ID<T>: Into<GlobalID>,
    {
        let mut builder_write = self.state.write();

        // the given flag is invalid.
        if to_flag > T::FINAL_STATE {
            return Err(BuildSymbolError::InvalidStateFlag {
                found: to_flag,
                max: T::FINAL_STATE,
            });
        }

        // if the symbol has already been built to the required state, then
        // return early.
        if T::get_states(&builder_write)
            .get(&id)
            .ok_or(EntryNotFoundError)?
            .synchronization
            .state_flag
            .lock()
            .map_or(false, |x| x >= to_flag)
        {
            return Ok(());
        }

        // cyclic dependency detection
        if let Some(required_from) = required_from {
            if required_from == id.into() {
                if builder_write
                    .reported_cyclic_dependencies
                    .insert(std::iter::once(id.into()).collect())
                    && builder_write.should_report_cyclic_error(
                        required_from,
                        to_flag,
                        &[(id.into(), to_flag)],
                    )
                {
                    handler.receive(Box::new(CyclicDependency {
                        participants: std::iter::once(id.into()).collect(),
                    }));
                }

                return Err(BuildSymbolError::CyclicDependency);
            }

            // dependency cyclic check starts here
            let mut dependency_stack = vec![(id.into(), to_flag)];

            while let Some(dependency) = builder_write
                .dependencies_by_dependant
                .get(&dependency_stack.last().unwrap().0)
                .copied()
            {
                dependency_stack.push((
                    dependency,
                    builder_write
                        .get_dependants_by_flag_of(dependency)
                        .unwrap()
                        .iter()
                        .find_map(|(flag, depdendants)| {
                            if depdendants
                                .contains(&dependency_stack.last().unwrap().0)
                            {
                                Some(*flag)
                            } else {
                                None
                            }
                        })
                        .unwrap(),
                ));

                // cyclic dependency found
                if dependency == required_from {
                    let dependency_stack_set = dependency_stack
                        .iter()
                        .copied()
                        .map(|(id, _)| id)
                        .chain(std::iter::once(required_from))
                        .collect::<BTreeSet<_>>();

                    let is_reported = builder_write
                        .reported_cyclic_dependencies
                        .contains(&dependency_stack_set);

                    if !is_reported
                        && builder_write.should_report_cyclic_error(
                            required_from,
                            to_flag,
                            &dependency_stack,
                        )
                    {
                        handler.receive(Box::new(CyclicDependency {
                            participants: dependency_stack_set.clone(),
                        }));
                        builder_write
                            .reported_cyclic_dependencies
                            .insert(dependency_stack_set);
                    }

                    return Err(BuildSymbolError::CyclicDependency);
                }
            }

            // no cyclic dependency found, so we can safely add the dependency
            // to the builder
            builder_write
                .dependencies_by_dependant
                .insert(required_from, id.into());
            T::get_states_mut(&mut builder_write)
                .get_mut(&id)
                .unwrap()
                .dependants_by_flag
                .entry(to_flag)
                .or_default()
                .insert(required_from);
        }

        // drop the builder write
        drop(builder_write);

        Ok(self.acquire_build_lock(id, to_flag, handler)?)
    }
}

impl Table<Building<RwLockContainer, Finalizer>> {
    /// Builds all symbols in the builder to the last state.
    pub fn build_all(&self, handler: &dyn Handler<Box<dyn error::Error>>) {
        macro_rules! make_ids {
            ($field_name:ident) => {
                paste! {
                     self.state
                        .read()
                        .[<$field_name:snake s>]
                        .keys()
                        .copied()
                        .collect::<Vec<_>>()
                }
            };
        }

        let enum_ids = make_ids!(Enum);
        let struct_ids = make_ids!(Struct);
        let constant_ids = make_ids!(Constant);
        let type_ids = make_ids!(Type);
        let function_ids = make_ids!(Function);
        let trait_ids = make_ids!(Trait);
        let trait_constant_ids = make_ids!(TraitConstant);
        let trait_type_ids = make_ids!(TraitType);
        let trait_function_ids = make_ids!(TraitFunction);
        let negative_trait_implementation_ids =
            make_ids!(NegativeTraitImplementation);
        let positive_trait_implementation_ids =
            make_ids!(PositiveTraitImplementation);
        let trait_implementation_function_ids =
            make_ids!(TraitImplementationFunction);
        let trait_implementation_constant_ids =
            make_ids!(TraitImplementationConstant);
        let trait_implementation_type_ids = make_ids!(TraitImplementationType);
        let adt_implementation_ids = make_ids!(AdtImplementation);
        let adt_implementation_function_ids =
            make_ids!(AdtImplementationFunction);

        macro_rules! build_id {
            ($field_name:ident) => {
                paste! {
                    [<$field_name:snake _ids>].into_par_iter()
                        .map(|x| {
                            let _ = self.build_to(
                                x,
                                None,
                                <$field_name as Finalize>::FINAL_STATE,
                                handler
                            );
                        })
                }
            };
        }

        macro_rules! build_all {
            ($first_name:ident, $($names:ident),*) => {
                build_id!($first_name)
                    $( .chain(build_id!($names)) )*
                    .for_each(|()| {})
            };
        }

        build_all!(
            Enum,
            Struct,
            Constant,
            Type,
            Function,
            Trait,
            TraitConstant,
            TraitType,
            TraitFunction,
            NegativeTraitImplementation,
            PositiveTraitImplementation,
            TraitImplementationFunction,
            TraitImplementationConstant,
            TraitImplementationType,
            AdtImplementation,
            AdtImplementationFunction
        );
    }
}
