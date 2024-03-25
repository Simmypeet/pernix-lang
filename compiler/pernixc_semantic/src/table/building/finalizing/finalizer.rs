use std::{
    collections::{hash_map::Entry, BTreeSet, HashMap, HashSet},
    sync::{
        atomic::{self, AtomicBool},
        Arc,
    },
};

use parking_lot::{Condvar, Mutex, RwLock};
use paste::paste;
use pernixc_base::diagnostic::Handler;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use super::finalize::{self, Finalize, Flag};
use crate::{
    arena::ID,
    error::{self, CyclicDependency},
    symbol::{
        AdtImplementation, AdtImplementationConstant,
        AdtImplementationFunction, AdtImplementationType, Constant, Enum,
        Function, GenericID, GlobalID, NegativeTraitImplementation, Struct,
        Trait, TraitConstant, TraitFunction, TraitImplementation,
        TraitImplementationConstant, TraitImplementationFunction,
        TraitImplementationType, TraitType, Type, Variant,
    },
    table::{self, RwLockContainer, Table},
};

#[derive(Debug)]
pub struct Synchronization<F> {
    notify: Condvar,
    building: AtomicBool,
    state_flag: Mutex<Option<F>>,
}

#[derive(Debug)]
pub struct State<S: Finalize> {
    syntax_tree: Arc<S::SyntaxTree>,
    synchronization: Arc<Synchronization<S::Flag>>,
    data: Arc<Mutex<S::Data>>,

    /// Maps the state of the symbol to the set of symbols that depends on it.
    ///
    /// This is used to detect cyclic dependency, notifying the symbols that
    /// depends on it when the symbol is finalized, and avoiding deadlock
    /// when building the symbol.
    dependants_by_flag: HashMap<S::Flag, HashSet<GlobalID>>,
}
#[derive(Debug, Default, derive_more::Deref, derive_more::DerefMut)]
pub struct Finalizer(RwLock<Representation>);

impl table::State for Finalizer {
    type Container = RwLockContainer;

    #[allow(clippy::too_many_lines)]
    fn on_global_id_resolved(
        table: &table::Table<Self>,
        global_id: GlobalID,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let Ok(generic_id) = GenericID::try_from(global_id) else {
            return;
        };

        let _ = table.build_generic_parameter(
            generic_id,
            referring_site,
            true,
            handler,
        );
    }

    fn on_resolved(
        _: &table::Table<Self>,
        _: table::resolution::Resolution,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) {
    }
}

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

    dependencies_by_dependant: HashMap<GlobalID, GlobalID>,
    reported_cyclic_dependencies: HashSet<BTreeSet<GlobalID>>,
}

impl Representation {
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
            GlobalID::Variant(id) => self.variants.get(&id).map(|x| {
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
            GlobalID::AdtImplementationConstant(id) => {
                self.adt_implementation_constants.get(&id).map(|x| {
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
            GlobalID::AdtImplementationType(id) => {
                self.adt_implementation_types.get(&id).map(|x| {
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
            GlobalID::TraitImplementation(id) => {
                self.trait_implementations.get(&id).map(|x| {
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
            GlobalID::Module(_) => None,
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
}

impl Table<Finalizer> {
    fn build_loop<T: Finalize + Element>(
        &self,
        id: ID<T>,
        to_flag: T::Flag,
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

            let next_flag = synchronization.state_flag.lock().map_or(
                <T::Flag as Flag>::first(),
                |mut x| {
                    x.increment();
                    x
                },
            );

            T::finalize(
                self,
                id,
                next_flag,
                &syntax_tree,
                &mut data.lock(),
                handler,
            );

            // update the state of the symbol
            synchronization.state_flag.lock().replace(next_flag);

            // notify all waiting threads
            let mut builder_write = self.state.write();
            let states = T::get_states_mut(&mut builder_write);

            let dependencies = states
                .get_mut(&id)
                .ok_or(EntryNotFoundError)?
                .dependants_by_flag
                .remove(&next_flag);

            for dependant in dependencies.into_iter().flatten() {
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
        to_flag: T::Flag,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), EntryNotFoundError>
    where
        ID<T>: Into<GlobalID>,
    {
        let synchronization = T::get_states(&self.state.read())
            .get(&id)
            .ok_or(EntryNotFoundError)?
            .synchronization
            .clone();

        // if the symbol is being built by another thread, then this thread will
        // wait until the other thread finishes building the symbol.
        // Otherwise, this thread will build the symbol.

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
            self.build_loop(id, to_flag, handler)
        } else {
            // some other thread is building the symbol, so we wait for it to
            // finish.
            let mut flag = synchronization.state_flag.lock();

            // wait until either the symbol is finalized beyond or equal to the
            // given flag, or the symbol is no longer being built by
            // another thread.
            if let Some(flag_some) = *flag {
                if flag_some < to_flag {
                    synchronization.notify.wait_while(&mut flag, |flag| {
                        flag.map_or(true, |x| x < to_flag)
                            && synchronization
                                .building
                                .load(atomic::Ordering::SeqCst)
                    });
                }

                if flag_some >= to_flag {
                    return Ok(());
                }
            }

            // IMPORTANT: we must release the lock before calling
            // `attempt_build_to`, otherwise we will deadlock.
            drop(flag);

            // try to acquire the build lock again
            self.acquire_build_lock(id, to_flag, handler)
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
        to_flag: T::Flag,
        report_cyclic_dependency_to_handler: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), BuildSymbolError>
    where
        ID<T>: Into<GlobalID>,
    {
        let mut builder_write = self.state.write();

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
        if let Some(referring_site) = required_from {
            if referring_site == id.into() {
                if report_cyclic_dependency_to_handler
                    && builder_write
                        .reported_cyclic_dependencies
                        .insert(std::iter::once(id.into()).collect())
                {
                    handler.receive(Box::new(CyclicDependency {
                        participants: vec![id.into()],
                    }));
                }

                return Err(BuildSymbolError::CyclicDependency);
            }

            // dependency cyclic check starts here
            let mut dependency_stack = vec![id.into()];
            let mut current_node = id.into();

            while let Some(dependency) = builder_write
                .dependencies_by_dependant
                .get(&current_node)
                .copied()
            {
                // cyclic dependency found
                if dependency == referring_site {
                    let dependency_stack_set = dependency_stack
                        .iter()
                        .copied()
                        .collect::<BTreeSet<_>>();

                    let is_reported = builder_write
                        .reported_cyclic_dependencies
                        .contains(&dependency_stack_set);

                    if !is_reported && report_cyclic_dependency_to_handler {
                        handler.receive(Box::new(CyclicDependency {
                            participants: dependency_stack.clone(),
                        }));
                        builder_write
                            .reported_cyclic_dependencies
                            .insert(dependency_stack_set);
                    }

                    return Err(BuildSymbolError::CyclicDependency);
                }

                dependency_stack.push(dependency);
                current_node = dependency;
            }

            // no cyclic dependency found, so we can safely add the dependency
            // to the builder
            builder_write
                .dependencies_by_dependant
                .insert(referring_site, id.into());
            T::get_states_mut(&mut builder_write)
                .get_mut(&id)
                .unwrap()
                .dependants_by_flag
                .entry(to_flag)
                .or_default()
                .insert(referring_site);
        }

        // drop the builder write
        drop(builder_write);

        Ok(self.acquire_build_lock(id, to_flag, handler)?)
    }
}

macro_rules! implements_build_to {
    ($symbol_id:ident, $table:ident, $dependant:ident, $flag:ident, $handler:ident, $cyclic_dependency_as_error:ident) => {
        match $symbol_id {
            GenericID::Struct(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::r#struct::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::Trait(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::r#trait::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::Enum(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::r#enum::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::Type(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::r#type::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::Constant(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::constant::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::Function(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::function::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::TraitType(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::trait_type::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::TraitFunction(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::trait_function::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::TraitConstant(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::trait_constant::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::TraitImplementation(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::trait_implementation::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::NegativeTraitImplementation(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::negative_trait_implementation::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::TraitImplementationFunction(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::trait_implementation_function::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::TraitImplementationType(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::trait_implementation_type::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::TraitImplementationConstant(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::trait_implementation_constant::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::AdtImplementation(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::adt_implementation::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::AdtImplementationFunction(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::adt_implementation_function::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::AdtImplementationType(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::adt_implementation_type::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
            GenericID::AdtImplementationConstant(id) => $table.build_to(
                id,
                Some($dependant),
                finalize::adt_implementation_constant::Flag::$flag,
                $cyclic_dependency_as_error,
                $handler,
            ),
        }
    };
}

impl Table<Finalizer> {
    /// Builds the given `dependency` symbol so that it has generic parameters
    /// information available.
    ///
    /// # Parameters
    ///
    /// - `dependency`: The ID of the symbol to build.
    /// - `dependant`: The ID of the symbol that requires the `dependency` to be
    ///   built to the specified state. This is used to detect cyclic
    ///   dependency.
    /// - `handler`: The handler to report the error to.
    ///
    /// # Errors
    ///
    /// See [`BuildSymbolError`] for the list of errors that can be returned.
    pub fn build_generic_parameter(
        &self,
        dependency: GenericID,
        dependant: GlobalID,
        cyclic_dependency_as_error: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), BuildSymbolError> {
        implements_build_to!(
            dependency,
            self,
            dependant,
            GenericParameter,
            handler,
            cyclic_dependency_as_error
        )
    }

    /// Builds the given `dependency` symbol to completion. (without checking
    /// bounds)
    ///
    /// # Parameters
    ///
    /// - `dependency`: The ID of the symbol to build.
    /// - `dependant`: The ID of the symbol that requires the `dependency` to be
    ///   built to the specified state. This is used to detect cyclic
    ///   dependency.
    /// - `handler`: The handler to report the error to.
    ///
    /// # Errors
    ///
    /// - [`EntryNotFoundError`]: If the `dependency` or `dependant` symbol is
    ///   not found in the builder.
    #[allow(clippy::too_many_lines)]
    pub fn build_to_completion(
        &self,
        dependency: GlobalID,
        dependant: GlobalID,
        cyclic_dependency_as_error: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), BuildSymbolError> {
        match dependency {
            GlobalID::Struct(id) => self.build_to(
                id,
                Some(dependant),
                finalize::r#struct::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::Trait(id) => self.build_to(
                id,
                Some(dependant),
                finalize::r#trait::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::Enum(id) => self.build_to(
                id,
                Some(dependant),
                finalize::r#enum::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::Type(id) => self.build_to(
                id,
                Some(dependant),
                finalize::r#type::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::Constant(id) => self.build_to(
                id,
                Some(dependant),
                finalize::constant::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::Function(id) => self.build_to(
                id,
                Some(dependant),
                finalize::function::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::TraitType(id) => self.build_to(
                id,
                Some(dependant),
                finalize::trait_type::Flag::WhereClause,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::TraitFunction(id) => self.build_to(
                id,
                Some(dependant),
                finalize::trait_function::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::TraitConstant(id) => self.build_to(
                id,
                Some(dependant),
                finalize::trait_constant::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::TraitImplementation(id) => self.build_to(
                id,
                Some(dependant),
                finalize::trait_implementation::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::NegativeTraitImplementation(id) => self.build_to(
                id,
                Some(dependant),
                finalize::negative_trait_implementation::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::TraitImplementationFunction(id) => self.build_to(
                id,
                Some(dependant),
                finalize::trait_implementation_function::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::TraitImplementationType(id) => self.build_to(
                id,
                Some(dependant),
                finalize::trait_implementation_type::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::TraitImplementationConstant(id) => self.build_to(
                id,
                Some(dependant),
                finalize::trait_implementation_constant::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::AdtImplementation(id) => self.build_to(
                id,
                Some(dependant),
                finalize::adt_implementation::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::AdtImplementationFunction(id) => self.build_to(
                id,
                Some(dependant),
                finalize::adt_implementation_function::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::AdtImplementationType(id) => self.build_to(
                id,
                Some(dependant),
                finalize::adt_implementation_type::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::AdtImplementationConstant(id) => self.build_to(
                id,
                Some(dependant),
                finalize::adt_implementation_constant::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
            GlobalID::Module(_) => Ok(()),
            GlobalID::Variant(id) => self.build_to(
                id,
                Some(dependant),
                finalize::variant::Flag::Complete,
                cyclic_dependency_as_error,
                handler,
            ),
        }
    }

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
        let variant_ids = make_ids!(Variant);
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
        let trait_implementation_ids = make_ids!(TraitImplementation);
        let trait_implementation_function_ids =
            make_ids!(TraitImplementationFunction);
        let trait_implementation_constant_ids =
            make_ids!(TraitImplementationConstant);
        let trait_implementation_type_ids = make_ids!(TraitImplementationType);
        let adt_implementation_ids = make_ids!(AdtImplementation);
        let adt_implementation_constant_ids =
            make_ids!(AdtImplementationConstant);
        let adt_implementation_function_ids =
            make_ids!(AdtImplementationFunction);
        let adt_implementation_type_ids = make_ids!(AdtImplementationType);

        macro_rules! build_id {
            ($field_name:ident) => {
                paste! {
                    [<$field_name:snake _ids>].into_par_iter()
                        .map(|x| {
                            let _ = self.build_to(
                                x,
                                None,
                                <<$field_name as Finalize>::Flag as Flag>::last(),
                                true,
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
            Variant,
            Struct,
            Constant,
            Type,
            Function,
            Trait,
            TraitConstant,
            TraitType,
            TraitFunction,
            NegativeTraitImplementation,
            TraitImplementation,
            TraitImplementationFunction,
            TraitImplementationConstant,
            TraitImplementationType,
            AdtImplementation,
            AdtImplementationConstant,
            AdtImplementationFunction,
            AdtImplementationType
        );
    }
}
