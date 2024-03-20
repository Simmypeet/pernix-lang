use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    sync::{
        atomic::{self, AtomicBool},
        Arc,
    },
};

use parking_lot::{Condvar, Mutex, RwLock};
use paste::paste;
use pernixc_base::{diagnostic::Handler, source_file::Span};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use thiserror::Error;

use super::{build::Build, resolution, Table};
use crate::{
    arena::{Arena, Key, ID},
    error::{self, CyclicDependency},
    semantic::{
        model::Model,
        term::{self, lifetime::Lifetime},
    },
    symbol::{
        Constant, Enum, Function, GlobalID, Implementation,
        ImplementationConstant, ImplementationFunction, ImplementationType,
        NegativeImplementation, Struct, Trait, TraitConstant, TraitFunction,
        TraitType, Type, Variant,
    },
};

pub(super) mod constant;
pub(super) mod r#enum;
pub(super) mod function;
pub(super) mod implementation;
pub(super) mod implementation_constant;
pub(super) mod implementation_function;
pub(super) mod implementation_type;
pub(super) mod negative_implementation;
pub(super) mod r#struct;
pub(super) mod r#trait;
pub(super) mod trait_constant;
pub(super) mod trait_function;
pub(super) mod trait_type;
pub(super) mod r#type;
pub(super) mod variant;

/// The trait for flag, indicating the state of a symbol.
pub(super) trait Flag:
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
    /// The state of a symbol when it is first created.
    fn drafting_state() -> Self;

    /// The state of a symbol when it is fully defined.
    fn final_state() -> Self;

    /// Increments the state of a symnol.
    fn increment(&mut self);
}

/// The trait for a symbol in the symbol table.
pub(super) trait Symbol: Sized + 'static
where
    ID<Self>: Into<GlobalID>,
{
    /// The type of the flag.
    type Flag: Flag;

    /// The type of the syntax tree which the symbol based on.
    type SyntaxTree: Debug;

    /// The type of the data associated while building the symbol.
    type Data: Debug + Send + Sync + Default;

    fn get_arena(table: &Table) -> &Arena<RwLock<Self>, ID<Self>>;
    fn get_arena_mut(table: &mut Table) -> &mut Arena<RwLock<Self>, ID<Self>>;

    fn get_states(builder: &Builder) -> &HashMap<ID<Self>, State<Self>>;
    fn get_states_mut(
        builder: &mut Builder,
    ) -> &mut HashMap<ID<Self>, State<Self>>;
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
                unsafe { std::mem::transmute($crate::table::state::count!($($variant),*) - 1) }
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

pub(super) use build_flag;
pub(super) use count;

/// The object used to synchronize the building of various interdependent
/// symbols.
#[derive(Debug)]
struct Synchronization<F: Flag> {
    notify: Condvar,
    building: AtomicBool,
    finalize_flag: Mutex<F>,
}

/// Contains the state of a symbol.
#[derive(Debug)]
pub(super) struct State<S: Symbol>
where
    ID<S>: Into<GlobalID>,
{
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

/// The resolution configuration which will build the symbol to the appropriate
/// state when the symbol is resolved.
#[derive(Debug, Clone)]
pub(super) struct DependencyAwareResolutionConfig;

#[derive(Debug, Default)]
pub struct Builder {
    states_by_enum_id: HashMap<ID<Enum>, State<Enum>>,
    states_by_variant_id: HashMap<ID<Variant>, State<Variant>>,

    states_by_struct_id: HashMap<ID<Struct>, State<Struct>>,
    states_by_constant_id: HashMap<ID<Constant>, State<Constant>>,
    states_by_type_id: HashMap<ID<Type>, State<Type>>,
    states_by_function_id: HashMap<ID<Function>, State<Function>>,

    states_by_trait_id: HashMap<ID<Trait>, State<Trait>>,
    states_by_trait_constant_id:
        HashMap<ID<TraitConstant>, State<TraitConstant>>,
    states_by_trait_type_id: HashMap<ID<TraitType>, State<TraitType>>,
    states_by_trait_function_id:
        HashMap<ID<TraitFunction>, State<TraitFunction>>,

    states_by_negative_implementation_id:
        HashMap<ID<NegativeImplementation>, State<NegativeImplementation>>,
    states_by_implementation_id:
        HashMap<ID<Implementation>, State<Implementation>>,
    states_by_implementation_function_id:
        HashMap<ID<ImplementationFunction>, State<ImplementationFunction>>,
    states_by_implementation_constant_id:
        HashMap<ID<ImplementationConstant>, State<ImplementationConstant>>,
    states_by_implementation_type_id:
        HashMap<ID<ImplementationType>, State<ImplementationType>>,

    dependencies: HashMap<GlobalID, GlobalID>,
    reported_cyclic_dependencies: HashSet<BTreeSet<GlobalID>>,
}

#[derive(Clone, Copy)]
pub(super) struct Config<'a, T: Symbol>
where
    ID<T>: Into<GlobalID>,
{
    builder: &'a RwLock<Builder>,
    current_id: ID<T>,
    table: &'a Table,
    handler: &'a dyn Handler<error::Error>,
    syntax_tree: &'a T::SyntaxTree,
}

impl<'a, T: Symbol> Config<'a, T>
where
    ID<T>: Into<GlobalID>,
{
    pub(super) fn table(&self) -> &'a Table { self.table }

    pub(super) fn current_id(&self) -> ID<T> { self.current_id }

    pub(super) fn syntax_tree(&self) -> &'a T::SyntaxTree { self.syntax_tree }

    pub(super) fn handler(&self) -> &'a dyn Handler<error::Error> {
        self.handler
    }

    pub(super) fn build_to<U: Build>(
        &self,
        id: ID<U>,
        to_flag: U::Flag,
    ) -> Result<(), BuildError>
    where
        ID<U>: Into<GlobalID>,
    {
        Builder::build_to::<U>(
            self.builder,
            self.table,
            id,
            Some(self.current_id.into()),
            to_flag,
            self.handler,
        )
    }
}

impl<'a, T: Symbol, S: Model> resolution::Config<S> for Config<'a, T>
where
    ID<T>: Into<GlobalID>,
{
    fn lifetime_arguments_placeholder(&mut self) -> Option<Lifetime<S>> { None }

    fn type_arguments_placeholder(&mut self) -> Option<term::r#type::Type<S>> {
        None
    }

    fn constant_arguments_placeholder(
        &mut self,
    ) -> Option<term::constant::Constant<S>> {
        None
    }

    fn on_global_id_resolved(&mut self, global_id: GlobalID, _: &Span) {
        let _ = match global_id {
            GlobalID::TraitConstant(_)
            | GlobalID::Variant(_)
            | GlobalID::Constant(_)
            | GlobalID::Module(_) => Ok(()),

            GlobalID::Struct(id) => Builder::build_to::<Struct>(
                self.builder,
                self.table,
                id,
                Some(self.current_id.into()),
                r#struct::Flag::GenericParameter,
                self.handler,
            ),
            GlobalID::Trait(id) => Builder::build_to::<Trait>(
                self.builder,
                self.table,
                id,
                Some(self.current_id.into()),
                r#trait::Flag::GenericParameter,
                self.handler,
            ),
            GlobalID::Enum(id) => Builder::build_to::<Enum>(
                self.builder,
                self.table,
                id,
                Some(self.current_id.into()),
                r#enum::Flag::GenericParameter,
                self.handler,
            ),
            GlobalID::Type(id) => Builder::build_to::<Type>(
                self.builder,
                self.table,
                id,
                Some(self.current_id.into()),
                r#type::Flag::Body, // build to the body
                self.handler,
            ),
            GlobalID::Function(id) => Builder::build_to::<Function>(
                self.builder,
                self.table,
                id,
                Some(self.current_id.into()),
                function::Flag::GenericParameter,
                self.handler,
            ),
            GlobalID::TraitType(id) => Builder::build_to::<TraitType>(
                self.builder,
                self.table,
                id,
                Some(self.current_id.into()),
                trait_type::Flag::GenericParameter,
                self.handler,
            ),
            GlobalID::TraitFunction(id) => Builder::build_to::<TraitFunction>(
                self.builder,
                self.table,
                id,
                Some(self.current_id.into()),
                trait_function::Flag::GenericParameter,
                self.handler,
            ),
            GlobalID::Implementation(_)
            | GlobalID::NegativeImplementation(_) => {
                unreachable!(
                    "implementation can't be referred by qualified name"
                )
            }
            GlobalID::ImplementationFunction(id) => {
                Builder::build_to::<ImplementationFunction>(
                    self.builder,
                    self.table,
                    id,
                    Some(self.current_id.into()),
                    implementation_function::Flag::GenericParameter,
                    self.handler,
                )
            }
            GlobalID::ImplementationType(id) => {
                Builder::build_to::<ImplementationType>(
                    self.builder,
                    self.table,
                    id,
                    Some(self.current_id.into()),
                    implementation_type::Flag::Body,
                    self.handler,
                )
            }
            GlobalID::ImplementationConstant(_) => {
                todo!("will be decided later")
            }
        };
    }

    fn on_generic_arguments_resolved(
        &mut self,
        _: GlobalID,
        _: Option<&term::GenericArguments<S>>,
        _: &Span,
    ) {
    }

    fn on_resolved(&mut self, _: &resolution::Resolution<S>, _: &Span) {}

    fn on_constant_arguments_resolved(
        &mut self,
        _: &term::constant::Constant<S>,
        _: &term::r#type::Type<S>,
        _: &Span,
    ) {
    }

    fn extra_lifetime_provider(&self, _: &str) -> Option<Lifetime<S>> { None }

    fn on_type_resolved(&mut self, _: &term::r#type::Type<S>, _: &Span) {}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("threre's no state for the given symbol")]
pub(super) struct EntryNotFoundError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("there's already a state for the given symbol")]
pub(super) struct ExistingStateError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
pub(super) enum BuildError {
    #[error("the symbol is not found in the builder entry")]
    EntryNotFound(#[from] EntryNotFoundError),

    #[error("cyclic depedency detected")]
    CyclicDependency,
}

impl Builder {
    /// Drafts a symbol into the symbol table.
    pub(super) fn draft_symbol<T: Symbol>(
        &mut self,
        table: &mut Table,
        symbol_fn: impl FnOnce(ID<T>, &T::SyntaxTree) -> T,
        syntax_tree: T::SyntaxTree,
    ) -> ID<T>
    where
        ID<T>: Into<GlobalID>,
    {
        let arena = T::get_arena_mut(table);
        let id = ID::from_index(arena.len());
        arena.insert(RwLock::new(symbol_fn(id, &syntax_tree)));
        let states = T::get_states_mut(self);

        let state = State {
            syntax_tree: Arc::new(syntax_tree),
            synchronization: Arc::new(Synchronization {
                notify: Condvar::new(),
                building: AtomicBool::new(false),
                finalize_flag: Mutex::new(<T::Flag as Flag>::drafting_state()),
            }),
            data: Arc::new(Mutex::new(T::Data::default())),
            dependencies: HashMap::new(),
        };

        assert!(
            states.insert(id, state).is_none(),
            "should have no existing id"
        );

        id
    }

    fn build_to_loop<T: Build>(
        builder: &RwLock<Self>,
        table: &Table,
        id: ID<T>,
        to_flag: T::Flag,
        handler: &dyn Handler<error::Error>,
    ) -> Result<(), EntryNotFoundError>
    where
        ID<T>: Into<GlobalID>,
    {
        let (synchronization, syntax_tree, data) = {
            let builder_read = builder.read();
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
            if *synchronization.finalize_flag.lock() >= to_flag {
                synchronization.building.store(false, atomic::Ordering::SeqCst);
                synchronization.notify.notify_all();
                return Ok(());
            }

            let mut next_flag = *synchronization.finalize_flag.lock();
            next_flag.increment();

            // build the symbol
            T::build(
                Config {
                    builder,
                    current_id: id,
                    table,
                    handler,
                    syntax_tree: &syntax_tree,
                },
                &mut data.lock(),
                next_flag,
            );

            // update the state of the symbol
            synchronization.finalize_flag.lock().increment();

            // notify all waiting threads
            let mut builder_write = builder.write();
            let states = T::get_states_mut(&mut builder_write);

            let dependencies = states
                .get_mut(&id)
                .ok_or(EntryNotFoundError)?
                .dependencies
                .remove(&synchronization.finalize_flag.lock());

            for dependency in dependencies.into_iter().flatten() {
                // no assertion, because the dependency may be removed by cyclic
                // dependency
                builder_write.dependencies.remove(&dependency);
            }

            drop(builder_write);

            synchronization.notify.notify_all();
        }
    }

    fn attempt_build_to<T: Build>(
        builder: &RwLock<Self>,
        table: &Table,
        id: ID<T>,
        to_flag: T::Flag,
        handler: &dyn Handler<error::Error>,
    ) -> Result<(), EntryNotFoundError>
    where
        ID<T>: Into<GlobalID>,
    {
        let synchronization = T::get_states(&builder.read())
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
            Self::build_to_loop(builder, table, id, to_flag, handler)
        } else {
            // some other thread is building the symbol, so we wait for it to
            // finish.
            let mut flag = synchronization.finalize_flag.lock();

            // wait until either the symbol is finalized beyond or equal to the
            // given flag, or the symbol is no longer being built by
            // another thread.
            if *flag < to_flag {
                synchronization.notify.wait_while(&mut flag, |flag| {
                    let done = *flag >= to_flag
                        || !synchronization
                            .building
                            .load(atomic::Ordering::SeqCst);

                    !done
                });

                if *flag >= to_flag {
                    return Ok(());
                }
            }

            // IMPORTANT: we must release the lock before calling
            // `attempt_build_to`, otherwise we will deadlock.
            drop(flag);

            Self::attempt_build_to(builder, table, id, to_flag, handler)
        }
    }

    fn build_to<T: Build>(
        builder: &RwLock<Self>,
        table: &Table,
        id: ID<T>,
        required_from: Option<GlobalID>,
        to_flag: T::Flag,
        handler: &dyn Handler<error::Error>,
    ) -> Result<(), BuildError>
    where
        ID<T>: Into<GlobalID>,
    {
        // if the state is already finalized beyond the given flag, then we can
        // just return.
        let mut builder_write = builder.write();
        if *T::get_states(&builder_write)
            .get(&id)
            .ok_or(EntryNotFoundError)?
            .synchronization
            .finalize_flag
            .lock()
            >= to_flag
        {
            return Ok(());
        }

        // cyclic dependency detection
        if let Some(required_site) = required_from {
            if required_site == id.into() {
                handler.receive(error::Error::CyclicDependency(
                    CyclicDependency { participants: vec![id.into()] },
                ));

                return Err(BuildError::CyclicDependency);
            }

            // dependency cyclic check starts here
            let mut dependency_stack = vec![id.into()];
            let mut current_node = id.into();

            while let Some(dependant) =
                builder_write.dependencies.get(&current_node)
            {
                let states = T::get_states(&builder_write);
                dependency_stack.push(dependant);

                // cyclic dependency found
                if dependant == required_site {
                    let dependency_stack_set =
                        dependency_stack.iter().copied().collect();

                    let reported = !builder_write
                        .reported_cyclic_dependencies
                        .contains(&dependency_stack_set);

                    if reported {
                        builder_write
                            .reported_cyclic_dependencies
                            .insert(dependency_stack_set);
                        handler.receive(error::Error::CyclicDependency(
                            CyclicDependency {
                                participants: dependency_stack.clone(),
                            },
                        ));
                    }

                    return Err(BuildError::CyclicDependency);
                }

                current_node = dependant;
            }

            // no cyclic dependency, add it to the dependency graph

            assert!(
                builder_write
                    .dependencies
                    .insert(required_site, id.into())
                    .is_none(),
                "should have no duplication"
            );

            let states = T::get_states_mut(&mut builder_write);

            states
                .get_mut(&id)
                .unwrap()
                .dependencies
                .entry(to_flag)
                .or_default()
                .insert(required_site);
        }

        drop(builder_write);

        let result =
            Self::attempt_build_to(builder, table, id, to_flag, handler);

        Ok(result?)
    }
}

impl Table {
    pub(super) fn build_all(
        &self,
        builder: &RwLock<Builder>,
        handler: &dyn Handler<error::Error>,
    ) {
        macro_rules! make_ids {
            ($field_name:ident) => {
                paste! {
                     builder
                        .read()
                        .[<states_by_ $field_name:snake _id>]
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
        let negative_implementation_ids = make_ids!(NegativeImplementation);
        let implementation_ids = make_ids!(Implementation);
        let implementation_function_ids = make_ids!(ImplementationFunction);
        let implementation_constant_ids = make_ids!(ImplementationConstant);
        let implementation_type_ids = make_ids!(ImplementationType);

        macro_rules! build_id {
            ($field_name:ident) => {
                paste! {
                    [<$field_name:snake _ids>].into_par_iter()
                        .map(|x| {
                            let _ = Builder::build_to(
                                builder,
                                self,
                                x,
                                None,
                                <<$field_name as Symbol>::Flag as Flag>::final_state(),
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
            NegativeImplementation,
            Implementation,
            ImplementationFunction,
            ImplementationConstant,
            ImplementationType
        );
    }
}
