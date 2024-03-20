use std::{
    collections::{hash_map::Entry, BTreeSet, HashMap, HashSet},
    sync::{
        atomic::{self, AtomicBool},
        Arc,
    },
};

use getset::CopyGetters;
use parking_lot::{
    Condvar, MappedRwLockReadGuard, Mutex, RwLock, RwLockReadGuard,
};
use paste::paste;
use pernixc_base::{diagnostic::Handler, source_file::Span};
use pernixc_syntax::syntax_tree::{self};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use thiserror::Error;

use super::{
    resolution::{self, Checking},
    Index, Table,
};
use crate::{
    arena::ID,
    error::{self, CyclicDependency},
    symbol::{
        self, Constant, Enum, Function, GlobalID, Implementation,
        ImplementationConstant, ImplementationFunction, ImplementationType,
        NegativeImplementation, Struct, Trait, TraitConstant, TraitFunction,
        TraitType, Type, Variant,
    },
};

/// Implemented by all kinds of Global symbols.
pub(in crate::table) trait StatefulSymbol:
    Sized
    + finalizing::generic_parameter::Finalize<
        <Self as StatefulSymbol>::SyntaxTree,
    > + finalizing::where_clause::Finalize<<Self as StatefulSymbol>::SyntaxTree>
where
    ID<Self>: Into<GlobalID>,
{
    type SyntaxTree;

    fn get_state(
        state_manager: &super::Manager,
        id: ID<Self>,
    ) -> Option<&State<Self::SyntaxTree>>;

    fn get_state_mut(
        state_manager: &mut Manager,
        id: ID<Self>,
    ) -> Option<&mut State<Self::SyntaxTree>>;

    fn draft_symbol(
        state_manager: &mut Manager,
        id: ID<Self>,
        syntax_tree: Self::SyntaxTree,
    ) -> Result<(), ExistingStateError>;
}

macro_rules! impl_state {
    ($kind:ident, $syntax_tree:path) => {
        paste!{
            impl StatefulSymbol for $kind {
                type SyntaxTree = $syntax_tree;

                fn get_state(state_manager: &Manager, id: ID<Self>) -> Option<&State<Self::SyntaxTree>> {
                    state_manager .[<states_by_ $kind:snake _id>] .get(&id) } fn get_state_mut( state_manager: &mut Manager,
                    id: ID<Self>,
                ) -> Option<&mut State<Self::SyntaxTree>> {
                    state_manager
                        .[<states_by_ $kind:snake _id>]
                        .get_mut(&id)
                }

                fn draft_symbol(
                    state_manager: &mut Manager,
                    id: ID<Self>,
                    syntax_tree: Self::SyntaxTree,
                ) -> Result<(), ExistingStateError> {
                    match state_manager
                        .[<states_by_ $kind:snake _id>]
                        .entry(id)
                    {
                        Entry::Occupied(_) => Err(ExistingStateError),
                        Entry::Vacant(entry) => {
                            entry.insert(State::drafted(syntax_tree));
                            Ok(())
                        }
                    }
                }
            }
        }
    };
}

impl_state!(Enum, syntax_tree::item::EnumSignature);
impl_state!(Variant, syntax_tree::item::Variant);
impl_state!(Struct, syntax_tree::item::Struct);
impl_state!(Constant, syntax_tree::item::Constant);
impl_state!(Type, syntax_tree::item::Type);
impl_state!(Function, syntax_tree::item::Function);
impl_state!(Trait, syntax_tree::item::TraitSignature);
impl_state!(TraitFunction, syntax_tree::item::TraitFunction);
impl_state!(TraitType, syntax_tree::item::TraitType);
impl_state!(TraitConstant, syntax_tree::item::TraitConstant);
impl_state!(Implementation, syntax_tree::item::ImplementationSignature);
impl_state!(NegativeImplementation, syntax_tree::item::ImplementationSignature);
impl_state!(ImplementationFunction, syntax_tree::item::ImplementationFunction);
impl_state!(ImplementationType, syntax_tree::item::ImplementationType);
impl_state!(ImplementationConstant, syntax_tree::item::ImplementationConstant);

#[derive(Debug)]
struct Synchronization {
    notify: Condvar,
    building: AtomicBool,
    finalize_flag: Mutex<FinalizeFlag>,
}

#[derive(Debug)]
pub(super) struct State<SyntaxTree> {
    syntax_tree: Arc<SyntaxTree>,
    synchornization: Arc<Synchronization>,
    checkings: Vec<(Checking<Symbolic>, Span)>,
}

impl<SyntaxTree> State<SyntaxTree> {
    pub(super) fn add_checking(
        &mut self,
        checking: Checking<Symbolic>,
        span: Span,
    ) {
        self.checkings.push((checking, span));
    }

    pub(super) fn drafted(syntax_tree: SyntaxTree) -> Self {
        Self {
            syntax_tree: Arc::new(syntax_tree),
            checkings: Vec::new(),
            synchornization: Arc::new(Synchronization {
                notify: Condvar::new(),
                building: AtomicBool::new(false),
                finalize_flag: Mutex::new(FinalizeFlag::Drafted),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum FinalizeFlag {
    Drafted,
    GenericParameter,
    WhereClause,
    Definition,
    Body, /* Mainly for function kinds */
}

#[derive(Debug, Default)]
pub(super) struct Manager {
    states_by_enum_id:
        HashMap<ID<Enum>, State<syntax_tree::item::EnumSignature>>,
    states_by_variant_id:
        HashMap<ID<symbol::Variant>, State<syntax_tree::item::Variant>>,

    states_by_struct_id: HashMap<ID<Struct>, State<syntax_tree::item::Struct>>,
    states_by_constant_id:
        HashMap<ID<Constant>, State<syntax_tree::item::Constant>>,
    states_by_type_id: HashMap<ID<Type>, State<syntax_tree::item::Type>>,
    states_by_function_id:
        HashMap<ID<Function>, State<syntax_tree::item::Function>>,

    states_by_trait_id:
        HashMap<ID<Trait>, State<syntax_tree::item::TraitSignature>>,
    states_by_trait_function_id:
        HashMap<ID<TraitFunction>, State<syntax_tree::item::TraitFunction>>,
    states_by_trait_type_id:
        HashMap<ID<TraitType>, State<syntax_tree::item::TraitType>>,
    states_by_trait_constant_id:
        HashMap<ID<TraitConstant>, State<syntax_tree::item::TraitConstant>>,

    states_by_negative_implementation_id: HashMap<
        ID<NegativeImplementation>,
        State<syntax_tree::item::ImplementationSignature>,
    >,
    states_by_implementation_id: HashMap<
        ID<Implementation>,
        State<syntax_tree::item::ImplementationSignature>,
    >,

    states_by_implementation_function_id: HashMap<
        ID<ImplementationFunction>,
        State<syntax_tree::item::ImplementationFunction>,
    >,
    states_by_implementation_type_id: HashMap<
        ID<ImplementationType>,
        State<syntax_tree::item::ImplementationType>,
    >,
    states_by_implementation_constant_id: HashMap<
        ID<ImplementationConstant>,
        State<syntax_tree::item::ImplementationConstant>,
    >,

    dependencies: HashMap<GlobalID, (GlobalID, FinalizeFlag)>,
    reported_cyclic_dependencies: HashSet<BTreeSet<GlobalID>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("threre's no state for the given symbol")]
pub(super) struct EntryNotFoundError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("there's already a state for the given symbol")]
pub(super) struct ExistingStateError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("cannot finalize the symbol")]
pub(super) enum FinalizeError {
    EntryNotFound(#[from] EntryNotFoundError),
    CyclicDependency,
}

#[derive(CopyGetters)]
pub(super) struct Config<'a, T> {
    state_manager: &'a RwLock<Manager>,

    #[get_copy = "pub(super)"]
    current_id: ID<T>,

    table: &'a Table,

    handler: &'a dyn Handler<error::Error>,
}

impl<'a, T> Config<'a, T>
where
    T: StatefulSymbol,
    ID<T>: Into<GlobalID>,
{
    pub(super) fn handler(&self) -> &'a dyn Handler<error::Error> {
        self.handler
    }

    pub(super) fn finalize_to(
        &self,
        id: GlobalID,
        to: FinalizeFlag,
    ) -> Result<(), FinalizeError> {
        Manager::finalize_to_global_id(
            self.state_manager,
            self.table,
            id,
            Some(self.current_id.into()),
            to,
            self.handler,
        )
    }

    pub(super) fn checkings(
        &self,
    ) -> MappedRwLockReadGuard<'a, [(Checking<Symbolic>, Span)]>
    where
        T::SyntaxTree: 'static,
    {
        let state_manager = self.state_manager.read();

        RwLockReadGuard::map(state_manager, |x| {
            T::get_state(x, self.current_id).unwrap().checkings.as_slice()
        })
    }
}

impl<'a, T> resolution::Config<Symbolic> for Config<'a, T>
where
    T: StatefulSymbol,
    ID<T>: Into<GlobalID>,
{
    fn lifetime_arguments_placeholder(
        &mut self,
    ) -> Option<lifetime::Lifetime<Symbolic>> {
        None
    }

    fn type_arguments_placeholder(&mut self) -> Option<r#type::Type<Symbolic>> {
        None
    }

    fn constant_arguments_placeholder(
        &mut self,
    ) -> Option<constant::Constant<Symbolic>> {
        None
    }

    fn check(&mut self, checking: Checking<Symbolic>, span: Span) {
        let mut state_manager = self.state_manager.write();

        T::get_state_mut(&mut state_manager, self.current_id)
            .unwrap()
            .add_checking(checking, span);
    }

    fn on_global_id_resolved(&mut self, global_id: GlobalID) {
        let finalize_flag = match global_id {
            GlobalID::Module(_) => return,

            GlobalID::Struct(_)
            | GlobalID::Enum(_)
            | GlobalID::Type(_)
            | GlobalID::Function(_)
            | GlobalID::Constant(_)
            | GlobalID::ImplementationType(_)
            | GlobalID::ImplementationFunction(_)
            | GlobalID::ImplementationConstant(_)
            | GlobalID::Variant(_) => FinalizeFlag::Definition,

            GlobalID::Trait(_)
            | GlobalID::TraitType(_)
            | GlobalID::TraitFunction(_)
            | GlobalID::TraitConstant(_)
            | GlobalID::Implementation(_)
            | GlobalID::NegativeImplementation(_) => FinalizeFlag::WhereClause,
        };

        let _ = Manager::finalize_to_global_id(
            self.state_manager,
            self.table,
            global_id,
            Some(self.current_id.into()),
            finalize_flag,
            self.handler,
        );
    }

    #[allow(clippy::too_many_lines)]
    fn on_resolved(&mut self, resolved: &resolution::Resolution<Symbolic>) {
        macro_rules! trait_finalization {
            ($trait_id:expr, $trait_member_id:expr, $trait_generic_arguments:expr, $map:ident) => {
                let trait_id = $trait_id;
                let trait_member_id = $trait_member_id;
                let trait_generic_arguments = $trait_generic_arguments;

                let predicates = self
                    .table
                    .get_premise_predicates(trait_member_id.into())
                    .unwrap();

                let premises = Premises::from_predicates(
                    predicates.into_iter().map(|x| x.predicate),
                );

                // finalize its implementation counter part.
                if let Ok(implementation) = self.table.resolve_implementation(
                    trait_id,
                    trait_generic_arguments,
                    &premises,
                ) {
                    let implementation = self
                        .table
                        .get(implementation.implementation_id)
                        .unwrap();
                    let implementation_member =
                        implementation.$map.get(&$trait_member_id).copied();
                    drop(implementation);

                    if let Some(implementation_member) = implementation_member {
                        let _ = Manager::finalize_to_global_id(
                            self.state_manager,
                            self.table,
                            implementation_member.into(),
                            Some(self.current_id.into()),
                            FinalizeFlag::Definition,
                            self.handler,
                        );
                    }
                }
            };
        }
        match resolved {
            resolution::Resolution::Trait(resolved) => {
                let predicates = self
                    .table
                    .get_premise_predicates(resolved.id.into())
                    .unwrap();

                // resolve to the definition, make it ready for resolution.
                if resolved.generic_arguments.is_definite(
                    &Premises::from_predicates(
                        predicates.into_iter().map(|x| x.predicate),
                    ),
                    self.table,
                ) {
                    let _ = Manager::finalize_to_global_id(
                        self.state_manager,
                        self.table,
                        resolved.id.into(),
                        Some(self.current_id.into()),
                        FinalizeFlag::Definition,
                        self.handler,
                    );

                    // all implementations of the trait should be ready for
                    // resolution.
                    let implementations = {
                        let trait_sym = self.table.get(resolved.id).unwrap();

                        trait_sym
                            .implementations
                            .iter()
                            .copied()
                            .map(Into::into)
                            .chain(
                                trait_sym
                                    .negative_implementations
                                    .iter()
                                    .copied()
                                    .map(Into::into),
                            )
                            .collect::<Vec<_>>()
                    };

                    implementations.into_par_iter().for_each(|x| {
                        let _ = Manager::finalize_to_global_id(
                            self.state_manager,
                            self.table,
                            x,
                            Some(self.current_id.into()),
                            FinalizeFlag::WhereClause,
                            self.handler,
                        );
                    });
                }
            }
            resolution::Resolution::TraitFunction(resolved) => {
                trait_finalization!(
                    {
                        self.table
                            .get(resolved.member.id)
                            .unwrap()
                            .parent_trait_id
                    },
                    resolved.member.id,
                    &resolved.parent_generic_arguments,
                    implementation_function_ids_by_trait_function_id
                );
            }
            resolution::Resolution::TraitType(resolved) => {
                trait_finalization!(
                    {
                        self.table
                            .get(resolved.member.id)
                            .unwrap()
                            .parent_trait_id
                    },
                    resolved.member.id,
                    &resolved.parent_generic_arguments,
                    implementation_type_ids_by_trait_type_id
                );
            }
            resolution::Resolution::TraitConstant(resolved) => {
                trait_finalization!(
                    {
                        self.table.get(resolved.member).unwrap().parent_trait_id
                    },
                    resolved.member,
                    &resolved.parent_generic_arguments,
                    implementation_constant_ids_by_trait_constant_id
                );
            }
            _ => {}
        }
    }
}

impl Manager {
    /// Drafts a symbol into the state manager.
    pub(super) fn draft_symbol<T: StatefulSymbol>(
        &mut self,
        id: ID<T>,
        syntax_tree: T::SyntaxTree,
    ) -> Result<(), ExistingStateError>
    where
        ID<T>: Into<GlobalID>,
    {
        T::draft_symbol(self, id, syntax_tree)
    }

    fn all_symbol(&self) -> impl Iterator<Item = GlobalID> + '_ {
        self.states_by_enum_id
            .keys()
            .copied()
            .map(Into::into)
            .chain(self.states_by_variant_id.keys().copied().map(Into::into))
            .chain(self.states_by_struct_id.keys().copied().map(Into::into))
            .chain(self.states_by_constant_id.keys().copied().map(Into::into))
            .chain(self.states_by_type_id.keys().copied().map(Into::into))
            .chain(self.states_by_function_id.keys().copied().map(Into::into))
            .chain(self.states_by_trait_id.keys().copied().map(Into::into))
            .chain(
                self.states_by_trait_function_id
                    .keys()
                    .copied()
                    .map(Into::into),
            )
            .chain(self.states_by_trait_type_id.keys().copied().map(Into::into))
            .chain(
                self.states_by_trait_constant_id
                    .keys()
                    .copied()
                    .map(Into::into),
            )
            .chain(
                self.states_by_negative_implementation_id
                    .keys()
                    .copied()
                    .map(Into::into),
            )
            .chain(
                self.states_by_implementation_id
                    .keys()
                    .copied()
                    .map(Into::into),
            )
            .chain(
                self.states_by_implementation_function_id
                    .keys()
                    .copied()
                    .map(Into::into),
            )
            .chain(
                self.states_by_implementation_type_id
                    .keys()
                    .copied()
                    .map(Into::into),
            )
            .chain(
                self.states_by_implementation_constant_id
                    .keys()
                    .copied()
                    .map(Into::into),
            )
    }

    fn finalize_to_global_id(
        state_manager: &RwLock<Self>,
        table: &Table,
        id: GlobalID,
        referred_from: Option<GlobalID>,
        to_flag: FinalizeFlag,
        handler: &dyn Handler<error::Error>,
    ) -> Result<(), FinalizeError> {
        macro_rules! implements {
            ($($kind:ident),*) => {
                paste! {
                    match id {
                        GlobalID::Module(_) => Ok(()),
                        $(
                            GlobalID::$kind(id) => Self::finalize_to(
                                state_manager,
                                table,
                                id,
                                referred_from,
                                to_flag,
                                handler
                            )
                        ),*
                    }
                }
            };
        }

        implements!(
            Enum,
            Struct,
            Constant,
            Type,
            Function,
            Trait,
            Implementation,
            Variant,
            TraitConstant,
            TraitType,
            TraitFunction,
            NegativeImplementation,
            ImplementationFunction,
            ImplementationType,
            ImplementationConstant
        )
    }

    fn finalize_to_loop<T: StatefulSymbol>(
        state_manager: &RwLock<Self>,
        table: &Table,
        id: ID<T>,
        to_flag: FinalizeFlag,
        handler: &dyn Handler<error::Error>,
    ) -> Result<(), EntryNotFoundError>
    where
        ID<T>: Into<GlobalID>,
    {
        loop {
            // If the state is already finalized beyond the given flag, then we
            // can just return.
            let (synchronization, syntax_tree) = {
                let state_manager_read = state_manager.read();
                (
                    T::get_state(&state_manager_read, id)
                        .ok_or(EntryNotFoundError)?
                        .synchornization
                        .clone(),
                    T::get_state(&state_manager_read, id)
                        .ok_or(EntryNotFoundError)?
                        .syntax_tree
                        .clone(),
                )
            };

            // done building the symbol, wake up all the threads that are
            // waiting for it.
            if *synchronization.finalize_flag.lock() >= to_flag {
                return Ok(());
            }

            // finalize the symbol.
            let current_flag = *synchronization.finalize_flag.lock();
            match current_flag {
                FinalizeFlag::Drafted => {
                    finalizing::generic_parameter::Finalize::finalize(
                        table,
                        &*syntax_tree,
                        &mut Config {
                            state_manager,
                            current_id: id,
                            table,
                            handler,
                        },
                    );
                    *synchronization.finalize_flag.lock() =
                        FinalizeFlag::GenericParameter;
                }
                FinalizeFlag::GenericParameter => {
                    finalizing::where_clause::Finalize::finalize(
                        table,
                        &*syntax_tree,
                        &mut Config {
                            state_manager,
                            current_id: id,
                            table,
                            handler,
                        },
                    );

                    *synchronization.finalize_flag.lock() =
                        FinalizeFlag::WhereClause;
                }
                FinalizeFlag::WhereClause => {
                    // TODO: finalize definition
                    *synchronization.finalize_flag.lock() =
                        FinalizeFlag::Definition;
                }
                FinalizeFlag::Definition => {
                    // TODO: finalize body
                    *synchronization.finalize_flag.lock() = FinalizeFlag::Body;
                }
                FinalizeFlag::Body => {
                    unreachable!()
                }
            };

            {
                let mut dependencies = Vec::new();
                let mut state_manager_read = state_manager.upgradable_read();

                for (key, val) in &state_manager_read.dependencies {
                    if val.0 == id.into() && to_flag >= val.1 {
                        dependencies.push(*key);
                    }
                }

                state_manager_read.with_upgraded(|x| {
                    for dependency in dependencies {
                        x.dependencies.remove(&dependency);
                    }
                });

                drop(state_manager_read);

                synchronization.building.store(false, atomic::Ordering::SeqCst);
                synchronization.notify.notify_all();
            }
        }
    }

    fn attemp_finalize_to<T: StatefulSymbol>(
        state_manager: &RwLock<Self>,
        table: &Table,
        id: ID<T>,
        to_flag: FinalizeFlag,
        handler: &dyn Handler<error::Error>,
    ) -> Result<(), EntryNotFoundError>
    where
        ID<T>: Into<GlobalID>,
    {
        let synchronization = T::get_state(&state_manager.read(), id)
            .ok_or(EntryNotFoundError)?
            .synchornization
            .clone();

        // if the symbol is currently being built by another thread, then we
        // wait for it to finish or we will just build it ourselves.

        // none of the threads are building the symbol, so this thread will
        // build it.
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
            Self::finalize_to_loop(state_manager, table, id, to_flag, handler)
        } else {
            // some other thread is building the symbol, so we wait for it to
            // finish.
            let mut flag = synchronization.finalize_flag.lock();

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

            drop(flag);

            // try to finalize the symbol again.
            Self::attemp_finalize_to(state_manager, table, id, to_flag, handler)
        }
    }

    fn finalize_to<T: StatefulSymbol>(
        state_manager: &RwLock<Self>,
        table: &Table,
        id: ID<T>,
        referred_from: Option<GlobalID>,
        to_flag: FinalizeFlag,
        handler: &dyn Handler<error::Error>,
    ) -> Result<(), FinalizeError>
    where
        ID<T>: Into<GlobalID>,
    {
        let mut state_manager_read = state_manager.upgradable_read();

        // if the state is already finalized beyond the given flag, then we can
        // just return.
        if *T::get_state(&state_manager_read, id)
            .ok_or(EntryNotFoundError)?
            .synchornization
            .finalize_flag
            .lock()
            >= to_flag
        {
            return Ok(());
        }

        // cyclic dependency detection
        if let Some(referring_site) = referred_from {
            if referring_site == id.into() {
                handler.receive(error::Error::CyclicDependency(
                    CyclicDependency { participants: vec![id.into()] },
                ));

                return Err(FinalizeError::CyclicDependency);
            }

            // dependency cyclic check starts here
            let mut dependency_stack = vec![id.into()];
            let mut current_node = id.into();

            while let Some(dependant) = state_manager_read
                .dependencies
                .get(&current_node)
                .copied()
                .map(|x| x.0)
            {
                dependency_stack.push(dependant);

                // cyclic dependency found
                if dependant == referring_site {
                    let dependency_stack_set =
                        dependency_stack.iter().copied().collect();

                    let reported = !state_manager_read
                        .reported_cyclic_dependencies
                        .contains(&dependency_stack_set);
                    if reported {
                        state_manager_read.with_upgraded(|x| {
                            x.reported_cyclic_dependencies
                                .insert(dependency_stack_set)
                        });
                        handler.receive(error::Error::CyclicDependency(
                            CyclicDependency { participants: dependency_stack },
                        ));
                    }

                    return Err(FinalizeError::CyclicDependency);
                }

                current_node = dependant;
            }

            // no cyclic dependency, add it to the dependency graph
            state_manager_read.with_upgraded(|x| {
                x.dependencies.insert(referring_site, (id.into(), to_flag))
            });
        }

        drop(state_manager_read);

        let result = Self::attemp_finalize_to(
            state_manager,
            table,
            id,
            to_flag,
            handler,
        );

        Ok(result?)
    }
}

impl Table {
    pub(super) fn finalize(
        &self,
        state_manager: &RwLock<Manager>,
        handler: &dyn Handler<error::Error>,
    ) {
        let all_symbols = state_manager.read().all_symbol().collect::<Vec<_>>();

        all_symbols.into_par_iter().for_each(|id| {
            let _ = Manager::finalize_to_global_id(
                state_manager,
                self,
                id,
                None,
                FinalizeFlag::Body,
                handler,
            );
        });
    }
}
