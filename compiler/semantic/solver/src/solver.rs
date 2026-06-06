use std::{
    any::Any,
    hash::{Hash, Hasher},
    sync::Arc,
};

use pernixc_hash::FxHashMap;
use pernixc_qbice::TrackedEngine;
use pernixc_type::{
    self,
    predicate::Predicate,
    substitution::Substitution,
    r#type::{
        Type, bound::Instantiate, context::TyContext,
        inference::InferenceVariable, kind::TyKind,
    },
};
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    constraints::Constraints,
    premise::Premise,
    solver::{
        universe::{Universe, UniverseIndex},
        variable_info::VariableInfos,
    },
};

pub mod universe;
pub mod variable_info;

/// A trait for identifying type's equality dynamically.
#[allow(missing_docs)]
pub trait DynIdent: Any + Send + Sync + 'static {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn dyn_eq(&self, other: &dyn DynIdent) -> bool;
    fn dyn_hash(&self, state: &mut dyn Hasher);
}

impl<T: Any + Send + Sync + Eq + Hash> DynIdent for T {
    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }

    fn dyn_eq(&self, other: &dyn DynIdent) -> bool {
        other.as_any().downcast_ref::<T>() == Some(self)
    }

    fn dyn_hash(&self, mut state: &mut dyn Hasher) { self.hash(&mut state); }
}

impl PartialEq for dyn DynIdent {
    fn eq(&self, other: &Self) -> bool { self.dyn_eq(other) }
}

impl Eq for dyn DynIdent {}

impl Hash for dyn DynIdent {
    fn hash<H: Hasher>(&self, state: &mut H) { self.dyn_hash(state); }
}

/// An error that occurs when the number of queries exceeds the limit.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    thiserror::Error,
)]
#[error(
    "exceeded the limit of the number of queries; the error hasn't been \
     reported to the user yet as it requires more context"
)]
pub struct OverflowError(());

pub struct Solver<'a> {
    premise: &'a Premise,
    engine: &'a TrackedEngine,

    map: FxHashMap<Arc<dyn DynIdent>, Box<dyn Any + Send + Sync>>,
    call_stack: Vec<Call<Arc<dyn DynIdent>, Box<dyn Any + Send + Sync>>>,

    limit_step: usize,
    current_step: usize,

    variable_infos: VariableInfos,
    universe: Universe,

    unwinding: bool,
}

impl std::fmt::Debug for Solver<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Solver")
            .field("premise", &self.premise)
            .field("engine", &self.engine)
            .finish()
    }
}

impl<'a> Solver<'a> {
    #[must_use]
    pub fn new(premise: &'a Premise, engine: &'a TrackedEngine) -> Self {
        Self {
            premise,
            engine,
            map: FxHashMap::default(),
            call_stack: Vec::new(),
            limit_step: 69_420, // hehe
            variable_infos: VariableInfos::default(),
            current_step: 0,
            unwinding: false,
            universe: Universe::default(),
        }
    }
}

/// A struct storing the call to compute a query and the in progress state.
#[derive(Debug)]
#[allow(missing_docs)]
struct Call<Q, I> {
    query: Q,
    provisional: Option<I>,
    is_in_scc: bool,
}

/// Primiarily used for checking whether the provisional results in a cycle are
/// consistent with each other.
pub trait Agree {
    fn agree(&self, other: &Self) -> bool;
}

impl Agree for Type {
    fn agree(&self, other: &Self) -> bool { self == other }
}

impl Agree for Constraints {
    fn agree(&self, other: &Self) -> bool { self == other }
}

impl Agree for Substitution {
    fn agree(&self, other: &Self) -> bool { self == other }
}

impl<T: Agree, U: Agree> Agree for (T, U) {
    fn agree(&self, other: &Self) -> bool {
        self.0.agree(&other.0) && self.1.agree(&other.1)
    }
}

impl<T: Agree> Agree for Box<T> {
    fn agree(&self, other: &Self) -> bool {
        self.as_ref().agree(other.as_ref())
    }
}

impl<T: Agree> Agree for Interned<T> {
    fn agree(&self, other: &Self) -> bool {
        self.as_ref().agree(other.as_ref())
    }
}

impl<T: Agree> Agree for Option<T> {
    fn agree(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => a.agree(b),
            (None, None) => true,
            _ => false,
        }
    }
}

/// The main interface for the solver.
pub trait Solve: Clone + Eq + Hash + DynIdent {
    type Result: Any + Send + Sync + Clone + Agree;

    fn solve(
        &self,
        solver: &mut Solver,
    ) -> impl Future<Output = Result<Self::Result, OverflowError>> + Send;

    fn provisional_result(&self) -> Provisional<Self::Result>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Provisional<T> {
    Continue(T),
    Bail,
}

enum CurrentStatus<Q> {
    Solved(Q),
    NonStarted,
}

pub type BoundInstantiation = Vec<Interned<Type>>;

impl Solver<'_> {
    /// Creates a fresh inference variables for each kind in `kinds`, and
    /// returns an array of all the created inference variables.
    pub fn create_inference_instantiations(
        &mut self,
        kinds: impl Iterator<Item = TyKind>,
    ) -> BoundInstantiation {
        kinds
            .map(|kind| {
                let var = self.fresh_inference_variable(kind);
                self.intern(Type::InferenceVariable(var))
            })
            .collect()
    }

    /// Creates a fresh skolemized variables for each kind in `kinds`, and
    /// returns an array of all the created inference variables.
    pub fn create_skolem_instantiations(
        &mut self,
        kinds: impl Iterator<Item = TyKind>,
    ) -> BoundInstantiation {
        kinds
            .map(|kind| {
                let var = self.fresh_skolem_variable(kind);
                self.intern(Type::SkolemizedVariable(var))
            })
            .collect()
    }

    pub fn compose_subst(
        &mut self,
        sub1: &mut Substitution,
        sub2: Substitution,
    ) {
        sub1.compose(sub2, self.engine());
    }

    #[must_use]
    pub fn instantiate(
        &self,
        ty: &Interned<Type>,
        instantiations: &[Interned<Type>],
    ) -> Interned<Type> {
        ty.instantiate(instantiations, self.engine())
    }

    #[must_use]
    pub fn max_universe_index(&self, ty: &Type) -> UniverseIndex {
        match ty {
            Type::BoundVariable(_) | Type::GenericParameter(_) => {
                UniverseIndex::root()
            }

            Type::InferenceVariable(inference_variable) => {
                self.get_inference_variable_universe(*inference_variable)
            }

            Type::SkolemizedVariable(skolemized_variable) => {
                self.get_skolemized_variable_universe(*skolemized_variable)
            }

            Type::Application(application) => application
                .arguments()
                .iter()
                .map(|x| self.max_universe_index(x))
                .max()
                .unwrap_or(UniverseIndex::root()),
        }
    }
}

pub(crate) fn occur_check(
    inference_variable: InferenceVariable,
    ty: &Interned<Type>,
) -> bool {
    match ty.as_ref() {
        Type::InferenceVariable(var) => *var == inference_variable,

        Type::BoundVariable(_)
        | Type::GenericParameter(_)
        | Type::SkolemizedVariable(_) => false,

        Type::Application(application) => application
            .arguments()
            .iter()
            .any(|arg| occur_check(inference_variable, arg)),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum DoOccurCheck {
    Yes,
    No,
}

impl<'a> Solver<'a> {
    #[must_use]
    pub const fn engine(&self) -> &'a TrackedEngine { self.engine }

    pub fn premise_predicates(
        &self,
    ) -> impl Iterator<Item = &'a Predicate> + 'a {
        self.premise.iter()
    }

    #[must_use]
    pub async fn kind_of(&self, ty: &Type) -> TyKind {
        ty.kind(self.engine(), self).await
    }

    pub fn intern<T>(&self, value: T) -> Interned<T>
    where
        T: StableHash + Identifiable + Send + Sync + 'static,
    {
        self.engine().intern(value)
    }

    pub(crate) async fn can_bind_inference_variable_to_type(
        &mut self,
        inference_variable: InferenceVariable,
        ty: &Interned<Type>,
        do_occur_check: DoOccurCheck,
    ) -> bool {
        let var_kind = self.get_inference_variable_kind(&inference_variable);
        let subject_kind = self.kind_of(ty).await;

        if var_kind != subject_kind {
            return false;
        }

        let var_uni = self.get_inference_variable_universe(inference_variable);
        let subject_uni = self.max_universe_index(ty);

        if var_uni < subject_uni {
            return false;
        }

        if do_occur_check == DoOccurCheck::Yes
            && occur_check(inference_variable, ty)
        {
            return false;
        }

        true
    }

    pub(crate) async fn solve<Q: Solve>(
        &mut self,
        query: &Q,
    ) -> Result<Q::Result, OverflowError> {
        assert!(!self.unwinding, "the query has overflowed, has to abort now");

        match self.mark_as_in_progress(query) {
            Ok(CurrentStatus::Solved(result)) => return Ok(result),
            Ok(CurrentStatus::NonStarted) => {}
            Err(err) => return Err(err),
        }

        let result = query.solve(self).await;

        match result {
            Ok(mut result) => {
                /// Immediately `OverflowError` if we have to run to fixpoint
                /// for too many times, to avoid infinite loop in pathological
                /// cases.
                const FIXED_POINT_COUNT: usize = 16;

                assert!(
                    !self.unwinding,
                    "invalid state: should never return Ok on your own after \
                     overflow"
                );

                let (has_cycle, is_in_scc) = {
                    let frame = self.call_stack.last().unwrap();
                    (frame.provisional.is_some(), frame.is_in_scc)
                };

                // run to fixpoint if the query is in a cycle, to make sure all
                // the provisional results in the cycle are consistent with each
                // other.
                if has_cycle {
                    let mut count = FIXED_POINT_COUNT;

                    loop {
                        if count == 0 {
                            let err = self.bail();
                            return Err(self.handle_overflow(err));
                        }

                        let provisional = self
                            .call_stack
                            .last_mut()
                            .unwrap()
                            .provisional
                            .as_mut()
                            .unwrap()
                            .as_mut()
                            .downcast_mut::<Q::Result>()
                            .unwrap();

                        if provisional.agree(&result) {
                            break;
                        }

                        *provisional = result;
                        result = match query.solve(self).await {
                            Ok(res) => res,
                            Err(err) => return Err(self.handle_overflow(err)),
                        };

                        count -= 1;
                    }
                }

                self.mark_as_solved(query, &result, is_in_scc);

                Ok(result)
            }

            Err(err) => Err(self.handle_overflow(err)),
        }
    }

    const fn bail(&mut self) -> OverflowError {
        self.unwinding = true;
        OverflowError(())
    }

    fn handle_overflow(&mut self, er: OverflowError) -> OverflowError {
        assert!(
            self.unwinding,
            "invalid state: should never return OverflowError on your own"
        );

        self.call_stack.pop();

        // all the overflowed calls has unwinded, so we can return
        // to normal state
        if self.call_stack.is_empty() {
            self.unwinding = false;
            self.current_step = 0;
        }

        er
    }

    fn mark_as_solved<Q: Solve>(
        &mut self,
        query: &Q,
        result: &Q::Result,
        is_in_scc: bool,
    ) {
        let arc_query = Arc::new(query.clone()) as Arc<dyn DynIdent>;

        // can only memoize if the query is not in the cyclic.
        if !is_in_scc {
            self.map.insert(arc_query, Box::new(result.clone()));
        }

        self.call_stack.pop();

        if self.call_stack.is_empty() {
            self.current_step = 0;
        }
    }

    fn mark_as_in_progress<Q: Solve>(
        &mut self,
        query: &Q,
    ) -> Result<CurrentStatus<Q::Result>, OverflowError> {
        if self.current_step >= self.limit_step {
            return Err(self.bail());
        }

        // if the query is already computed, return the result
        if let Some(result) = self.map.get(query as &dyn DynIdent) {
            let any = &**result;

            return Ok(CurrentStatus::Solved(
                any.downcast_ref::<Q::Result>().unwrap().clone(),
            ));
        }

        // check if the query already on stack
        let position = self.call_stack.iter().position(|x| {
            let any = &*x.query;

            any == query as &dyn DynIdent
        });

        if let Some(position) = position {
            let provisional = if let Some(provisional) =
                &self.call_stack[position].provisional
            {
                provisional.downcast_ref::<Q::Result>().unwrap().clone()
            } else {
                let provisional = match query.provisional_result() {
                    Provisional::Continue(provisional) => provisional,
                    Provisional::Bail => return Err(self.bail()),
                };

                self.call_stack[position].provisional =
                    Some(Box::new(provisional.clone()));

                provisional
            };

            for call in &mut self.call_stack[(position + 1)..] {
                call.is_in_scc = true;
            }

            Ok(CurrentStatus::Solved(provisional))
        } else {
            let arc_query = Arc::new(query.clone()) as Arc<dyn DynIdent>;

            self.current_step += 1;
            self.call_stack.push(Call {
                query: arc_query,
                provisional: None,
                is_in_scc: false,
            });

            Ok(CurrentStatus::NonStarted)
        }
    }
}
