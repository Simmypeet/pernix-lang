use std::{
    any::Any,
    hash::{Hash, Hasher},
    sync::Arc,
};

use pernixc_hash::FxHashMap;
use pernixc_qbice::TrackedEngine;
use pernixc_type::{
    substitution::Substitution,
    r#type::{Type, constructor::Application, kind::TyKind},
};
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{premise::Premise, solver::variable_info::VariableInfos};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UniverseIndex(usize);

impl UniverseIndex {
    #[must_use]
    pub const fn root() -> Self { Self(0) }

    #[must_use]
    pub const fn next(&self) -> Self { Self(self.0 + 1) }
}

pub struct Solver<'a> {
    premise: &'a Premise,
    engine: &'a TrackedEngine,

    map: FxHashMap<Arc<dyn DynIdent>, Box<dyn Any + Send + Sync>>,
    call_stack: Vec<Call<Arc<dyn DynIdent>, Box<dyn Any + Send + Sync>>>,

    limit_step: usize,
    current_step: usize,

    variable_infos: VariableInfos,
    universe_index: UniverseIndex,

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
            universe_index: UniverseIndex::root(),
        }
    }
}

/// A struct storing the call to compute a query and the in progress state.
#[derive(Debug)]
#[allow(missing_docs)]
struct Call<Q, I> {
    query: Q,
    provisional: I,
    is_in_scc: bool,
    is_cyclic_root: bool,
}

/// Primiarily used for checking whether the provisional results in a cycle are
/// consistent with each other.
pub trait Agree {
    fn agree(&self, other: &Self) -> bool;
}

impl<T: Agree> Agree for Box<T> {
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
    ) -> impl Future<Output = Result<Self::Result, OverflowError>>;

    fn provisional_result(&self) -> Self::Result;
}

enum CurrentStatus<Q> {
    Solved(Q),
    NonStarted,
}

impl Solver<'_> {
    pub async fn new_universe<T>(
        &mut self,
        f: impl AsyncFnOnce(&mut Solver) -> T,
    ) -> T {
        let current = self.universe_index;
        self.universe_index = current.next();

        let x = f(self).await;

        self.universe_index = current;

        x
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
                .map(|x| self.max_universe_index(x))
                .max()
                .unwrap_or(UniverseIndex::root()),
        }
    }
}

impl<'a> Solver<'a> {
    #[must_use]
    const fn engine(&self) -> &'a TrackedEngine { self.engine }

    #[must_use]
    pub fn destructure(
        &self,
        left_ap: &'a Application,
        right_ap: &'a Application,
    ) -> Option<impl Iterator<Item = (Interned<Type>, Interned<Type>)> + 'a>
    {
        left_ap.destructure(right_ap, self.engine())
    }

    #[must_use]
    pub fn apply(
        &self,
        subst: &Substitution,
        ty: &Interned<Type>,
    ) -> Interned<Type> {
        subst.apply(ty, self.engine())
    }

    pub async fn kind_of(&self, ty: &Type) -> TyKind {
        ty.kind(self.engine(), self).await
    }

    pub(crate) async fn solve<Q: Solve>(
        &mut self,
        query: &Q,
    ) -> Result<Q::Result, OverflowError> {
        assert!(!self.unwinding, "the query has overflowed, has to abort now");

        if let CurrentStatus::Solved(result) =
            self.mark_as_in_progress(query)?
        {
            return Ok(result);
        }

        let result = query.solve(self).await;

        match result {
            Ok(mut result) => {
                assert!(
                    !self.unwinding,
                    "invalid state: should never return Ok on your own after \
                     overflow"
                );

                let (is_cyclic_root, is_in_scc) = {
                    let frame = self.call_stack.last().unwrap();
                    (frame.is_cyclic_root, frame.is_in_scc)
                };

                // run to fixpoint if the query is in a cycle, to make sure all
                // the provisional results in the cycle are consistent with each
                // other.
                if is_cyclic_root {
                    loop {
                        let provisional = self
                            .call_stack
                            .last_mut()
                            .unwrap()
                            .provisional
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
                        }
                    }
                }

                self.mark_as_solved(query, &result, is_in_scc);

                Ok(result)
            }

            Err(err) => Err(self.handle_overflow(err)),
        }
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
            self.unwinding = true;
            return Err(OverflowError(()));
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
            let provisitional = self.call_stack[position]
                .provisional
                .as_ref()
                .downcast_ref::<Q::Result>()
                .unwrap()
                .clone();

            self.call_stack[position].is_cyclic_root = true;
            for call in &mut self.call_stack[(position + 1)..] {
                call.is_in_scc = true;
            }

            Ok(CurrentStatus::Solved(provisitional))
        } else {
            let arc_query = Arc::new(query.clone()) as Arc<dyn DynIdent>;

            self.current_step += 1;
            self.call_stack.push(Call {
                query: arc_query,
                provisional: Box::new(query.provisional_result()),
                is_in_scc: false,
                is_cyclic_root: false,
            });

            Ok(CurrentStatus::NonStarted)
        }
    }
}
