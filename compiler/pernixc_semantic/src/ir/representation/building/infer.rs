use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    sync::atomic::{AtomicUsize, Ordering},
};

use enum_as_inner::EnumAsInner;
use getset::{Getters, MutGetters};

use crate::{
    arena::{Arena, ID},
    semantic::{
        fresh::Fresh,
        mapping::Mapping,
        model::Model,
        normalizer::Normalizer,
        session::{self, ExceedLimitError, Limit, Session},
        simplify::simplify,
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        unification, Environment, Premise,
    },
    symbol::table::{self, Table},
};

/// A unique identifier for an inference variable.
pub struct InferenceVariable<T> {
    id: usize,
    _phantom: std::marker::PhantomData<T>,
}

impl<T> std::fmt::Debug for InferenceVariable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InferenceVariable({})", self.id)
    }
}

impl<T> Clone for InferenceVariable<T> {
    fn clone(&self) -> Self { *self }
}

impl<T> Copy for InferenceVariable<T> {}

impl<T> PartialEq for InferenceVariable<T> {
    fn eq(&self, other: &Self) -> bool { self.id == other.id }
}

impl<T> Eq for InferenceVariable<T> {}

impl<T> std::hash::Hash for InferenceVariable<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.id.hash(state); }
}

impl<T> PartialOrd for InferenceVariable<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for InferenceVariable<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.id.cmp(&other.id) }
}

impl<T> InferenceVariable<T> {
    /// Creates a new inference variable that is unique for all any created
    /// ones.
    pub fn new() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);

        Self {
            id: COUNTER.fetch_add(1, Ordering::SeqCst),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T> std::default::Default for InferenceVariable<T> {
    fn default() -> Self { Self::new() }
}

impl<T> Fresh for InferenceVariable<T> {
    fn fresh() -> Self { Self::new() }
}

/// A constraint used for restricting the possible value of an inference.
pub trait Constraint<T>: std::fmt::Debug + Clone {
    /// Determines whether a term satisfies the constraint (can be inferred to).
    fn satisfies(&self, term: &T) -> bool;

    /// Combines two constraints into a single one.
    fn combine(&self, another: &Self) -> Self;
}

/// An enumeration of either a known value or an inference in progress.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum Inference<T, C> {
    /// The concrete value of the inference variable is known.
    Known(T),

    /// The inference variable is being inferred.
    Inferring(ID<C>),
}

/// An implementation of the inference context for a specific term
#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct ContextImpl<T: Term, C: 'static> {
    inference_by_ids: HashMap<T::InferenceVariable, Inference<T, C>>,
    constraints: Arena<C>,
}

impl<T: Term, C: 'static> ContextImpl<T, C> {
    #[must_use]
    fn register(
        &mut self,
        inference_variable: T::InferenceVariable,
        constraint: C,
    ) -> bool {
        match self.inference_by_ids.entry(inference_variable) {
            Entry::Occupied(_) => false,
            Entry::Vacant(entry) => {
                let constraint_id = self.constraints.insert(constraint);
                entry.insert(Inference::Inferring(constraint_id));

                true
            }
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn get_inference(
        &self,
        inference_variable: T::InferenceVariable,
    ) -> Option<&Inference<T, C>> {
        self.inference_by_ids.get(&inference_variable)
    }

    fn get_constraint(&self, constraint_id: ID<C>) -> Option<&C> {
        self.constraints.get(constraint_id)
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "failed to unify the inference variable with the constraint due to \
     unsatisfiable constraint"
)]
pub struct UnsatisfiedConstraintError<T, C> {
    /// The term that failed to satisfy the constraint.
    pub term: T,

    /// The constraint that the term failed to satisfy.
    pub constraint: C,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "the inference variable is used before being registered in the context"
)]
pub struct UnregisteredInferenceVariableError<ID>(pub ID);

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum UnifyConstraintError<T: Term, C> {
    #[error(transparent)]
    UnsatisfiedConstraintError(#[from] UnsatisfiedConstraintError<T, C>),

    #[error(transparent)]
    UnregisteredInferenceVariableError(
        #[from] UnregisteredInferenceVariableError<T::InferenceVariable>,
    ),
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum AssignKnownValueError<T, C> {
    #[error(transparent)]
    UnsatisfiedConstraintError(#[from] UnsatisfiedConstraintError<T, C>),

    #[error("the given constraint ID doesn't exist in the context")]
    InvalidConstraintID(ID<C>),
}

impl<T: Term, C: Constraint<T> + 'static> ContextImpl<T, C> {
    #[allow(clippy::needless_pass_by_value)]
    fn unify_with_constraint(
        &mut self,
        inference_variable: T::InferenceVariable,
        constraint: &C,
    ) -> Result<(), UnifyConstraintError<T, C>> {
        let inference = self
            .inference_by_ids
            .get_mut(&inference_variable)
            .ok_or(UnifyConstraintError::UnregisteredInferenceVariableError(
                UnregisteredInferenceVariableError(inference_variable.clone()),
            ))?;

        match inference {
            Inference::Known(inferred) => {
                if !constraint.satisfies(inferred) {
                    return Err(
                        UnifyConstraintError::UnsatisfiedConstraintError(
                            UnsatisfiedConstraintError {
                                term: inferred.clone(),
                                constraint: constraint.clone(),
                            },
                        ),
                    );
                }
            }

            Inference::Inferring(constraint_id) => {
                let current_constraint =
                    self.constraints.get_mut(*constraint_id).unwrap();

                current_constraint.combine(constraint);
            }
        }

        Ok(())
    }

    fn assign_infer_to_known(
        &mut self,
        constraint_id: ID<C>,
        known: T,
    ) -> Result<(), AssignKnownValueError<T, C>> {
        // check if the known value satisfies the constraint
        let constraint = self
            .constraints
            .get(constraint_id)
            .ok_or(AssignKnownValueError::InvalidConstraintID(constraint_id))?;

        // check if the known value satisfies the constraint
        if !constraint.satisfies(&known) {
            return Err(AssignKnownValueError::UnsatisfiedConstraintError(
                UnsatisfiedConstraintError {
                    term: known,
                    constraint: constraint.clone(),
                },
            ));
        }

        // replace every occurrence of the constraint with the known value
        for infer in self.inference_by_ids.values_mut() {
            let Inference::Inferring(infer_constraint_id) = infer else {
                continue;
            };
            let infer_constraint_id = *infer_constraint_id;

            if infer_constraint_id != constraint_id {
                continue;
            }

            *infer = Inference::Known(known.clone());
        }

        // remove the constraint
        assert!(self.constraints.remove(constraint_id).is_some());

        Ok(())
    }
}

/// The constraints used for type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum TypeConstraint {
    /// The type can be inferred into any type.
    #[default]
    All,

    /// The type can be any number type. (signed/unsigned/floating)
    Number,

    /// The type can be signed number type. (signed integer/floating)
    Signed,

    /// The type can be only floating number type. (float32/float64)
    Floating,
}

impl<M: Model> Constraint<Type<M>> for TypeConstraint {
    fn satisfies(&self, term: &Type<M>) -> bool {
        use crate::semantic::term::r#type::Primitive;

        match self {
            Self::All => true,
            Self::Number => matches!(
                term,
                Type::Primitive(
                    Primitive::Int8
                        | Primitive::Int16
                        | Primitive::Int32
                        | Primitive::Int64
                        | Primitive::Uint8
                        | Primitive::Uint16
                        | Primitive::Uint32
                        | Primitive::Uint64
                        | Primitive::Float32
                        | Primitive::Float64
                )
            ),
            Self::Signed => matches!(
                term,
                Type::Primitive(
                    Primitive::Int8
                        | Primitive::Int16
                        | Primitive::Int32
                        | Primitive::Int64
                        | Primitive::Float32
                        | Primitive::Float64
                )
            ),
            Self::Floating => matches!(
                term,
                Type::Primitive(Primitive::Float32 | Primitive::Float64)
            ),
        }
    }

    fn combine(&self, another: &Self) -> Self { *self.max(another) }
}

/// The struct implementing the [`Constraint`] trait for no restriction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NoConstraint;

impl<T> Constraint<T> for NoConstraint {
    fn satisfies(&self, _: &T) -> bool { true }

    fn combine(&self, _: &Self) -> Self { Self }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
pub enum UnifyError {
    #[error(transparent)]
    UnregisteredTypeInferenceVariable(
        #[from]
        UnregisteredInferenceVariableError<
            InferenceVariable<Type<super::Model>>,
        >,
    ),

    #[error(transparent)]
    UnregisteredConstantInferenceVariable(
        #[from]
        UnregisteredInferenceVariableError<
            InferenceVariable<Constant<super::Model>>,
        >,
    ),

    #[error(
        "cannot unify the term due to the undecidability (exceeding the \
         computation limit)"
    )]
    ExceedLimitError(#[from] ExceedLimitError),

    #[error("the two types cannot be unified or are mismatched")]
    IncompatibleTypes { lhs: Type<super::Model>, rhs: Type<super::Model> },

    #[error("the two constants cannot be unified or are mismatched")]
    IncompatibleConstants {
        lhs: Constant<super::Model>,
        rhs: Constant<super::Model>,
    },

    #[error(transparent)]
    UnsatisfiedConstraint(
        UnsatisfiedConstraintError<Type<super::Model>, TypeConstraint>,
    ),
}

/// The inference context storing the inference variables and constraints.
#[derive(Debug, Clone, PartialEq, Eq, Getters, MutGetters, Default)]
pub struct Context {
    type_inference_context: ContextImpl<Type<super::Model>, TypeConstraint>,
    constant_inference_context:
        ContextImpl<Constant<super::Model>, NoConstraint>,
}

/// Implemented by types and constants, which can be inferree.
pub trait Inferable: Term<InferenceVariable = InferenceVariable<Self>> {
    type Constraint: Constraint<Self> + 'static;
}

impl Inferable for Type<super::Model> {
    type Constraint = TypeConstraint;
}

impl Inferable for Constant<super::Model> {
    type Constraint = NoConstraint;
}

trait InferableSealed: Inferable {
    fn get_context(infer: &Context) -> &ContextImpl<Self, Self::Constraint>;
    fn get_context_mut(
        infer: &mut Context,
    ) -> &mut ContextImpl<Self, Self::Constraint>;
}

impl InferableSealed for Type<super::Model> {
    fn get_context(infer: &Context) -> &ContextImpl<Self, Self::Constraint> {
        &infer.type_inference_context
    }

    fn get_context_mut(
        infer: &mut Context,
    ) -> &mut ContextImpl<Self, Self::Constraint> {
        &mut infer.type_inference_context
    }
}

impl InferableSealed for Constant<super::Model> {
    fn get_context(infer: &Context) -> &ContextImpl<Self, Self::Constraint> {
        &infer.constant_inference_context
    }

    fn get_context_mut(
        infer: &mut Context,
    ) -> &mut ContextImpl<Self, Self::Constraint> {
        &mut infer.constant_inference_context
    }
}

// delegate the methods to the context
impl Context {
    /// Registers a new inference variable with a constraint.
    ///
    /// # Returns
    ///
    /// Returns `true` if the inference variable was not already registered in
    /// this context.
    #[must_use]
    pub fn register<T: InferableSealed>(
        &mut self,
        inference_variable: InferenceVariable<T>,
        constraint: T::Constraint,
    ) -> bool {
        T::get_context_mut(self).register(inference_variable, constraint)
    }

    /// Gets the [`Inference`] for an inference variable.
    #[must_use]
    pub fn get_inference<T: InferableSealed>(
        &self,
        inference_variable: InferenceVariable<T>,
    ) -> Option<&Inference<T, T::Constraint>> {
        T::get_context(self).get_inference(inference_variable)
    }

    /// Gets the constraint for a constraint ID.
    #[must_use]
    pub fn get_constraint<T: InferableSealed>(
        &self,
        constraint_id: ID<T::Constraint>,
    ) -> Option<&T::Constraint> {
        T::get_context(self).get_constraint(constraint_id)
    }

    /// Unifies the given inference variable with a constraint.
    ///
    /// # Returns
    ///
    /// Returns `true` if the inference variable was successfully unified with
    /// the constraint. Otherwise, `false` if the constraint was not satisfied.
    pub fn unify_with_constraint<T: InferableSealed>(
        &mut self,
        inference_variable: InferenceVariable<T>,
        constraint: &T::Constraint,
    ) -> Result<(), UnifyConstraintError<T, T::Constraint>> {
        T::get_context_mut(self)
            .unify_with_constraint(inference_variable, constraint)
    }

    /// Infers the given `contraint_id` to a known value.
    ///
    /// If the given known value satsifies the constraint, every occurrence of
    /// the `constraint_id` in the inference context will be replaced with the
    /// known value.
    pub fn assign_infer_to_known<T: InferableSealed>(
        &mut self,
        constraint_id: ID<T::Constraint>,
        known: T,
    ) -> Result<(), AssignKnownValueError<T, T::Constraint>> {
        T::get_context_mut(self).assign_infer_to_known(constraint_id, known)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct UnificationConfig;

impl unification::Config<super::Model> for UnificationConfig {
    fn lifetime_unifiable(
        &mut self,
        from: &Lifetime<super::Model>,
        to: &Lifetime<super::Model>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(from.is_inference() || to.is_inference())
    }

    fn type_unifiable(
        &mut self,
        from: &Type<super::Model>,
        to: &Type<super::Model>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(from.is_inference() || to.is_inference())
    }

    fn constant_unifiable(
        &mut self,
        from: &Constant<super::Model>,
        to: &Constant<super::Model>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(from.is_inference() || to.is_inference())
    }
}

impl Normalizer<super::Model> for Context {
    fn normalize_lifetime(
        _: &InferenceVariable<Lifetime<super::Model>>,
        _: &Environment<super::Model, impl table::State, Self>,
        _: &mut Limit<
            impl Session<Lifetime<super::Model>>
                + Session<Type<super::Model>>
                + Session<Constant<super::Model>>,
        >,
    ) -> Result<Option<Lifetime<super::Model>>, ExceedLimitError> {
        Ok(None)
    }

    fn normalize_type(
        ty: &InferenceVariable<Type<super::Model>>,
        environment: &Environment<super::Model, impl table::State, Self>,
        _: &mut Limit<
            impl Session<Lifetime<super::Model>>
                + Session<Type<super::Model>>
                + Session<Constant<super::Model>>,
        >,
    ) -> Result<Option<Type<super::Model>>, ExceedLimitError> {
        Ok(
            if let Some(Inference::Known(normalized)) =
                environment.normalizer.type_inference_context.get_inference(*ty)
            {
                Some(normalized.clone())
            } else {
                None
            },
        )
    }

    fn normalize_constant(
        constant: &InferenceVariable<Constant<super::Model>>,
        environment: &Environment<super::Model, impl table::State, Self>,
        _: &mut Limit<
            impl Session<Lifetime<super::Model>>
                + Session<Type<super::Model>>
                + Session<Constant<super::Model>>,
        >,
    ) -> Result<Option<Constant<super::Model>>, ExceedLimitError> {
        Ok(
            if let Some(Inference::Known(normalized)) = environment
                .normalizer
                .constant_inference_context
                .get_inference(*constant)
            {
                Some(normalized.clone())
            } else {
                None
            },
        )
    }
}

impl Context {
    fn handle_known_and_infer<
        T: Term<Model = super::Model>,
        C: 'static + Constraint<T>,
    >(
        &mut self,
        inferring: ID<C>,
        known: &T,
        premise: &Premise<super::Model>,
        table: &Table<impl table::State>,
        context: &impl Fn(&mut Self) -> &mut ContextImpl<T, C>,
        into_unify_error: &impl Fn(UnsatisfiedConstraintError<T, C>) -> UnifyError,
    ) -> Result<(), UnifyError>
    where
        session::Default<super::Model>: Session<T>,
    {
        // simplify the known value. idk, if this is
        // necessary
        let known = simplify(
            known,
            &Environment { premise, table, normalizer: self },
            &mut Limit::new(&mut session::Default::default()),
        )?;

        match context(self).assign_infer_to_known(inferring, known) {
            Ok(()) => Ok(()),
            Err(AssignKnownValueError::InvalidConstraintID(id)) => {
                panic!(
                    "found invalid {id:?} constraint id in the inference \
                     context"
                )
            }

            Err(AssignKnownValueError::UnsatisfiedConstraintError(error)) => {
                Err(into_unify_error(error))
            }
        }
    }

    fn handle_mapping<
        T: Term<Model = super::Model, InferenceVariable = InferenceVariable<T>>,
        S: table::State,
        C: 'static + Constraint<T>,
    >(
        &mut self,
        premise: &Premise<super::Model>,
        table: &Table<S>,
        mapping: &HashMap<T, HashSet<T>>,
        inference_context: &impl Fn(&mut Self) -> &mut ContextImpl<T, C>,
        unify: &impl Fn(
            &mut Self,
            &T,
            &T,
            &Premise<super::Model>,
            &Table<S>,
        ) -> Result<(), UnifyError>,
        constraint_error: &impl Fn(UnsatisfiedConstraintError<T, C>) -> UnifyError,
    ) -> Result<(), UnifyError>
    where
        session::Default<super::Model>: Session<T>,

        UnifyError:
            From<UnregisteredInferenceVariableError<InferenceVariable<T>>>,
    {
        for (lhs, rhs) in mapping
            .iter()
            .flat_map(|(lhs, rhs)| std::iter::once(lhs).zip(rhs.iter()))
        {
            match (lhs.as_inference().cloned(), rhs.as_inference().cloned()) {
                (Some(lhs), Some(rhs)) => {
                    let lhs_inference = inference_context(self)
                        .get_inference(lhs)
                        .ok_or(
                            UnregisteredInferenceVariableError(lhs.clone())
                                .into(),
                        )?
                        .clone();

                    let rhs_inference = inference_context(self)
                        .get_inference(rhs)
                        .ok_or(
                            UnregisteredInferenceVariableError(rhs.clone())
                                .into(),
                        )?
                        .clone();

                    match (lhs_inference, rhs_inference) {
                        // recursively unify the two inferences
                        (
                            Inference::Known(known_lhs),
                            Inference::Known(known_rhs),
                        ) => {
                            unify(
                                self, &known_lhs, &known_rhs, premise, table,
                            )?;
                        }

                        // unify the known value with the inferring value
                        (
                            Inference::Known(known),
                            Inference::Inferring(inferring),
                        )
                        | (
                            Inference::Inferring(inferring),
                            Inference::Known(known),
                        ) => self.handle_known_and_infer::<T, C>(
                            inferring,
                            &known,
                            premise,
                            table,
                            inference_context,
                            constraint_error,
                        )?,

                        // merge the constraints of the two inferences and make
                        // them use the same constraint
                        // id
                        (
                            Inference::Inferring(lhs_inferring_id),
                            Inference::Inferring(rhs_inferring_id),
                        ) => {
                            // will make rhs use lhs's constraint id
                            let rhs_constraint = inference_context(self)
                                .constraints
                                .remove(rhs_inferring_id)
                                .unwrap();

                            let lhs_constraint = inference_context(self)
                                .constraints
                                .get_mut(lhs_inferring_id)
                                .unwrap();

                            *lhs_constraint =
                                lhs_constraint.combine(&rhs_constraint);

                            // replace all the occurrences of rhs's constraint
                            // id with
                            // lhs's constraint id
                            for replace in inference_context(self)
                                .inference_by_ids
                                .values_mut()
                                .filter_map(|x| match x {
                                    Inference::Known(_) => None,
                                    Inference::Inferring(infer) => {
                                        if infer == &rhs_inferring_id {
                                            Some(infer)
                                        } else {
                                            None
                                        }
                                    }
                                })
                            {
                                *replace = lhs_inferring_id;
                            }
                        }
                    }
                }

                (Some(inference), None) | (None, Some(inference)) => {
                    let another_known =
                        if lhs.as_inference().is_none() { lhs } else { rhs };

                    match inference_context(self)
                        .get_inference(inference)
                        .cloned()
                        .ok_or(
                            UnregisteredInferenceVariableError(inference)
                                .into(),
                        )? {
                        Inference::Known(known) => {
                            unify(self, &known, another_known, premise, table)?;
                        }
                        Inference::Inferring(inferring) => {
                            self.handle_known_and_infer(
                                inferring,
                                another_known,
                                premise,
                                table,
                                inference_context,
                                constraint_error,
                            )?;
                        }
                    }
                }

                (_, _) => unreachable!(),
            }
        }

        Ok(())
    }

    fn type_inference_context_mut(
        &mut self,
    ) -> &mut ContextImpl<Type<super::Model>, TypeConstraint> {
        &mut self.type_inference_context
    }

    fn constant_inference_context_mut(
        &mut self,
    ) -> &mut ContextImpl<Constant<super::Model>, NoConstraint> {
        &mut self.constant_inference_context
    }

    fn handle_unification<
        T: Term<Model = super::Model, InferenceVariable = InferenceVariable<T>>,
    >(
        &mut self,
        unification: unification::Unification<T>,
        premise: &Premise<super::Model>,
        table: &Table<impl table::State>,
    ) -> Result<(), UnifyError> {
        // turns the unification into mapping pairs
        let mapping: Mapping<super::Model> =
            Mapping::from_unification(unification);

        self.handle_mapping(
            premise,
            table,
            &mapping.types,
            &Self::type_inference_context_mut,
            &Self::unify_type,
            &UnifyError::UnsatisfiedConstraint,
        )?;

        self.handle_mapping(
            premise,
            table,
            &mapping.constants,
            &Self::constant_inference_context_mut,
            &Self::unify_constant,
            &(|_| unreachable!("constant can always be unified")),
        )?;

        Ok(())
    }

    pub fn unify_constant(
        &mut self,
        lhs: &Constant<super::Model>,
        rhs: &Constant<super::Model>,
        premise: &Premise<super::Model>,
        table: &Table<impl table::State>,
    ) -> Result<(), UnifyError> {
        // obtains the unification result
        let Some(unification) = unification::unify(
            lhs,
            rhs,
            &mut UnificationConfig,
            &Environment { premise, table, normalizer: self },
            &mut Limit::new(&mut session::Default::default()),
        )?
        else {
            return Err(UnifyError::IncompatibleConstants {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            });
        };

        self.handle_unification(unification, premise, table)
    }

    pub fn unify_type(
        &mut self,
        lhs: &Type<super::Model>,
        rhs: &Type<super::Model>,
        premise: &Premise<super::Model>,
        table: &Table<impl table::State>,
    ) -> Result<(), UnifyError> {
        // obtains the unification result
        let Some(unification) = unification::unify(
            lhs,
            rhs,
            &mut UnificationConfig,
            &Environment { premise, table, normalizer: self },
            &mut Limit::new(&mut session::Default::default()),
        )?
        else {
            return Err(UnifyError::IncompatibleTypes {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            });
        };

        self.handle_unification(unification, premise, table)
    }
}

#[cfg(test)]
mod tests;
