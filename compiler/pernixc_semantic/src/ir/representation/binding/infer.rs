//! Contains logic related to hindley-milner type inference algorithm.

use std::{
    collections::{hash_map::Entry, BTreeMap, BTreeSet, HashMap},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use enum_as_inner::EnumAsInner;
use getset::{Getters, MutGetters};
use lazy_static::lazy_static;

use crate::{
    arena::{Arena, ID},
    ir::{ConstraintModel, Erased, NoConstraint},
    symbol::table::{self, Building, Table},
    type_system::{
        environment::Environment,
        fresh::Fresh,
        mapping::Mapping,
        model,
        normalizer::Normalizer,
        observer::{self, Observer},
        query,
        simplify::simplify,
        term::{
            self,
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Type},
            Never, Term, Tuple,
        },
        unification::{self, Log, Unification},
        visitor::RecursiveIterator,
        Compute, Output, OverflowError, Premise, Satisfied, Succeeded,
    },
};

/// The model used for building the IR
///
/// This model enables the use of inference variables
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Model;

impl<T> From<Never> for InferenceVariable<T> {
    fn from(value: Never) -> Self { match value {} }
}

impl model::Model for Model {
    type LifetimeInference = Erased;
    type TypeInference = InferenceVariable<Type<Self>>;
    type ConstantInference = InferenceVariable<Constant<Self>>;

    fn from_default_type(ty: Type<model::Default>) -> Type<Self> {
        Type::from_other_model(ty)
    }

    fn from_default_lifetime(
        lifetime: Lifetime<model::Default>,
    ) -> Lifetime<Self> {
        Lifetime::from_other_model(lifetime)
    }

    fn from_default_constant(
        constant: Constant<model::Default>,
    ) -> Constant<Self> {
        Constant::from_other_model(constant)
    }
}

impl<T: table::State, U> table::Display<T> for InferenceVariable<U> {
    fn fmt(
        &self,
        _: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "?")
    }
}

/// A unique identifier for an inference variable.
pub struct InferenceVariable<T> {
    id: usize,
    _phantom: std::marker::PhantomData<Box<T>>,
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
    #[must_use]
    fn combine(&self, another: &Self) -> Option<Self>;
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
#[derive(Debug, Clone, PartialEq, Eq)]
struct ContextImpl<T: Term, C: 'static> {
    inference_by_ids: HashMap<T::InferenceVariable, Inference<T, C>>,
    constraints: Arena<C>,
}

impl<T: Term, C: 'static> std::default::Default for ContextImpl<T, C> {
    fn default() -> Self {
        Self {
            inference_by_ids: HashMap::default(),
            constraints: Arena::default(),
        }
    }
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
#[allow(missing_docs)]
pub struct UnsatisfiedConstraintError<T, C> {
    /// The term that failed to satisfy the constraint.
    pub term: T,

    /// The constraint that the term failed to satisfy.
    pub constraint: C,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum UnifyConstraintError<T: Term, C> {
    #[error(transparent)]
    UnsatisfiedConstraint(#[from] UnsatisfiedConstraintError<T, C>),

    #[error(transparent)]
    CombineConstraint(#[from] CombineConstraintError<C>),
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum AssignKnownValueError<T, C> {
    #[error(transparent)]
    UnsatisfiedConstraintError(#[from] UnsatisfiedConstraintError<T, C>),
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
            .expect("invalid inference variable");

        match inference {
            Inference::Known(inferred) => {
                if !constraint.satisfies(inferred) {
                    return Err(UnifyConstraintError::UnsatisfiedConstraint(
                        UnsatisfiedConstraintError {
                            term: inferred.clone(),
                            constraint: constraint.clone(),
                        },
                    ));
                }
            }

            Inference::Inferring(constraint_id) => {
                let current_constraint =
                    self.constraints.get_mut(*constraint_id).unwrap();

                *current_constraint = current_constraint
                    .combine(constraint)
                    .ok_or(UnifyConstraintError::CombineConstraint(
                        CombineConstraintError::<C> {
                            lhs: current_constraint.clone(),
                            rhs: constraint.clone(),
                        },
                    ))?;
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
        let constraint =
            self.constraints.get(constraint_id).expect("invalid constraint ID");

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

impl<M: model::Model> Constraint<Type<M>> for r#type::Constraint {
    fn satisfies(&self, term: &Type<M>) -> bool {
        use crate::type_system::term::r#type::Primitive;

        match self {
            Self::All(_) => true,
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
                        | Primitive::Usize
                        | Primitive::Isize
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
                        | Primitive::Isize
                )
            ),
            Self::UnsignedInteger => matches!(
                term,
                Type::Primitive(
                    Primitive::Uint8
                        | Primitive::Uint16
                        | Primitive::Uint32
                        | Primitive::Uint64
                        | Primitive::Usize
                )
            ),
            Self::Floating => {
                matches!(
                    term,
                    Type::Primitive(Primitive::Float32 | Primitive::Float64)
                )
            }
            Self::Integer => matches!(
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
                        | Primitive::Usize
                        | Primitive::Isize
                )
            ),
            Self::SignedInteger => matches!(
                term,
                Type::Primitive(
                    Primitive::Int8
                        | Primitive::Int16
                        | Primitive::Int32
                        | Primitive::Int64
                        | Primitive::Isize
                )
            ),
        }
    }

    fn combine(&self, another: &Self) -> Option<Self> {
        match self {
            Self::All(default_as_unit) => Some(match *another {
                Self::All(other_as_unit) => {
                    Self::All(*default_as_unit || other_as_unit)
                }

                another => another,
            }),

            Self::Number => Some(match *another {
                Self::All(_) => Self::Number,
                another => another,
            }),

            Self::Integer => match *another {
                Self::All(_) | Self::Number => Some(Self::Integer),

                Self::Signed => Some(Self::SignedInteger),

                another @ (Self::Integer
                | Self::SignedInteger
                | Self::UnsignedInteger) => Some(another),

                Self::Floating => None,
            },

            Self::UnsignedInteger => match *another {
                Self::All(_)
                | Self::Number
                | Self::Integer
                | Self::UnsignedInteger => Some(Self::UnsignedInteger),

                Self::SignedInteger | Self::Signed | Self::Floating => None,
            },

            Self::SignedInteger => match *another {
                Self::All(_)
                | Self::Number
                | Self::Integer
                | Self::SignedInteger
                | Self::Signed => Some(Self::SignedInteger),

                Self::UnsignedInteger | Self::Floating => None,
            },

            Self::Signed => match *another {
                Self::Signed | Self::All(_) | Self::Number => {
                    Some(Self::Signed)
                }

                Self::SignedInteger | Self::Integer => {
                    Some(Self::SignedInteger)
                }

                Self::Floating => Some(Self::Floating),

                Self::UnsignedInteger => None,
            },

            Self::Floating => match *another {
                Self::All(_) | Self::Number | Self::Signed | Self::Floating => {
                    Some(Self::Floating)
                }

                Self::UnsignedInteger | Self::Integer | Self::SignedInteger => {
                    None
                }
            },
        }
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("the two constraints cannot be combined")]
#[allow(missing_docs)]
pub struct CombineConstraintError<C> {
    /// The left-hand side constraint.
    pub lhs: C,

    /// The right-hand side constraint.
    pub rhs: C,
}

impl<T> Constraint<T> for NoConstraint {
    fn satisfies(&self, _: &T) -> bool { true }

    fn combine(&self, _: &Self) -> Option<Self> { Some(Self) }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("found an inference variable that exists both in the lhs and rhs")]
#[allow(missing_docs)]
pub struct CyclicInferenceError<T>(InferenceVariable<T>);

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum UnifyError {
    #[error(
        "cannot unify the term due to the undecidability (exceeding the \
         computation limit)"
    )]
    ExceedLimitError(#[from] OverflowError),

    #[error("the two types cannot be unified or are mismatched")]
    IncompatibleTypes { lhs: Type<Model>, rhs: Type<Model> },

    #[error("the two constants cannot be unified or are mismatched")]
    IncompatibleConstants { lhs: Constant<Model>, rhs: Constant<Model> },

    #[error(transparent)]
    UnsatisfiedConstraint(
        UnsatisfiedConstraintError<Type<Model>, r#type::Constraint>,
    ),

    #[error(transparent)]
    CombineConstraint(#[from] CombineConstraintError<r#type::Constraint>),

    #[error(transparent)]
    CyclicConstantInference(#[from] CyclicInferenceError<Constant<Model>>),

    #[error(transparent)]
    CyclicTypeInference(#[from] CyclicInferenceError<Type<Model>>),
}

/// The inference context storing the inference variables and constraints.
#[derive(Debug, Clone, PartialEq, Eq, Getters, MutGetters, Default)]
pub struct Context {
    type_inference_context: ContextImpl<Type<Model>, r#type::Constraint>,
    constant_inference_context: ContextImpl<Constant<Model>, NoConstraint>,
}

/// Implemented by types and constants, which can be inferree.
pub trait Inferable: Term<InferenceVariable = InferenceVariable<Self>> {
    /// The type of the constraint for the inference.
    type Constraint: Constraint<Self> + 'static;
}

impl Inferable for Type<Model> {
    type Constraint = r#type::Constraint;
}

impl Inferable for Constant<Model> {
    type Constraint = NoConstraint;
}

trait InferableSealed: Inferable {
    fn get_context(infer: &Context) -> &ContextImpl<Self, Self::Constraint>;
    fn get_context_mut(
        infer: &mut Context,
    ) -> &mut ContextImpl<Self, Self::Constraint>;
}

impl InferableSealed for Type<Model> {
    fn get_context(infer: &Context) -> &ContextImpl<Self, Self::Constraint> {
        &infer.type_inference_context
    }

    fn get_context_mut(
        infer: &mut Context,
    ) -> &mut ContextImpl<Self, Self::Constraint> {
        &mut infer.type_inference_context
    }
}

impl InferableSealed for Constant<Model> {
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
    #[allow(private_bounds)]
    pub fn register<T: InferableSealed>(
        &mut self,
        inference_variable: InferenceVariable<T>,
        constraint: T::Constraint,
    ) -> bool {
        T::get_context_mut(self).register(inference_variable, constraint)
    }

    /// Gets the [`Inference`] for an inference variable.
    #[must_use]
    #[allow(private_bounds)]
    pub fn get_inference<T: InferableSealed>(
        &self,
        inference_variable: InferenceVariable<T>,
    ) -> Option<&Inference<T, T::Constraint>> {
        T::get_context(self).get_inference(inference_variable)
    }

    /// Gets the constraint for a constraint ID.
    #[must_use]
    #[allow(private_bounds)]
    pub fn get_constraint<T: InferableSealed>(
        &self,
        constraint_id: ID<T::Constraint>,
    ) -> Option<&T::Constraint> {
        T::get_context(self).get_constraint(constraint_id)
    }

    /// Unifies the given inference variable with a constraint.
    ///
    /// # Errors
    ///
    /// See [`UnifyConstraintError`] for more information.
    #[allow(private_bounds)]
    pub fn unify_with_constraint<T: InferableSealed>(
        &mut self,
        inference_variable: InferenceVariable<T>,
        constraint: &T::Constraint,
    ) -> Result<(), UnifyConstraintError<T, T::Constraint>> {
        T::get_context_mut(self)
            .unify_with_constraint(inference_variable, constraint)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct UnificationConfig;

impl unification::Predicate<Lifetime<Model>> for UnificationConfig {
    fn unifiable(
        &self,
        _: &Lifetime<Model>,
        _: &Lifetime<Model>,
        _: &[Log<Model>],
        _: &[Log<Model>],
    ) -> Result<Output<Satisfied, Model>, OverflowError> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Type<Model>> for UnificationConfig {
    fn unifiable(
        &self,
        from: &Type<Model>,
        to: &Type<Model>,
        _: &[Log<Model>],
        _: &[Log<Model>],
    ) -> Result<Output<Satisfied, Model>, OverflowError> {
        Ok((from.is_inference() || to.is_inference())
            .then_some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Constant<Model>> for UnificationConfig {
    fn unifiable(
        &self,
        from: &Constant<Model>,
        to: &Constant<Model>,
        _: &[Log<Model>],
        _: &[Log<Model>],
    ) -> Result<Output<Satisfied, Model>, OverflowError> {
        Ok((from.is_inference() || to.is_inference())
            .then_some(Succeeded::satisfied()))
    }
}

impl<S: table::State> Normalizer<Model, S> for Context {
    fn normalize_type(
        ty: &Type<Model>,
        environment: &Environment<Model, S, Self, impl Observer<Model, S>>,
        _: &mut query::Context<Model>,
    ) -> Result<Output<Type<Model>, Model>, OverflowError> {
        let Type::Inference(ty) = ty else {
            return Ok(None);
        };

        Ok(
            if let Some(Inference::Known(normalized)) = environment
                .normalizer()
                .type_inference_context
                .get_inference(*ty)
            {
                Some(Succeeded::new(normalized.clone()))
            } else {
                None
            },
        )
    }

    fn normalize_constant(
        constant: &Constant<Model>,
        environment: &Environment<Model, S, Self, impl Observer<Model, S>>,
        _: &mut query::Context<Model>,
    ) -> Result<Output<Constant<Model>, Model>, OverflowError> {
        let Constant::Inference(constant) = constant else {
            return Ok(None);
        };

        Ok(
            if let Some(Inference::Known(normalized)) = environment
                .normalizer()
                .constant_inference_context
                .get_inference(*constant)
            {
                Some(Succeeded::new(normalized.clone()))
            } else {
                None
            },
        )
    }
}

impl Context {
    fn handle_known_and_infer<
        T: Term<Model = Model, InferenceVariable = InferenceVariable<T>>,
        C: 'static + Constraint<T>,
    >(
        &mut self,
        inferring: ID<C>,
        known: &T,
        context: &impl Fn(&mut Self) -> &mut ContextImpl<T, C>,
        into_unify_error: &impl Fn(UnsatisfiedConstraintError<T, C>) -> UnifyError,
    ) -> Result<(), UnifyError>
    where
        UnifyError: From<CyclicInferenceError<T>>,
    {
        // check if there's a constraint with the given ID on the known
        for (term, _) in RecursiveIterator::new(known) {
            let Ok(Some(inference_variable)) =
                T::try_from_kind(term).map(|x| x.as_inference())
            else {
                continue;
            };

            let Inference::Inferring(inference) = context(self)
                .get_inference(*inference_variable)
                .expect("invalid inference variable")
                .clone()
            else {
                continue;
            };

            if inference == inferring {
                return Err(CyclicInferenceError(*inference_variable).into());
            }
        }

        match context(self).assign_infer_to_known(inferring, known.clone()) {
            Ok(()) => Ok(()),

            Err(AssignKnownValueError::UnsatisfiedConstraintError(error)) => {
                Err(into_unify_error(error))
            }
        }
    }

    #[allow(clippy::too_many_lines, clippy::too_many_arguments)]
    fn handle_mapping<
        T: Term<Model = Model, InferenceVariable = InferenceVariable<T>>,
        S: table::State,
        C: 'static + Constraint<T>,
        O: Observer<Model, S>,
    >(
        &mut self,
        premise: &Premise<Model>,
        table: &Table<S>,
        observer: &O,
        mapping: &BTreeMap<T, BTreeSet<T>>,
        inference_context: &impl Fn(&mut Self) -> &mut ContextImpl<T, C>,
        unify: &impl Fn(
            &mut Self,
            &T,
            &T,
            &Premise<Model>,
            &Table<S>,
            &O,
        ) -> Result<(), UnifyError>,
        constraint_error: &impl Fn(UnsatisfiedConstraintError<T, C>) -> UnifyError,
        combine_constraint_error: &impl Fn(CombineConstraintError<C>) -> UnifyError,
    ) -> Result<(), UnifyError>
    where
        UnifyError: From<CyclicInferenceError<T>>,
    {
        for (lhs, rhs) in mapping
            .iter()
            .flat_map(|(lhs, rhs)| std::iter::once(lhs).zip(rhs.iter()))
        {
            match (lhs.as_inference().copied(), rhs.as_inference().copied()) {
                (Some(lhs), Some(rhs)) => {
                    let lhs_inference = inference_context(self)
                        .get_inference(lhs)
                        .expect("invalid inference variable")
                        .clone();

                    let rhs_inference = inference_context(self)
                        .get_inference(rhs)
                        .expect("invalid inference variable")
                        .clone();

                    match (lhs_inference, rhs_inference) {
                        // recursively unify the two inferences
                        (
                            Inference::Known(known_lhs),
                            Inference::Known(known_rhs),
                        ) => {
                            unify(
                                self, &known_lhs, &known_rhs, premise, table,
                                observer,
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
                            if lhs_inferring_id == rhs_inferring_id {
                                continue;
                            }

                            // will make rhs use lhs's constraint id
                            let rhs_constraint = inference_context(self)
                                .constraints
                                .remove(rhs_inferring_id)
                                .unwrap();

                            let lhs_constraint = inference_context(self)
                                .constraints
                                .get_mut(lhs_inferring_id)
                                .unwrap();

                            *lhs_constraint = if let Some(new_constraint) =
                                lhs_constraint.combine(&rhs_constraint)
                            {
                                new_constraint
                            } else {
                                let lhs_constraint = lhs_constraint.clone();

                                // restore the rhs's constraint
                                // back to the context
                                assert!(inference_context(self)
                                    .constraints
                                    .insert_with_id(
                                        rhs_inferring_id,
                                        rhs_constraint.clone(),
                                    )
                                    .is_ok());

                                return Err(combine_constraint_error(
                                    CombineConstraintError {
                                        lhs: lhs_constraint,
                                        rhs: rhs_constraint,
                                    },
                                ));
                            };

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
                        .expect("invalid inference variable")
                    {
                        Inference::Known(known) => {
                            unify(
                                self,
                                &known,
                                another_known,
                                premise,
                                table,
                                observer,
                            )?;
                        }
                        Inference::Inferring(inferring) => {
                            self.handle_known_and_infer(
                                inferring,
                                another_known,
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
    ) -> &mut ContextImpl<Type<Model>, r#type::Constraint> {
        &mut self.type_inference_context
    }

    fn constant_inference_context_mut(
        &mut self,
    ) -> &mut ContextImpl<Constant<Model>, NoConstraint> {
        &mut self.constant_inference_context
    }

    fn handle_unifer<
        T: Term<Model = Model, InferenceVariable = InferenceVariable<T>>,
        S: table::State,
        O: Observer<Model, S>,
    >(
        &mut self,
        unifier: unification::Unifier<T>,
        premise: &Premise<Model>,
        table: &Table<S>,
        observer: &O,
    ) -> Result<(), UnifyError> {
        // turns the unification into mapping pairs
        let mapping: Mapping<Model> = Mapping::from_unifier(unifier);

        self.handle_mapping(
            premise,
            table,
            observer,
            &mapping.types,
            &Self::type_inference_context_mut,
            &Self::unify_type,
            &UnifyError::UnsatisfiedConstraint,
            &UnifyError::CombineConstraint,
        )?;

        self.handle_mapping(
            premise,
            table,
            observer,
            &mapping.constants,
            &Self::constant_inference_context_mut,
            &Self::unify_constant,
            &(|_| unreachable!("constant can always be unified")),
            &(|_| unreachable!("constant has no constraint to combine")),
        )?;

        Ok(())
    }

    /// Unifies the two constants and updates the inference context.
    ///
    /// # Errors
    ///
    /// See [`UnifyError`] for the possible errors.
    pub fn unify_constant<S: table::State, O: Observer<Model, S>>(
        &mut self,
        lhs: &Constant<Model>,
        rhs: &Constant<Model>,
        premise: &Premise<Model>,
        table: &Table<S>,
        observer: &O,
    ) -> Result<(), UnifyError> {
        let (environment, _) =
            Environment::new_with(premise.clone(), table, self, observer);

        // obtains the unification result
        let Some(Succeeded { result: unifier, .. }) = Unification::new(
            lhs.clone(),
            rhs.clone(),
            Arc::new(UnificationConfig),
        )
        .query(&environment)?
        else {
            return Err(UnifyError::IncompatibleConstants {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            });
        };

        self.handle_unifer(unifier, premise, table, observer)
    }

    /// Unifies the two types and updates the inference context.
    ///
    /// # Errors
    ///
    /// See [`UnifyError`] for the possible errors.
    pub fn unify_type<S: table::State, O: Observer<Model, S>>(
        &mut self,
        lhs: &Type<Model>,
        rhs: &Type<Model>,
        premise: &Premise<Model>,
        table: &Table<S>,
        observer: &O,
    ) -> Result<(), UnifyError> {
        let (environment, _) =
            Environment::new_with(premise.clone(), table, self, observer);

        // obtains the unification result
        let Some(Succeeded { result: unifier, .. }) = Unification::new(
            lhs.clone(),
            rhs.clone(),
            Arc::new(UnificationConfig),
        )
        .query(&environment)?
        else {
            return Err(UnifyError::IncompatibleTypes {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            });
        };

        self.handle_unifer(unifier, premise, table, observer)
    }
}

impl Context {
    /// Scans through every the inference variables and fills the default type
    /// for them if possible (e.g., {number} -> int32)
    pub fn fill_default_inferences(&mut self) {
        let inference_variables = self
            .type_inference_context
            .inference_by_ids
            .keys()
            .copied()
            .collect::<Vec<_>>();

        for inference_variable in inference_variables {
            let Inference::Inferring(inference) = self
                .type_inference_context
                .get_inference(inference_variable)
                .unwrap()
            else {
                continue;
            };

            let default_type = match *self
                .type_inference_context
                .get_constraint(*inference)
                .unwrap()
            {
                r#type::Constraint::All(can_default) => {
                    if !can_default {
                        continue;
                    }

                    Type::Tuple(Tuple { elements: Vec::new() })
                }

                r#type::Constraint::Signed
                | r#type::Constraint::Integer
                | r#type::Constraint::SignedInteger
                | r#type::Constraint::Number => {
                    Type::Primitive(r#type::Primitive::Int32)
                }

                r#type::Constraint::UnsignedInteger => {
                    Type::Primitive(r#type::Primitive::Uint32)
                }

                r#type::Constraint::Floating => {
                    Type::Primitive(r#type::Primitive::Float32)
                }
            };

            self.type_inference_context
                .assign_infer_to_known(*inference, default_type)
                .unwrap();
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum IntoConstraintModelError {
    #[error("the inference variable is not registered in the context")]
    UnregisteredTypeInferenceVariable(InferenceVariable<Type<Model>>),

    #[error("the inference variable is not registered in the context")]
    UnregisteredConstantInferenceVariable(InferenceVariable<Constant<Model>>),

    #[error(transparent)]
    Overflow(#[from] OverflowError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum InferenceOrConstraint<ID, C> {
    InferenceID(ID),
    Constraint(C),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct IntermediaryModel;

impl<ID, C> From<Never> for InferenceOrConstraint<ID, C> {
    fn from(value: Never) -> Self { match value {} }
}

impl From<InferenceVariable<Lifetime<Model>>> for NoConstraint {
    fn from(_: InferenceVariable<Lifetime<Model>>) -> Self { Self }
}

impl From<InferenceVariable<Type<Model>>>
    for InferenceOrConstraint<
        InferenceVariable<Type<Model>>,
        r#type::Constraint,
    >
{
    fn from(value: InferenceVariable<Type<Model>>) -> Self {
        Self::InferenceID(value)
    }
}

impl From<InferenceVariable<Constant<Model>>>
    for InferenceOrConstraint<InferenceVariable<Constant<Model>>, NoConstraint>
{
    fn from(value: InferenceVariable<Constant<Model>>) -> Self {
        Self::InferenceID(value)
    }
}

impl model::Model for IntermediaryModel {
    type LifetimeInference = Erased;
    type TypeInference = InferenceOrConstraint<
        InferenceVariable<Type<Model>>,
        r#type::Constraint,
    >;
    type ConstantInference =
        InferenceOrConstraint<InferenceVariable<Constant<Model>>, NoConstraint>;

    fn from_default_type(ty: Type<model::Default>) -> Type<Self> {
        Type::from_other_model(ty)
    }

    fn from_default_lifetime(
        lifetime: Lifetime<model::Default>,
    ) -> Lifetime<Self> {
        Lifetime::from_other_model(lifetime)
    }

    fn from_default_constant(
        constant: Constant<model::Default>,
    ) -> Constant<Self> {
        Constant::from_other_model(constant)
    }
}

struct ConstraintNormalizer<'a> {
    context: &'a Context,
}

impl<'a, S: table::State> Normalizer<IntermediaryModel, S>
    for ConstraintNormalizer<'a>
{
    fn normalize_type(
        ty: &Type<IntermediaryModel>,
        environment: &Environment<
            IntermediaryModel,
            S,
            Self,
            impl Observer<IntermediaryModel, S>,
        >,
        _: &mut query::Context<IntermediaryModel>,
    ) -> Result<Output<Type<IntermediaryModel>, IntermediaryModel>, OverflowError>
    {
        let Type::Inference(ty) = ty else {
            return Ok(None);
        };

        match ty {
            InferenceOrConstraint::InferenceID(inference_id) => environment
                .normalizer()
                .context
                .get_inference(*inference_id)
                .map_or_else(
                    || Ok(Some(Succeeded::new(Type::Error(term::Error)))),
                    |x| match x {
                        Inference::Known(known) => Ok(Some(Succeeded::new(
                            Type::from_other_model(known.clone()),
                        ))),
                        Inference::Inferring(constraint_id) => {
                            Ok(Some(Succeeded::new(Type::Inference(
                                InferenceOrConstraint::Constraint(
                                    *environment
                                        .normalizer()
                                        .context
                                        .get_constraint::<Type<_>>(
                                            *constraint_id,
                                        )
                                        .unwrap(),
                                ),
                            ))))
                        }
                    },
                ),
            InferenceOrConstraint::Constraint(_) => Ok(None), /* no need to
                                                               * normalize */
        }
    }

    fn normalize_constant(
        constant: &Constant<IntermediaryModel>,
        environment: &Environment<
            IntermediaryModel,
            S,
            Self,
            impl Observer<IntermediaryModel, S>,
        >,
        _: &mut query::Context<IntermediaryModel>,
    ) -> Result<
        Output<Constant<IntermediaryModel>, IntermediaryModel>,
        OverflowError,
    > {
        let Constant::Inference(constant) = constant else {
            return Ok(None);
        };

        match constant {
            InferenceOrConstraint::InferenceID(inference_id) => environment
                .normalizer()
                .context
                .get_inference(*inference_id)
                .map_or_else(
                    || Ok(Some(Succeeded::new(Constant::Error(term::Error)))),
                    |x| match x {
                        Inference::Known(known) => Ok(Some(Succeeded::new(
                            Constant::from_other_model(known.clone()),
                        ))),
                        Inference::Inferring(constraint_id) => {
                            Ok(Some(Succeeded::new(Constant::Inference(
                                InferenceOrConstraint::Constraint(
                                    *environment
                                        .normalizer()
                                        .context
                                        .get_constraint::<Constant<_>>(
                                            *constraint_id,
                                        )
                                        .unwrap(),
                                ),
                            ))))
                        }
                    },
                ),
            InferenceOrConstraint::Constraint(_) => Ok(None), /* no need to
                                                               * normalize */
        }
    }
}

impl TryFrom<Erased> for NoConstraint {
    type Error = IntoConstraintModelError;

    fn try_from(_: Erased) -> Result<Self, Self::Error> { Ok(Self) }
}

impl TryFrom<InferenceOrConstraint<InferenceVariable<Type<Model>>, Self>>
    for r#type::Constraint
{
    type Error = IntoConstraintModelError;

    fn try_from(
        value: InferenceOrConstraint<InferenceVariable<Type<Model>>, Self>,
    ) -> Result<Self, Self::Error> {
        match value {
            InferenceOrConstraint::InferenceID(inference_id) => Err(
                IntoConstraintModelError::UnregisteredTypeInferenceVariable(
                    inference_id,
                ),
            ),
            InferenceOrConstraint::Constraint(constraint) => Ok(constraint),
        }
    }
}

impl TryFrom<InferenceOrConstraint<InferenceVariable<Constant<Model>>, Self>>
    for NoConstraint
{
    type Error = IntoConstraintModelError;

    fn try_from(
        value: InferenceOrConstraint<InferenceVariable<Constant<Model>>, Self>,
    ) -> Result<Self, Self::Error> {
        match value {
            InferenceOrConstraint::InferenceID(inference_id) => Err(
                IntoConstraintModelError::UnregisteredConstantInferenceVariable(
                    inference_id,
                ),
            ),
            InferenceOrConstraint::Constraint(constraint) => Ok(constraint),
        }
    }
}

impl Context {
    /// Converts the type with [`Model`] into the type with the
    /// [`ConstraintModel`] model.
    ///
    /// All type inference variables will be replaced with the constraints they
    /// currently infer.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    pub fn transform_type_into_constraint_model(
        &self,
        ty: Type<Model>,
    ) -> Result<Type<ConstraintModel>, OverflowError> {
        let mut intermediary_type =
            Type::<IntermediaryModel>::from_other_model(ty);

        lazy_static! {
            static ref DUMMY_TABLE: Table<Building> = Table::default();
            static ref DUMMY_PREMISE: Premise<IntermediaryModel> =
                Premise::default();
        };

        // normalze all the inference variables
        let constraint_normalizer = ConstraintNormalizer { context: self };

        let (environment, _) = Environment::new_with(
            DUMMY_PREMISE.clone(),
            &DUMMY_TABLE,
            &constraint_normalizer,
            observer::NO_OP,
        );

        intermediary_type = simplify(&intermediary_type, &environment)?.result;

        Ok(Type::try_from_other_model(intermediary_type).unwrap())
    }

    /// Converts the constant with [`Model`] into the constant with the
    /// [`ConstraintModel`] model.
    ///
    /// All type inference variables will be replaced with the constraints they
    /// currently infer.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    pub fn transform_constant_into_constraint_model(
        &self,
        constant: Constant<Model>,
    ) -> Result<Constant<ConstraintModel>, OverflowError> {
        let mut intermediary_constant =
            Constant::<IntermediaryModel>::from_other_model(constant);

        lazy_static! {
            static ref DUMMY_TABLE: Table<Building> = Table::default();
            static ref DUMMY_PREMISE: Premise<IntermediaryModel> =
                Premise::default();
        };

        // normalze all the inference variables
        let constraint_normalizer = ConstraintNormalizer { context: self };

        let (environment, _) = Environment::new_with(
            DUMMY_PREMISE.clone(),
            &DUMMY_TABLE,
            &constraint_normalizer,
            observer::NO_OP,
        );

        intermediary_constant =
            simplify(&intermediary_constant, &environment)?.result;

        Ok(Constant::try_from_other_model(intermediary_constant).unwrap())
    }
}

#[cfg(test)]
mod tests;
