//! Provides the type inference system.

use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
};

use getset::Getters;
use pernixc_arena::ID;
use pernixc_query::TrackedEngine;
use pernixc_term::{
    constant::Constant, inference, lifetime::Lifetime, r#type::Type,
    visitor::RecursiveIterator,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    mapping::Mapping,
    normalizer::Normalizer,
    term::Term,
    unification, Satisfied, Succeeded,
};

use crate::inference_context::{
    constraint::Constraint,
    sealed::Sealed,
    table::{
        AssignConstraintError, CombineConstraintError, Inference,
        UnsatisfiedConstraintError,
    },
};

pub mod constraint;
pub mod table;

/// The main context that manages the type and constant inference tables.
///
/// The method `unify` can be used to unify two types or constants, handling
/// the necessary inference logic.
///
/// The type itself can also be used as a [`Normalizer`] to normalize types and
/// constants by resolving inference variables to their known values when
/// possible.
#[derive(Debug, Default, Getters)]
pub struct InferenceContext {
    /// The inference table for the type term.
    #[get = "pub"]
    type_table: table::Table<constraint::Type>,

    /// The inference table for the constant term.
    #[get = "pub"]
    const_table: table::Table<constraint::Constant>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct InferenceUnificationPredicate;

impl unification::Predicate<Lifetime> for InferenceUnificationPredicate {
    fn unifiable(
        &self,
        _: &Lifetime,
        _: &Lifetime,
        _: &[unification::Log],
        _: &[unification::Log],
    ) -> Result<Option<Succeeded<Satisfied>>, pernixc_type_system::Error> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Type> for InferenceUnificationPredicate {
    fn unifiable(
        &self,
        lhs: &Type,
        rhs: &Type,
        _: &[unification::Log],
        _: &[unification::Log],
    ) -> Result<Option<Succeeded<Satisfied>>, pernixc_type_system::Error> {
        Ok((lhs.is_inference() || rhs.is_inference())
            .then(Succeeded::satisfied))
    }
}

impl unification::Predicate<Constant> for InferenceUnificationPredicate {
    fn unifiable(
        &self,
        lhs: &Constant,
        rhs: &Constant,
        _: &[unification::Log],
        _: &[unification::Log],
    ) -> Result<Option<Succeeded<Satisfied>>, pernixc_type_system::Error> {
        Ok((lhs.is_inference() || rhs.is_inference())
            .then(Succeeded::satisfied))
    }
}

impl Normalizer for InferenceContext {
    async fn normalize_type(
        ty: &Type,
        environment: &Environment<'_, Self>,
    ) -> Result<Option<Succeeded<Type>>, pernixc_type_system::Error> {
        let Type::Inference(inference) = ty else {
            return Ok(None);
        };

        let Some(inference) =
            environment.normalizer().type_table.get_inference(*inference)
        else {
            return Ok(None);
        };

        if let table::Inference::Known(ty) = inference {
            Ok(Some(Succeeded::new(ty.clone())))
        } else {
            Ok(None)
        }
    }

    async fn normalize_constant(
        constant: &Constant,
        environment: &Environment<'_, Self>,
    ) -> Result<Option<Succeeded<Constant>>, pernixc_type_system::Error> {
        let Constant::Inference(inference) = constant else {
            return Ok(None);
        };

        let Some(inference) =
            environment.normalizer().const_table.get_inference(*inference)
        else {
            return Ok(None);
        };

        if let table::Inference::Known(constant) = inference {
            Ok(Some(Succeeded::new(constant.clone())))
        } else {
            Ok(None)
        }
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("found an inference variable that exists both in the lhs and rhs")]
#[allow(missing_docs)]
pub struct CyclicInferenceError<T>(inference::Variable<T>);

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum UnifyError {
    #[error(transparent)]
    TypeSystem(#[from] pernixc_type_system::Error),

    #[error("the two types cannot be unified or are mismatched")]
    IncompatibleTypes { lhs: Type, rhs: Type },

    #[error("the two constants cannot be unified or are mismatched")]
    IncompatibleConstants { lhs: Constant, rhs: Constant },

    #[error(transparent)]
    UnsatisfiedConstraint(UnsatisfiedConstraintError<constraint::Type>),

    #[error(transparent)]
    CombineConstraint(#[from] CombineConstraintError<constraint::Type>),

    #[error(transparent)]
    CyclicConstantInference(#[from] CyclicInferenceError<Constant>),

    #[error(transparent)]
    CyclicTypeInference(#[from] CyclicInferenceError<Type>),
}

mod sealed {
    use super::{
        inference, table, CombineConstraintError, Constraint,
        CyclicInferenceError, InferenceContext, Term, UnifyError,
        UnsatisfiedConstraintError,
    };

    pub trait Sealed: Term {
        type Constraint: Constraint<Term = Self>;

        fn from_incompatible_error(lhs: &Self, rhs: &Self) -> UnifyError;

        fn from_constraint_error(
            error: UnsatisfiedConstraintError<Self::Constraint>,
        ) -> UnifyError;

        fn from_combine_constraint_error(
            error: CombineConstraintError<Self::Constraint>,
        ) -> UnifyError;

        fn from_cyclic_inference_error(
            error: CyclicInferenceError<Self>,
        ) -> UnifyError;

        fn as_inference(&self) -> Option<&inference::Variable<Self>>;

        fn inference_table(
            ctx: &InferenceContext,
        ) -> &table::Table<Self::Constraint>;

        fn inference_table_mut(
            ctx: &mut InferenceContext,
        ) -> &mut table::Table<Self::Constraint>;

        fn try_from_term_ref(
            kind: pernixc_term::TermRef<'_>,
        ) -> Option<&'_ Self>;
    }
}

/// A trait implemented internally by the types and constants, which can be
/// unified.
pub trait Unifiable: Sealed {}

impl<T> Unifiable for T where T: Sealed {}

impl sealed::Sealed for Type {
    type Constraint = constraint::Type;

    fn from_incompatible_error(lhs: &Self, rhs: &Self) -> UnifyError {
        UnifyError::IncompatibleTypes { lhs: lhs.clone(), rhs: rhs.clone() }
    }

    fn from_constraint_error(
        error: UnsatisfiedConstraintError<Self::Constraint>,
    ) -> UnifyError {
        UnifyError::UnsatisfiedConstraint(error)
    }

    fn from_combine_constraint_error(
        error: CombineConstraintError<Self::Constraint>,
    ) -> UnifyError {
        UnifyError::CombineConstraint(error)
    }

    fn as_inference(&self) -> Option<&inference::Variable<Self>> {
        if let Self::Inference(var) = self {
            Some(var)
        } else {
            None
        }
    }

    fn inference_table(
        ctx: &InferenceContext,
    ) -> &table::Table<Self::Constraint> {
        &ctx.type_table
    }

    fn inference_table_mut(
        ctx: &mut InferenceContext,
    ) -> &mut table::Table<Self::Constraint> {
        &mut ctx.type_table
    }

    fn try_from_term_ref(kind: pernixc_term::TermRef<'_>) -> Option<&'_ Self> {
        if let pernixc_term::TermRef::Type(ty) = kind {
            Some(ty)
        } else {
            None
        }
    }

    fn from_cyclic_inference_error(
        error: CyclicInferenceError<Self>,
    ) -> UnifyError {
        UnifyError::CyclicTypeInference(error)
    }
}

impl sealed::Sealed for Constant {
    type Constraint = constraint::Constant;

    fn from_incompatible_error(lhs: &Self, rhs: &Self) -> UnifyError {
        UnifyError::IncompatibleConstants { lhs: lhs.clone(), rhs: rhs.clone() }
    }

    fn from_constraint_error(
        _: UnsatisfiedConstraintError<Self::Constraint>,
    ) -> UnifyError {
        unreachable!("constant doesn't have constraint restriction");
    }

    fn from_combine_constraint_error(
        _: CombineConstraintError<Self::Constraint>,
    ) -> UnifyError {
        unreachable!("constant doesn't have constraint restriction");
    }

    fn as_inference(&self) -> Option<&inference::Variable<Self>> {
        if let Self::Inference(var) = self {
            Some(var)
        } else {
            None
        }
    }

    fn inference_table(
        ctx: &InferenceContext,
    ) -> &table::Table<Self::Constraint> {
        &ctx.const_table
    }

    fn inference_table_mut(
        ctx: &mut InferenceContext,
    ) -> &mut table::Table<Self::Constraint> {
        &mut ctx.const_table
    }

    fn try_from_term_ref(kind: pernixc_term::TermRef<'_>) -> Option<&'_ Self> {
        if let pernixc_term::TermRef::Constant(constant) = kind {
            Some(constant)
        } else {
            None
        }
    }

    fn from_cyclic_inference_error(
        error: CyclicInferenceError<Self>,
    ) -> UnifyError {
        UnifyError::CyclicConstantInference(error)
    }
}

/// A checkpoint for both type and constant inference tables.
#[derive(Debug)]
pub struct Checkpoint {
    type_checkpoint: table::Checkpoint,
    const_checkpoint: table::Checkpoint,
}

impl InferenceContext {
    /// Start a new checkpoint for both type and constant inference tables.
    /// The checkpoint can be replayed later via [`Self::restore`].
    #[must_use]
    pub fn start_checkpoint(&mut self) -> Checkpoint {
        Checkpoint {
            type_checkpoint: self.type_table.start_checkpoint(),
            const_checkpoint: self.const_table.start_checkpoint(),
        }
    }

    /// Commits both type and constant inference table checkpoints, making it
    /// impossible to restore to the state at the checkpoints.
    pub fn commit_checkpoint(&mut self, checkpoint: Checkpoint) {
        self.type_table.commit_checkpoint(checkpoint.type_checkpoint);
        self.const_table.commit_checkpoint(checkpoint.const_checkpoint);
    }

    /// Restores both type and constant inference tables to the state at the
    /// checkpoint.
    #[allow(clippy::needless_pass_by_value)] // intentionally pass by value
    pub fn restore(&mut self, checkpoint: Checkpoint) {
        self.type_table.restore(checkpoint.type_checkpoint);
        self.const_table.restore(checkpoint.const_checkpoint);
    }
}

impl InferenceContext {
    /// Registers a new inference variable with the given constraint.
    pub fn register<T: Unifiable>(
        &mut self,
        inference_variable: inference::Variable<T>,
        constraint: T::Constraint,
    ) -> bool {
        T::inference_table_mut(self).register(inference_variable, constraint)
    }

    /// Unifies an inference variable with a constraint, ensuring that the
    /// variable satisfies the constraint.
    pub fn unify_with_constraint<T: Unifiable>(
        &mut self,
        inference_variable: inference::Variable<T>,
        constraint: T::Constraint,
    ) -> Result<(), AssignConstraintError<T::Constraint>> {
        T::inference_table_mut(self)
            .assign_constraint(inference_variable, constraint)
    }

    /// Unifies two types/constants
    pub async fn unify<T: Unifiable>(
        &mut self,
        lhs: &T,
        rhs: &T,
        premise: &Premise,
        tracked_engine: &TrackedEngine,
    ) -> Result<(), UnifyError> {
        let environment = Environment::new(
            Cow::Borrowed(premise),
            Cow::Borrowed(tracked_engine),
            self,
        );

        let Some(result) = environment
            .query(&unification::Unification::new(
                lhs.clone(),
                rhs.clone(),
                InferenceUnificationPredicate,
            ))
            .await?
        else {
            return Err(T::from_incompatible_error(lhs, rhs));
        };

        self.handle_unifer(result.result.clone(), premise, tracked_engine).await
    }

    async fn handle_unifer<T: Unifiable>(
        &mut self,
        unifier: unification::Unifier<T>,
        premise: &Premise,
        engine: &TrackedEngine,
    ) -> Result<(), UnifyError> {
        // turns the unification into mapping pairs
        let mapping = Mapping::from_unifier(unifier);

        self.handle_mapping(premise, engine, &mapping.types).await?;
        self.handle_mapping(premise, engine, &mapping.constants).await?;

        Ok(())
    }

    #[allow(clippy::too_many_lines, clippy::too_many_arguments)]
    async fn handle_mapping<T: Unifiable>(
        &mut self,
        premise: &Premise,
        engine: &TrackedEngine,
        mapping: &BTreeMap<T, BTreeSet<T>>,
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
                    let lhs_inference = T::inference_table(self)
                        .get_inference(lhs)
                        .expect("invalid inference variable")
                        .clone();

                    let rhs_inference = T::inference_table(self)
                        .get_inference(rhs)
                        .expect("invalid inference variable")
                        .clone();

                    match (lhs_inference, rhs_inference) {
                        // recursively unify the two inferences, most likely
                        // gonna get error
                        (
                            Inference::Known(known_lhs),
                            Inference::Known(known_rhs),
                        ) => {
                            Box::pin(self.unify(
                                &known_lhs, &known_rhs, premise, engine,
                            ))
                            .await?;
                        }

                        // unify the known value with the inferring value
                        (
                            Inference::Known(known),
                            Inference::Inferring(inferring),
                        )
                        | (
                            Inference::Inferring(inferring),
                            Inference::Known(known),
                        ) => {
                            self.handle_known_and_infer(
                                inferring, &known, premise, engine,
                            )
                            .await?;
                        }

                        // merge the constraints of the two inferences and make
                        // them use the same constraint
                        // id
                        (
                            Inference::Inferring(lhs_inferring_id),
                            Inference::Inferring(rhs_inferring_id),
                        ) => T::inference_table_mut(self)
                            .unify_infers(lhs_inferring_id, rhs_inferring_id)
                            .map_err(T::from_combine_constraint_error)?,
                    }
                }

                (Some(inference), None) | (None, Some(inference)) => {
                    let another_known =
                        if lhs.as_inference().is_none() { lhs } else { rhs };

                    match T::inference_table(self)
                        .get_inference(inference)
                        .cloned()
                        .expect("invalid inference variable")
                    {
                        Inference::Known(known) => {
                            Box::pin(self.unify(
                                &known,
                                another_known,
                                premise,
                                engine,
                            ))
                            .await?;
                        }
                        Inference::Inferring(inferring) => {
                            self.handle_known_and_infer(
                                inferring,
                                another_known,
                                premise,
                                engine,
                            )
                            .await?;
                        }
                    }
                }

                (_, _) => unreachable!(),
            }
        }

        Ok(())
    }

    #[allow(clippy::result_large_err)]
    async fn handle_known_and_infer<T: Unifiable>(
        &mut self,
        inferring: ID<T::Constraint>,
        known: &T,
        premise: &Premise,
        engine: &TrackedEngine,
    ) -> Result<(), UnifyError>
    where
        UnifyError: From<CyclicInferenceError<T>>,
    {
        // check if there's a constraint with the given ID on the known
        let env = Environment::new(
            Cow::Borrowed(premise),
            Cow::Borrowed(engine),
            self,
        );
        let simplfied = env.simplify(known.clone()).await?;

        for (term, _) in RecursiveIterator::new(&simplfied.result) {
            let Some(inference_variable) =
                T::try_from_term_ref(term).and_then(|x| x.as_inference())
            else {
                continue;
            };

            let Inference::Inferring(inference) = T::inference_table(self)
                .get_inference(*inference_variable)
                .expect("invalid inference variable")
                .clone()
            else {
                continue;
            };

            dbg!(inference, inferring);
            if inference == inferring {
                return Err(T::from_cyclic_inference_error(
                    CyclicInferenceError(*inference_variable),
                ));
            }
        }

        match T::inference_table_mut(self)
            .assign_known(inferring, known.clone())
        {
            Ok(()) => Ok(()),

            Err(error) => Err(T::from_constraint_error(error)),
        }
    }
}

#[cfg(test)]
mod test;
