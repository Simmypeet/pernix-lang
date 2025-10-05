//! Defines the inference context that manages type and constant
//! inference.

use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    marker::PhantomData,
};

use getset::Getters;
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_resolution::{
    qualified_identifier::{resolve_qualified_identifier, Resolution},
    term::{
        resolve_generic_arguments, resolve_type, verify_generic_arguments_for,
    },
    Config, ElidedTermProvider,
};
use pernixc_source_file::SourceElement;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    inference,
    lifetime::Lifetime,
    r#type::{Primitive, Type},
    visitor::RecursiveIterator,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    mapping::Mapping,
    normalizer::Normalizer,
    unification, Satisfied, Succeeded, UnrecoverableError,
};

use crate::{
    binder::{
        self, inference_context::sealed::Sealed, Binder, BindingError, Error,
    },
    diagnostic::Diagnostic,
    infer::{
        constraint,
        table::{
            self, AssignConstraintError, CombineConstraintError, Inference,
            UnsatisfiedConstraintError,
        },
    },
};

/// A simple counter that generates unique inference variables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct InferenceCounter<T>(u64, PhantomData<T>);

impl<T> InferenceCounter<T> {
    /// Generates the next inference variable and increments the internal
    /// counter.
    pub const fn next(&mut self) -> inference::Variable<T> {
        let var = inference::Variable::new(self.0);
        self.0 += 1;
        var
    }
}

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

    type_inference_counter: InferenceCounter<Type>,
    constant_inference_counter: InferenceCounter<Constant>,
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
    use pernixc_term::inference;
    use pernixc_type_system::term::Term;

    use crate::{
        binder::inference_context::{
            CyclicInferenceError, InferenceContext, UnifyError,
        },
        infer::{
            constraint::Constraint,
            table::{self, CombineConstraintError, UnsatisfiedConstraintError},
        },
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

    /// Creates a new type inference variable that's unique in this binder
    /// context.
    pub const fn next_type_inference_variable(
        &mut self,
    ) -> inference::Variable<Type> {
        self.type_inference_counter.next()
    }

    /// Creates a new constant inference variable that's unique in this binder
    /// context.
    pub const fn next_constant_inference_variable(
        &mut self,
    ) -> inference::Variable<Constant> {
        self.constant_inference_counter.next()
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

    /// Scans through every the inference variables and fills the default
    /// type for them if possible (e.g., {number} -> int32)
    pub fn fill_default_inferences(&mut self) {
        let inference_variables = self
            .type_table
            .inference_by_ids()
            .keys()
            .copied()
            .collect::<Vec<_>>();

        for inference_variable in inference_variables {
            let Inference::Inferring(inference) =
                self.type_table.get_inference(inference_variable).unwrap()
            else {
                continue;
            };

            let default_type =
                match *self.type_table.get_constraint(*inference).unwrap() {
                    constraint::Type::All(can_default) => {
                        if !can_default {
                            continue;
                        }

                        Type::Tuple(pernixc_term::tuple::Tuple {
                            elements: Vec::new(),
                        })
                    }

                    constraint::Type::Signed
                    | constraint::Type::Integer
                    | constraint::Type::SignedInteger
                    | constraint::Type::Number => {
                        Type::Primitive(Primitive::Int32)
                    }

                    constraint::Type::UnsignedInteger => {
                        Type::Primitive(Primitive::Uint32)
                    }

                    constraint::Type::Floating => {
                        Type::Primitive(Primitive::Float32)
                    }
                };

            self.type_table.assign_known(*inference, default_type).unwrap();
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct InferenceProvider<'x, T> {
    created_inferences: Vec<inference::Variable<T>>,
    inference_counter: &'x mut InferenceCounter<T>,
}

impl<T: From<inference::Variable<T>> + Send> ElidedTermProvider<T>
    for InferenceProvider<'_, T>
{
    fn create(&mut self) -> T {
        let inference = self.inference_counter.next();
        let inference_term = T::from(inference);

        self.created_inferences.push(inference);

        inference_term
    }
}

/// A provider that creates a [`Lifetime::Erased`] for every elided lifetime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ErasedLifetimeProvider;

impl ElidedTermProvider<Lifetime> for ErasedLifetimeProvider {
    fn create(&mut self) -> Lifetime { Lifetime::Erased }
}

impl Binder<'_> {
    /// Resolves a qualified identifier with possible type and constant
    /// inferences.
    pub async fn resolve_qualified_identifier_with_inference(
        &mut self,
        syntax_tree: &pernixc_syntax::QualifiedIdentifier,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Resolution, Error> {
        let extra_namespace = &self.environment.extra_namespace;
        let current_site = self.current_site();

        let mut lifetime_inference_providers = ErasedLifetimeProvider;

        let mut type_inferences = InferenceProvider {
            created_inferences: Vec::new(),
            inference_counter: &mut self
                .inference_context
                .type_inference_counter,
        };
        let mut constant_inferences = InferenceProvider {
            created_inferences: Vec::new(),
            inference_counter: &mut self
                .inference_context
                .constant_inference_counter,
        };

        let config = Config::builder()
            .extra_namespace(extra_namespace)
            .elided_lifetime_provider(&mut lifetime_inference_providers)
            .elided_type_provider(&mut type_inferences)
            .elided_constant_provider(&mut constant_inferences)
            .referring_site(current_site)
            .build();

        let resolution = self
            .engine
            .resolve_qualified_identifier(syntax_tree, config, &handler)
            .await;

        let resolution = match resolution {
            Ok(result) => result,
            Err(pernixc_resolution::Error::Cyclic(error)) => {
                return Err(binder::Error::Unrecoverable(
                    UnrecoverableError::CyclicDependency(error),
                ))
            }
            Err(pernixc_resolution::Error::Abort) => {
                return Err(binder::Error::Binding(BindingError(
                    syntax_tree.span(),
                )))
            }
        };

        let created_type_inferences = type_inferences.created_inferences;
        let created_constant_inferences =
            constant_inferences.created_inferences;

        for inference in created_type_inferences {
            assert!(self
                .inference_context
                .register(inference, constraint::Type::All(false)));
        }

        for inference in created_constant_inferences {
            assert!(self
                .inference_context
                .register(inference, constraint::Constant));
        }

        Ok(resolution)
    }

    /// Resolves the given `syntax_tree` to a type where inference is allowed.
    pub async fn resolve_type_with_inference(
        &mut self,
        syntax_tree: &pernixc_syntax::r#type::Type,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Type, UnrecoverableError> {
        let extra_namespace = &self.environment.extra_namespace;
        let current_site = self.current_site();

        let mut type_inferences = InferenceProvider {
            created_inferences: Vec::new(),
            inference_counter: &mut self
                .inference_context
                .type_inference_counter,
        };
        let mut constant_inferences = InferenceProvider {
            created_inferences: Vec::new(),
            inference_counter: &mut self
                .inference_context
                .constant_inference_counter,
        };

        let mut lifetime_inference_providers = ErasedLifetimeProvider;

        let ty = self
            .engine
            .resolve_type(
                syntax_tree,
                pernixc_resolution::Config::builder()
                    .extra_namespace(extra_namespace)
                    .elided_lifetime_provider(&mut lifetime_inference_providers)
                    .elided_type_provider(&mut type_inferences)
                    .elided_constant_provider(&mut constant_inferences)
                    .referring_site(current_site)
                    .build(),
                &handler,
            )
            .await;

        let resolution = match ty {
            Ok(result) => result,
            Err(err) => {
                return Err(UnrecoverableError::CyclicDependency(err));
            }
        };

        let created_type_inferences = type_inferences.created_inferences;
        let created_constant_inferences =
            constant_inferences.created_inferences;

        for inference in created_type_inferences {
            assert!(self
                .inference_context
                .register(inference, constraint::Type::All(false)));
        }

        for inference in created_constant_inferences {
            assert!(self
                .inference_context
                .register(inference, constraint::Constant));
        }

        Ok(resolution)
    }

    /// Resolves the given `generic_arguments` to a `GenericArguments` term
    /// where inference is allowed.
    pub async fn resolve_generic_arguments_with_inference(
        &mut self,
        generic_arguments: &pernixc_syntax::GenericArguments,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<GenericArguments, UnrecoverableError> {
        let extra_namespace = &self.environment.extra_namespace;
        let current_site = self.current_site();

        let mut type_inferences = InferenceProvider {
            created_inferences: Vec::new(),
            inference_counter: &mut self
                .inference_context
                .type_inference_counter,
        };

        let mut constant_inferences = InferenceProvider {
            created_inferences: Vec::new(),
            inference_counter: &mut self
                .inference_context
                .constant_inference_counter,
        };

        let mut lifetime_inference_providers = ErasedLifetimeProvider;

        let ty = self
            .engine
            .resolve_generic_arguments(
                generic_arguments,
                pernixc_resolution::Config::builder()
                    .extra_namespace(extra_namespace)
                    .elided_lifetime_provider(&mut lifetime_inference_providers)
                    .elided_type_provider(&mut type_inferences)
                    .elided_constant_provider(&mut constant_inferences)
                    .referring_site(current_site)
                    .build(),
                &handler,
            )
            .await;

        let resolution = match ty {
            Ok(result) => result,
            Err(err) => {
                return Err(UnrecoverableError::CyclicDependency(err));
            }
        };

        let created_type_inferences = type_inferences.created_inferences;
        let created_constant_inferences =
            constant_inferences.created_inferences;

        for inference in created_type_inferences {
            assert!(self
                .inference_context
                .register(inference, constraint::Type::All(false)));
        }

        for inference in created_constant_inferences {
            assert!(self
                .inference_context
                .register(inference, constraint::Constant));
        }

        Ok(resolution)
    }

    /// Verifies that the given `generic_arguments` are valid for the
    /// given `resolved_id`.
    pub async fn verify_generic_arguments_for_with_inference(
        &mut self,
        generic_arguments: GenericArguments,
        resolved_id: Global<pernixc_symbol::ID>,
        generic_identifier_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<GenericArguments, UnrecoverableError> {
        let extra_namespace = &self.environment.extra_namespace;
        let current_site = self.current_site();

        let mut type_inferences = InferenceProvider {
            created_inferences: Vec::new(),
            inference_counter: &mut self
                .inference_context
                .type_inference_counter,
        };

        let mut constant_inferences = InferenceProvider {
            created_inferences: Vec::new(),
            inference_counter: &mut self
                .inference_context
                .constant_inference_counter,
        };

        let mut lifetime_inference_providers = ErasedLifetimeProvider;

        let (arg, diags) = self
            .engine
            .verify_generic_arguments_for(
                generic_arguments,
                resolved_id,
                generic_identifier_span,
                pernixc_resolution::Config::builder()
                    .extra_namespace(extra_namespace)
                    .elided_lifetime_provider(&mut lifetime_inference_providers)
                    .elided_type_provider(&mut type_inferences)
                    .elided_constant_provider(&mut constant_inferences)
                    .referring_site(current_site)
                    .build(),
            )
            .await?;

        for diag in diags {
            (&handler).receive(diag);
        }

        let created_type_inferences = type_inferences.created_inferences;
        let created_constant_inferences =
            constant_inferences.created_inferences;

        for inference in created_type_inferences {
            assert!(self
                .inference_context
                .register(inference, constraint::Type::All(false)));
        }

        for inference in created_constant_inferences {
            assert!(self
                .inference_context
                .register(inference, constraint::Constant));
        }

        Ok(arg)
    }
}

#[cfg(test)]
mod test;
