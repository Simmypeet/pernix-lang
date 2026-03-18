//! Defines the inference context that manages type and constant
//! inference.

use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    marker::PhantomData,
    ops::Deref,
};

use getset::Getters;
use pernixc_arena::ID;
use pernixc_handler::{Handler, Storage};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_resolution::{
    ElidedTermProvider, Resolver, qualified_identifier::Resolution,
};
use pernixc_source_file::SourceElement;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    display::{self, InferenceRenderingMap},
    generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters,
    inference,
    instance::Instance,
    instantiation::Instantiation,
    lifetime::Lifetime,
    r#type::{Primitive, Type},
    visitor::RecursiveIterator,
};
use pernixc_type_system::{
    OverflowError, Satisfied, Succeeded, UnrecoverableError,
    environment::{Environment, Premise},
    mapping::Mapping,
    normalizer::Normalizer,
    unification,
};
use qbice::{Decode, Encode, StableHash};

use crate::{
    binder::{
        self, Binder, BindingError, Error, inference_context::sealed::Sealed,
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
    type_table: table::Table<constraint::Type>,
    const_table: table::Table<constraint::Constant>,
    instance_table: table::Table<constraint::Instance>,

    type_inference_counter: InferenceCounter<Type>,
    constant_inference_counter: InferenceCounter<Constant>,
    instance_inference_counter: InferenceCounter<Instance>,
}

/// A struct that holds the rendering maps for all inference variables, which
/// can be used for diagnostics.A
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Encode, Decode)]
pub struct RenderingMap {
    types: InferenceRenderingMap<Type>,
    constants: InferenceRenderingMap<Constant>,
    instances: InferenceRenderingMap<Instance>,
}

impl RenderingMap {
    /// Creates a new rendering map from the given inference context.
    #[must_use]
    pub fn configuration(&self) -> display::Configuration<'_> {
        display::Configuration::builder()
            .type_inferences(&self.types)
            .constant_inferences(&self.constants)
            .instance_infernces(&self.instances)
            .build()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct InferenceUnificationPredicate;

impl unification::Predicate<Lifetime> for InferenceUnificationPredicate {
    fn unifiable(
        &self,
        _: &Lifetime,
        _: &Lifetime,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Type> for InferenceUnificationPredicate {
    fn unifiable(
        &self,
        lhs: &Type,
        rhs: &Type,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok((lhs.is_inference() || rhs.is_inference())
            .then(Succeeded::satisfied))
    }
}

impl unification::Predicate<Constant> for InferenceUnificationPredicate {
    fn unifiable(
        &self,
        lhs: &Constant,
        rhs: &Constant,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok((lhs.is_inference() || rhs.is_inference())
            .then(Succeeded::satisfied))
    }
}

impl unification::Predicate<Instance> for InferenceUnificationPredicate {
    fn unifiable(
        &self,
        lhs: &Instance,
        rhs: &Instance,
    ) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
        Ok((lhs.is_inference() || rhs.is_inference())
            .then(Succeeded::satisfied))
    }
}

impl Normalizer for InferenceContext {
    async fn normalize_type(
        ty: &Type,
        environment: &Environment<'_, Self>,
    ) -> Result<Option<Succeeded<Type>>, OverflowError> {
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
    ) -> Result<Option<Succeeded<Constant>>, OverflowError> {
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

    async fn normalize_instance(
        instance: &Instance,
        environment: &Environment<'_, Self>,
    ) -> Result<Option<Succeeded<Instance>>, OverflowError> {
        let Instance::Inference(inference) = instance else {
            return Ok(None);
        };

        let Some(inference) =
            environment.normalizer().instance_table.get_inference(*inference)
        else {
            return Ok(None);
        };

        if let table::Inference::Known(instance) = inference {
            Ok(Some(Succeeded::new(instance.clone())))
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
#[allow(missing_docs, clippy::large_enum_variant)]
pub enum UnifyError {
    #[error(transparent)]
    OverflowError(#[from] OverflowError),

    #[error("the two types cannot be unified or are mismatched")]
    IncompatibleTypes { lhs: Type, rhs: Type },

    #[error("the two constants cannot be unified or are mismatched")]
    IncompatibleConstants { lhs: Constant, rhs: Constant },

    #[error("the two instances cannot be unified or are mismatched")]
    IncompatibleInstances { lhs: Instance, rhs: Instance },

    #[error(transparent)]
    UnsatisfiedConstraint(UnsatisfiedConstraintError<constraint::Type>),

    #[error(transparent)]
    CombineConstraint(#[from] CombineConstraintError<constraint::Type>),

    #[error(transparent)]
    CyclicConstantInference(#[from] CyclicInferenceError<Constant>),

    #[error(transparent)]
    CyclicInstanceInference(#[from] CyclicInferenceError<Instance>),

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
        if let Self::Inference(var) = self { Some(var) } else { None }
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
        if let pernixc_term::TermRef::Type(ty) = kind { Some(ty) } else { None }
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
        if let Self::Inference(var) = self { Some(var) } else { None }
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

impl sealed::Sealed for Instance {
    type Constraint = constraint::Instance;

    fn from_incompatible_error(lhs: &Self, rhs: &Self) -> UnifyError {
        UnifyError::IncompatibleInstances { lhs: lhs.clone(), rhs: rhs.clone() }
    }

    fn from_constraint_error(
        _: UnsatisfiedConstraintError<Self::Constraint>,
    ) -> UnifyError {
        unreachable!("instance doesn't have constraint restriction");
    }

    fn from_combine_constraint_error(
        _: CombineConstraintError<Self::Constraint>,
    ) -> UnifyError {
        unreachable!("instance doesn't have constraint restriction");
    }

    fn as_inference(&self) -> Option<&inference::Variable<Self>> {
        if let Self::Inference(var) = self { Some(var) } else { None }
    }

    fn inference_table(
        ctx: &InferenceContext,
    ) -> &table::Table<Self::Constraint> {
        &ctx.instance_table
    }

    fn inference_table_mut(
        ctx: &mut InferenceContext,
    ) -> &mut table::Table<Self::Constraint> {
        &mut ctx.instance_table
    }

    fn try_from_term_ref(kind: pernixc_term::TermRef<'_>) -> Option<&'_ Self> {
        if let pernixc_term::TermRef::Instance(instance) = kind {
            Some(instance)
        } else {
            None
        }
    }

    fn from_cyclic_inference_error(
        error: CyclicInferenceError<Self>,
    ) -> UnifyError {
        UnifyError::CyclicInstanceInference(error)
    }
}

/// A checkpoint for both type and constant inference tables.
#[derive(Debug)]
pub struct Checkpoint {
    type_checkpoint: table::Checkpoint,
    const_checkpoint: table::Checkpoint,
}

impl InferenceContext {
    /// Creates the rendering map for all inference variables that can be used
    /// for diagnostics.
    #[must_use]
    pub fn get_rendering_map(&self) -> RenderingMap {
        RenderingMap {
            types: self.type_table.get_inference_rendering_map(),
            constants: self.const_table.get_inference_rendering_map(),
            instances: self.instance_table.get_inference_rendering_map(),
        }
    }

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

    /// Unifies two types/constants/instances
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
        self.handle_mapping(premise, engine, &mapping.instances).await?;

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

                        Type::unit()
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
        let mut instance_inferences = InferenceProvider {
            created_inferences: Vec::new(),
            inference_counter: &mut self
                .inference_context
                .instance_inference_counter,
        };

        let resolution_handler =
            Storage::<pernixc_resolution::diagnostic::Diagnostic>::new();

        let mut resolver = Resolver::builder()
            .tracked_engine(self.engine)
            .handler(&resolution_handler)
            .extra_namespace(extra_namespace)
            .elided_lifetime_provider(&mut lifetime_inference_providers)
            .elided_type_provider(&mut type_inferences)
            .elided_constant_provider(&mut constant_inferences)
            .elided_instance_provider(&mut instance_inferences)
            .referring_site(current_site)
            .build();

        let resolution =
            resolver.resolve_qualified_identifier(syntax_tree).await;

        resolution_handler.propagate(handler);

        let resolution = match resolution {
            Ok(result) => result,

            Err(pernixc_resolution::Error::Abort) => {
                return Err(binder::Error::Binding(BindingError(
                    syntax_tree.span(),
                )));
            }
        };

        let created_type_inferences = type_inferences.created_inferences;
        let created_constant_inferences =
            constant_inferences.created_inferences;
        let created_instance_inferences =
            instance_inferences.created_inferences;

        for inference in created_type_inferences {
            assert!(
                self.inference_context
                    .register(inference, constraint::Type::All(false))
            );
        }

        for inference in created_constant_inferences {
            assert!(
                self.inference_context
                    .register(inference, constraint::Constant)
            );
        }

        for inference in created_instance_inferences {
            assert!(
                self.inference_context
                    .register(inference, constraint::Instance)
            );
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
        let mut instance_inferences = InferenceProvider {
            created_inferences: Vec::new(),
            inference_counter: &mut self
                .inference_context
                .instance_inference_counter,
        };

        let mut lifetime_inference_providers = ErasedLifetimeProvider;

        let resolution_handler =
            Storage::<pernixc_resolution::diagnostic::Diagnostic>::new();

        let ty = pernixc_resolution::Resolver::builder()
            .tracked_engine(self.engine)
            .handler(&resolution_handler)
            .extra_namespace(extra_namespace)
            .elided_lifetime_provider(&mut lifetime_inference_providers)
            .elided_type_provider(&mut type_inferences)
            .elided_constant_provider(&mut constant_inferences)
            .elided_instance_provider(&mut instance_inferences)
            .referring_site(current_site)
            .build()
            .resolve_type(syntax_tree)
            .await;

        resolution_handler.propagate(handler);

        let created_type_inferences = type_inferences.created_inferences;
        let created_constant_inferences =
            constant_inferences.created_inferences;
        let created_instance_inferences =
            instance_inferences.created_inferences;

        for inference in created_type_inferences {
            assert!(
                self.inference_context
                    .register(inference, constraint::Type::All(false))
            );
        }

        for inference in created_constant_inferences {
            assert!(
                self.inference_context
                    .register(inference, constraint::Constant)
            );
        }

        for inference in created_instance_inferences {
            assert!(
                self.inference_context
                    .register(inference, constraint::Instance)
            );
        }

        Ok(ty)
    }

    /// Resolves the given `generic_arguments` to a `GenericArguments` term
    /// where inference is allowed.
    pub async fn resolve_generic_arguments_with_inference(
        &mut self,
        generic_arguments: &pernixc_syntax::GenericIdentifier,
        id: Global<pernixc_symbol::ID>,
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

        let mut instance_inferences = InferenceProvider {
            created_inferences: Vec::new(),
            inference_counter: &mut self
                .inference_context
                .instance_inference_counter,
        };

        let mut lifetime_inference_providers = ErasedLifetimeProvider;

        let resolution_handler =
            Storage::<pernixc_resolution::diagnostic::Diagnostic>::new();

        let ty = pernixc_resolution::Resolver::builder()
            .tracked_engine(self.engine)
            .handler(&resolution_handler)
            .extra_namespace(extra_namespace)
            .elided_lifetime_provider(&mut lifetime_inference_providers)
            .elided_type_provider(&mut type_inferences)
            .elided_constant_provider(&mut constant_inferences)
            .elided_instance_provider(&mut instance_inferences)
            .referring_site(current_site)
            .build()
            .resolve_generic_arguments_for(id, generic_arguments)
            .await;

        resolution_handler.propagate(handler);

        let created_type_inferences = type_inferences.created_inferences;
        let created_constant_inferences =
            constant_inferences.created_inferences;
        let created_instance_inferences =
            instance_inferences.created_inferences;

        for inference in created_type_inferences {
            assert!(
                self.inference_context
                    .register(inference, constraint::Type::All(false))
            );
        }

        for inference in created_constant_inferences {
            assert!(
                self.inference_context
                    .register(inference, constraint::Constant)
            );
        }

        for inference in created_instance_inferences {
            assert!(
                self.inference_context
                    .register(inference, constraint::Instance)
            );
        }

        Ok(ty)
    }
}

impl Binder<'_> {
    /// Creates a new type inference variable and assigns it to the inference
    /// context with the given constraint.
    pub fn create_type_inference(
        &mut self,
        constraint: constraint::Type,
    ) -> inference::Variable<Type> {
        let infer_var = self.inference_context.type_inference_counter.next();
        assert!(self.inference_context.register(infer_var, constraint));

        infer_var
    }

    /// Creates a new constant inference variable and assigns it to the
    /// inference context.
    pub fn create_constant_inference(
        &mut self,
    ) -> inference::Variable<Constant> {
        let infer_var =
            self.inference_context.constant_inference_counter.next();
        assert!(
            self.inference_context.register(infer_var, constraint::Constant)
        );

        infer_var
    }

    #[must_use]
    pub fn create_instance_inference(
        &mut self,
    ) -> inference::Variable<Instance> {
        let infer_var =
            self.inference_context.instance_inference_counter.next();
        assert!(
            self.inference_context.register(infer_var, constraint::Instance)
        );

        infer_var
    }
    /// Gets the inference state of a type inference variable.
    #[must_use]
    pub fn get_type_inference(
        &self,
        inference_variable: inference::Variable<Type>,
    ) -> Option<&table::Inference<constraint::Type>> {
        self.inference_context.type_table.get_inference(inference_variable)
    }

    /// Gets the constraint associated with the given type constraint ID.
    #[must_use]
    pub fn get_type_constraint(
        &self,
        id: pernixc_arena::ID<constraint::Type>,
    ) -> Option<&constraint::Type> {
        self.inference_context.type_table.get_constraint(id)
    }

    /// Gets the rendering map for all inference variables, which can be used
    /// for diagnostics.
    #[must_use]
    pub fn get_rendering_map(&self) -> RenderingMap {
        RenderingMap {
            types: self
                .inference_context
                .type_table
                .get_inference_rendering_map(),
            constants: self
                .inference_context
                .const_table
                .get_inference_rendering_map(),
            instances: self
                .inference_context
                .instance_table
                .get_inference_rendering_map(),
        }
    }
}

impl InferenceContext {
    pub async fn resolve_inferring_instance_variable(
        &mut self,
        symbol_id: Global<pernixc_symbol::ID>,
        instantiation_usage: &Instantiation,
        span: &RelativeSpan,
        engine: &TrackedEngine,
        premise: &Premise,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        let generic_parameters = engine.get_generic_parameters(symbol_id).await;

        // iterate through each instance parameter and see if there're any
        // inferring varabiles.
        for (instance_parameter_id, instance_parameter) in
            generic_parameters.instance_parameters_as_order()
        {
            let instance_term = instantiation_usage
                .get_instance_mapping(&Instance::new_parameter(
                    symbol_id,
                    instance_parameter_id,
                ))
                .unwrap();

            // if it's inference variable, then resolve it and update the
            // inference table
            let Instance::Inference(instance_inference) = instance_term else {
                // has already been resolved, no need to resolve again
                continue;
            };

            let inferring_id = match self
                .instance_table
                .get_inference(*instance_inference)
                .unwrap()
            {
                Inference::Known(_) => {
                    // has already been resolved, no need to resolve again
                    continue;
                }
                Inference::Inferring(id) => id,
            };

            let Some(mut expected_trait_ref) =
                instance_parameter.trait_ref().map(|x| x.deref().clone())
            else {
                // instance parameter is malformed, cannot resolve. assign
                // it to a dummy instance.
                self.instance_table
                    .assign_known(*inferring_id, Instance::new_error())
                    .unwrap();

                continue;
            };

            expected_trait_ref.instantiate(instantiation_usage);

            // use this expected trait ref to guide resolution.
            let env = Environment::new(
                Cow::Borrowed(premise),
                Cow::Borrowed(engine),
                self,
            );

            match env.resolve_instance(&expected_trait_ref).await.map_err(
                |x| x.report_as_type_calculating_overflow(*span, &handler),
            )? {
                Ok(resolve) => {
                    self.instance_table
                        .assign_known(*inferring_id, resolve.instance().clone())
                        .unwrap();
                }

                Err(err) => {
                    for err in
                        err.generate_diagnostics(&expected_trait_ref, *span)
                    {
                        handler.receive(err.into());
                    }

                    // failed to resolve, assign it to a dummy instance.
                    self.instance_table
                        .assign_known(*inferring_id, Instance::new_error())
                        .unwrap();
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test;
