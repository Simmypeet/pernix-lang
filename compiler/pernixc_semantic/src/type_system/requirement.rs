//! Contains the definition of [`Requirement`] and [`requires()`]

use std::collections::HashSet;

use super::{
    instantiation::{
        FromGenericArgumentsError, Instantiation,
        MismatchedGenericArgumentCountError,
    },
    model::Model,
    normalizer::Normalizer,
    predicate::{self, Outlives, Predicate},
    query::{self, Limit, Session},
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::{self, Type},
        GenericArguments, ModelOf, Term,
    },
    visitor::{self, Recursive},
    Environment, OverflowError,
};
use crate::{
    semantic::deduction,
    symbol::{
        table::{representation::Index, State},
        GenericID, ImplementationID,
    },
};

/// A requirement that must be satisfied for the term to be valid.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Requirement<M: Model> {
    /// The requirement comes from where-clause predicates
    Predicate(Predicate<M>),

    /// The requirement comes from constant arguments
    ConstantTypeEquality(Constant<M>, Type<M>),
}

/// An error that occurs when retrieving the requirements of a term.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error<M: Model> {
    #[error("found a `GenericID` that does not exist in the table")]
    InvalidID(GenericID),

    #[error(
        "encountered a mismatch in the number of generic arguments supplied \
         to the symbol"
    )]
    MismatchedGenericArgumentCount(#[from] MismatchedGenericArgumentCountError),

    #[error(transparent)]
    ExceedLimit(#[from] OverflowError),

    #[error("failed to deduce the generic arguments with the implementation")]
    DeductionFailure {
        implementation_id: ImplementationID,
        generic_arguments: GenericArguments<M>,
    },
}

pub(super) trait Require: ModelOf {
    fn requires<S: State>(
        &self,
        requirements: &mut HashSet<Requirement<Self::Model>>,
        environment: &Environment<Self::Model, S, impl Normalizer<Self::Model>>,
        limit: &mut Limit<
            impl Session<Lifetime<Self::Model>>
                + Session<Type<Self::Model>>
                + Session<Constant<Self::Model>>,
        >,
    ) -> Result<(), Error<Self::Model>>;
}

impl<M: Model> Require for Lifetime<M>
where
    Type<M>: ModelOf<Model = M>,
{
    fn requires<S: State>(
        &self,
        _: &mut HashSet<Requirement<M>>,
        _: &Environment<M, S, impl Normalizer<M>>,
        _: &mut Limit<
            impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
        >,
    ) -> Result<(), Error<M>> {
        Ok(())
    }
}

impl<M: Model> Require for Type<M>
where
    Type<M>: ModelOf<Model = M>,
{
    fn requires<S: State>(
        &self,
        requirements: &mut HashSet<Requirement<M>>,
        environment: &Environment<M, S, impl Normalizer<M>>,
        limit: &mut Limit<
            impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
        >,
    ) -> Result<(), Error<M>> {
        match self {
            Type::Error
            | Type::Local(_)
            | Type::Phantom(_)
            | Type::Pointer(_)
            | Type::Primitive(_)
            | Type::Parameter(_)
            | Type::Inference(_) => {}

            Type::Symbol(symbol) => {
                let generic_id = GenericID::from(symbol.id);
                let generic = environment
                    .table
                    .get_generic(generic_id)
                    .ok_or(Error::InvalidID(generic_id))?;

                let instantiation = Instantiation::from_generic_arguments(
                    symbol.generic_arguments.clone(),
                    generic_id,
                    &generic.generic_declaration().parameters,
                )?;

                requirements.extend(
                    generic.generic_declaration().predicates.iter().map(
                        |predicate| {
                            let mut predicate = Predicate::from_default_model(
                                predicate.predicate.clone(),
                            );
                            predicate.instantiate(&instantiation);
                            Requirement::Predicate(predicate)
                        },
                    ),
                )
            }

            // the pointee must outlive the lifetime of the reference
            Type::Reference(reference_of) => {
                requirements.insert(Requirement::Predicate(
                    Predicate::TypeOutlives(Outlives {
                        operand: *reference_of.pointee.clone(),
                        bound: reference_of.lifetime.clone(),
                    }),
                ));
            }

            // array length must be a usize
            Type::Array(array) => {
                requirements.insert(Requirement::ConstantTypeEquality(
                    array.length.clone(),
                    r#type::Type::Primitive(r#type::Primitive::Usize),
                ));
            }

            // unpacked element must be a tuple
            Type::Tuple(tuple) => {
                requirements.extend(
                    tuple
                        .elements
                        .iter()
                        .cloned()
                        .filter_map(|x| x.is_unpacked.then(|| x.term))
                        .map(|x| {
                            Requirement::Predicate(Predicate::TupleType(
                                predicate::Tuple(x),
                            ))
                        }),
                );
            }

            Type::MemberSymbol(member_symbol) => {
                let (generic_arguments, implementation_id) =
                    match member_symbol.id {
                        r#type::MemberSymbolID::TraitImplementation(imp) => {
                            let parent_implementation_id = environment
                                .table
                                .get(imp)
                                .ok_or(Error::InvalidID(imp.into()))?
                                .parent_id();

                            (
                                GenericArguments::<M>::from_default_model(
                                    environment
                                        .table
                                        .get(parent_implementation_id)
                                        .unwrap()
                                        .arguments
                                        .clone(),
                                ),
                                parent_implementation_id.into(),
                            )
                        }
                        r#type::MemberSymbolID::AdtImplementation(imp) => {
                            let parent_implementation_id = environment
                                .table
                                .get(imp)
                                .ok_or(Error::InvalidID(imp.into()))?
                                .parent_id();

                            (
                                GenericArguments::<M>::from_default_model(
                                    environment
                                        .table
                                        .get(parent_implementation_id)
                                        .unwrap()
                                        .arguments
                                        .clone(),
                                ),
                                parent_implementation_id.into(),
                            )
                        }
                    };

                let mut instantiation = match generic_arguments.deduce_impl(
                    &member_symbol.parent_generic_arguments,
                    environment,
                    limit,
                ) {
                    Ok(instantiation) => instantiation,

                    Err(deduction::Error::ExceedLimit(error)) => {
                        return Err(error.into())
                    }

                    Err(_) => {
                        return Err(Error::DeductionFailure {
                            implementation_id,
                            generic_arguments,
                        })
                    }
                };

                // check for error
                if let Err(error) = instantiation.append_from_generic_arguments(
                    member_symbol.member_generic_arguments.clone(),
                    member_symbol.id.into(),
                    &environment
                        .table
                        .get_generic(member_symbol.id.into())
                        .unwrap()
                        .generic_declaration()
                        .parameters,
                ) {
                    use FromGenericArgumentsError::*;

                    match error {
                        LifetimeParameterCollision(_)
                        | TypeParameterCollision(_)
                        | ConstantParameterCollision(_) => unreachable!(),
                        MismatchedGenericParameterCount(mismatched) => {
                            return Err(Error::MismatchedGenericArgumentCount(
                                mismatched,
                            ));
                        }
                    }
                }

                let implementation_sym = environment
                    .table
                    .get_generic(implementation_id.into())
                    .unwrap();
                let member_sym = environment
                    .table
                    .get_generic(member_symbol.id.into())
                    .unwrap();

                requirements.extend(
                    implementation_sym
                        .generic_declaration()
                        .predicates
                        .iter()
                        .chain(
                            member_sym.generic_declaration().predicates.iter(),
                        )
                        .map(|predicate| {
                            let mut predicate = Predicate::from_default_model(
                                predicate.predicate.clone(),
                            );
                            predicate.instantiate(&instantiation);
                            Requirement::Predicate(predicate)
                        }),
                )
            }

            Type::TraitMember(trait_member) => {
                let trait_member_sym =
                    environment
                        .table
                        .get(trait_member.id)
                        .ok_or(Error::InvalidID(trait_member.id.into()))?;
                let parent_trait_id = trait_member_sym.parent_id();
                let parent_trait_sym =
                    environment.table.get(parent_trait_id).unwrap();

                let mut instantiation = Instantiation::from_generic_arguments(
                    trait_member.parent_generic_arguments.clone(),
                    parent_trait_id.into(),
                    &parent_trait_sym.generic_declaration.parameters,
                )?;

                // check for error
                if let Err(error) = instantiation.append_from_generic_arguments(
                    trait_member.member_generic_arguments.clone(),
                    trait_member.id.into(),
                    &trait_member_sym.generic_declaration.parameters,
                ) {
                    use FromGenericArgumentsError::*;

                    match error {
                        LifetimeParameterCollision(_)
                        | TypeParameterCollision(_)
                        | ConstantParameterCollision(_) => unreachable!(),
                        MismatchedGenericParameterCount(mismatched) => {
                            return Err(Error::MismatchedGenericArgumentCount(
                                mismatched,
                            ));
                        }
                    }
                }

                requirements.extend(
                    parent_trait_sym
                        .generic_declaration
                        .predicates
                        .iter()
                        .chain(
                            trait_member_sym
                                .generic_declaration
                                .predicates
                                .iter(),
                        )
                        .map(|predicate| {
                            let mut predicate = Predicate::from_default_model(
                                predicate.predicate.clone(),
                            );
                            predicate.instantiate(&instantiation);
                            Requirement::Predicate(predicate)
                        }),
                )
            }
        }

        Ok(())
    }
}

impl<M: Model> Require for Constant<M>
where
    Type<M>: ModelOf<Model = M>,
{
    fn requires<S: State>(
        &self,
        requirements: &mut HashSet<Requirement<M>>,
        _: &Environment<M, S, impl Normalizer<M>>,
        _: &mut Limit<
            impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
        >,
    ) -> Result<(), Error<M>> {
        match self {
            Constant::Phantom
            | Constant::Error
            | Constant::Primitive(_)
            | Constant::Inference(_)
            | Constant::Struct(_)
            | Constant::Enum(_)
            | Constant::Array(_)
            | Constant::Parameter(_)
            | Constant::Local(_) => {}

            Constant::Tuple(tuple) => {
                requirements.extend(
                    tuple
                        .elements
                        .iter()
                        .cloned()
                        .filter_map(|x| x.is_unpacked.then(|| x.term))
                        .map(|x| {
                            Requirement::Predicate(Predicate::TupleConstant(
                                predicate::Tuple(x),
                            ))
                        }),
                );
            }
        }

        Ok(())
    }
}

struct RequirementVisitor<
    'h,
    'e,
    'l,
    M: Model,
    S: State,
    N: Normalizer<M>,
    R: Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
> {
    requirements: &'h mut HashSet<Requirement<M>>,
    environment: &'e Environment<'e, M, S, N>,
    result: Result<(), Error<M>>,
    limit: &'l mut Limit<R>,
}

impl<
        'a,
        'h,
        'e,
        'l,
        T: Term,
        S: State,
        N: Normalizer<T::Model>,
        R: Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    > Recursive<'a, T> for RequirementVisitor<'h, 'e, 'l, T::Model, S, N, R>
{
    fn visit(
        &mut self,
        term: &'a T,
        _: impl Iterator<Item = super::sub_term::TermLocation>,
    ) -> bool {
        match self.result {
            Ok(()) => {
                if let Err(error) = term.requires(
                    self.requirements,
                    self.environment,
                    self.limit,
                ) {
                    self.result = Err(error);
                    return false;
                }

                true
            }
            Err(_) => false,
        }
    }
}

/// Gets the list of requirements for the given term.
///
/// # Errors
///
/// See [`Error`] for more information.
pub fn requires<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
) -> Result<HashSet<Requirement<T::Model>>, Error<T::Model>> {
    let mut limit = Limit::<query::Default<_>>::default();
    requires_impl(term, environment, &mut limit)
}

pub(super) fn requires_impl<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    limit: &mut Limit<
        impl Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    >,
) -> Result<HashSet<Requirement<T::Model>>, Error<T::Model>> {
    let mut requirements = HashSet::new();
    let mut visitor = RequirementVisitor {
        requirements: &mut requirements,
        environment,
        result: Ok(()),
        limit,
    };

    let _ = visitor::accept_recursive(term, &mut visitor);

    visitor.result?;

    Ok(requirements)
}
