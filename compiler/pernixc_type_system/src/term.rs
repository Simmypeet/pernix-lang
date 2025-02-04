//! Contains the definition of [`Term`]

use std::{cmp::Eq, fmt::Debug, hash::Hash};

use pernixc_arena::ID;
use pernixc_component::{
    fields::Fields, type_alias::TypeAlias, variant::Variant,
};
use pernixc_table::{
    component::{Member, Name, Parent, SymbolKind},
    GlobalID, MemberID, Table,
};
use pernixc_term::{
    constant::Constant,
    generic_parameter::{
        ConstantParameter, GenericParameter, GenericParameters,
        LifetimeParameter, TypeParameter,
    },
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    matching,
    predicate::{self, Compatible, Outlives, Predicate},
    r#type::{TraitMember, Type},
    sub_term, visitor, Model, ModelOf, Never, Symbol, Tuple, TupleElement,
};

use crate::{
    compatible, environment::Environment, equivalences, mapping,
    normalizer::Normalizer, resolution, unification, AbruptError,
    Satisfiability, Succeeded,
};

/// A trait implemented by all three fundamental terms of the language:
/// [`Lifetime`], [`Type`], and [`Constant`].
///
/// This trait provides a common interface for all terms to be used in the
/// type system. Since most of the queries and operations in the type system are
/// generic over the kind of term.
pub trait Term:
    Debug
    + Eq
    + Hash
    + Sized
    + Clone
    + Ord
    + ModelOf
    + visitor::Element
    + matching::Match
    + sub_term::SubTerm
    + unification::Element
    + equivalences::Equivalence
    + mapping::Element
    + instantiation::Element
    + compatible::Compatible
    + From<MemberID<ID<Self::GenericParameter>>>
    + From<pernixc_term::Error>
    + From<Self::TraitMember>
    + Send
    + Sync
    + 'static
{
    /// The type of generic parameters of this term kind.
    type GenericParameter: GenericParameter + 'static;

    /// The type of trait member symbol that stores this term kind.
    type TraitMember: Debug + Eq + Hash + Sized + Clone + Ord + 'static;

    /// The inference variable type of this term kind.
    type InferenceVariable: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync;

    /// An algorithm that normalizes the term.
    ///
    /// Normalization converts the term into a canonical form. For example,
    /// a type alias is expanded into its definition.
    ///
    /// ```pnx
    /// public type A = int32
    /// ```
    ///
    /// The type `A` is normalized into `int32`.
    #[doc(hidden)]
    fn normalize(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Option<Succeeded<Self, Self::Model>>, AbruptError>;

    #[doc(hidden)]
    fn as_trait_member_compatible_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&Compatible<Self::TraitMember, Self>>;

    #[doc(hidden)]
    fn as_trait_member_compatible_predicate_mut(
        predicate: &mut Predicate<Self::Model>,
    ) -> Option<&mut Compatible<Self::TraitMember, Self>>;

    #[doc(hidden)]
    fn into_trait_member_copmatible_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<Compatible<Self::TraitMember, Self>, Predicate<Self::Model>>;

    #[doc(hidden)]
    fn as_trait_member(&self) -> Option<&Self::TraitMember>;

    #[doc(hidden)]
    fn definite_satisfiability(&self) -> Satisfiability;

    #[doc(hidden)]
    fn get_adt_fields(
        &self,
        table: &Table,
    ) -> Result<Option<Vec<Self>>, AbruptError>;

    #[doc(hidden)]
    fn outlives_satisfiability(
        &self,
        lifetime: &Lifetime<Self::Model>,
    ) -> Satisfiability;

    #[doc(hidden)]
    fn as_outlives_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&Outlives<Self>>;

    #[doc(hidden)]
    fn as_tuple_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&predicate::Tuple<Self>>;

    #[doc(hidden)]
    fn as_tuple(&self) -> Option<&pernixc_term::Tuple<Self>>;
}

impl<M: Model> Term for Lifetime<M> {
    type GenericParameter = LifetimeParameter;
    type TraitMember = Never;
    type InferenceVariable = M::LifetimeInference;

    fn normalize(
        &self,
        _: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Option<Succeeded<Self, Self::Model>>, AbruptError> {
        Ok(None)
    }

    fn as_trait_member_compatible_predicate(
        _: &Predicate<Self::Model>,
    ) -> Option<&Compatible<Self::TraitMember, Self>> {
        None
    }

    fn as_trait_member_compatible_predicate_mut(
        _: &mut Predicate<Self::Model>,
    ) -> Option<&mut Compatible<Self::TraitMember, Self>> {
        None
    }

    fn into_trait_member_copmatible_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<Compatible<Self::TraitMember, Self>, Predicate<Self::Model>>
    {
        Err(predicate)
    }

    fn as_trait_member(&self) -> Option<&Self::TraitMember> { None }

    fn definite_satisfiability(&self) -> Satisfiability {
        Satisfiability::Satisfied
    }

    fn get_adt_fields(
        &self,
        _: &Table,
    ) -> Result<Option<Vec<Self>>, AbruptError> {
        Ok(None)
    }

    fn outlives_satisfiability(&self, other: &Self) -> Satisfiability {
        if self == other {
            return Satisfiability::Satisfied;
        }

        if self.is_static() {
            Satisfiability::Satisfied
        } else {
            Satisfiability::Unsatisfied
        }
    }

    fn as_outlives_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&Outlives<Self>> {
        predicate.as_lifetime_outlives()
    }

    fn as_tuple_predicate(
        _: &Predicate<Self::Model>,
    ) -> Option<&predicate::Tuple<Self>> {
        None
    }

    fn as_tuple(&self) -> Option<&pernixc_term::Tuple<Self>> { None }
}

fn normalize_trait_member<M: Model>(
    trait_member: &TraitMember<M>,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<Option<Succeeded<Type<M>, M>>, AbruptError> {
    let trait_id =
        environment.table().get::<Parent>(trait_member.id).parent.unwrap();

    // resolve the trait implementation
    let mut resolution = match environment.resolve_implementation(
        GlobalID::new(trait_member.id.target_id, trait_id),
        &trait_member.parent_generic_arguments,
    ) {
        Ok(resolution) => resolution,

        Err(resolution::Error::Abrupt(error)) => {
            return Err(error);
        }

        Err(_) => return Ok(None),
    };

    let trait_member_name =
        environment.table().get::<Name>(trait_member.id).0.clone();

    // not a trait implementation
    if *environment.table().get::<SymbolKind>(resolution.result.id)
        != SymbolKind::PositiveTraitImplementation
    {
        return Ok(None);
    }

    let Some(implementation_member_id) = environment
        .table()
        .get::<Member>(resolution.result.id)
        .0
        .get(&trait_member_name)
        .copied()
        .map(|x| GlobalID::new(resolution.result.id.target_id, x))
    else {
        return Ok(None);
    };

    // check if is the type
    if *environment.table().get::<SymbolKind>(implementation_member_id)
        != SymbolKind::TraitImplementationType
    {
        return Ok(None);
    }

    // should have no collision and no mismatched generic arguments
    // count
    {
        let generic_parameter = environment
            .table()
            .query::<GenericParameters>(implementation_member_id)
            .ok_or(AbruptError::CyclicDependency)?;

        if resolution
            .result
            .instantiation
            .append_from_generic_arguments(
                trait_member.member_generic_arguments.clone(),
                implementation_member_id,
                &generic_parameter,
            )
            .map_or(true, |x| !x.is_empty())
        {
            return Ok(None);
        }
    }

    let mut new_term = M::from_default_type(
        environment
            .table()
            .query::<TypeAlias>(implementation_member_id)
            .ok_or(AbruptError::CyclicDependency)?
            .0
            .clone(),
    );

    instantiation::instantiate(&mut new_term, &resolution.result.instantiation);

    Ok(Some(Succeeded::with_constraints(new_term, resolution.constraints)))
}

fn unpack_tuple<T: Term + From<Tuple<T>> + TryInto<Tuple<T>, Error = T>>(
    tuple: &pernixc_term::Tuple<T>,
) -> Option<Succeeded<T, T::Model>> {
    let contain_upacked = tuple.elements.iter().any(|x| x.is_unpacked);

    if !contain_upacked {
        return None;
    }

    let mut result = Vec::new();

    for element in tuple.elements.iter().cloned() {
        if element.is_unpacked {
            match element.term.try_into() {
                Ok(inner) => {
                    result.extend(inner.elements);
                }
                Err(term) => {
                    result.push(TupleElement { term, is_unpacked: true });
                }
            }
        } else {
            result.push(element);
        }
    }

    Some(Succeeded::new(Tuple { elements: result }.into()))
}

impl<M: Model> Term for Type<M> {
    type GenericParameter = TypeParameter;
    type TraitMember = TraitMember<M>;
    type InferenceVariable = M::TypeInference;

    fn normalize(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Option<Succeeded<Self, Self::Model>>, AbruptError> {
        let normalized = match self {
            // transform the trait-member into trait-implementation-type
            // equivalent
            Self::TraitMember(trait_member) => {
                normalize_trait_member(trait_member, environment)?
            }

            // unpack the tuple
            Self::Tuple(tuple) => unpack_tuple(tuple),

            _ => None,
        };

        if let Some(mut normalized) = normalized {
            if let Some(x) =
                Normalizer::normalize_type(&normalized.result, environment)?
            {
                normalized.result = x.result;
                normalized.constraints.extend(x.constraints);
            }

            Ok(Some(normalized))
        } else {
            Normalizer::normalize_type(self, environment)?
                .map_or_else(|| Ok(None), |x| Ok(Some(x)))
        }
    }

    fn as_trait_member_compatible_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&Compatible<TraitMember<M>, Self>> {
        if let Predicate::TraitTypeCompatible(compatible) = predicate {
            Some(compatible)
        } else {
            None
        }
    }

    fn as_trait_member_compatible_predicate_mut(
        predicate: &mut Predicate<Self::Model>,
    ) -> Option<&mut Compatible<TraitMember<M>, Self>> {
        if let Predicate::TraitTypeCompatible(compatible) = predicate {
            Some(compatible)
        } else {
            None
        }
    }

    fn into_trait_member_copmatible_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<Compatible<TraitMember<M>, Self>, Predicate<Self::Model>> {
        match predicate {
            Predicate::TraitTypeCompatible(compatible) => Ok(compatible),
            predicate => Err(predicate),
        }
    }

    fn as_trait_member(&self) -> Option<&TraitMember<M>> {
        if let Self::TraitMember(trait_type) = self {
            Some(trait_type)
        } else {
            None
        }
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Error(_) | Self::Parameter(_) | Self::Inference(_) => {
                Satisfiability::Unsatisfied
            }

            Self::Primitive(_) => Satisfiability::Satisfied,

            Self::MemberSymbol(_)
            | Self::FunctionSignature(_)
            | Self::Pointer(_)
            | Self::Symbol(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Phantom(_)
            | Self::TraitMember(_)
            | Self::Tuple(_) => Satisfiability::Congruent,
        }
    }

    fn get_adt_fields(
        &self,
        table: &Table,
    ) -> Result<Option<Vec<Self>>, AbruptError> {
        let Self::Symbol(Symbol { id, generic_arguments }) = self else {
            return Ok(None);
        };
        let id = *id;
        let symbol_kind = *table.get::<SymbolKind>(id);

        match symbol_kind {
            SymbolKind::Struct => {
                let Ok(inst) = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    id,
                    &*table
                        .query::<GenericParameters>(id)
                        .ok_or(AbruptError::CyclicDependency)?,
                ) else {
                    return Ok(None);
                };

                Ok(Some(
                    table
                        .query::<Fields>(id)
                        .ok_or(AbruptError::CyclicDependency)?
                        .fields
                        .iter()
                        .map(|field| {
                            let mut ty =
                                M::from_default_type(field.1.r#type.clone());
                            instantiation::instantiate(&mut ty, &inst);
                            ty
                        })
                        .collect(),
                ))
            }

            SymbolKind::Enum => {
                let Ok(inst) = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    id,
                    &*table
                        .query::<GenericParameters>(id)
                        .ok_or(AbruptError::CyclicDependency)?,
                ) else {
                    return Ok(None);
                };

                let member = table.get::<Member>(id);

                let mut variants_ty = Vec::new();
                for variant in member
                    .0
                    .values()
                    .copied()
                    .map(|x| GlobalID::new(id.target_id, x))
                {
                    let variant = table
                        .query::<Variant>(variant)
                        .ok_or(AbruptError::CyclicDependency)?;

                    variants_ty.extend(variant.associated_type.clone().map(
                        |x| {
                            let mut ty = M::from_default_type(x);
                            instantiation::instantiate(&mut ty, &inst);
                            ty
                        },
                    ));
                }

                Ok(Some(variants_ty))
            }

            _ => Ok(None),
        }
    }

    fn outlives_satisfiability(&self, _: &Lifetime<M>) -> Satisfiability {
        match self {
            Self::Primitive(_) => Satisfiability::Satisfied,

            Self::Error(_) | Self::Inference(_) | Self::Parameter(_) => {
                Satisfiability::Unsatisfied
            }

            Self::MemberSymbol(_)
            | Self::FunctionSignature(_)
            | Self::Symbol(_)
            | Self::Pointer(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Tuple(_)
            | Self::TraitMember(_)
            | Self::Phantom(_) => Satisfiability::Congruent,
        }
    }

    fn as_outlives_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&Outlives<Self>> {
        predicate.as_type_outlives()
    }

    fn as_tuple_predicate(
        predicate: &Predicate<Self::Model>,
    ) -> Option<&predicate::Tuple<Self>> {
        predicate.as_tuple_type()
    }

    fn as_tuple(&self) -> Option<&pernixc_term::Tuple<Self>> {
        if let Self::Tuple(tuple) = self {
            Some(tuple)
        } else {
            None
        }
    }
}

impl<M: Model> Term for Constant<M> {
    type GenericParameter = ConstantParameter;
    type TraitMember = Never;
    type InferenceVariable = M::ConstantInference;

    fn normalize(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Option<Succeeded<Self, Self::Model>>, AbruptError> {
        let normalized = match self {
            // unpack the tuple
            Self::Tuple(tuple) => unpack_tuple(tuple),

            _ => None,
        };

        if let Some(mut normalized) = normalized {
            if let Some(x) =
                Normalizer::normalize_constant(&normalized.result, environment)?
            {
                normalized.result = x.result;
                normalized.constraints.extend(x.constraints);
            }

            Ok(Some(normalized))
        } else {
            Normalizer::normalize_constant(self, environment)?
                .map_or_else(|| Ok(None), |x| Ok(Some(x)))
        }
    }

    fn as_trait_member_compatible_predicate(
        _: &Predicate<Self::Model>,
    ) -> Option<&Compatible<Self::TraitMember, Self>> {
        None
    }

    fn as_trait_member_compatible_predicate_mut(
        _: &mut Predicate<Self::Model>,
    ) -> Option<&mut Compatible<Self::TraitMember, Self>> {
        None
    }

    fn into_trait_member_copmatible_predicate(
        predicate: Predicate<Self::Model>,
    ) -> Result<Compatible<Self::TraitMember, Self>, Predicate<Self::Model>>
    {
        Err(predicate)
    }

    fn as_trait_member(&self) -> Option<&Self::TraitMember> { None }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Error(_) | Self::Parameter(_) | Self::Inference(_) => {
                Satisfiability::Unsatisfied
            }

            Self::Phantom | Self::Primitive(_) => Satisfiability::Satisfied,

            Self::Struct(_)
            | Self::Enum(_)
            | Self::Array(_)
            | Self::Tuple(_) => Satisfiability::Congruent,
        }
    }

    fn get_adt_fields(
        &self,
        _: &Table,
    ) -> Result<Option<Vec<Self>>, AbruptError> {
        Ok(None)
    }

    fn outlives_satisfiability(&self, _: &Lifetime<M>) -> Satisfiability {
        // constants value do not have lifetimes
        Satisfiability::Satisfied
    }

    fn as_outlives_predicate(
        _: &Predicate<Self::Model>,
    ) -> Option<&Outlives<Self>> {
        None
    }

    fn as_tuple_predicate(
        _: &Predicate<Self::Model>,
    ) -> Option<&predicate::Tuple<Self>> {
        None
    }

    fn as_tuple(&self) -> Option<&pernixc_term::Tuple<Self>> { None }
}
