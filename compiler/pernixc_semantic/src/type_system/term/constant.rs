//! Contains the definition of [`Constant`].

use core::fmt;
use std::{
    self,
    collections::{BTreeMap, BTreeSet},
    ops::Deref,
};

use enum_as_inner::EnumAsInner;

use super::{
    lifetime::Lifetime, r#type::Type, Error, GenericArguments, Kind, KindMut,
    ModelOf, Never, Term,
};
use crate::{
    arena::ID,
    symbol::{
        self,
        table::{self, representation::Index, DisplayObject, State, Table},
        ConstantParameter, ConstantParameterID, ItemID, Variant,
    },
    type_system::{
        self,
        equality::Equality,
        instantiation::Instantiation,
        mapping::Mapping,
        matching::{self, Match},
        model::{Default, Model},
        normalizer::Normalizer,
        observer::Observer,
        predicate::{self, Outlives, Predicate, Satisfiability},
        query::Context,
        sub_term::{
            self, AssignSubTermError, Location, SubTerm, SubTupleLocation,
            TermLocation,
        },
        unification::{self, Unifier},
        Environment, Output,
    },
};

/// Represents a primitive constant.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum Primitive {
    #[display(fmt = "{_0}")]
    Integer(i128),
    #[display(fmt = "{_0}")]
    Bool(bool),
}

/// Represents a struct constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct<M: Model> {
    /// The ID to the struct.
    pub id: ID<symbol::Struct>,

    /// The fields of the struct constant value.
    pub fields: Vec<Constant<M>>,
}

/// Represents an enum constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum<M: Model> {
    /// The variant that the enum constant value is.
    pub variant_id: ID<Variant>,

    /// The associated value of the enum constant value (if any).
    pub associated_value: Option<Box<Constant<M>>>,
}

/// Represents an array constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array<M: Model> {
    /// The value of each element in the array constant value.
    pub elements: Vec<Constant<M>>,
}

/// Represents a tuple constant value, denoted by `(value, value, ...value)`
/// syntax.
pub type Tuple<M> = super::Tuple<Constant<M>>;

/// The location pointing to a sub-constant term in a constant.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubConstantLocation {
    /// The index of the element in a [`Tuple`] constant.
    #[from]
    Tuple(SubTupleLocation),

    /// The index of the field in a [`Struct`] constant.
    Struct(usize),

    /// The value in a [`Enum::associated_value`] field (if any).
    Enum,

    /// The index of the element in an [`Array`] constant.
    Array(usize),
}

impl From<SubConstantLocation> for TermLocation {
    fn from(value: SubConstantLocation) -> Self {
        Self::Constant(sub_term::SubConstantLocation::FromConstant(value))
    }
}

impl<M: Model> Location<Constant<M>, Constant<M>> for SubConstantLocation {
    fn assign_sub_term(
        self,
        term: &mut Constant<M>,
        sub_term: Constant<M>,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => {
                return tuple.assign_sub_term(location, sub_term)
            }

            (Self::Struct(location), Constant::Struct(constant)) => constant
                .fields
                .get_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (
                Self::Enum,
                Constant::Enum(Enum {
                    associated_value: Some(constant), ..
                }),
            ) => constant,

            (Self::Array(location), Constant::Array(constant)) => constant
                .elements
                .get_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_term;

        Ok(())
    }

    fn get_sub_term(self, term: &Constant<M>) -> Option<Constant<M>> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get(single).map(|x| x.term.clone())
                }
                SubTupleLocation::Range { begin, end } => tuple
                    .elements
                    .get(begin..end)
                    .map(|x| Constant::Tuple(Tuple { elements: x.to_vec() })),
            },

            (Self::Struct(location), Constant::Struct(constant)) => {
                constant.fields.get(location).cloned()
            }

            (
                Self::Enum,
                Constant::Enum(Enum {
                    associated_value: Some(constant), ..
                }),
            ) => Some(constant.deref().clone()),

            (Self::Array(location), Constant::Array(constant)) => {
                constant.elements.get(location).cloned()
            }

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Constant<M>) -> Option<&Constant<M>> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get(single).map(|x| &x.term)
                }
                SubTupleLocation::Range { .. } => None,
            },

            (Self::Struct(location), Constant::Struct(constant)) => {
                constant.fields.get(location)
            }

            (
                Self::Enum,
                Constant::Enum(Enum {
                    associated_value: Some(constant), ..
                }),
            ) => Some(&**constant),

            (Self::Array(location), Constant::Array(constant)) => {
                constant.elements.get(location)
            }

            _ => None,
        }
    }

    fn get_sub_term_mut(
        self,
        term: &mut Constant<M>,
    ) -> Option<&mut Constant<M>> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get_mut(single).map(|x| &mut x.term)
                }
                SubTupleLocation::Range { .. } => None,
            },

            (Self::Struct(location), Constant::Struct(constant)) => {
                constant.fields.get_mut(location)
            }

            (
                Self::Enum,
                Constant::Enum(Enum {
                    associated_value: Some(constant), ..
                }),
            ) => Some(&mut *constant),

            (Self::Array(location), Constant::Array(constant)) => {
                constant.elements.get_mut(location)
            }

            _ => None,
        }
    }
}

/// Represents a compile-time constant term.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
#[non_exhaustive]
pub enum Constant<M: Model> {
    #[from]
    Primitive(Primitive),
    Inference(M::ConstantInference),
    #[from]
    Struct(Struct<M>),
    #[from]
    Enum(Enum<M>),
    #[from]
    Array(Array<M>),
    #[from]
    Parameter(ConstantParameterID),
    #[from]
    Tuple(Tuple<M>),

    Phantom,
    #[from]
    Error(Error),
}

impl<M: Model> From<Never> for Constant<M> {
    fn from(value: Never) -> Self { match value {} }
}

impl<M: Model> ModelOf for Constant<M> {
    type Model = M;
}

impl<M: Model> TryFrom<Constant<M>> for Tuple<M> {
    type Error = Constant<M>;

    fn try_from(value: Constant<M>) -> Result<Self, Self::Error> {
        value.into_tuple()
    }
}

impl<M: Model> TryFrom<Constant<M>> for ConstantParameterID {
    type Error = Constant<M>;

    fn try_from(value: Constant<M>) -> Result<Self, Self::Error> {
        value.into_parameter()
    }
}

impl<M: Model> std::default::Default for Constant<M> {
    fn default() -> Self { Self::Tuple(Tuple { elements: Vec::new() }) }
}

impl<M: Model> SubTerm for Constant<M> {
    type SubTypeLocation = Never;
    type SubConstantLocation = SubConstantLocation;
    type SubLifetimeLocation = Never;
    type ThisSubTermLocation = SubConstantLocation;
}

impl<M: Model> Location<Constant<M>, Type<M>> for Never {
    fn assign_sub_term(
        self,
        _: &mut Constant<M>,
        _: Type<M>,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Constant<M>) -> Option<Type<M>> { match self {} }

    fn get_sub_term_ref(self, _: &Constant<M>) -> Option<&Type<M>> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Constant<M>) -> Option<&mut Type<M>> {
        match self {}
    }
}

impl<M: Model> Location<Constant<M>, Lifetime<M>> for Never {
    fn assign_sub_term(
        self,
        _: &mut Constant<M>,
        _: Lifetime<M>,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Constant<M>) -> Option<Lifetime<M>> {
        match self {}
    }

    fn get_sub_term_ref(self, _: &Constant<M>) -> Option<&Lifetime<M>> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Constant<M>) -> Option<&mut Lifetime<M>> {
        match self {}
    }
}

impl<M: Model> Match for Constant<M> {
    #[allow(clippy::too_many_lines)]
    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    > {
        match (self, other) {
            (Self::Struct(lhs), Self::Struct(rhs))
                if lhs.id == rhs.id && lhs.fields.len() == rhs.fields.len() =>
            {
                Some(matching::Substructural {
                    lifetimes: Vec::new(),
                    types: Vec::new(),
                    constants: lhs
                        .fields
                        .iter()
                        .zip(rhs.fields.iter())
                        .enumerate()
                        .map(|(idx, (lhs, rhs))| matching::Matching {
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                            lhs_location: SubConstantLocation::Struct(idx),
                            rhs_location: SubConstantLocation::Struct(idx),
                        })
                        .collect(),
                })
            }

            (Self::Enum(lhs), Self::Enum(rhs))
                if lhs.variant_id == rhs.variant_id =>
            {
                match (&lhs.associated_value, &rhs.associated_value) {
                    (Some(lhs), Some(rhs)) => Some(matching::Substructural {
                        lifetimes: Vec::new(),
                        types: Vec::new(),
                        constants: vec![matching::Matching {
                            lhs: lhs.deref().clone(),
                            rhs: rhs.deref().clone(),
                            lhs_location: SubConstantLocation::Enum,
                            rhs_location: SubConstantLocation::Enum,
                        }],
                    }),
                    (None, None) => Some(matching::Substructural::default()),
                    _ => None,
                }
            }

            (Self::Array(lhs), Self::Array(rhs))
                if lhs.elements.len() == rhs.elements.len() =>
            {
                Some(matching::Substructural {
                    lifetimes: Vec::new(),
                    types: Vec::new(),
                    constants: lhs
                        .elements
                        .iter()
                        .cloned()
                        .zip(rhs.elements.iter().cloned())
                        .enumerate()
                        .map(|(idx, (lhs, rhs))| matching::Matching {
                            lhs,
                            rhs,
                            lhs_location: SubConstantLocation::Array(idx),
                            rhs_location: SubConstantLocation::Array(idx),
                        })
                        .collect(),
                })
            }

            (Self::Tuple(lhs), Self::Tuple(rhs)) => {
                lhs.substructural_match(rhs)
            }

            _ => None,
        }
    }

    fn get_substructural(
        substructural: &matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<matching::Matching<Self, Self::ThisSubTermLocation>> {
        &substructural.constants
    }

    fn get_substructural_mut(
        substructural: &mut matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<matching::Matching<Self, Self::ThisSubTermLocation>> {
        &mut substructural.constants
    }
}

impl<M: Model> Term for Constant<M>
where
    Self: ModelOf<Model = M>,
{
    type GenericParameter = ConstantParameter;
    type TraitMember = Never;
    type InferenceVariable = M::ConstantInference;
    type Rebind<Ms: Model> = Constant<Ms>;

    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        match term {
            Constant::Primitive(primitive) => Self::Primitive(primitive),
            Constant::Inference(inference) => {
                Self::Inference(M::ConstantInference::from(inference))
            }
            Constant::Struct(value) => Self::Struct(Struct {
                id: value.id,
                fields: value
                    .fields
                    .into_iter()
                    .map(Self::from_other_model)
                    .collect(),
            }),
            Constant::Enum(value) => Self::Enum(Enum {
                variant_id: value.variant_id,
                associated_value: value
                    .associated_value
                    .map(|x| Box::new(Self::from_other_model(*x))),
            }),
            Constant::Array(array) => Self::Array(Array {
                elements: array
                    .elements
                    .into_iter()
                    .map(Self::from_other_model)
                    .collect(),
            }),
            Constant::Parameter(parameter) => Self::Parameter(parameter),
            Constant::Tuple(tuple) => {
                Self::Tuple(Tuple::from_other_model(tuple))
            }
            Constant::Phantom => Self::Phantom,
            Constant::Error(Error) => Self::Error(Error),
        }
    }

    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(match term {
            Constant::Primitive(primitive) => Self::Primitive(primitive),
            Constant::Inference(inference) => {
                Self::Inference(M::ConstantInference::try_from(inference)?)
            }
            Constant::Struct(value) => Self::Struct(Struct {
                id: value.id,
                fields: value
                    .fields
                    .into_iter()
                    .map(Self::try_from_other_model)
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            Constant::Enum(value) => match value.associated_value {
                Some(associated_value) => {
                    let associated_value =
                        Self::try_from_other_model(*associated_value)?;

                    Self::Enum(Enum {
                        variant_id: value.variant_id,
                        associated_value: Some(Box::new(associated_value)),
                    })
                }
                None => Self::Enum(Enum {
                    variant_id: value.variant_id,
                    associated_value: None,
                }),
            },
            Constant::Array(array) => Self::Array(Array {
                elements: array
                    .elements
                    .into_iter()
                    .map(Self::try_from_other_model)
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            Constant::Parameter(parameter) => Self::Parameter(parameter),
            Constant::Tuple(tuple) => {
                Self::Tuple(Tuple::try_from_other_model(tuple)?)
            }
            Constant::Phantom => Self::Phantom,
            Constant::Error(Error) => Self::Error(Error),
        })
    }

    #[allow(private_bounds, private_interfaces)]
    fn normalize<S: State>(
        &self,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        context: &mut Context<M>,
    ) -> Result<Output<Self, M>, type_system::OverflowError> {
        Normalizer::normalize_constant(self, environment, context)?
            .map_or_else(|| Ok(None), |x| Ok(Some(x)))
    }

    fn as_kind(&self) -> Kind<M> { Kind::Constant(self) }

    fn as_kind_mut(&mut self) -> KindMut<M> { KindMut::Constant(self) }

    fn try_from_kind(
        kind: Kind<Self::Model>,
    ) -> Result<&Self, Kind<Self::Model>> {
        match kind {
            Kind::Constant(constant) => Ok(constant),
            _ => Err(kind),
        }
    }

    fn try_from_kind_mut(
        kind: KindMut<Self::Model>,
    ) -> Result<&mut Self, KindMut<Self::Model>> {
        match kind {
            KindMut::Constant(constant) => Ok(constant),
            _ => Err(kind),
        }
    }

    #[allow(private_bounds, private_interfaces)]
    fn outlives_satisfiability(&self, _: &Lifetime<M>) -> Satisfiability {
        // constants value do not have lifetimes
        Satisfiability::Satisfied
    }

    fn from_inference(inference: Self::InferenceVariable) -> Self {
        Self::Inference(inference)
    }

    fn as_generic_parameter(&self) -> Option<&ConstantParameterID> {
        self.as_parameter()
    }

    fn as_generic_parameter_mut(&mut self) -> Option<&mut ConstantParameterID> {
        self.as_parameter_mut()
    }

    fn into_generic_parameter(self) -> Result<ConstantParameterID, Self> {
        self.into_parameter()
    }

    fn as_trait_member(&self) -> Option<&Never> { None }

    fn as_trait_member_mut(&mut self) -> Option<&mut Never> { None }

    fn into_trait_member(self) -> Result<Never, Self> { Err(self) }

    fn as_tuple(&self) -> Option<&Tuple<M>> {
        match self {
            Self::Tuple(tuple) => Some(tuple),
            _ => None,
        }
    }

    fn as_tuple_mut(&mut self) -> Option<&mut Tuple<M>> {
        match self {
            Self::Tuple(tuple) => Some(tuple),
            _ => None,
        }
    }

    fn into_tuple(self) -> Result<Tuple<M>, Self> {
        match self {
            Self::Tuple(tuple) => Ok(tuple),
            x => Err(x),
        }
    }

    fn as_inference(&self) -> Option<&Self::InferenceVariable> {
        match self {
            Self::Inference(inference) => Some(inference),
            _ => None,
        }
    }

    fn as_inference_mut(&mut self) -> Option<&mut Self::InferenceVariable> {
        match self {
            Self::Inference(inference) => Some(inference),
            _ => None,
        }
    }

    fn into_inference(self) -> Result<Self::InferenceVariable, Self> {
        match self {
            Self::Inference(inference) => Ok(inference),
            x => Err(x),
        }
    }

    fn get_adt_fields(&self, _: &Table<impl State>) -> Option<Vec<Self>> {
        None
    }

    fn as_outlive_predicate(_: &Predicate<M>) -> Option<&Outlives<Self>> {
        None
    }

    fn as_outlive_predicate_mut(
        _: &mut Predicate<M>,
    ) -> Option<&mut Outlives<Self>> {
        None
    }

    fn into_outlive_predicate(
        predicate: Predicate<M>,
    ) -> Result<Outlives<Self>, Predicate<M>> {
        Err(predicate)
    }

    fn as_trait_member_equality_predicate(
        _: &Predicate<M>,
    ) -> Option<&Equality<Never, Self>> {
        None
    }

    fn as_trait_member_equality_predicate_mut(
        _: &mut Predicate<M>,
    ) -> Option<&mut Equality<Never, Self>> {
        None
    }

    fn into_trait_member_equality_predicate(
        predicate: Predicate<M>,
    ) -> Result<Equality<Never, Self>, Predicate<M>> {
        Err(predicate)
    }

    fn as_tuple_predicate(_: &Predicate<M>) -> Option<&predicate::Tuple<Self>> {
        None
    }

    fn as_tuple_predicate_mut(
        _: &mut Predicate<M>,
    ) -> Option<&mut predicate::Tuple<Self>> {
        None
    }

    fn into_tuple_predicate(
        predicate: Predicate<M>,
    ) -> Result<predicate::Tuple<Self>, Predicate<M>> {
        Err(predicate)
    }

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

    fn get_instantiation(
        instantiation: &Instantiation<M>,
    ) -> &BTreeMap<Self, Self> {
        &instantiation.constants
    }

    fn get_instantiation_mut(
        instantiation: &mut Instantiation<M>,
    ) -> &mut BTreeMap<Self, Self> {
        &mut instantiation.constants
    }

    fn get_substructural_unifier<'a, T: Term>(
        substructural: &'a unification::Substructural<T>,
    ) -> impl Iterator<Item = &'a Unifier<Constant<T::Model>>>
    where
        Self: 'a,
    {
        substructural.constants.values()
    }

    fn get_mapping(mapping: &Mapping<M>) -> &BTreeMap<Self, BTreeSet<Self>> {
        &mapping.constants
    }

    fn get_mapping_mut(
        mapping: &mut Mapping<M>,
    ) -> &mut BTreeMap<Self, BTreeSet<Self>> {
        &mut mapping.constants
    }

    fn get_generic_arguments(
        generic_arguments: &GenericArguments<M>,
    ) -> &[Self] {
        &generic_arguments.constants
    }

    fn get_generic_arguments_mut(
        generic_arguments: &mut GenericArguments<M>,
    ) -> &mut Vec<Self> {
        &mut generic_arguments.constants
    }

    fn from_default_model(term: Constant<Default>) -> Self {
        M::from_default_constant(term)
    }
}

impl<M: Model> Constant<M> {
    /// Gets a list of [`ItemID`]s that occur in the constant.
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn get_item_id_dependencies(
        &self,
        table: &Table<impl State>,
    ) -> Option<Vec<ItemID>> {
        let mut occurrences = match self {
            Self::Phantom
            | Self::Error(_)
            | Self::Parameter(_)
            | Self::Primitive(_)
            | Self::Inference(_) => {
                return Some(Vec::new());
            }

            Self::Struct(val) => {
                let mut occurrences = Vec::new();

                occurrences.push(val.id.into());

                for field in &val.fields {
                    occurrences.extend(field.get_item_id_dependencies(table)?);
                }

                occurrences
            }
            Self::Enum(val) => {
                let parent_enum_id =
                    table.get(val.variant_id)?.parent_enum_id();

                let mut occurrences =
                    vec![parent_enum_id.into(), val.variant_id.into()];

                if let Some(associated_value) = &val.associated_value {
                    occurrences.extend(
                        associated_value.get_item_id_dependencies(table)?,
                    );
                }

                occurrences
            }
            Self::Array(val) => {
                let mut occurrences = Vec::new();

                for element in &val.elements {
                    occurrences
                        .extend(element.get_item_id_dependencies(table)?);
                }

                occurrences
            }
            Self::Tuple(tuple) => {
                let mut occurrences = Vec::new();

                for element in &tuple.elements {
                    occurrences
                        .extend(element.term.get_item_id_dependencies(table)?);
                }

                occurrences
            }
        };

        occurrences.sort_unstable();
        occurrences.dedup();

        Some(occurrences)
    }
}

impl<T: State, M: Model> table::Display<T> for Constant<M>
where
    M::ConstantInference: table::Display<T>,
{
    fn fmt(
        &self,
        table: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Phantom => write!(f, "phantom"),
            Self::Primitive(val) => write!(f, "{val}"),
            Self::Inference(inference) => {
                write!(f, "{}", DisplayObject { display: inference, table })
            }
            Self::Struct(val) => {
                let qualified_name = table
                    .get_qualified_name(val.id.into())
                    .ok_or(fmt::Error)?;

                write!(f, "{qualified_name} {{ ")?;

                let mut fields = val.fields.iter().peekable();
                while let Some(field) = fields.next() {
                    let is_last = fields.peek().is_none();

                    write!(f, "{}", DisplayObject { display: field, table })?;

                    if !is_last {
                        write!(f, ", ")?;
                    }
                }

                write!(f, " }}")
            }
            Self::Enum(id) => {
                let qualified_name = table
                    .get_qualified_name(id.variant_id.into())
                    .ok_or(fmt::Error)?;

                write!(f, "{qualified_name}")?;

                if let Some(associated_value) = &id.associated_value {
                    write!(f, "({})", DisplayObject {
                        display: &**associated_value,
                        table
                    })?;
                }

                Ok(())
            }
            Self::Array(array) => {
                write!(f, "[")?;

                let mut elements = array.elements.iter().peekable();
                while let Some(element) = elements.next() {
                    let is_last = elements.peek().is_none();

                    write!(f, "{}", DisplayObject { display: element, table })?;

                    if !is_last {
                        write!(f, ", ")?;
                    }
                }

                write!(f, "]")
            }
            Self::Parameter(parameter) => {
                write!(
                    f,
                    "{}",
                    table
                        .get_generic(parameter.parent)
                        .ok_or(fmt::Error)?
                        .generic_declaration()
                        .parameters
                        .constants()
                        .get(parameter.id)
                        .ok_or(fmt::Error)?
                        .name
                        .as_deref()
                        .unwrap_or("{unknown}")
                )
            }
            Self::Tuple(tuple) => {
                write!(f, "{}", DisplayObject { display: tuple, table })
            }
            Self::Error(_) => write!(f, "{{error}}"),
        }
    }
}

#[cfg(test)]
mod tests;
