//! Contains the definition of [`Constant`].

use core::fmt;
use std::{
    self,
    collections::{HashMap, HashSet},
    ops::Deref,
};

use enum_as_inner::EnumAsInner;

use super::{
    lifetime::Lifetime, r#type::Type, GenericArguments, Local, Never, Term,
    TupleElement,
};
use crate::{
    arena::ID,
    semantic::{
        instantiation::Instantiation,
        mapping::Mapping,
        matching::{self, Match},
        predicate::{self, Outlives, Predicate, Satisfiability},
        session::{ExceedLimitError, Limit, Session},
        sub_term::{AssignSubTermError, Location, SubTerm, SubTupleLocation},
        unification::{self, Unification},
        Environment,
    },
    symbol::{
        self, ConstantParameter, ConstantParameterID, GenericID, GlobalID,
        Variant,
    },
    table::{self, DisplayObject, Index, State, Table},
};

/// Enumeration of either a trait implementation constant or an ADT
/// implementation constant.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum MemberSymbolID {
    TraitImplementation(ID<symbol::TraitImplementationConstant>),
    AdtImplementation(ID<symbol::AdtImplementationConstant>),
}

impl From<MemberSymbolID> for GlobalID {
    fn from(value: MemberSymbolID) -> Self {
        match value {
            MemberSymbolID::TraitImplementation(id) => id.into(),
            MemberSymbolID::AdtImplementation(id) => id.into(),
        }
    }
}

impl From<MemberSymbolID> for GenericID {
    fn from(value: MemberSymbolID) -> Self {
        match value {
            MemberSymbolID::TraitImplementation(id) => id.into(),
            MemberSymbolID::AdtImplementation(id) => id.into(),
        }
    }
}

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
    #[display(fmt = "{_0}i8")]
    Int8(i8),
    #[display(fmt = "{_0}i16")]
    Int16(i16),
    #[display(fmt = "{_0}i32")]
    Int32(i32),
    #[display(fmt = "{_0}i64")]
    Int64(i64),
    #[display(fmt = "{_0}u8")]
    Uint8(u8),
    #[display(fmt = "{_0}u16")]
    Uint16(u16),
    #[display(fmt = "{_0}u32")]
    Uint32(u32),
    #[display(fmt = "{_0}u64")]
    Uint64(u64),
    #[display(fmt = "{_0}")]
    Bool(bool),
    #[display(fmt = "{_0}usize")]
    Usize(usize),
    #[display(fmt = "{_0}isize")]
    Isize(isize),
}

/// Represents a struct constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    /// The ID to the struct.
    pub id: ID<symbol::Struct>,

    /// The fields of the struct constant value.
    pub fields: Vec<Constant>,
}

/// Represents an enum constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    /// The variant that the enum constant value is.
    pub variant_id: ID<Variant>,

    /// The associated value of the enum constant value (if any).
    pub associated_value: Option<Box<Constant>>,
}

/// Represents an array constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array {
    /// The value of each element in the array constant value.
    pub elements: Vec<Constant>,
}

/// Represents a tuple constant value, denoted by `(value, value, ...value)`
/// syntax.
pub type Tuple = super::Tuple<Constant>;

/// Represents a constant inference variable in hindley-milner type inference.
pub type Inference = Never; /* will be changed */

/// Represents a constant value denoted by a `phantom` syntax.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Phantom;

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

    /// The inner constant of a [`Local`] constant.
    Local,
}

impl Location<Constant, Constant> for SubConstantLocation {
    fn assign_sub_term(
        self,
        term: &mut Constant,
        sub_term: Constant,
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

            (Self::Local, Constant::Local(local)) => &mut local.0,

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_term;

        Ok(())
    }

    fn get_sub_term(self, term: &Constant) -> Option<Constant> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get(single).map(|x| x.as_term().clone())
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

            (Self::Local, Constant::Local(local)) => {
                Some(local.0.deref().clone())
            }

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Constant) -> Option<&Constant> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get(single).map(TupleElement::as_term)
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

            (Self::Local, Constant::Local(local)) => Some(&*local.0),

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Constant) -> Option<&mut Constant> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => tuple
                    .elements
                    .get_mut(single)
                    .map(TupleElement::as_term_mut),
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

            (Self::Local, Constant::Local(local)) => Some(&mut *local.0),

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
pub enum Constant {
    Primitive(Primitive),
    Inference(Inference),
    Struct(Struct),
    Enum(Enum),
    Array(Array),
    Parameter(ConstantParameterID),
    Local(Local<Self>),
    Tuple(Tuple),
    Phantom(Phantom),
}

impl TryFrom<Constant> for Tuple {
    type Error = Constant;

    fn try_from(value: Constant) -> Result<Self, Self::Error> {
        value.into_tuple()
    }
}

impl TryFrom<Constant> for ConstantParameterID {
    type Error = Constant;

    fn try_from(value: Constant) -> Result<Self, Self::Error> {
        value.into_parameter()
    }
}

impl Default for Constant {
    fn default() -> Self { Self::Tuple(Tuple { elements: Vec::new() }) }
}

impl SubTerm for Constant {
    type SubTypeLocation = Never;
    type SubConstantLocation = SubConstantLocation;
    type SubLifetimeLocation = Never;
    type ThisSubTermLocation = SubConstantLocation;
}

impl Location<Constant, Type> for Never {
    fn assign_sub_term(
        self,
        _: &mut Constant,
        _: Type,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Constant) -> Option<Type> { match self {} }

    fn get_sub_term_ref(self, _: &Constant) -> Option<&Type> { match self {} }

    fn get_sub_term_mut(self, _: &mut Constant) -> Option<&mut Type> {
        match self {}
    }
}

impl Location<Constant, Lifetime> for Never {
    fn assign_sub_term(
        self,
        _: &mut Constant,
        _: Lifetime,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Constant) -> Option<Lifetime> { match self {} }

    fn get_sub_term_ref(self, _: &Constant) -> Option<&Lifetime> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Constant) -> Option<&mut Lifetime> {
        match self {}
    }
}

impl Match for Constant {
    #[allow(clippy::too_many_lines)]
    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        matching::Substructural<
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

            (Self::Local(lhs), Self::Local(rhs)) => {
                Some(matching::Substructural {
                    lifetimes: Vec::new(),
                    types: Vec::new(),
                    constants: vec![matching::Matching {
                        lhs: lhs.0.deref().clone(),
                        rhs: rhs.0.deref().clone(),
                        lhs_location: SubConstantLocation::Local,
                        rhs_location: SubConstantLocation::Local,
                    }],
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
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<matching::Matching<Self, Self::ThisSubTermLocation>> {
        &substructural.constants
    }

    fn get_substructural_mut(
        substructural: &mut matching::Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<matching::Matching<Self, Self::ThisSubTermLocation>> {
        &mut substructural.constants
    }
}

impl Term for Constant {
    type GenericParameter = ConstantParameter;
    type TraitMember = Never;

    fn normalize(
        &self,
        _: &Environment<impl State>,
        _: &mut Limit<impl Session<Lifetime> + Session<Type> + Session<Self>>,
    ) -> Result<Option<Self>, ExceedLimitError> {
        // TODO: Implement this.
        Ok(None)
    }

    fn outlives_satisfiability(
        &self,
        _: &Lifetime,
        _: &Environment<impl State>,
        _: &mut Limit<impl Session<Lifetime> + Session<Type> + Session<Self>>,
    ) -> Result<Satisfiability, ExceedLimitError> {
        // constants value do not have lifetimes
        Ok(Satisfiability::Satisfied)
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

    fn as_tuple(&self) -> Option<&Tuple> {
        match self {
            Self::Tuple(tuple) => Some(tuple),
            _ => None,
        }
    }

    fn as_tuple_mut(&mut self) -> Option<&mut Tuple> {
        match self {
            Self::Tuple(tuple) => Some(tuple),
            _ => None,
        }
    }

    fn into_tuple(self) -> Result<Tuple, Self> {
        match self {
            Self::Tuple(tuple) => Ok(tuple),
            x => Err(x),
        }
    }

    fn get_adt_fields(&self, _: &Table<impl State>) -> Option<Vec<Self>> {
        None
    }

    fn as_outlive_predicate(_: &Predicate) -> Option<&Outlives<Self>> { None }

    fn as_outlive_predicate_mut(
        _: &mut Predicate,
    ) -> Option<&mut Outlives<Self>> {
        None
    }

    fn into_outlive_predicate(
        predicate: Predicate,
    ) -> Result<Outlives<Self>, Predicate> {
        Err(predicate)
    }

    fn as_constant_type_predicate(_: &Predicate) -> Option<&Self> { None }

    fn as_constant_type_predicate_mut(_: &mut Predicate) -> Option<&mut Self> {
        None
    }

    fn into_constant_type_predicate(
        predicate: Predicate,
    ) -> Result<Self, Predicate> {
        Err(predicate)
    }

    fn as_trait_member_equality_predicate(
        _: &Predicate,
    ) -> Option<&predicate::TraitMemberEquality<Self>> {
        None
    }

    fn as_trait_member_equality_predicate_mut(
        _: &mut Predicate,
    ) -> Option<&mut predicate::TraitMemberEquality<Self>> {
        None
    }

    fn into_trait_member_equality_predicate(
        predicate: Predicate,
    ) -> Result<predicate::TraitMemberEquality<Self>, Predicate> {
        Err(predicate)
    }

    fn as_tuple_predicate(
        predicate: &Predicate,
    ) -> Option<&predicate::Tuple<Self>> {
        predicate.as_tuple_constant()
    }

    fn as_tuple_predicate_mut(
        predicate: &mut Predicate,
    ) -> Option<&mut predicate::Tuple<Self>> {
        predicate.as_tuple_constant_mut()
    }

    fn into_tuple_predicate(
        predicate: Predicate,
    ) -> Result<predicate::Tuple<Self>, Predicate> {
        predicate.into_tuple_constant()
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Parameter(_) | Self::Inference(_) => {
                Satisfiability::Unsatisfied
            }

            Self::Phantom(Phantom) | Self::Primitive(_) => {
                Satisfiability::Satisfied
            }

            Self::Struct(_)
            | Self::Enum(_)
            | Self::Array(_)
            | Self::Local(_)
            | Self::Tuple(_) => Satisfiability::Congruent,
        }
    }

    fn constant_type_satisfiability(&self) -> Satisfiability {
        Satisfiability::Satisfied
    }

    fn get_instantiation(
        instantiation: &Instantiation,
    ) -> &HashMap<Self, Self> {
        &instantiation.constants
    }

    fn get_instantiation_mut(
        instantiation: &mut Instantiation,
    ) -> &mut HashMap<Self, Self> {
        &mut instantiation.constants
    }

    fn get_substructural_unification<'a, T: Term>(
        substructural: &'a unification::Substructural<T>,
    ) -> impl Iterator<Item = &'a Unification<Self>>
    where
        Self: 'a,
    {
        substructural.constants.values()
    }

    fn get_mapping(mapping: &Mapping) -> &HashMap<Self, HashSet<Self>> {
        &mapping.constants
    }

    fn get_mapping_mut(
        mapping: &mut Mapping,
    ) -> &mut HashMap<Self, HashSet<Self>> {
        &mut mapping.constants
    }

    fn get_generic_arguments(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.constants
    }

    fn get_generic_arguments_mut(
        generic_arguments: &mut GenericArguments,
    ) -> &mut Vec<Self> {
        &mut generic_arguments.constants
    }
}

impl Constant {
    /// Gets a list of [`GlobalID`]s that occur in the constant.
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn get_global_id_dependencies(
        &self,
        table: &Table<impl State>,
    ) -> Option<Vec<GlobalID>> {
        let mut occurrences = match self {
            Self::Phantom(Phantom)
            | Self::Parameter(_)
            | Self::Primitive(_)
            | Self::Inference(_) => {
                return Some(Vec::new());
            }

            Self::Struct(val) => {
                let mut occurrences = Vec::new();

                occurrences.push(val.id.into());

                for field in &val.fields {
                    occurrences
                        .extend(field.get_global_id_dependencies(table)?);
                }

                occurrences
            }
            Self::Enum(val) => {
                let parent_enum_id = table.get(val.variant_id)?.parent_enum_id;

                let mut occurrences =
                    vec![parent_enum_id.into(), val.variant_id.into()];

                for associated_value in &val.associated_value {
                    occurrences.extend(
                        associated_value.get_global_id_dependencies(table)?,
                    );
                }

                occurrences
            }
            Self::Array(val) => {
                let mut occurrences = Vec::new();

                for element in &val.elements {
                    occurrences
                        .extend(element.get_global_id_dependencies(table)?);
                }

                occurrences
            }
            Self::Local(local) => local.0.get_global_id_dependencies(table)?,
            Self::Tuple(tuple) => {
                let mut occurrences = Vec::new();

                for element in &tuple.elements {
                    occurrences.extend(
                        element.as_term().get_global_id_dependencies(table)?,
                    );
                }

                occurrences
            }
        };

        occurrences.sort_unstable();
        occurrences.dedup();

        Some(occurrences)
    }
}

impl<T: State> table::Display<T> for Constant {
    fn fmt(
        &self,
        table: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Phantom(_) => write!(f, "phantom"),
            Self::Primitive(val) => write!(f, "{val}"),
            Self::Inference(_) => write!(f, "?"),
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
            Self::Local(local) => {
                write!(f, "local {}", DisplayObject {
                    display: &*local.0,
                    table
                })
            }
            Self::Tuple(tuple) => {
                write!(f, "{}", DisplayObject { display: tuple, table })
            }
        }
    }
}

#[cfg(test)]
mod tests;
