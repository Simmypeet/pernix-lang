//! Contains the definition of [`Constant`].

use core::fmt;
use std::{
    self,
    collections::{HashMap, HashSet},
    ops::Deref,
};

use enum_as_inner::EnumAsInner;

use super::{
    lifetime::Lifetime, r#type::Type, GenericArguments, Local, MemberSymbol,
    Never, Symbol, Term,
};
use crate::{
    arena::ID,
    semantic::{
        instantiation::Instantiation,
        mapping::Mapping,
        matching::{self, Match},
        predicate::{Outlives, Satisfiability},
        sub_term::{
            AssignSubTermError, Location, SubMemberSymbolLocation,
            SubSymbolLocation, SubTerm, SubTraitMemberLocation,
            SubTupleLocation,
        },
        unification::{self, Unification},
        Premise,
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

/// Represents a trait-member constant, denoted by `TRAIT[ARGS]::CONSTANT[`
/// syntax.
pub type TraitMember = MemberSymbol<ID<symbol::TraitConstant>>;

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

    /// The index of the constant argument in a [`Symbol`] constant.
    #[from]
    Symbol(SubSymbolLocation),

    /// A constant argument in a [`MemberSymbol`] constant.
    #[from]
    MemberSymbol(SubMemberSymbolLocation),

    /// The inner constant of a [`Local`] constant.
    Local,

    /// The constant argument in a [`Constant::TraitMember`] constant.
    #[from]
    TraitMember(SubTraitMemberLocation),
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

            (Self::Symbol(location), Constant::Symbol(symbol)) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (Self::MemberSymbol(location), Constant::MemberSymbol(symbol)) => {
                symbol
                    .get_term_mut(location)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            (Self::Local, Constant::Local(local)) => &mut local.0,

            (Self::TraitMember(location), Constant::TraitMember(symbol)) => {
                symbol
                    .get_term_mut(location.0)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

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

            (Self::Symbol(location), Constant::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::MemberSymbol(location), Constant::MemberSymbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::Local, Constant::Local(local)) => {
                Some(local.0.deref().clone())
            }

            (Self::TraitMember(location), Constant::TraitMember(symbol)) => {
                symbol.get_term(location.0).cloned()
            }

            _ => None,
        }
    }
}

/// The location pointing to a sub-type term in a constant.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubTypeLocation {
    /// The index of the element in a [`Symbol`] constant.
    #[from]
    Symbol(SubSymbolLocation),

    /// A constant argument in a [`MemberSymbol`] constant.
    #[from]
    MemberSymbol(SubMemberSymbolLocation),

    /// The constant argument in a [`Constant::TraitMember`] constant.
    #[from]
    TraitMember(SubTraitMemberLocation),
}

impl Location<Constant, Type> for SubTypeLocation {
    fn assign_sub_term(
        self,
        term: &mut Constant,
        sub_term: Type,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (self, term) {
            (Self::Symbol(location), Constant::Symbol(symbol)) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (Self::MemberSymbol(location), Constant::MemberSymbol(symbol)) => {
                symbol
                    .get_term_mut(location)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            (Self::TraitMember(location), Constant::TraitMember(symbol)) => {
                symbol
                    .get_term_mut(location.0)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_term;

        Ok(())
    }

    fn get_sub_term(self, term: &Constant) -> Option<Type> {
        match (self, term) {
            (Self::Symbol(location), Constant::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::MemberSymbol(location), Constant::MemberSymbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::TraitMember(location), Constant::TraitMember(symbol)) => {
                symbol.get_term(location.0).cloned()
            }

            _ => None,
        }
    }
}

/// The location pointing to a sub-lifetime term in a constant.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubLifetimeLocation {
    /// The index of the element in a [`Symbol`] constant.
    #[from]
    Symbol(SubSymbolLocation),

    /// A constant argument in a [`MemberSymbol`] constant.
    #[from]
    MemberSymbol(SubMemberSymbolLocation),

    /// The constant argument in a [`Constant::TraitMember`] constant.
    #[from]
    TraitMember(SubTraitMemberLocation),
}

impl Location<Constant, Lifetime> for SubLifetimeLocation {
    fn assign_sub_term(
        self,
        term: &mut Constant,
        sub_term: Lifetime,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (self, term) {
            (Self::Symbol(location), Constant::Symbol(symbol)) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (Self::MemberSymbol(location), Constant::MemberSymbol(symbol)) => {
                symbol
                    .get_term_mut(location)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            (Self::TraitMember(location), Constant::TraitMember(symbol)) => {
                symbol
                    .get_term_mut(location.0)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_term;

        Ok(())
    }

    fn get_sub_term(self, term: &Constant) -> Option<Lifetime> {
        match (self, term) {
            (Self::Symbol(location), Constant::Symbol(symbol)) => {
                symbol.get_term(location).copied()
            }

            (Self::MemberSymbol(location), Constant::MemberSymbol(symbol)) => {
                symbol.get_term(location).copied()
            }

            (Self::TraitMember(location), Constant::TraitMember(symbol)) => {
                symbol.get_term(location.0).copied()
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
pub enum Constant {
    Primitive(Primitive),
    Inference(Inference),
    Struct(Struct),
    Enum(Enum),
    Array(Array),
    Parameter(ConstantParameterID),
    Local(Local<Self>),
    Tuple(Tuple),
    Symbol(Symbol<ID<symbol::Constant>>),
    Phantom(Phantom),

    /// Please notice the differences
    ///
    /// In the **AdtImplementation** case, the `parent_generic_arguments` field
    /// is **not** deduced from the implementation directly, bur rather
    /// from the ADT that the implementation is for.
    ///
    /// In the **TraitImplementation** case, the `parent_generic_arguments`
    /// field **is** deduced from the implementation.
    MemberSymbol(MemberSymbol<MemberSymbolID>),

    TraitMember(MemberSymbol<ID<symbol::TraitConstant>>),
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
    type SubTypeLocation = SubTypeLocation;
    type SubConstantLocation = SubConstantLocation;
    type SubLifetimeLocation = SubLifetimeLocation;
    type ThisSubTermLocation = SubConstantLocation;
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

            (Self::Symbol(lhs), Self::Symbol(rhs)) if lhs.id == rhs.id => {
                lhs.generic_arguments.substructural_match(
                    &rhs.generic_arguments,
                    matching::Substructural::default(),
                    SubSymbolLocation,
                )
            }

            (Self::TraitMember(lhs), Self::TraitMember(rhs))
                if lhs.id == rhs.id =>
            {
                lhs.parent_generic_arguments
                    .substructural_match(
                        &rhs.parent_generic_arguments,
                        matching::Substructural::default(),
                        |index| {
                            SubTraitMemberLocation(SubMemberSymbolLocation {
                                index,
                                from_parent: true,
                            })
                        },
                    )
                    .and_then(|x| {
                        lhs.member_generic_arguments.substructural_match(
                            &rhs.member_generic_arguments,
                            x,
                            |index| {
                                SubTraitMemberLocation(
                                    SubMemberSymbolLocation {
                                        index,
                                        from_parent: false,
                                    },
                                )
                            },
                        )
                    })
            }

            (Self::MemberSymbol(lhs), Self::MemberSymbol(rhs))
                if lhs.id == rhs.id =>
            {
                lhs.parent_generic_arguments
                    .substructural_match(
                        &rhs.parent_generic_arguments,
                        matching::Substructural::default(),
                        |index| SubMemberSymbolLocation {
                            index,
                            from_parent: true,
                        },
                    )
                    .and_then(|x| {
                        lhs.member_generic_arguments.substructural_match(
                            &rhs.member_generic_arguments,
                            x,
                            |index| SubMemberSymbolLocation {
                                index,
                                from_parent: false,
                            },
                        )
                    })
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
    type TraitMember = symbol::TraitConstant;

    fn as_generic_parameter(&self) -> Option<&ConstantParameterID> {
        self.as_parameter()
    }

    fn as_generic_parameter_mut(&mut self) -> Option<&mut ConstantParameterID> {
        self.as_parameter_mut()
    }

    fn into_generic_parameter(self) -> Result<ConstantParameterID, Self> {
        self.into_parameter()
    }

    fn as_trait_member(&self) -> Option<&TraitMember> {
        match self {
            Self::TraitMember(id) => Some(id),
            _ => None,
        }
    }

    fn as_trait_member_mut(&mut self) -> Option<&mut TraitMember> {
        match self {
            Self::TraitMember(id) => Some(id),
            _ => None,
        }
    }

    fn into_trait_member(self) -> Result<TraitMember, Self> {
        match self {
            Self::TraitMember(id) => Ok(id),
            x => Err(x),
        }
    }

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

    fn outlives_predicates<'a>(
        _: &'a Premise,
    ) -> impl Iterator<Item = &'a Outlives<Self>>
    where
        Self: 'a,
    {
        std::iter::empty()
    }

    fn constant_type_predicates<'a>(
        _: &'a Premise,
    ) -> impl Iterator<Item = &'a Self>
    where
        Self: 'a,
    {
        std::iter::empty()
    }

    fn tuple_predicates<'a>(
        premise: &'a Premise,
    ) -> impl Iterator<Item = &'a Self>
    where
        Self: 'a,
    {
        premise
            .non_equality_predicates
            .iter()
            .filter_map(|x| x.as_tuple_constant().map(|x| &x.0))
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Parameter(_) | Self::Inference(_) => {
                Satisfiability::Unsatisfied
            }

            Self::Phantom(Phantom) | Self::Primitive(_) => {
                Satisfiability::Satisfied
            }

            Self::TraitMember(_)
            | Self::Struct(_)
            | Self::Enum(_)
            | Self::Array(_)
            | Self::Local(_)
            | Self::Tuple(_)
            | Self::Symbol(_)
            | Self::MemberSymbol(_) => Satisfiability::Congruent,
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
            Self::Symbol(symbol) => {
                let mut occurrences = symbol
                    .generic_arguments
                    .get_global_id_dependencies(table)?;
                occurrences.push(symbol.id.into());

                occurrences
            }
            Self::MemberSymbol(member_symbol) => {
                let mut occurrences = member_symbol
                    .parent_generic_arguments
                    .get_global_id_dependencies(table)?;
                occurrences.extend(
                    member_symbol
                        .member_generic_arguments
                        .get_global_id_dependencies(table)?,
                );

                occurrences.push(member_symbol.id.into());

                match member_symbol.id {
                    MemberSymbolID::TraitImplementation(id) => {
                        let implementation_id = table.get(id)?.parent_id;
                        let trait_id = table
                            .get(implementation_id)?
                            .signature
                            .implemented_id;

                        occurrences.push(implementation_id.into());
                        occurrences.push(trait_id.into());
                    }
                    MemberSymbolID::AdtImplementation(id) => {
                        let implementation_id = table.get(id)?.parent_id;
                        let adt_kind_id = table
                            .get(implementation_id)?
                            .signature
                            .implemented_id;

                        occurrences.push(implementation_id.into());
                        occurrences.push(adt_kind_id.into());
                    }
                }

                occurrences
            }
            Self::TraitMember(member_symbol) => {
                let mut occurrences = member_symbol
                    .parent_generic_arguments
                    .get_global_id_dependencies(table)?;
                occurrences.extend(
                    member_symbol
                        .member_generic_arguments
                        .get_global_id_dependencies(table)?,
                );

                occurrences.push(member_symbol.id.into());
                occurrences.push(table.get(member_symbol.id)?.parent_id.into());

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
            Self::Symbol(symol) => {
                write!(f, "{}", DisplayObject { display: symol, table })
            }
            Self::MemberSymbol(symbol) => {
                write!(f, "{}", DisplayObject { display: symbol, table })
            }
            Self::TraitMember(symbol) => {
                write!(f, "{}", DisplayObject { display: symbol, table })
            }
        }
    }
}

#[cfg(test)]
mod tests;
