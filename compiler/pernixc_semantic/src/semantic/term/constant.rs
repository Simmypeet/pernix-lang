//! Contains the definition of [`Constant`].

use std::{
    self,
    collections::{HashMap, HashSet},
    ops::Deref,
};

use enum_as_inner::EnumAsInner;

use super::{
    lifetime::Lifetime, r#type::Type, GenericArguments, GetVarianceError,
    Local, MemberSymbol, Never, Symbol, Term,
};
use crate::{
    arena::{Arena, ID},
    semantic::{
        instantiation::Instantiation,
        mapping::Mapping,
        matching::{self, Match},
        predicate::{Outlives, Satisfiability},
        subterm::{
            AssignSubTermError, Location, SubMemberSymbolLocation,
            SubSymbolLocation, SubTraitMemberLocation, SubTupleLocation,
        },
        unification::{self, Unification},
        Premise,
    },
    symbol::{
        self, ConstantParameter, ConstantParameterID, GenericID,
        GenericParameters, GlobalID, MemberID, Variance, Variant,
    },
    table::{Index, State, Table},
};

/// Enumeration of either a trait implementation constant or an ADT
/// implementation constant.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum MemberSymbolKindID {
    TraitImplementation(ID<symbol::TraitImplementationConstant>),
    AdtImplementation(ID<symbol::AdtImplementationConstant>),
}

impl From<MemberSymbolKindID> for GlobalID {
    fn from(value: MemberSymbolKindID) -> Self {
        match value {
            MemberSymbolKindID::TraitImplementation(id) => id.into(),
            MemberSymbolKindID::AdtImplementation(id) => id.into(),
        }
    }
}

impl From<MemberSymbolKindID> for GenericID {
    fn from(value: MemberSymbolKindID) -> Self {
        match value {
            MemberSymbolKindID::TraitImplementation(id) => id.into(),
            MemberSymbolKindID::AdtImplementation(id) => id.into(),
        }
    }
}

/// Represents a primitive constant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Primitive {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
    Bool(bool),
    Usize(usize),
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

    fn get_sub_variance(
        self,
        term: &Constant,
        table: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        match (self, term) {
            (Self::Local, Constant::Local(_))
            | (Self::Array(_), Constant::Array(_))
            | (Self::Enum, Constant::Enum(_))
            | (Self::Struct(_), Constant::Struct(_))
            | (Self::Tuple(_), Constant::Tuple(_)) => Ok(Variance::Covariant),

            (Self::Symbol(location), Constant::Symbol(symbol)) => table
                .get_generic_parameter_variance::<Constant>(
                    symbol.id.into(),
                    location.0,
                ),

            (Self::TraitMember(_), Constant::TraitMember(_)) => {
                Ok(Variance::Invariant)
            }

            (Self::MemberSymbol(location), Constant::MemberSymbol(symbol)) => {
                let id = match (symbol.id, location.from_parent) {
                    (MemberSymbolKindID::TraitImplementation(id), false) => {
                        id.into()
                    }

                    (MemberSymbolKindID::AdtImplementation(id), true) => {
                        let implementation_id = table
                            .get(id)
                            .ok_or(GetVarianceError::InvalidLocation)?
                            .parent_id;

                        let adt_kind_id = table
                            .get(implementation_id)
                            .ok_or(GetVarianceError::InvalidLocation)?
                            .signature
                            .implemented_id;

                        adt_kind_id.into()
                    }
                    (MemberSymbolKindID::AdtImplementation(id), false) => {
                        id.into()
                    }

                    (MemberSymbolKindID::TraitImplementation(_), true) => {
                        return Ok(symbol::Variance::Invariant)
                    }
                };

                table.get_generic_parameter_variance::<Constant>(
                    id,
                    location.index,
                )
            }

            _ => Err(GetVarianceError::InvalidLocation),
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

    fn get_sub_variance(
        self,
        term: &Constant,
        table: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        match (self, term) {
            (Self::Symbol(location), Constant::Symbol(symbol)) => table
                .get_generic_parameter_variance::<Type>(
                    symbol.id.into(),
                    location.0,
                ),

            (Self::TraitMember(_), Constant::TraitMember(_)) => {
                Ok(symbol::Variance::Invariant)
            }

            (Self::MemberSymbol(location), Constant::MemberSymbol(symbol)) => {
                let id = match (symbol.id, location.from_parent) {
                    (MemberSymbolKindID::TraitImplementation(id), false) => {
                        id.into()
                    }

                    (MemberSymbolKindID::AdtImplementation(id), true) => {
                        let implementation_id = table
                            .get(id)
                            .ok_or(GetVarianceError::InvalidLocation)?
                            .parent_id;

                        let adt_kind_id = table
                            .get(implementation_id)
                            .ok_or(GetVarianceError::InvalidLocation)?
                            .signature
                            .implemented_id;

                        adt_kind_id.into()
                    }
                    (MemberSymbolKindID::AdtImplementation(id), false) => {
                        id.into()
                    }

                    (MemberSymbolKindID::TraitImplementation(_), true) => {
                        return Ok(symbol::Variance::Invariant)
                    }
                };

                table.get_generic_parameter_variance::<Type>(id, location.index)
            }

            _ => Err(GetVarianceError::InvalidLocation),
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

    fn get_sub_variance(
        self,
        term: &Constant,
        table: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        match (self, term) {
            (Self::Symbol(location), Constant::Symbol(symbol)) => table
                .get_generic_parameter_variance::<Lifetime>(
                    symbol.id.into(),
                    location.0,
                ),

            (Self::TraitMember(_), Constant::TraitMember(_)) => {
                Ok(symbol::Variance::Invariant)
            }

            (Self::MemberSymbol(location), Constant::MemberSymbol(symbol)) => {
                let id = match (symbol.id, location.from_parent) {
                    (MemberSymbolKindID::TraitImplementation(id), false) => {
                        id.into()
                    }

                    (MemberSymbolKindID::AdtImplementation(id), true) => {
                        let implementation_id = table
                            .get(id)
                            .ok_or(GetVarianceError::InvalidLocation)?
                            .parent_id;

                        let adt_kind_id = table
                            .get(implementation_id)
                            .ok_or(GetVarianceError::InvalidLocation)?
                            .signature
                            .implemented_id;

                        adt_kind_id.into()
                    }
                    (MemberSymbolKindID::AdtImplementation(id), false) => {
                        id.into()
                    }

                    (MemberSymbolKindID::TraitImplementation(_), true) => {
                        return Ok(symbol::Variance::Invariant)
                    }
                };

                table.get_generic_parameter_variance::<Lifetime>(
                    id,
                    location.index,
                )
            }

            _ => Err(GetVarianceError::InvalidLocation),
        }
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

    /// Please notice the differences
    ///
    /// In the **AdtImplementation** case, the `parent_generic_arguments` field
    /// is **not** deduced from the implementation directly, bur rather
    /// from the ADT that the implementation is for.
    ///
    /// In the **TraitImplementation** case, the `parent_generic_arguments`
    /// field **is** deduced from the implementation.
    MemberSymbol(MemberSymbol<MemberSymbolKindID>),

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

impl Match for Constant {
    type SubTypeLocation = SubTypeLocation;
    type SubLifetimeLocation = SubLifetimeLocation;
    type SubConstantLocation = SubConstantLocation;
    type ThisSubTermLocation = Self::SubConstantLocation;

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

    fn as_generic_parameter(
        &self,
    ) -> Option<&MemberID<ID<Self::GenericParameter>, GenericID>> {
        self.as_parameter()
    }

    fn as_generic_parameter_mut(
        &mut self,
    ) -> Option<&mut MemberID<ID<Self::GenericParameter>, GenericID>> {
        self.as_parameter_mut()
    }

    fn into_generic_parameter(
        self,
    ) -> Result<MemberID<ID<Self::GenericParameter>, GenericID>, Self> {
        self.into_parameter()
    }

    fn as_trait_member(
        &self,
    ) -> Option<&MemberSymbol<ID<symbol::TraitConstant>>> {
        match self {
            Self::TraitMember(id) => Some(id),
            _ => None,
        }
    }

    fn as_trait_member_mut(
        &mut self,
    ) -> Option<&mut MemberSymbol<ID<symbol::TraitConstant>>> {
        match self {
            Self::TraitMember(id) => Some(id),
            _ => None,
        }
    }

    fn into_trait_member(
        self,
    ) -> Result<MemberSymbol<ID<symbol::TraitConstant>>, Self> {
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

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Parameter(_) | Self::Inference(_) => {
                Satisfiability::Unsatisfied
            }

            Self::Primitive(_) => Satisfiability::Satisfied,

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
    ) -> &HashMap<MemberID<ID<Self::GenericParameter>, GenericID>, Self> {
        &instantiation.constants
    }

    fn get_instantiation_mut(
        instantiation: &mut Instantiation,
    ) -> &mut HashMap<MemberID<ID<Self::GenericParameter>, GenericID>, Self>
    {
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

    fn get_generic_parameters(
        parameters: &GenericParameters,
    ) -> &Arena<Self::GenericParameter> {
        &parameters.constants
    }

    fn get_generic_parameter_order(
        parameters: &GenericParameters,
    ) -> &[ID<Self::GenericParameter>] {
        &parameters.constant_order
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

#[cfg(test)]
mod tests;
