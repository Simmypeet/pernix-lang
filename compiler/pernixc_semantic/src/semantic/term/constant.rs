//! Contains the definition of [`Constant`].

use std::{
    self,
    collections::{HashMap, HashSet},
    ops::Deref,
};

use enum_as_inner::EnumAsInner;

use super::{
    lifetime::Lifetime, r#type::Type, AssignSubTermError, GenericArguments,
    GetVarianceError, Local, Match, MemberSymbol, Never,
    SubMemberSymbolTermLocation, SubSymbolTermLocation, SubTupleTermLocation,
    Substructural, Symbol, Term,
};
use crate::{
    arena::{Arena, ID},
    semantic::{
        predicate::{Outlives, Satisfiability},
        unification::Unification,
        Premise,
    },
    symbol::{
        self, ConstantParameter, ConstantParameterID, GenericID,
        GenericParameters, GlobalID, Variance, Variant,
    },
    table::{State, Table},
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
    Trait(ID<symbol::TraitConstant>),
}

impl From<MemberSymbolKindID> for GlobalID {
    fn from(value: MemberSymbolKindID) -> Self {
        match value {
            MemberSymbolKindID::TraitImplementation(id) => id.into(),
            MemberSymbolKindID::AdtImplementation(id) => id.into(),
            MemberSymbolKindID::Trait(id) => id.into(),
        }
    }
}

impl From<MemberSymbolKindID> for GenericID {
    fn from(value: MemberSymbolKindID) -> Self {
        match value {
            MemberSymbolKindID::TraitImplementation(id) => id.into(),
            MemberSymbolKindID::AdtImplementation(id) => id.into(),
            MemberSymbolKindID::Trait(id) => id.into(),
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
    Tuple(SubTupleTermLocation),

    /// The index of the field in a [`Struct`] constant.
    Struct(usize),

    /// The value in a [`Enum::associated_value`] field (if any).
    Enum,

    /// The index of the element in an [`Array`] constant.
    Array(usize),

    /// The index of the constant argument in a [`Symbol`] constant.
    #[from]
    Symbol(SubSymbolTermLocation),

    /// A constant argument in a [`MemberSymbol`] constant.
    #[from]
    MemberSymbol(SubMemberSymbolTermLocation),

    /// The inner constant of a [`Local`] constant.
    Local,
}

/// The location pointing to a sub-type term in a constant.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubTypeLocation {
    /// The index of the element in a [`Symbol`] constant.
    #[from]
    Symbol(SubSymbolTermLocation),

    /// A constant argument in a [`MemberSymbol`] constant.
    #[from]
    MemberSymbol(SubMemberSymbolTermLocation),
}

/// The location pointing to a sub-lifetime term in a constant.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubLifetimeLocation {
    /// The index of the element in a [`Symbol`] constant.
    #[from]
    Symbol(SubSymbolTermLocation),

    /// A constant argument in a [`MemberSymbol`] constant.
    #[from]
    MemberSymbol(SubMemberSymbolTermLocation),
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

impl Term for Constant {
    type GenericParameter = ConstantParameter;
    type SubConstantLocation = SubConstantLocation;
    type SubLifetimeLocation = SubLifetimeLocation;
    type SubTypeLocation = SubTypeLocation;
    type ThisSubTermLocation = SubConstantLocation;

    #[allow(clippy::too_many_lines)]
    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    > {
        match (self, other) {
            (Self::Struct(lhs), Self::Struct(rhs))
                if lhs.id == rhs.id && lhs.fields.len() == rhs.fields.len() =>
            {
                Some(Substructural {
                    lifetimes: Vec::new(),
                    types: Vec::new(),
                    constants: lhs
                        .fields
                        .iter()
                        .zip(rhs.fields.iter())
                        .enumerate()
                        .map(|(idx, (lhs, rhs))| Match {
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
                    (Some(lhs), Some(rhs)) => Some(Substructural {
                        lifetimes: Vec::new(),
                        types: Vec::new(),
                        constants: vec![Match {
                            lhs: lhs.deref().clone(),
                            rhs: rhs.deref().clone(),
                            lhs_location: SubConstantLocation::Enum,
                            rhs_location: SubConstantLocation::Enum,
                        }],
                    }),
                    (None, None) => Some(Substructural::default()),
                    _ => None,
                }
            }

            (Self::Array(lhs), Self::Array(rhs))
                if lhs.elements.len() == rhs.elements.len() =>
            {
                Some(Substructural {
                    lifetimes: Vec::new(),
                    types: Vec::new(),
                    constants: lhs
                        .elements
                        .iter()
                        .cloned()
                        .zip(rhs.elements.iter().cloned())
                        .enumerate()
                        .map(|(idx, (lhs, rhs))| Match {
                            lhs,
                            rhs,
                            lhs_location: SubConstantLocation::Array(idx),
                            rhs_location: SubConstantLocation::Array(idx),
                        })
                        .collect(),
                })
            }

            (Self::Local(lhs), Self::Local(rhs)) => Some(Substructural {
                lifetimes: Vec::new(),
                types: Vec::new(),
                constants: vec![Match {
                    lhs: lhs.0.deref().clone(),
                    rhs: rhs.0.deref().clone(),
                    lhs_location: SubConstantLocation::Local,
                    rhs_location: SubConstantLocation::Local,
                }],
            }),

            (Self::Tuple(lhs), Self::Tuple(rhs)) => {
                lhs.substructural_match(rhs)
            }

            (Self::Symbol(lhs), Self::Symbol(rhs)) if lhs.id == rhs.id => {
                lhs.generic_arguments.substructural_match(
                    &rhs.generic_arguments,
                    Substructural::default(),
                    SubSymbolTermLocation,
                )
            }

            (Self::MemberSymbol(lhs), Self::MemberSymbol(rhs))
                if lhs.id == rhs.id =>
            {
                lhs.parent_generic_arguments
                    .substructural_match(
                        &rhs.parent_generic_arguments,
                        Substructural::default(),
                        |index| SubMemberSymbolTermLocation {
                            index,
                            from_parent: true,
                        },
                    )
                    .and_then(|x| {
                        lhs.member_generic_arguments.substructural_match(
                            &rhs.member_generic_arguments,
                            x,
                            |index| SubMemberSymbolTermLocation {
                                index,
                                from_parent: false,
                            },
                        )
                    })
            }

            _ => None,
        }
    }

    fn assign_sub_lifetime(
        &mut self,
        location: Self::SubLifetimeLocation,
        sub_lifetime: Lifetime,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (location, self) {
            (SubLifetimeLocation::Symbol(location), Self::Symbol(symbol)) => {
                symbol
                    .get_term_mut(location)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }
            (
                SubLifetimeLocation::MemberSymbol(location),
                Self::MemberSymbol(symbol),
            ) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_lifetime;

        Ok(())
    }

    fn assign_sub_type(
        &mut self,
        location: Self::SubTypeLocation,
        sub_type: Type,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (location, self) {
            (SubTypeLocation::Symbol(location), Self::Symbol(symbol)) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (
                SubTypeLocation::MemberSymbol(location),
                Self::MemberSymbol(symbol),
            ) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_type;

        Ok(())
    }

    fn assign_sub_constant(
        &mut self,
        location: Self::SubConstantLocation,
        sub_constant: Constant,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (location, self) {
            (SubConstantLocation::Tuple(location), Self::Tuple(tuple)) => {
                return tuple.assign_sub_term(location, sub_constant)
            }

            (SubConstantLocation::Struct(location), Self::Struct(constant)) => {
                constant
                    .fields
                    .get_mut(location)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            (
                SubConstantLocation::Enum,
                Self::Enum(Enum { associated_value: Some(constant), .. }),
            ) => constant,

            (SubConstantLocation::Array(location), Self::Array(constant)) => {
                constant
                    .elements
                    .get_mut(location)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            (SubConstantLocation::Symbol(location), Self::Symbol(symbol)) => {
                symbol
                    .get_term_mut(location)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            (
                SubConstantLocation::MemberSymbol(location),
                Self::MemberSymbol(symbol),
            ) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (SubConstantLocation::Local, Self::Local(local)) => &mut local.0,

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_constant;

        Ok(())
    }

    fn get_sub_lifetime_variance(
        &self,
        _: Self::SubLifetimeLocation,
        _: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        Ok(symbol::Variance::Invariant)
    }

    fn get_sub_type_variance(
        &self,
        _: Self::SubTypeLocation,
        _: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        Ok(symbol::Variance::Invariant)
    }

    fn get_sub_constant_variance(
        &self,
        _: Self::SubConstantLocation,
        _: &Table<impl State>,
    ) -> Result<Variance, GetVarianceError> {
        Ok(symbol::Variance::Invariant)
    }

    fn is_tuple(&self) -> bool { matches!(self, Self::Tuple(..)) }

    fn outlives_predicates<'a>(
        _: &'a Premise,
    ) -> impl Iterator<Item = &'a Outlives<Self>>
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

            Self::Struct(_)
            | Self::Enum(_)
            | Self::Array(_)
            | Self::Local(_)
            | Self::Tuple(_)
            | Self::Symbol(_)
            | Self::MemberSymbol(_) => Satisfiability::Congruent,
        }
    }

    fn get_substructural(
        substructural: &Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Match<Self, Self::ThisSubTermLocation>> {
        &substructural.constants
    }

    fn get_substructural_mut(
        substructural: &mut Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Match<Self, Self::ThisSubTermLocation>> {
        &mut substructural.constants
    }

    fn get_unification(
        unification: &Unification,
    ) -> &HashMap<Self, HashSet<Self>> {
        &unification.constants
    }

    fn get_unification_mut(
        unification: &mut Unification,
    ) -> &mut HashMap<Self, HashSet<Self>> {
        &mut unification.constants
    }

    fn get_generic_arguments(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.constants
    }

    fn get_generic_arguments_mut(
        generic_arguments: &mut GenericArguments,
    ) -> &mut Vec<Self> {
        &mut generic_arguments.constants
    }

    fn get_generic_parameters(
        parameters: &GenericParameters,
    ) -> &Arena<Self::GenericParameter> {
        &parameters.constants
    }
}

#[cfg(test)]
mod tests;
