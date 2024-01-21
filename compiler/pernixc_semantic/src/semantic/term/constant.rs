//! Contains the definition of [`Constant`].

use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
};

use enum_as_inner::EnumAsInner;

use super::{
    lifetime::Lifetime, r#type::Type, Local, MemberSymbol, Never, Substructural, Symbol, Term,
    TupleElement,
};
use crate::{
    arena::ID,
    semantic::{
        predicate::{Outlives, Satisfiability},
        unification::Unification,
        Premise,
    },
    symbol::{self, ConstantParameterID, GenericID, GlobalID, Variant},
};

/// Enumeration of either a trait implementation constant or an ADT implementation constant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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

/// Represents a tuple constant value, denoted by `(value, value, ...value)` syntax.
pub type Tuple = super::Tuple<Constant>;

/// Represents a constant inference variable in hindley-milner type inference.
pub type Inference = Never; /* will be changed */

/// The location pointing to a sub-constant term in a constant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SubConstantLocation {
    /// The index of the element in a [`Tuple`] constant.
    Tuple(usize),

    /// The index of the field in a [`Struct`] constant.
    Struct(usize),

    /// The value in a [`Enum::associated_value`] field (if any).
    AssociatedEnum,

    /// The index of the element in an [`Array`] constant.
    Array(usize),

    /// The index of the constant argument in a [`Symbol`] constant.
    Symbol(usize),

    /// A constant argument in a [`MemberSymbol`] constant.
    MemberSymbol {
        /// True if the constant is in the parent part, false if the constant is in the member
        /// part.
        from_parent: bool,

        /// The index of the constant argument.
        index: usize,
    },

    /// The inner constant of a [`Local`] constant.
    Local,
}

/// Represents a compile-time constant term.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From)]
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

    /// Pleace notice the differences
    ///
    /// In the **AdtImplementation** case, the `parent_generic_arguments` field is **not** deduced
    /// from the implementation directly, bur rather from the ADT that the implementation is for.
    ///
    /// In the **TraitImplementation** case, the `parent_generic_arguments` field **is** deduced
    /// from the implementation.
    MemberSymbol(MemberSymbol<MemberSymbolKindID>),
}

impl TryFrom<Constant> for Tuple {
    type Error = Constant;

    fn try_from(value: Constant) -> Result<Self, Self::Error> { value.into_tuple() }
}

impl TryFrom<Constant> for ConstantParameterID {
    type Error = Constant;

    fn try_from(value: Constant) -> Result<Self, Self::Error> { value.into_parameter() }
}

impl Default for Constant {
    fn default() -> Self {
        Self::Tuple(Tuple {
            elements: Vec::new(),
        })
    }
}

macro_rules! get_sub_constant_impl {
    ($location:ident, $self:ident, $get:ident, $as_deref:ident, $deref:ident) => {
        match ($location, $self) {
            (SubConstantLocation::Tuple(index), Self::Tuple(tuple)) => {
                tuple.elements.$get(index).map(|x| match x {
                    TupleElement::Regular(x) => x,
                    TupleElement::Unpacked(x) => x,
                })
            }
            (SubConstantLocation::Struct(index), Self::Struct(val)) => val.fields.$get(index),
            (SubConstantLocation::AssociatedEnum, Self::Enum(val)) => {
                val.associated_value.$as_deref()
            }
            (SubConstantLocation::Array(index), Self::Array(val)) => val.elements.$get(index),
            (SubConstantLocation::Symbol(index), Self::Symbol(val)) => {
                val.generic_arguments.constants.$get(index)
            }
            (SubConstantLocation::MemberSymbol { from_parent, index }, Self::MemberSymbol(val)) => {
                if from_parent {
                    val.parent_generic_arguments.constants.$get(index)
                } else {
                    val.member_generic_arguments.constants.$get(index)
                }
            }
            (SubConstantLocation::Local, Self::Local(val)) => Some(val.0.$deref()),

            _ => None,
        }
    };
}

impl Term for Constant {
    type SubConstantLocation = SubConstantLocation;
    type SubLifetimeLocation = Never;
    type SubTypeLocation = Never;

    fn get_sub_type(&self, location: Self::SubTypeLocation) -> Option<&Type> { match location {} }

    fn get_sub_type_mut(&mut self, location: Self::SubTypeLocation) -> Option<&mut Type> {
        match location {}
    }

    fn get_sub_lifetime(&self, location: Self::SubLifetimeLocation) -> Option<&Lifetime> {
        match location {}
    }

    fn get_sub_lifetime_mut(
        &mut self,
        location: Self::SubLifetimeLocation,
    ) -> Option<&mut Lifetime> {
        match location {}
    }

    fn get_sub_constant(&self, location: Self::SubConstantLocation) -> Option<&Constant> {
        get_sub_constant_impl!(location, self, get, as_deref, deref)
    }

    fn get_sub_constant_mut(
        &mut self,
        location: Self::SubConstantLocation,
    ) -> Option<&mut Constant> {
        get_sub_constant_impl!(location, self, get_mut, as_deref_mut, deref_mut)
    }

    fn substructural_match(&self, other: &Self) -> Option<Substructural> {
        match (self, other) {
            (Self::Struct(lhs), Self::Struct(rhs))
                if lhs.id == rhs.id && lhs.fields.len() == rhs.fields.len() =>
            {
                let mut result = Substructural::default();
                for (lhs, rhs) in lhs.fields.iter().zip(rhs.fields.iter()) {
                    result.constants.push((lhs.clone(), rhs.clone()));
                }

                Some(result)
            }

            (Self::Enum(lhs), Self::Enum(rhs)) if lhs.variant_id == rhs.variant_id => {
                match (&lhs.associated_value, &rhs.associated_value) {
                    (Some(lhs), Some(rhs)) => Some(Substructural {
                        lifetimes: Vec::new(),
                        types: Vec::new(),
                        constants: vec![(lhs.deref().clone(), rhs.deref().clone())],
                    }),
                    (None, None) => Some(Substructural::default()),
                    _ => None,
                }
            }

            (Self::Array(lhs), Self::Array(rhs)) if lhs.elements.len() == rhs.elements.len() => {
                let mut result = Substructural::default();
                for (lhs, rhs) in lhs.elements.iter().zip(rhs.elements.iter()) {
                    result.constants.push((lhs.clone(), rhs.clone()));
                }

                Some(result)
            }

            (Self::Local(lhs), Self::Local(rhs)) => Some(Substructural {
                lifetimes: Vec::new(),
                types: Vec::new(),
                constants: vec![(lhs.0.deref().clone(), rhs.0.deref().clone())],
            }),

            (Self::Tuple(lhs), Self::Tuple(rhs)) => lhs.substructural_match(rhs),

            (Self::Symbol(lhs), Self::Symbol(rhs)) if lhs.id == rhs.id => lhs
                .generic_arguments
                .substructural_match(&rhs.generic_arguments, Substructural::default()),

            (Self::MemberSymbol(lhs), Self::MemberSymbol(rhs)) if lhs.id == rhs.id => lhs
                .parent_generic_arguments
                .substructural_match(&rhs.parent_generic_arguments, Substructural::default())
                .and_then(|x| {
                    lhs.member_generic_arguments
                        .substructural_match(&rhs.member_generic_arguments, x)
                }),

            _ => None,
        }
    }

    fn is_tuple(&self) -> bool { matches!(self, Self::Tuple(..)) }

    fn outlives_predicates<'a>(_: &'a Premise) -> impl Iterator<Item = &'a Outlives<Self>>
    where
        Self: 'a,
    {
        std::iter::empty()
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Parameter(_) | Self::Inference(_) => Satisfiability::Unsatisfied,

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

    fn get_substructural(substructural: &Substructural) -> &Vec<(Self, Self)> {
        &substructural.constants
    }

    fn get_substructural_mut(substructural: &mut Substructural) -> &mut Vec<(Self, Self)> {
        &mut substructural.constants
    }

    fn get_unification(unification: &Unification) -> &HashMap<Self, HashSet<Self>> {
        &unification.constants
    }

    fn get_unification_mut(unification: &mut Unification) -> &mut HashMap<Self, HashSet<Self>> {
        &mut unification.constants
    }
}

#[cfg(test)]
mod tests;
