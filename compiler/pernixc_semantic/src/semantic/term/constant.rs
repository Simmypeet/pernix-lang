//! Contains the definition of [`Constant`].

use std::ops::Deref;

use enum_as_inner::EnumAsInner;

use super::{MemberSymbol, Never, Substructural, Symbol, Term};
use crate::{
    arena::ID,
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

/// Represents a local constant value, denoted by `local CONSTANT` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local {
    /// The constant under local modifier.
    pub constant: Box<Constant>,
}

/// Represents a constant inference variable in hindley-milner type inference.
pub type Inference = Never; /* will be changed */

/// Represents a compile-time constant term.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
#[non_exhaustive]
pub enum Constant {
    Primitive(Primitive),
    Inference(Inference),
    Struct(Struct),
    Enum(Enum),
    Array(Array),
    Parameter(ConstantParameterID),
    Local(Local),
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

impl From<Tuple> for Constant {
    fn from(value: Tuple) -> Self { Self::Tuple(value) }
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

impl From<ConstantParameterID> for Constant {
    fn from(value: ConstantParameterID) -> Self { Self::Parameter(value) }
}

impl Term for Constant {
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
                constants: vec![(lhs.constant.deref().clone(), rhs.constant.deref().clone())],
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

    fn get_substructural(substructural: &Substructural) -> &Vec<(Self, Self)> {
        &substructural.constants
    }

    fn get_substructural_mut(substructural: &mut Substructural) -> &mut Vec<(Self, Self)> {
        &mut substructural.constants
    }
}

#[cfg(test)]
mod tests;
