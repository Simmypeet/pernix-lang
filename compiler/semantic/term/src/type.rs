//! Contains the definition of [`Type`] term.

use std::{fmt::Write, ops::Deref};

use enum_as_inner::EnumAsInner;
use pernixc_target::Global;
use qbice::{Decode, Encode, Identifiable, StableHash};

use crate::{
    constant::Constant,
    error::Error,
    generic_arguments::{
        AssociatedSymbol, GenericArguments, SubAssociatedSymbolLocation,
        SubSymbolLocation, Symbol,
    },
    generic_parameters::{
        GenericParameter, TypeParameter, TypeParameterID,
        get_generic_parameters,
    },
    inference,
    instance::{
        Instance, InstanceAssociated, SubInstanceAssociatedGenericArgsLocation,
    },
    lifetime::Lifetime,
    matching::{Match, Matching, Substructural},
    sub_term::{self, Location, SubTerm, TermLocation},
    tuple::{self, SubTupleLocation},
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// A qualifier that can be applied to references/pointers.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum Qualifier {
    #[display("immutable")]
    Immutable,
    #[display("mutable")]
    Mutable,
}

/// Represents a pointer type, denoted by `*mutable? TYPE` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Pointer {
    /// Determines whether the pointer is mutable.
    pub mutable: bool,

    /// The type that the pointer points to.
    pub pointee: Box<Type>,
}

/// Represents a reference type, denoted by `&'LIFETIME QUALIFIER TYPE` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Reference {
    /// The qualifier applied to the reference.
    pub qualifier: Qualifier,

    /// The lifetime that the reference lives in.
    pub lifetime: Lifetime,

    /// The type that the reference points to.
    pub pointee: Box<Type>,
}

/// Represents an array type, denoted by `[ELEMENT: LENGTH]` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Array {
    /// Constant representing the length of the array.
    pub length: Constant,

    /// The type of the elements in the array.
    pub r#type: Box<Type>,
}

/// Contains all primitive types in the language.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    StableHash,
    Encode,
    Decode,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum Primitive {
    #[display("int8")]
    Int8,
    #[display("int16")]
    Int16,
    #[display("int32")]
    Int32,
    #[display("int64")]
    Int64,
    #[display("uint8")]
    Uint8,
    #[display("uint16")]
    Uint16,
    #[display("uint32")]
    Uint32,
    #[display("uint64")]
    Uint64,
    #[display("float32")]
    Float32,
    #[display("float64")]
    Float64,
    #[display("bool")]
    Bool,
    #[display("usize")]
    Usize,
    #[display("isize")]
    Isize,
}

/// Represents a tuple type, denoted by `(type, type, ...type)` syntax.
pub type Tuple = crate::tuple::Tuple<Type>;

/// Represents a phantom type, denoted by `phantom TYPE` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Phantom(pub Box<Type>);

/// Represents a function signature object. Can be used to represent a function
/// pointer.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct FunctionSignature {
    /// The list of function parameters
    pub parameters: Vec<Type>,

    /// The return type of the function
    pub return_type: Box<Type>,
}

/// A location pointing to either a parameter or the return type of a function
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
pub enum SubFunctionSignatureLocation {
    /// Points to a parameter at the given index in the function signature
    Parameter(usize),

    /// Points to the return type of the function signature
    ReturnType,
}

/// Represents a type term
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Identifiable,
    derive_more::From,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Type {
    #[from]
    Inference(inference::Variable<Self>),
    #[from]
    Primitive(Primitive),
    #[from]
    Parameter(TypeParameterID),
    #[from]
    Symbol(Symbol),
    #[from]
    Pointer(Pointer),
    #[from]
    Reference(Reference),
    #[from]
    Array(Array),
    #[from]
    Tuple(Tuple),
    #[from]
    Phantom(Phantom),
    #[from]
    InstanceAssociated(InstanceAssociated),
    #[from]
    AssociatedSymbol(AssociatedSymbol),
    #[from]
    FunctionSignature(FunctionSignature),
    #[from]
    Error(Error),
}

impl Default for Type {
    fn default() -> Self { Self::Tuple(Tuple::unit()) }
}

impl Type {
    /// Creates a unit type, which is represented as an empty tuple.
    #[must_use]
    pub const fn unit() -> Self { Self::Tuple(Tuple::unit()) }

    /// Creates a boolean type.
    #[must_use]
    pub const fn bool() -> Self { Self::Primitive(Primitive::Bool) }

    /// Creates a type parameter with the given symbol ID where the type
    /// parameter is declared and the ID of the type parameter in the
    /// generic parameters arena.
    #[must_use]
    pub fn new_parameter(
        parent_global_id: Global<pernixc_symbol::SymbolID>,
        ty_id: pernixc_arena::ID<TypeParameter>,
    ) -> Self {
        Self::Parameter(TypeParameterID::new(parent_global_id, ty_id))
    }

    /// Creates a new tuple type from the given list of types.
    #[must_use]
    pub fn new_tuple(elements: Vec<tuple::Element<Self>>) -> Self {
        Self::Tuple(Tuple::new(elements))
    }

    /// Creates a new instance associated type with the given instance, trait
    /// associated symbol ID and generic arguments.
    #[must_use]
    pub const fn new_instance_associated(
        instance: Box<Instance>,
        trait_associated_symbol_id: Global<pernixc_symbol::SymbolID>,
        trait_associated_symbol_generic_arguments: GenericArguments,
    ) -> Self {
        Self::InstanceAssociated(InstanceAssociated::new(
            instance,
            trait_associated_symbol_id,
            trait_associated_symbol_generic_arguments,
        ))
    }

    /// Creates a new symbol type with the given symbol ID and generic
    /// arguments.
    #[must_use]
    pub const fn new_symbol(
        symbol_id: Global<pernixc_symbol::SymbolID>,
        generic_arguments: GenericArguments,
    ) -> Self {
        Self::Symbol(Symbol::new(symbol_id, generic_arguments))
    }

    /// Keeps removing the reference until it reaches a non-reference type.
    ///
    /// This is useful for pattern matching.
    #[must_use]
    pub const fn reduce_reference(mut self: &Self) -> &Self {
        while let Self::Reference(reference) = self {
            self = &*reference.pointee;
        }
        self
    }

    /// Creates an immutable reference type to `self` with the given lifetime.
    #[must_use]
    pub fn to_immutable_reference(self, lifetime: Lifetime) -> Self {
        Self::Reference(Reference {
            qualifier: Qualifier::Immutable,
            lifetime,
            pointee: Box::new(self),
        })
    }
}

impl TryFrom<Type> for Tuple {
    type Error = Type;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
            Type::Tuple(tuple) => Ok(tuple),
            _ => Err(value),
        }
    }
}

/// Represents a sub-instance location within a [`Type::InstanceAssociated`]
/// variant.
///
/// This is used specifically for [`SubInstanceLocation`] to handle both the
/// direct `instance` field and instances within the generic arguments.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum SubInstanceAssociatedInstanceLocation {
    /// The direct `instance` field of the [`InstanceAssociated`].
    Instance,

    /// An instance within the `trait_associated_symbol_generic_arguments`.
    GenericArguments(SubInstanceAssociatedGenericArgsLocation),
}

/// The location pointing to a sub-lifetime term in a type.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
pub enum SubLifetimeLocation {
    /// The index of lifetime argument in a [`Type::Symbol`] type.
    #[from]
    Symbol(SubSymbolLocation),

    /// The lifetime of a reference.
    Reference,

    /// A lifetime argument in a [`Type::AssociatedSymbol`] variant.
    #[from]
    AssociatedSymbol(SubAssociatedSymbolLocation),

    /// A lifetime argument in a [`Type::InstanceAssociated`] variant.
    #[from]
    InstanceAssociated(SubInstanceAssociatedGenericArgsLocation),
}

impl From<SubLifetimeLocation> for TermLocation {
    fn from(value: SubLifetimeLocation) -> Self {
        Self::Lifetime(sub_term::SubLifetimeLocation::FromType(value))
    }
}

impl Location<Type, Lifetime> for SubLifetimeLocation {
    fn assign_sub_term(self, term: &mut Type, sub_term: Lifetime) {
        let reference = match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                &mut reference.lifetime
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location).unwrap()
            }

            (
                Type::AssociatedSymbol(member_symbol),
                Self::AssociatedSymbol(location),
            ) => member_symbol.get_term_mut(location).unwrap(),

            (
                Type::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(location),
            ) => instance_associated.get_lifetime_mut(location).unwrap(),

            term => panic!(
                "invalid sub-lifetime location: {self:?} for term: {term:?}"
            ),
        };

        *reference = sub_term;
    }

    fn get_sub_term(self, term: &Type) -> Option<Lifetime> {
        match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                Some(reference.lifetime.clone())
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Type::AssociatedSymbol(member_symbol),
                Self::AssociatedSymbol(location),
            ) => member_symbol.get_term(location).cloned(),

            (
                Type::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(location),
            ) => instance_associated.get_lifetime(location).cloned(),

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type) -> Option<&Lifetime> {
        match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                Some(&reference.lifetime)
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location)
            }

            (
                Type::AssociatedSymbol(member_symbol),
                Self::AssociatedSymbol(location),
            ) => member_symbol.get_term(location),

            (
                Type::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(location),
            ) => instance_associated.get_lifetime(location),

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Type) -> Option<&mut Lifetime> {
        match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                Some(&mut reference.lifetime)
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location)
            }

            (
                Type::AssociatedSymbol(member_symbol),
                Self::AssociatedSymbol(location),
            ) => member_symbol.get_term_mut(location),

            (
                Type::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(location),
            ) => instance_associated.get_lifetime_mut(location),

            _ => None,
        }
    }
}

/// The location pointing to a sub-type term in a type.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
pub enum SubTypeLocation {
    /// The index of the type argument in a [`Type::Symbol`] type.
    #[from]
    Symbol(SubSymbolLocation),

    /// The [`Pointer::pointee`] of a pointer.
    Pointer,

    /// The [`Reference::pointee`] of a reference.
    Reference,

    /// The [`Array::type`] of an array.
    Array,

    /// The index of the type element in a [`Type::Tuple`] type.
    #[from]
    Tuple(SubTupleLocation),

    /// The inner type of a [`Type::Phantom`] type.
    Phantom,

    /// The type argument in a [`Type::AssociatedSymbol`] type.
    #[from]
    AssociatedSymbol(SubAssociatedSymbolLocation),

    /// The return type or a parameter of a function signature.
    #[from]
    FunctionSignature(SubFunctionSignatureLocation),

    /// A type argument in a [`Type::InstanceAssociated`] type.
    #[from]
    InstanceAssociated(SubInstanceAssociatedGenericArgsLocation),
}

impl From<SubTypeLocation> for TermLocation {
    fn from(value: SubTypeLocation) -> Self {
        Self::Type(sub_term::SubTypeLocation::FromType(value))
    }
}

impl Location<Type, Type> for SubTypeLocation {
    fn assign_sub_term(self, term: &mut Type, sub_term: Type) {
        let reference = match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term_mut(location).unwrap()
            }

            (Self::Pointer, Type::Pointer(pointer)) => &mut *pointer.pointee,

            (Self::Phantom, Type::Phantom(phantom)) => &mut *phantom.0,

            (Self::Reference, Type::Reference(reference)) => {
                &mut *reference.pointee
            }

            (Self::Array, Type::Array(array)) => &mut *array.r#type,

            (Self::Tuple(location), Type::Tuple(tuple)) => {
                return tuple.assign_sub_term(location, sub_term);
            }

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(member_symbol),
            ) => member_symbol.get_term_mut(location).unwrap(),

            (
                Self::FunctionSignature(location),
                Type::FunctionSignature(function_signature),
            ) => match location {
                SubFunctionSignatureLocation::Parameter(idx) => {
                    function_signature.parameters.get_mut(idx)
                }
                SubFunctionSignatureLocation::ReturnType => {
                    Some(&mut *function_signature.return_type)
                }
            }
            .unwrap(),

            (
                Self::InstanceAssociated(location),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_type_mut(location).unwrap(),

            term => {
                panic!("invalid sub-type location: {self:?} for term: {term:?}")
            }
        };

        *reference = sub_term;
    }

    fn get_sub_term(self, term: &Type) -> Option<Type> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::Pointer, Type::Pointer(pointer)) => {
                Some((*pointer.pointee).clone())
            }

            (Self::Reference, Type::Reference(reference)) => {
                Some((*reference.pointee).clone())
            }

            (Self::Array, Type::Array(array)) => Some((*array.r#type).clone()),

            (Self::Tuple(location), Type::Tuple(tuple)) => {
                tuple.get_term(&location)
            }

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(trait_member),
            ) => trait_member.get_term(location).cloned(),

            (Self::Phantom, Type::Phantom(phantom)) => {
                Some((*phantom.0).clone())
            }

            (
                Self::FunctionSignature(location),
                Type::FunctionSignature(signature),
            ) => match location {
                SubFunctionSignatureLocation::Parameter(idx) => {
                    signature.parameters.get(idx).cloned()
                }
                SubFunctionSignatureLocation::ReturnType => {
                    Some((*signature.return_type).clone())
                }
            },

            (
                Self::InstanceAssociated(location),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_type(location).cloned(),

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type) -> Option<&Type> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location)
            }

            (Self::Pointer, Type::Pointer(pointer)) => Some(&*pointer.pointee),

            (Self::Reference, Type::Reference(reference)) => {
                Some(&*reference.pointee)
            }

            (Self::Array, Type::Array(array)) => Some(&*array.r#type),

            (Self::Tuple(location), Type::Tuple(tuple)) => {
                tuple.get_term_ref(&location)
            }

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(member_symbol),
            ) => member_symbol.get_term(location),

            (Self::Phantom, Type::Phantom(phantom)) => Some(&*phantom.0),

            (
                Self::FunctionSignature(location),
                Type::FunctionSignature(signature),
            ) => match location {
                SubFunctionSignatureLocation::Parameter(idx) => {
                    signature.parameters.get(idx)
                }
                SubFunctionSignatureLocation::ReturnType => {
                    Some(&*signature.return_type)
                }
            },

            (
                Self::InstanceAssociated(location),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_type(location),

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Type) -> Option<&mut Type> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term_mut(location)
            }

            (Self::Pointer, Type::Pointer(pointer)) => {
                Some(&mut *pointer.pointee)
            }

            (Self::Reference, Type::Reference(reference)) => {
                Some(&mut *reference.pointee)
            }

            (Self::Array, Type::Array(array)) => Some(&mut *array.r#type),

            (Self::Tuple(location), Type::Tuple(tuple)) => {
                tuple.get_term_mut(&location)
            }

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(member_symbol),
            ) => member_symbol.get_term_mut(location),

            (Self::Phantom, Type::Phantom(phantom)) => Some(&mut *phantom.0),

            (
                Self::FunctionSignature(location),
                Type::FunctionSignature(signature),
            ) => match location {
                SubFunctionSignatureLocation::Parameter(idx) => {
                    signature.parameters.get_mut(idx)
                }
                SubFunctionSignatureLocation::ReturnType => {
                    Some(&mut *signature.return_type)
                }
            },

            (
                Self::InstanceAssociated(location),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_type_mut(location),

            _ => None,
        }
    }
}

/// The location pointing to a sub-constant term in a type.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
pub enum SubConstantLocation {
    /// The index of the constant argument in a [`Type::Symbol`] type.
    #[from]
    Symbol(SubSymbolLocation),

    /// The constant argument in a [`Type::AssociatedSymbol`] type.
    #[from]
    AssociatedSymbol(SubAssociatedSymbolLocation),

    /// The [`Array::length`] of an array.
    Array,

    /// A constant argument in a [`Type::InstanceAssociated`] type.
    #[from]
    InstanceAssociated(SubInstanceAssociatedGenericArgsLocation),
}

impl From<SubConstantLocation> for TermLocation {
    fn from(value: SubConstantLocation) -> Self {
        Self::Constant(sub_term::SubConstantLocation::FromType(value))
    }
}

impl Location<Type, Constant> for SubConstantLocation {
    fn assign_sub_term(self, term: &mut Type, sub_term: Constant) {
        let reference = match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term_mut(location).unwrap()
            }

            (Self::Array, Type::Array(array)) => &mut array.length,

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(member_symbol),
            ) => member_symbol.get_term_mut(location).unwrap(),

            (
                Self::InstanceAssociated(location),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_constant_mut(location).unwrap(),

            term => panic!(
                "invalid sub-constant location: {self:?} for term: {term:?}"
            ),
        };

        *reference = sub_term;
    }

    fn get_sub_term(self, term: &Type) -> Option<Constant> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::Array, Type::Array(array)) => Some(array.length.clone()),

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(member_symbol),
            ) => member_symbol.get_term(location).cloned(),

            (
                Self::InstanceAssociated(location),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_constant(location).cloned(),

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type) -> Option<&Constant> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location)
            }

            (Self::Array, Type::Array(array)) => Some(&array.length),

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(member_symbol),
            ) => member_symbol.get_term(location),

            (
                Self::InstanceAssociated(location),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_constant(location),

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Type) -> Option<&mut Constant> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term_mut(location)
            }

            (Self::Array, Type::Array(array)) => Some(&mut array.length),

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(member_symbol),
            ) => member_symbol.get_term_mut(location),

            (
                Self::InstanceAssociated(location),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_constant_mut(location),

            _ => None,
        }
    }
}

/// The location pointing to a sub-instance term in a type.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
pub enum SubInstanceLocation {
    /// The index of instance argument in a [`Type::Symbol`] type.
    #[from]
    Symbol(SubSymbolLocation),

    /// An instance argument in a [`Type::AssociatedSymbol`] variant.
    #[from]
    AssociatedSymbol(SubAssociatedSymbolLocation),

    /// An instance argument in a [`Type::InstanceAssociated`] variant.
    #[from]
    InstanceAssociated(SubInstanceAssociatedInstanceLocation),
}

impl From<SubInstanceAssociatedGenericArgsLocation> for SubInstanceLocation {
    fn from(val: SubInstanceAssociatedGenericArgsLocation) -> Self {
        Self::InstanceAssociated(
            SubInstanceAssociatedInstanceLocation::GenericArguments(val),
        )
    }
}

impl From<SubInstanceLocation> for TermLocation {
    fn from(value: SubInstanceLocation) -> Self {
        Self::Instance(sub_term::SubInstanceLocation::FromType(value))
    }
}

impl Location<Type, Instance> for SubInstanceLocation {
    fn assign_sub_term(self, term: &mut Type, sub_term: Instance) {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                *symbol.get_term_mut(location).unwrap() = sub_term;
            }

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(member_symbol),
            ) => {
                *member_symbol.get_term_mut(location).unwrap() = sub_term;
            }

            (
                Self::InstanceAssociated(
                    SubInstanceAssociatedInstanceLocation::Instance,
                ),
                Type::InstanceAssociated(instance_associated),
            ) => {
                *instance_associated.instance_mut() = sub_term;
            }

            (
                Self::InstanceAssociated(
                    SubInstanceAssociatedInstanceLocation::GenericArguments(
                        idx,
                    ),
                ),
                Type::InstanceAssociated(instance_associated),
            ) => {
                *instance_associated.get_instance_mut(idx).unwrap() = sub_term;
            }

            term => panic!(
                "invalid sub-instance location: {self:?} for term: {term:?}"
            ),
        }
    }

    fn get_sub_term(self, term: &Type) -> Option<Instance> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(member_symbol),
            ) => member_symbol.get_term(location).cloned(),

            (
                Self::InstanceAssociated(
                    SubInstanceAssociatedInstanceLocation::Instance,
                ),
                Type::InstanceAssociated(instance_associated),
            ) => Some(instance_associated.instance().clone()),

            (
                Self::InstanceAssociated(
                    SubInstanceAssociatedInstanceLocation::GenericArguments(
                        idx,
                    ),
                ),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_instance(idx).cloned(),

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type) -> Option<&Instance> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location)
            }

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(member_symbol),
            ) => member_symbol.get_term(location),

            (
                Self::InstanceAssociated(
                    SubInstanceAssociatedInstanceLocation::Instance,
                ),
                Type::InstanceAssociated(instance_associated),
            ) => Some(instance_associated.instance()),

            (
                Self::InstanceAssociated(
                    SubInstanceAssociatedInstanceLocation::GenericArguments(
                        idx,
                    ),
                ),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_instance(idx),

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Type) -> Option<&mut Instance> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term_mut(location)
            }

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(member_symbol),
            ) => member_symbol.get_term_mut(location),

            (
                Self::InstanceAssociated(
                    SubInstanceAssociatedInstanceLocation::Instance,
                ),
                Type::InstanceAssociated(instance_associated),
            ) => Some(instance_associated.instance_mut()),

            (
                Self::InstanceAssociated(
                    SubInstanceAssociatedInstanceLocation::GenericArguments(
                        idx,
                    ),
                ),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_instance_mut(idx),

            _ => None,
        }
    }
}

impl SubTerm for Type {
    type SubLifetimeLocation = SubLifetimeLocation;
    type SubTypeLocation = SubTypeLocation;
    type SubConstantLocation = SubConstantLocation;
    type SubInstanceLocation = SubInstanceLocation;
    type ThisSubTermLocation = SubTypeLocation;
}

impl Match for Type {
    #[allow(clippy::too_many_lines)]
    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
            Self::SubInstanceLocation,
        >,
    > {
        match (self, other) {
            (Self::Symbol(lhs), Self::Symbol(rhs)) if lhs.id() == rhs.id() => {
                lhs.generic_arguments().substructural_match(
                    rhs.generic_arguments(),
                    Substructural::default(),
                    SubSymbolLocation::new,
                )
            }

            (Self::Pointer(lhs), Self::Pointer(rhs))
                if lhs.mutable == rhs.mutable =>
            {
                Some(Substructural::new(
                    Vec::new(),
                    vec![Matching::new(
                        (*lhs.pointee).clone(),
                        (*rhs.pointee).clone(),
                        SubTypeLocation::Pointer,
                        SubTypeLocation::Pointer,
                    )],
                    Vec::new(),
                    Vec::new(),
                ))
            }

            (Self::Reference(lhs), Self::Reference(rhs))
                if lhs.qualifier == rhs.qualifier =>
            {
                Some(Substructural::new(
                    vec![Matching::new(
                        lhs.lifetime.clone(),
                        rhs.lifetime.clone(),
                        SubLifetimeLocation::Reference,
                        SubLifetimeLocation::Reference,
                    )],
                    vec![Matching::new(
                        (*lhs.pointee).clone(),
                        (*rhs.pointee).clone(),
                        SubTypeLocation::Reference,
                        SubTypeLocation::Reference,
                    )],
                    Vec::new(),
                    Vec::new(),
                ))
            }

            (Self::Array(lhs), Self::Array(rhs)) => Some(Substructural::new(
                Vec::new(),
                vec![Matching::new(
                    (*lhs.r#type).clone(),
                    (*rhs.r#type).clone(),
                    SubTypeLocation::Array,
                    SubTypeLocation::Array,
                )],
                vec![Matching::new(
                    lhs.length.clone(),
                    rhs.length.clone(),
                    SubConstantLocation::Array,
                    SubConstantLocation::Array,
                )],
                Vec::new(),
            )),

            (Self::Tuple(lhs), Self::Tuple(rhs)) => {
                lhs.substructural_match(rhs)
            }

            (Self::AssociatedSymbol(lhs), Self::AssociatedSymbol(rhs))
                if lhs.id() == rhs.id() =>
            {
                lhs.parent_generic_arguments()
                    .substructural_match(
                        rhs.parent_generic_arguments(),
                        Substructural::default(),
                        |x| SubAssociatedSymbolLocation::new(x, true),
                    )
                    .and_then(|x| {
                        lhs.member_generic_arguments().substructural_match(
                            rhs.member_generic_arguments(),
                            x,
                            |x| SubAssociatedSymbolLocation::new(x, false),
                        )
                    })
            }

            (Self::Phantom(lhs), Self::Phantom(rhs)) => {
                Some(Substructural::new(
                    Vec::new(),
                    vec![Matching::new(
                        (*lhs.0).clone(),
                        (*rhs.0).clone(),
                        SubTypeLocation::Phantom,
                        SubTypeLocation::Phantom,
                    )],
                    Vec::new(),
                    Vec::new(),
                ))
            }

            (Self::FunctionSignature(lhs), Self::FunctionSignature(rhs)) => {
                if lhs.parameters.len() != rhs.parameters.len() {
                    return None;
                }

                let mut substructural = Substructural::default();

                for (i, (lhs, rhs)) in
                    lhs.parameters.iter().zip(rhs.parameters.iter()).enumerate()
                {
                    substructural.types_mut().push(Matching::new(
                        lhs.clone(),
                        rhs.clone(),
                        SubTypeLocation::FunctionSignature(
                            SubFunctionSignatureLocation::Parameter(i),
                        ),
                        SubTypeLocation::FunctionSignature(
                            SubFunctionSignatureLocation::Parameter(i),
                        ),
                    ));
                }

                substructural.types_mut().push(Matching::new(
                    (*lhs.return_type).clone(),
                    (*rhs.return_type).clone(),
                    SubTypeLocation::FunctionSignature(
                        SubFunctionSignatureLocation::ReturnType,
                    ),
                    SubTypeLocation::FunctionSignature(
                        SubFunctionSignatureLocation::ReturnType,
                    ),
                ));

                Some(substructural)
            }

            (Self::InstanceAssociated(lhs), Self::InstanceAssociated(rhs))
                if lhs.trait_associated_symbol_id()
                    == rhs.trait_associated_symbol_id() =>
            {
                lhs.substructural_match(
                    rhs,
                    |idx| {
                        SubLifetimeLocation::InstanceAssociated(
                            SubInstanceAssociatedGenericArgsLocation::new(idx),
                        )
                    },
                    |idx| {
                        SubTypeLocation::InstanceAssociated(
                            SubInstanceAssociatedGenericArgsLocation::new(idx),
                        )
                    },
                    |idx| {
                        SubConstantLocation::InstanceAssociated(
                            SubInstanceAssociatedGenericArgsLocation::new(idx),
                        )
                    },
                    |idx| {
                        SubInstanceLocation::InstanceAssociated(
                        SubInstanceAssociatedInstanceLocation::GenericArguments(
                            SubInstanceAssociatedGenericArgsLocation::new(idx),
                        ),
                    )
                    },
                    SubInstanceLocation::InstanceAssociated(
                        SubInstanceAssociatedInstanceLocation::Instance,
                    ),
                )
            }

            _ => None,
        }
    }

    fn get_substructural(
        substructural: &Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
            Self::SubInstanceLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>> {
        substructural.types()
    }

    fn get_substructural_mut(
        substructural: &mut Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
            Self::SubInstanceLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>> {
        substructural.types_mut()
    }
}

impl crate::display::Display for Type {
    async fn fmt(
        &self,
        engine: &pernixc_qbice::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::Inference(inference) => {
                let Some(rendering) = formatter
                    .configuration()
                    .type_inferences()
                    .and_then(|x| x.get(inference))
                else {
                    return write!(formatter, "_");
                };

                match rendering {
                    crate::display::InferenceRendering::Recurse(ty) => {
                        Box::pin(ty.fmt(engine, formatter)).await
                    }
                    crate::display::InferenceRendering::Rendered(flex_str) => {
                        write!(formatter, "{}", flex_str.as_ref())
                    }
                }
            }

            Self::Primitive(primitive) => write!(formatter, "{primitive}"),

            Self::Parameter(member_id) => {
                let generic_parameters =
                    engine.get_generic_parameters(member_id.parent_id()).await;

                write!(
                    formatter,
                    "{}",
                    &**generic_parameters
                        .get_type_parameter(member_id.id())
                        .name()
                )
            }

            Self::Symbol(symbol) => {
                Box::pin(symbol.fmt(engine, formatter)).await
            }

            Self::Pointer(pointer) => {
                write!(
                    formatter,
                    "*{}",
                    if pointer.mutable { "mut " } else { "" }
                )?;

                Box::pin(pointer.pointee.deref().fmt(engine, formatter)).await
            }

            Self::Reference(reference) => {
                write!(formatter, "&")?;

                if formatter
                    .configuration()
                    .lifetime_will_be_displayed(&reference.lifetime)
                {
                    reference.lifetime.fmt(engine, formatter).await?;
                    write!(formatter, " ")?;
                }

                if reference.qualifier == Qualifier::Mutable {
                    write!(formatter, "mut ")?;
                }

                Box::pin(reference.pointee.deref().fmt(engine, formatter)).await
            }

            Self::Array(array) => {
                write!(formatter, "[")?;
                Box::pin(array.r#type.fmt(engine, formatter)).await?;
                write!(formatter, " x ")?;
                Box::pin(array.length.fmt(engine, formatter)).await?;
                write!(formatter, "]")
            }

            Self::Tuple(tuple) => Box::pin(tuple.fmt(engine, formatter)).await,

            Self::Phantom(phantom) => {
                write!(formatter, "phantom ")?;
                Box::pin(phantom.0.fmt(engine, formatter)).await
            }

            Self::AssociatedSymbol(member_symbol) => {
                Box::pin(member_symbol.fmt(engine, formatter)).await
            }

            Self::FunctionSignature(function_signature) => {
                write!(formatter, "function(")?;
                for (i, ty) in function_signature.parameters.iter().enumerate()
                {
                    Box::pin(ty.fmt(engine, formatter)).await?;

                    if i != function_signature.parameters.len() - 1 {
                        write!(formatter, ", ")?;
                    }
                }

                write!(formatter, ") -> ")?;

                Box::pin(function_signature.return_type.fmt(engine, formatter))
                    .await
            }

            Self::InstanceAssociated(instance_associated) => {
                Box::pin(instance_associated.fmt(engine, formatter)).await
            }

            Self::Error(_) => {
                write!(formatter, "{{error}}")
            }
        }
    }
}
