//! Data definitions for type terms.

use enum_as_inner::EnumAsInner;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    constant::Constant,
    error::Error,
    generic_arguments::{AssociatedSymbol, Symbol},
    generic_parameters::{TypeParameter, TypeParameterID},
    inference,
    instance::InstanceAssociated,
    lifetime::Lifetime,
};

/// A qualifier that can be applied to references.
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

/// Represents a pointer type.
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
    mutable: bool,
    pointee: Interned<Type>,
}

/// Represents a reference type.
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
    qualifier: Qualifier,
    lifetime: Interned<Lifetime>,
    pointee: Interned<Type>,
}

/// Represents an array type.
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
    length: Interned<Constant>,
    r#type: Interned<Type>,
}

/// Contains all primitive types.
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

/// Represents a tuple type.
pub type Tuple = crate::tuple::Tuple<Interned<Type>>;

/// Represents a phantom type.
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
pub struct Phantom(Interned<Type>);

/// Represents a function signature.
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
    parameters: Vec<Interned<Type>>,
    return_type: Interned<Type>,
}

impl Pointer {
    /// Creates a new pointer type payload.
    #[must_use]
    pub const fn new(mutable: bool, pointee: Interned<Type>) -> Self {
        Self { mutable, pointee }
    }

    /// Returns whether the pointer is mutable.
    #[must_use]
    pub const fn is_mutable(&self) -> bool { self.mutable }

    /// Returns the pointee type.
    #[must_use]
    pub const fn pointee(&self) -> &Interned<Type> { &self.pointee }
}

impl Reference {
    /// Creates a new reference type payload.
    #[must_use]
    pub const fn new(
        qualifier: Qualifier,
        lifetime: Interned<Lifetime>,
        pointee: Interned<Type>,
    ) -> Self {
        Self { qualifier, lifetime, pointee }
    }

    /// Returns the qualifier.
    #[must_use]
    pub const fn qualifier(&self) -> Qualifier { self.qualifier }

    /// Returns the lifetime.
    #[must_use]
    pub const fn lifetime(&self) -> &Interned<Lifetime> { &self.lifetime }

    /// Returns the pointee type.
    #[must_use]
    pub const fn pointee(&self) -> &Interned<Type> { &self.pointee }
}

impl Array {
    /// Creates a new array type payload.
    #[must_use]
    pub const fn new(
        length: Interned<Constant>,
        r#type: Interned<Type>,
    ) -> Self {
        Self { length, r#type }
    }

    /// Returns the length constant.
    #[must_use]
    pub const fn length(&self) -> &Interned<Constant> { &self.length }

    /// Returns the element type.
    #[must_use]
    pub const fn r#type(&self) -> &Interned<Type> { &self.r#type }
}

impl Phantom {
    /// Creates a new phantom type payload.
    #[must_use]
    pub const fn new(r#type: Interned<Type>) -> Self { Self(r#type) }

    /// Returns the inner phantom type.
    #[must_use]
    pub const fn r#type(&self) -> &Interned<Type> { &self.0 }

    /// Returns the inner phantom type by value.
    #[must_use]
    pub fn into_type(self) -> Interned<Type> { self.0 }
}

impl FunctionSignature {
    /// Creates a new function signature payload.
    #[must_use]
    pub const fn new(
        parameters: Vec<Interned<Type>>,
        return_type: Interned<Type>,
    ) -> Self {
        Self { parameters, return_type }
    }

    /// Returns the function parameters.
    #[must_use]
    pub fn parameters(&self) -> &[Interned<Type>] { &self.parameters }

    /// Returns the function parameters mutably.
    #[must_use]
    pub fn parameters_mut(&mut self) -> &mut [Interned<Type>] {
        &mut self.parameters
    }

    /// Returns the return type.
    #[must_use]
    pub const fn return_type(&self) -> &Interned<Type> { &self.return_type }
}

/// Represents a type term payload.
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
    Inference(inference::Variable<Self>),
    Primitive(Primitive),
    Parameter(TypeParameterID),
    Symbol(Symbol),
    Pointer(Pointer),
    Reference(Reference),
    Array(Array),
    Tuple(Tuple),
    Phantom(Phantom),
    InstanceAssociated(InstanceAssociated),
    AssociatedSymbol(AssociatedSymbol),
    FunctionSignature(FunctionSignature),
    Error(Error),
}

impl Default for Type {
    fn default() -> Self { Self::Tuple(Tuple::unit()) }
}

impl Type {
    /// Creates the unit type payload.
    #[must_use]
    pub const fn unit() -> Self { Self::Tuple(Tuple::unit()) }

    /// Creates the boolean type payload.
    #[must_use]
    pub const fn bool() -> Self { Self::Primitive(Primitive::Bool) }

    /// Creates a type parameter reference.
    #[must_use]
    pub fn new_parameter(
        parent_global_id: Global<pernixc_symbol::SymbolID>,
        type_id: pernixc_arena::ID<TypeParameter>,
    ) -> Self {
        Self::Parameter(TypeParameterID::new(parent_global_id, type_id))
    }
}
