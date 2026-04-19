//! Data definitions for type terms.

use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    TermRef,
    constant::Constant,
    error::Error,
    generic_arguments::{
        AssociatedSymbol, SubAssociatedSymbolLocation, SubSymbolLocation,
        Symbol,
    },
    generic_parameters::{TypeParameter, TypeParameterID},
    inference,
    instance::{
        Instance, InstanceAssociated, SubInstanceAssociatedGenericArgsLocation,
    },
    lifetime::Lifetime,
    matching::{Match, Matching, Substructural},
    sub_term::{self, IterSubTerms, SubTerm},
    tuple::SubTupleLocation,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

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
pub type Tuple = crate::tuple::Tuple<Type>;

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

impl TryFrom<Type> for Tuple {
    type Error = Type;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
            Type::Tuple(tuple) => Ok(tuple),
            _ => Err(value),
        }
    }
}

/// A location inside a function signature.
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
    /// A function parameter at a specific index.
    Parameter(usize),

    /// The return type of the function.
    ReturnType,
}

/// A location for instance sub-terms inside an instance-associated type.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum SubInstanceAssociatedInstanceLocation {
    /// The direct `instance` field.
    Instance,

    /// An instance inside the associated symbol generic arguments.
    GenericArguments(SubInstanceAssociatedGenericArgsLocation),
}

/// The location of a lifetime sub-term in a type.
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
    #[from]
    Symbol(SubSymbolLocation),
    Reference,
    #[from]
    AssociatedSymbol(SubAssociatedSymbolLocation),
    #[from]
    InstanceAssociated(SubInstanceAssociatedGenericArgsLocation),
}

impl From<SubLifetimeLocation> for sub_term::TermLocation {
    fn from(value: SubLifetimeLocation) -> Self {
        Self::Lifetime(sub_term::SubLifetimeLocation::FromType(value))
    }
}

impl sub_term::Location<Type, Lifetime> for SubLifetimeLocation {
    fn try_get_sub_term(
        self,
        term: &Type,
        _: &TrackedEngine,
    ) -> Option<Interned<Lifetime>> {
        match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                Some(reference.lifetime().clone())
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Type::AssociatedSymbol(symbol),
                Self::AssociatedSymbol(location),
            ) => symbol.get_term(location).cloned(),

            (
                Type::InstanceAssociated(instance_associated),
                Self::InstanceAssociated(location),
            ) => instance_associated.get_lifetime(location).cloned(),

            _ => None,
        }
    }
}

/// The location of a type sub-term in a type.
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
    #[from]
    Symbol(SubSymbolLocation),
    Pointer,
    Reference,
    Array,
    #[from]
    Tuple(SubTupleLocation),
    Phantom,
    #[from]
    AssociatedSymbol(SubAssociatedSymbolLocation),
    #[from]
    FunctionSignature(SubFunctionSignatureLocation),
    #[from]
    InstanceAssociated(SubInstanceAssociatedGenericArgsLocation),
}

impl From<SubTypeLocation> for sub_term::TermLocation {
    fn from(value: SubTypeLocation) -> Self {
        Self::Type(sub_term::SubTypeLocation::FromType(value))
    }
}

impl sub_term::Location<Type, Type> for SubTypeLocation {
    fn try_get_sub_term(
        self,
        term: &Type,
        tracked_engine: &TrackedEngine,
    ) -> Option<Interned<Type>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::Pointer, Type::Pointer(pointer)) => {
                Some(pointer.pointee().clone())
            }

            (Self::Reference, Type::Reference(reference)) => {
                Some(reference.pointee().clone())
            }

            (Self::Array, Type::Array(array)) => Some(array.r#type().clone()),

            (Self::Tuple(location), Type::Tuple(tuple)) => {
                tuple.get_term(&location, tracked_engine)
            }

            (Self::Phantom, Type::Phantom(phantom)) => {
                Some(phantom.r#type().clone())
            }

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(symbol),
            ) => symbol.get_term(location).cloned(),

            (
                Self::FunctionSignature(
                    SubFunctionSignatureLocation::Parameter(index),
                ),
                Type::FunctionSignature(signature),
            ) => signature.parameters().get(index).cloned(),

            (
                Self::FunctionSignature(
                    SubFunctionSignatureLocation::ReturnType,
                ),
                Type::FunctionSignature(signature),
            ) => Some(signature.return_type().clone()),

            (
                Self::InstanceAssociated(location),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_type(location).cloned(),

            _ => None,
        }
    }
}

/// The location of a constant sub-term in a type.
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
    #[from]
    Symbol(SubSymbolLocation),
    #[from]
    AssociatedSymbol(SubAssociatedSymbolLocation),
    Array,
    #[from]
    InstanceAssociated(SubInstanceAssociatedGenericArgsLocation),
}

impl From<SubConstantLocation> for sub_term::TermLocation {
    fn from(value: SubConstantLocation) -> Self {
        Self::Constant(sub_term::SubConstantLocation::FromType(value))
    }
}

impl sub_term::Location<Type, Constant> for SubConstantLocation {
    fn try_get_sub_term(
        self,
        term: &Type,
        _: &TrackedEngine,
    ) -> Option<Interned<Constant>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::Array, Type::Array(array)) => Some(array.length().clone()),

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(symbol),
            ) => symbol.get_term(location).cloned(),

            (
                Self::InstanceAssociated(location),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_constant(location).cloned(),

            _ => None,
        }
    }
}

/// The location of an instance sub-term in a type.
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
    #[from]
    Symbol(SubSymbolLocation),
    #[from]
    AssociatedSymbol(SubAssociatedSymbolLocation),
    #[from]
    InstanceAssociated(SubInstanceAssociatedInstanceLocation),
}

impl From<SubInstanceAssociatedGenericArgsLocation> for SubInstanceLocation {
    fn from(value: SubInstanceAssociatedGenericArgsLocation) -> Self {
        Self::InstanceAssociated(
            SubInstanceAssociatedInstanceLocation::GenericArguments(value),
        )
    }
}

impl From<SubInstanceLocation> for sub_term::TermLocation {
    fn from(value: SubInstanceLocation) -> Self {
        Self::Instance(sub_term::SubInstanceLocation::FromType(value))
    }
}

impl sub_term::Location<Type, Instance> for SubInstanceLocation {
    fn try_get_sub_term(
        self,
        term: &Type,
        _: &TrackedEngine,
    ) -> Option<Interned<Instance>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (
                Self::AssociatedSymbol(location),
                Type::AssociatedSymbol(symbol),
            ) => symbol.get_term(location).cloned(),

            (
                Self::InstanceAssociated(
                    SubInstanceAssociatedInstanceLocation::Instance,
                ),
                Type::InstanceAssociated(instance_associated),
            ) => Some(instance_associated.instance().clone()),

            (
                Self::InstanceAssociated(
                    SubInstanceAssociatedInstanceLocation::GenericArguments(
                        location,
                    ),
                ),
                Type::InstanceAssociated(instance_associated),
            ) => instance_associated.get_instance(location).cloned(),

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
    fn substructural_match<'a>(
        &'a self,
        other: &'a Self,
    ) -> Option<
        impl Iterator<
            Item = Substructural<
                Self::SubLifetimeLocation,
                Self::SubTypeLocation,
                Self::SubConstantLocation,
                Self::SubInstanceLocation,
            >,
        > + 'a,
    > {
        enum MatchPlan<'a> {
            Symbol(&'a Symbol, &'a Symbol),
            Pointer(&'a Pointer, &'a Pointer),
            Reference(&'a Reference, &'a Reference),
            Array(&'a Array, &'a Array),
            Tuple(&'a Tuple, &'a Tuple),
            Phantom(&'a Phantom, &'a Phantom),
            AssociatedSymbol(&'a AssociatedSymbol, &'a AssociatedSymbol),
            FunctionSignature(&'a FunctionSignature, &'a FunctionSignature),
            InstanceAssociated(&'a InstanceAssociated, &'a InstanceAssociated),
        }

        let match_plan = match self {
            Self::Inference(_)
            | Self::Primitive(_)
            | Self::Parameter(_)
            | Self::Error(_) => None,

            Self::Symbol(lhs) => match other {
                Self::Symbol(rhs) if lhs.id() == rhs.id() => {
                    Some(MatchPlan::Symbol(lhs, rhs))
                }

                Self::Inference(_)
                | Self::Primitive(_)
                | Self::Parameter(_)
                | Self::Symbol(_)
                | Self::Pointer(_)
                | Self::Reference(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Phantom(_)
                | Self::InstanceAssociated(_)
                | Self::AssociatedSymbol(_)
                | Self::FunctionSignature(_)
                | Self::Error(_) => None,
            },

            Self::Pointer(lhs) => match other {
                Self::Pointer(rhs) if lhs.is_mutable() == rhs.is_mutable() => {
                    Some(MatchPlan::Pointer(lhs, rhs))
                }

                Self::Inference(_)
                | Self::Primitive(_)
                | Self::Parameter(_)
                | Self::Symbol(_)
                | Self::Pointer(_)
                | Self::Reference(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Phantom(_)
                | Self::InstanceAssociated(_)
                | Self::AssociatedSymbol(_)
                | Self::FunctionSignature(_)
                | Self::Error(_) => None,
            },

            Self::Reference(lhs) => match other {
                Self::Reference(rhs) if lhs.qualifier() == rhs.qualifier() => {
                    Some(MatchPlan::Reference(lhs, rhs))
                }

                Self::Inference(_)
                | Self::Primitive(_)
                | Self::Parameter(_)
                | Self::Symbol(_)
                | Self::Pointer(_)
                | Self::Reference(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Phantom(_)
                | Self::InstanceAssociated(_)
                | Self::AssociatedSymbol(_)
                | Self::FunctionSignature(_)
                | Self::Error(_) => None,
            },

            Self::Array(lhs) => match other {
                Self::Array(rhs) => Some(MatchPlan::Array(lhs, rhs)),

                Self::Inference(_)
                | Self::Primitive(_)
                | Self::Parameter(_)
                | Self::Symbol(_)
                | Self::Pointer(_)
                | Self::Reference(_)
                | Self::Tuple(_)
                | Self::Phantom(_)
                | Self::InstanceAssociated(_)
                | Self::AssociatedSymbol(_)
                | Self::FunctionSignature(_)
                | Self::Error(_) => None,
            },

            Self::Tuple(lhs) => match other {
                Self::Tuple(rhs) => Some(MatchPlan::Tuple(lhs, rhs)),

                Self::Inference(_)
                | Self::Primitive(_)
                | Self::Parameter(_)
                | Self::Symbol(_)
                | Self::Pointer(_)
                | Self::Reference(_)
                | Self::Array(_)
                | Self::Phantom(_)
                | Self::InstanceAssociated(_)
                | Self::AssociatedSymbol(_)
                | Self::FunctionSignature(_)
                | Self::Error(_) => None,
            },

            Self::Phantom(lhs) => match other {
                Self::Phantom(rhs) => Some(MatchPlan::Phantom(lhs, rhs)),

                Self::Inference(_)
                | Self::Primitive(_)
                | Self::Parameter(_)
                | Self::Symbol(_)
                | Self::Pointer(_)
                | Self::Reference(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::InstanceAssociated(_)
                | Self::AssociatedSymbol(_)
                | Self::FunctionSignature(_)
                | Self::Error(_) => None,
            },

            Self::InstanceAssociated(lhs) => match other {
                Self::InstanceAssociated(rhs)
                    if lhs.trait_associated_symbol_id()
                        == rhs.trait_associated_symbol_id() =>
                {
                    Some(MatchPlan::InstanceAssociated(lhs, rhs))
                }

                Self::Inference(_)
                | Self::Primitive(_)
                | Self::Parameter(_)
                | Self::Symbol(_)
                | Self::Pointer(_)
                | Self::Reference(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Phantom(_)
                | Self::InstanceAssociated(_)
                | Self::AssociatedSymbol(_)
                | Self::FunctionSignature(_)
                | Self::Error(_) => None,
            },

            Self::AssociatedSymbol(lhs) => match other {
                Self::AssociatedSymbol(rhs) if lhs.id() == rhs.id() => {
                    Some(MatchPlan::AssociatedSymbol(lhs, rhs))
                }

                Self::Inference(_)
                | Self::Primitive(_)
                | Self::Parameter(_)
                | Self::Symbol(_)
                | Self::Pointer(_)
                | Self::Reference(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Phantom(_)
                | Self::InstanceAssociated(_)
                | Self::AssociatedSymbol(_)
                | Self::FunctionSignature(_)
                | Self::Error(_) => None,
            },

            Self::FunctionSignature(lhs) => match other {
                Self::FunctionSignature(rhs)
                    if lhs.parameters().len() == rhs.parameters().len() =>
                {
                    Some(MatchPlan::FunctionSignature(lhs, rhs))
                }

                Self::Inference(_)
                | Self::Primitive(_)
                | Self::Parameter(_)
                | Self::Symbol(_)
                | Self::Pointer(_)
                | Self::Reference(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Phantom(_)
                | Self::InstanceAssociated(_)
                | Self::AssociatedSymbol(_)
                | Self::FunctionSignature(_)
                | Self::Error(_) => None,
            },
        }?;

        Some(pernixc_coroutine_iter::coroutine_iter!({
            match match_plan {
                MatchPlan::Symbol(lhs, rhs) => {
                    for substructural in lhs
                        .generic_arguments()
                        .as_ref()
                        .substructural_match(
                            rhs.generic_arguments().as_ref(),
                            SubSymbolLocation::new,
                        )
                        .expect("validated symbol ids before building iterator")
                    {
                        yield substructural;
                    }
                }

                MatchPlan::Pointer(lhs, rhs) => {
                    yield Substructural::Type(Matching::new(
                        lhs.pointee().clone(),
                        rhs.pointee().clone(),
                        SubTypeLocation::Pointer,
                        SubTypeLocation::Pointer,
                    ));
                }

                MatchPlan::Reference(lhs, rhs) => {
                    yield Substructural::Lifetime(Matching::new(
                        lhs.lifetime().clone(),
                        rhs.lifetime().clone(),
                        SubLifetimeLocation::Reference,
                        SubLifetimeLocation::Reference,
                    ));

                    yield Substructural::Type(Matching::new(
                        lhs.pointee().clone(),
                        rhs.pointee().clone(),
                        SubTypeLocation::Reference,
                        SubTypeLocation::Reference,
                    ));
                }

                MatchPlan::Array(lhs, rhs) => {
                    yield Substructural::Type(Matching::new(
                        lhs.r#type().clone(),
                        rhs.r#type().clone(),
                        SubTypeLocation::Array,
                        SubTypeLocation::Array,
                    ));

                    yield Substructural::Constant(Matching::new(
                        lhs.length().clone(),
                        rhs.length().clone(),
                        SubConstantLocation::Array,
                        SubConstantLocation::Array,
                    ));
                }

                MatchPlan::Tuple(lhs, rhs) => {
                    for substructural in lhs
                        .substructural_match(rhs)
                        .expect(
                            "validated tuple variants before building iterator",
                        )
                    {
                        yield substructural;
                    }
                }

                MatchPlan::Phantom(lhs, rhs) => {
                    yield Substructural::Type(Matching::new(
                        lhs.r#type().clone(),
                        rhs.r#type().clone(),
                        SubTypeLocation::Phantom,
                        SubTypeLocation::Phantom,
                    ));
                }

                MatchPlan::AssociatedSymbol(lhs, rhs) => {
                    for substructural in lhs
                        .parent_generic_arguments()
                        .as_ref()
                        .substructural_match(
                            rhs.parent_generic_arguments().as_ref(),
                            |idx| SubAssociatedSymbolLocation::new(idx, true),
                        )
                        .expect(
                            "validated associated-symbol parent arguments before building iterator",
                        )
                    {
                        yield substructural;
                    }

                    for substructural in lhs
                        .member_generic_arguments()
                        .as_ref()
                        .substructural_match(
                            rhs.member_generic_arguments().as_ref(),
                            |idx| SubAssociatedSymbolLocation::new(idx, false),
                        )
                        .expect(
                            "validated associated-symbol member arguments before building iterator",
                        )
                    {
                        yield substructural;
                    }
                }

                MatchPlan::FunctionSignature(lhs, rhs) => {
                    for (index, (lhs, rhs)) in lhs
                        .parameters()
                        .iter()
                        .cloned()
                        .zip(rhs.parameters().iter().cloned())
                        .enumerate()
                    {
                        yield Substructural::Type(Matching::new(
                            lhs,
                            rhs,
                            SubTypeLocation::FunctionSignature(
                                SubFunctionSignatureLocation::Parameter(index),
                            ),
                            SubTypeLocation::FunctionSignature(
                                SubFunctionSignatureLocation::Parameter(index),
                            ),
                        ));
                    }

                    yield Substructural::Type(Matching::new(
                        lhs.return_type().clone(),
                        rhs.return_type().clone(),
                        SubTypeLocation::FunctionSignature(
                            SubFunctionSignatureLocation::ReturnType,
                        ),
                        SubTypeLocation::FunctionSignature(
                            SubFunctionSignatureLocation::ReturnType,
                        ),
                    ));
                }

                MatchPlan::InstanceAssociated(lhs, rhs) => {
                    for substructural in lhs.substructural_match(
                        rhs,
                        |idx| {
                            SubLifetimeLocation::InstanceAssociated(
                                SubInstanceAssociatedGenericArgsLocation::new(
                                    idx,
                                ),
                            )
                        },
                        |idx| {
                            SubTypeLocation::InstanceAssociated(
                                SubInstanceAssociatedGenericArgsLocation::new(
                                    idx,
                                ),
                            )
                        },
                        |idx| {
                            SubConstantLocation::InstanceAssociated(
                                SubInstanceAssociatedGenericArgsLocation::new(
                                    idx,
                                ),
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
                    .expect(
                        "validated instance-associated ids before building iterator",
                    ) {
                        yield substructural;
                    }
                }
            }
        }))
    }

    fn from_self_matching(
        matching: Matching<Interned<Self>, Self::ThisSubTermLocation>,
    ) -> Substructural<
        Self::SubLifetimeLocation,
        Self::SubTypeLocation,
        Self::SubConstantLocation,
        Self::SubInstanceLocation,
    > {
        Substructural::Type(matching)
    }
}

/// Location of an immediate child yielded by [`IterSubTerms`] for [`Type`].
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
    derive_more::From,
)]
pub enum IterSubTermLocation {
    /// The child is a lifetime.
    Lifetime(SubLifetimeLocation),

    /// The child is a type.
    Type(SubTypeLocation),

    /// The child is a constant.
    Constant(SubConstantLocation),

    /// The child is an instance.
    Instance(SubInstanceLocation),
}

fn iter_symbol_sub_terms(
    symbol: &Symbol,
) -> impl Iterator<Item = (TermRef<'_>, IterSubTermLocation)> + '_ {
    symbol.iter_sub_terms(
        |location| {
            IterSubTermLocation::Lifetime(SubLifetimeLocation::Symbol(location))
        },
        |location| IterSubTermLocation::Type(SubTypeLocation::Symbol(location)),
        |location| {
            IterSubTermLocation::Constant(SubConstantLocation::Symbol(location))
        },
        |location| {
            IterSubTermLocation::Instance(SubInstanceLocation::Symbol(location))
        },
    )
}

fn iter_tuple_sub_terms(
    tuple: &Tuple,
) -> impl Iterator<Item = (TermRef<'_>, IterSubTermLocation)> + '_ {
    tuple
        .iter_terms_with_location(|location| {
            IterSubTermLocation::Type(SubTypeLocation::Tuple(location))
        })
        .map(|(term, location)| (TermRef::Type(term), location))
}

fn iter_instance_associated_sub_terms(
    instance_associated: &InstanceAssociated,
) -> impl Iterator<Item = (TermRef<'_>, IterSubTermLocation)> + '_ {
    pernixc_coroutine_iter::coroutine_iter!({
        for sub_term in instance_associated
            .associated_instance_generic_arguments()
            .iter_sub_terms_with_location(
                |location| {
                    IterSubTermLocation::Lifetime(
                        SubLifetimeLocation::InstanceAssociated(
                            SubInstanceAssociatedGenericArgsLocation::new(
                                location.index(),
                            ),
                        ),
                    )
                },
                |location| {
                    IterSubTermLocation::Type(SubTypeLocation::InstanceAssociated(
                        SubInstanceAssociatedGenericArgsLocation::new(
                            location.index(),
                        ),
                    ))
                },
                |location| {
                    IterSubTermLocation::Constant(
                        SubConstantLocation::InstanceAssociated(
                            SubInstanceAssociatedGenericArgsLocation::new(
                                location.index(),
                            ),
                        ),
                    )
                },
                |location| {
                    IterSubTermLocation::Instance(
                        SubInstanceLocation::InstanceAssociated(
                            SubInstanceAssociatedInstanceLocation::GenericArguments(
                                SubInstanceAssociatedGenericArgsLocation::new(
                                    location.index(),
                                ),
                            ),
                        ),
                    )
                },
            )
        {
            yield sub_term;
        }

        yield (
            TermRef::Instance(instance_associated.instance()),
            IterSubTermLocation::Instance(
                SubInstanceLocation::InstanceAssociated(
                    SubInstanceAssociatedInstanceLocation::Instance,
                ),
            ),
        );
    })
}

fn iter_associated_symbol_sub_terms(
    associated_symbol: &AssociatedSymbol,
) -> impl Iterator<Item = (TermRef<'_>, IterSubTermLocation)> + '_ {
    associated_symbol.iter_sub_terms(
        |location| {
            IterSubTermLocation::Lifetime(
                SubLifetimeLocation::AssociatedSymbol(location),
            )
        },
        |location| {
            IterSubTermLocation::Type(SubTypeLocation::AssociatedSymbol(
                location,
            ))
        },
        |location| {
            IterSubTermLocation::Constant(
                SubConstantLocation::AssociatedSymbol(location),
            )
        },
        |location| {
            IterSubTermLocation::Instance(
                SubInstanceLocation::AssociatedSymbol(location),
            )
        },
    )
}

fn iter_function_signature_sub_terms(
    signature: &FunctionSignature,
) -> impl Iterator<Item = (TermRef<'_>, IterSubTermLocation)> + '_ {
    pernixc_coroutine_iter::coroutine_iter!({
        for (index, parameter) in signature.parameters().iter().enumerate() {
            yield (
                TermRef::Type(parameter),
                IterSubTermLocation::Type(SubTypeLocation::FunctionSignature(
                    SubFunctionSignatureLocation::Parameter(index),
                )),
            );
        }

        yield (
            TermRef::Type(signature.return_type()),
            IterSubTermLocation::Type(SubTypeLocation::FunctionSignature(
                SubFunctionSignatureLocation::ReturnType,
            )),
        );
    })
}

impl IterSubTerms for Type {
    type TermLocation = IterSubTermLocation;

    fn iter_sub_terms(
        &self,
    ) -> impl Iterator<Item = (TermRef<'_>, Self::TermLocation)> + '_ {
        pernixc_coroutine_iter::coroutine_iter!({
            match self {
                Self::Inference(_)
                | Self::Primitive(_)
                | Self::Parameter(_)
                | Self::Error(_) => {}

                Self::Symbol(symbol) => {
                    for sub_term in iter_symbol_sub_terms(symbol) {
                        yield sub_term;
                    }
                }

                Self::Pointer(pointer) => {
                    yield (
                        TermRef::Type(pointer.pointee()),
                        Self::TermLocation::Type(SubTypeLocation::Pointer),
                    );
                }

                Self::Reference(reference) => {
                    yield (
                        TermRef::Lifetime(reference.lifetime()),
                        Self::TermLocation::Lifetime(
                            SubLifetimeLocation::Reference,
                        ),
                    );

                    yield (
                        TermRef::Type(reference.pointee()),
                        Self::TermLocation::Type(SubTypeLocation::Reference),
                    );
                }

                Self::Array(array) => {
                    yield (
                        TermRef::Type(array.r#type()),
                        Self::TermLocation::Type(SubTypeLocation::Array),
                    );

                    yield (
                        TermRef::Constant(array.length()),
                        Self::TermLocation::Constant(
                            SubConstantLocation::Array,
                        ),
                    );
                }

                Self::Tuple(tuple) => {
                    for sub_term in iter_tuple_sub_terms(tuple) {
                        yield sub_term;
                    }
                }

                Self::Phantom(phantom) => {
                    yield (
                        TermRef::Type(phantom.r#type()),
                        Self::TermLocation::Type(SubTypeLocation::Phantom),
                    );
                }

                Self::InstanceAssociated(instance_associated) => {
                    for sub_term in
                        iter_instance_associated_sub_terms(instance_associated)
                    {
                        yield sub_term;
                    }
                }

                Self::AssociatedSymbol(associated_symbol) => {
                    for sub_term in
                        iter_associated_symbol_sub_terms(associated_symbol)
                    {
                        yield sub_term;
                    }
                }

                Self::FunctionSignature(signature) => {
                    for sub_term in iter_function_signature_sub_terms(signature)
                    {
                        yield sub_term;
                    }
                }
            }
        })
    }
}
