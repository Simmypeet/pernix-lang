//! Contains the definition of the intermediate representation of the program.

use std::{fmt::Debug, hash::Hash};

use address::Address;
use alloca::Alloca;
use pernixc_base::source_file::Span;
use value::{register::Register, Value};

use self::representation::Representation;
use crate::{
    arena::ID,
    symbol::{
        self,
        table::{self, Table},
        CallableID, GlobalID, Parameter,
    },
    type_system::{
        fresh::Fresh,
        instantiation::MismatchedGenericArgumentCountError,
        model,
        term::{
            self, constant::Constant, lifetime::Lifetime, r#type::Type, Never,
            Term,
        },
        OverflowError,
    },
};

pub mod address;
pub mod alloca;
pub mod control_flow_graph;
pub mod instruction;
pub mod pattern;
pub mod representation;
pub mod scope;
pub mod value;

/// The model to used to generate the IR.
pub trait State {
    /// The model to use for the type system.
    type Model: model::Model;
}

/// The type system model used in the IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Model;

/// The inference variable used for lifetimes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Erased;

impl<T: table::State> table::Display<T> for Erased {
    fn fmt(
        &self,
        _: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "_")
    }
}

impl From<Never> for Erased {
    fn from(value: Never) -> Self { match value {} }
}

impl Fresh for Erased {
    fn fresh() -> Self { Self }
}

impl model::Model for Model {
    type LifetimeInference = Erased;
    type TypeInference = Never;
    type ConstantInference = Never;

    fn from_default_type(ty: Type<model::Default>) -> Type<Self> {
        Type::from_other_model(ty)
    }

    fn from_default_lifetime(
        lifetime: Lifetime<model::Default>,
    ) -> Lifetime<Self> {
        Lifetime::from_other_model(lifetime)
    }

    fn from_default_constant(
        constant: Constant<model::Default>,
    ) -> Constant<Self> {
        Constant::from_other_model(constant)
    }
}

/// A tag type representing a successfully generated IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Success(() /* Prevent arbitrary instantiation */);

impl State for Success {
    type Model = Model;
}

/// A tag type representing an IR that is suboptimal (contains an error).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Suboptimal;

impl State for Suboptimal {
    type Model = Model;
}

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default, derive_more::Deref)]
pub struct IR<T: State> {
    #[deref]
    pub(crate) representation: Representation<T::Model>,

    state: T,
}

/// A trait for transforming terms from one model to another.
///
/// For example, this can be used when finishing the inference process to
/// transform the terms from the inference model to the final concrete model.
pub trait Transform<T: Term> {
    /// The target model to transform the terms to.
    type Target: model::Model;

    /// The error that might occur when transforming the terms.
    type Error;

    /// Inspects the term. This is called before transforming the term.
    ///
    /// The method should report the error seen in the term. For exmple, if the
    /// term contains an un-inferrable type. This is called at most onece per
    /// expression/entity.
    fn inspect(&mut self, term: &T, span: Span) -> Result<(), Self::Error>;

    /// Transforms the term.
    fn transform(
        &mut self,
        term: T,
        span: Span,
    ) -> Result<T::Rebind<Self::Target>, Self::Error>;

    /// Inspects and transforms the term.
    fn inspect_and_transform(
        &mut self,
        term: T,
        span: Span,
    ) -> Result<T::Rebind<Self::Target>, Self::Error> {
        self.inspect(&term, span.clone())?;
        self.transform(term, span)
    }
}

/// An error returned calling [`representation::Values::type_of_register`] and
/// [`representation::Values::type_of_address`].
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum TypeOfError<M: model::Model> {
    #[error(
        "the `Field` address requires `struct_address` field to have an \
         adress of type `struct` but found the other type"
    )]
    NonStructAddressType {
        /// The address that doesn't have the struct type.
        address: Address<M>,

        /// The type of the [`address::Field::struct_address`].
        r#type: Type<M>,
    },

    #[error(
        "the `Variant` address requires the field `variant_id` to have an \
         variant that has an associated value"
    )]
    VariantHasNoAssociatedValue {
        /// The variant ID that doesn't have an associated value.
        variant_id: ID<symbol::Variant>,
    },

    #[error(
        "the `Variant` address contains a variant ID that has mismatched \
         parent enum ID according to the `enum_address` field"
    )]
    InvalidVariantID {
        /// The variant ID that is invalid.
        variant_id: ID<symbol::Variant>,

        /// The expected parent enum ID.
        enum_id: ID<symbol::Enum>,
    },

    #[error(
        "the variant `Address::Base(Memory::ReferenceValue(register_id))` \
         requires the register to have a type of `Type::Reference` but found \
         the other type"
    )]
    NonReferenceValueType {
        /// The address that doesn't have the reference type.
        value: Value<M>,

        /// The type of the [`address::Memory::ReferenceValue`].
        r#type: Type<M>,
    },

    #[error(
        "the variant `ReferenceAddress::address` requires the address to have \
         a type of `Type::Reference` but found the other type"
    )]
    NonReferenceAddressType {
        /// The address that doesn't have the reference type.
        address: Address<M>,

        /// The type of the [`address::ReferenceAddress`].
        r#type: Type<M>,
    },

    #[error(
        "the `Variant` address requires the field `enum_address` to have an \
         address of type enum but found the other type"
    )]
    NonEnumAddressType {
        /// The address that doesn't have the enum type.
        address: Address<M>,

        /// The type of the [`address::Variant::enum_address`].
        r#type: Type<M>,
    },

    #[error(
        "the `Tuple` address requires the field `tuple_address` to have an \
         address of type tuple but found the other type"
    )]
    NonTupleAddressType {
        /// The address that doesn't have the tuple type.
        address: Address<M>,

        /// The type of the [`address::Tuple::tuple_address`].
        r#type: Type<M>,
    },

    #[error(
        "the `Prefix` assignment requires the field `operand` to have an \
         assignment of type `local` when its prefix operator is `unlocal` but \
         found the other type"
    )]
    NonLocalAssignmentType {
        /// The value that doesn't have the local type.
        value: Value<M>,

        /// The type of the [`value::register::Prefix::operand`].
        r#type: Type<M>,
    },

    #[error(
        "the `Index` address requires the field `array_address` to have an \
         address of type `array` but found the other type"
    )]
    NonArrayAddressType {
        /// The address that doesn't have the array type.
        address: Address<M>,

        /// The type of the [`address::Index::array_address`].
        r#type: Type<M>,
    },

    #[error(
        "the field `offset` in `Tuple` address struct points to an invalid \
         element in the tuple"
    )]
    InvalidTupleOffset {
        /// The offset that points to the invalid tuple element.
        offset: address::Offset,

        /// The type of the tuple address in the
        /// [`address::Tuple::tuple_address`]
        tuple_type: term::Tuple<Type<M>>,
    },

    #[error("the address contains an invalid field ID")]
    InvalidFieldID {
        /// The field ID that is invalid.
        field_id: ID<symbol::Field>,

        /// The struct where the field is expected to be.
        in_struct: ID<symbol::Struct>,
    },

    #[error("the `Variant::variant_address` contains an invalid enum type")]
    InvalidEnumAddressInstantiation {
        /// The enum id
        enum_id: ID<symbol::Enum>,

        /// The error occurred when calculating the instantiation.
        mismatched_generic_argument_error:
            MismatchedGenericArgumentCountError<M>,
    },

    #[error("the `Field::struct_address` contains an invalid struct type")]
    InvalidStructAddressInstantiation {
        /// The struct id
        struct_id: ID<symbol::Struct>,

        /// The error occurred when calculating the instantiation.
        mismatched_generic_argument_error:
            MismatchedGenericArgumentCountError<M>,
    },

    #[error("the address contains an invalid alloca ID")]
    InvalidAllocaID(ID<Alloca<M>>),

    #[error("the address contains an invalid register ID")]
    InvalidRegisterID(ID<Register<M>>),

    #[error(
        "the address requires function parameters but the `current_site` is \
         not a function"
    )]
    CurrentSiteIsNotFunction(GlobalID),

    #[error(
        "the address requires an information from a global ID but the ID is \
         not found in the table"
    )]
    InvalidGlobalID(GlobalID),

    #[error(
        "the address requires a parameter ID in a particular function but the \
         ID is invalid"
    )]
    InvalidParameterID {
        /// The parameter ID that is invalid.
        parameter_id: ID<Parameter>,

        /// The function where the parameter is expected to be.
        in_function: CallableID,
    },

    #[error(transparent)]
    OverflowError(#[from] OverflowError),
}
