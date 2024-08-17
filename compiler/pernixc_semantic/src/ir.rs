//! Contains the definition of the intermediate representation of the program.
//!
//! ## Steps of Creating IR
//!
//! ### Binding
//!
//! The syntax tree is traversed and the Control Flow Graph (CFG) is built.
//! During this step, the type inference and type checking are performed.
//!
//! The model used for the type system in this step is
//! [`representation::binding::infer::Model`].
//!
//! ### Auto Move Analysis
//!
//! In the prior step, every load instruction is performed as copy. In this
//! step, the redundant copy instructions are removed and replaced with move
//! instructions.
//!
//! ### Alternate Drop Analysis
//!
//! The required drop instructions are inserted in the CFG. These drop
//! instructions are inserted in the alternative path when the value is moved
//! out.
//!
//! ### Well-formedness
//!
//! After the Binding step, when the full type information is available, the
//! IR is checked for well-formedness. This step includes checking for
//! where clauses predicates, type bounds, move/copy,and other
//! type-related checks.
//!
//! ### Borrow Checking
//!
//! The copy of IR is created and all the "erased" lifetimes are replaced with
//! the inference variables. The borrow checking is performed on this copy.

use std::{fmt::Debug, hash::Hash};

use address::Address;
use alloca::Alloca;
use value::{register::Register, Value};

use self::representation::Representation;
use crate::{
    arena::ID,
    symbol::{self, CallableID, GlobalID, Parameter},
    type_system::{
        instantiation::MismatchedGenericArgumentCountError,
        model::{self, Model},
        term::{self, r#type::Type},
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
    type Model: Model;
}

/// A tag type representing a successfully generated IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Success(() /* Prevent arbitrary instantiation */);

impl State for Success {
    type Model = model::Default;
}

/// A tag type representing an IR that is suboptimal (contains an error).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Suboptimal;

impl State for Suboptimal {
    type Model = model::Default;
}

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default, derive_more::Deref)]
pub struct IR<T: State> {
    #[deref]
    pub(crate) representation: Representation<T::Model>,

    state: T,
}

/// An error returned calling [`Representation::type_of_register`] and
/// [`Representation::type_of_address`].
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum TypeOfError<M: Model> {
    #[error(
        "the `Field` address requires `struct_address` field to have an \
         adress of type `struct` but found the other type"
    )]
    NonStructAddressType {
        /// The address that doesn't have the struct type.
        address: Address<M>,

        /// The type of the [`NonStructAddressType::address`].
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
    NonReferenceAddressType {
        /// The address that doesn't have the reference type.
        value: Value<M>,

        /// The type of the [`NonReferenceAddressType::address`].
        r#type: Type<M>,
    },

    #[error(
        "the `Variant` address requires the field `enum_address` to have an \
         address of type enum but found the other type"
    )]
    NonEnumAddressType {
        /// The address that doesn't have the enum type.
        address: Address<M>,

        /// The type of the [`NonEnumAddressType::address`].
        r#type: Type<M>,
    },

    #[error(
        "the `Tuple` address requires the field `tuple_address` to have an \
         address of type tuple but found the other type"
    )]
    NonTupleAddressType {
        /// The address that doesn't have the tuple type.
        address: Address<M>,

        /// The type of the [`NonTupleAddressType::address`].
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

        /// The type of the [`NonLocalAssignmentType::register`].
        r#type: Type<M>,
    },

    #[error(
        "the `ReferenceOf` assignment requires the field `address` to have an \
         address of type `local` when `is_loal` is true but found the other \
         type"
    )]
    NonLocalAddressType {
        /// The address that doesn't have the local type.
        address: Address<M>,

        /// The type of the [`NonLocalAddressType::address`].
        r#type: Type<M>,
    },

    #[error(
        "the `Index` address requires the field `array_address` to have an \
         address of type `array` but found the other type"
    )]
    NonArrayAddressType {
        /// The address that doesn't have the array type.
        address: Address<M>,

        /// The type of the [`NonArrayAddressType::address`].
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
}
