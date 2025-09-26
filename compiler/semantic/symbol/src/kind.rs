//!  Contains the definition of the [`Kind`] enum.

use std::{fmt::Debug, hash::Hash};

use pernixc_extend::extend;
use pernixc_query::Value;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::ID;

/// An enumeration used to identify the kind of a symbol in the Pernix. This
/// value should be set to every symbol that is defined in the compilation
/// target.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Default,
    StableHash,
    Value,
)]
#[allow(missing_docs)]
#[id(Global<ID>)]
#[extend(method(get_kind), no_cyclic)]
pub enum Kind {
    #[default]
    Module,
    Struct,
    Trait,
    Enum,
    Type,
    Constant,
    Function,
    ExternFunction,
    Variant,
    TraitType,
    TraitFunction,
    TraitConstant,
    Effect,
    EffectOperation,
    Marker,
    PositiveImplementation,
    NegativeImplementation,
    ImplementationType,
    ImplementationFunction,
    ImplementationConstant,
}

impl Kind {
    /// Checks if this kind of symbol has a [`Member`] component.
    #[must_use]
    pub const fn has_member(&self) -> bool {
        matches!(
            self,
            Self::Module
                | Self::Enum
                | Self::Trait
                | Self::PositiveImplementation
                | Self::Effect
        )
    }

    /// Checks if this kind of symbol has a [`Implemented`] component.
    #[must_use]
    pub const fn has_implemented(&self) -> bool {
        matches!(self, Self::Trait | Self::Enum | Self::Struct | Self::Marker)
    }

    /// Checks if the symbol is either a struct or an enum.
    #[must_use]
    pub const fn is_adt(&self) -> bool {
        matches!(self, Self::Struct | Self::Enum)
    }

    /// Gets the description string of the kind.
    #[must_use]
    pub const fn kind_str(&self) -> &'static str {
        match self {
            Self::Module => "module",
            Self::Struct => "struct",
            Self::Trait => "trait",
            Self::Enum => "enum",
            Self::Type => "type",
            Self::Constant => "constant",
            Self::Function => "function",
            Self::Variant => "variant",
            Self::TraitType => "trait type",
            Self::TraitFunction => "trait function",
            Self::TraitConstant => "trait constant",
            Self::ExternFunction => "extern function",
            Self::PositiveImplementation => "implementation",
            Self::NegativeImplementation => "negative implementation",
            Self::ImplementationFunction => "implementation function",
            Self::ImplementationType => "implementation type",
            Self::ImplementationConstant => "implementation constant",
            Self::Marker => "marker",
            Self::Effect => "effect",
            Self::EffectOperation => "effect operation",
        }
    }

    /// Checks if the symbol kind has generic parameters.
    #[must_use]
    pub const fn has_generic_parameters(&self) -> bool {
        matches!(
            self,
            Self::Struct
                | Self::Trait
                | Self::Enum
                | Self::Type
                | Self::Constant
                | Self::Function
                | Self::TraitType
                | Self::TraitFunction
                | Self::TraitConstant
                | Self::Marker
                | Self::ExternFunction
                | Self::PositiveImplementation
                | Self::NegativeImplementation
                | Self::ImplementationType
                | Self::ImplementationFunction
                | Self::ImplementationConstant
                | Self::Effect
        )
    }

    /// Checks if the symbol kind has a where clause.
    #[must_use]
    pub const fn has_where_clause(&self) -> bool {
        matches!(
            self,
            Self::Struct
                | Self::Trait
                | Self::Enum
                | Self::Type
                | Self::Constant
                | Self::Function
                | Self::TraitType
                | Self::TraitFunction
                | Self::TraitConstant
                | Self::Marker
                | Self::ExternFunction
                | Self::PositiveImplementation
                | Self::NegativeImplementation
                | Self::ImplementationType
                | Self::ImplementationFunction
                | Self::ImplementationConstant
                | Self::Effect
        )
    }

    /// Checks if the symbol is a kind of `implements SYMBOL`.
    #[must_use]
    pub const fn is_implementation(&self) -> bool {
        matches!(
            self,
            Self::PositiveImplementation | Self::NegativeImplementation
        )
    }

    /// Checks if the symbol has a type alias definition such as `type T =
    /// TYPE_ALIAS`.
    ///
    /// Trait type symbol is not included becuase it doesn't have the definition
    /// of the type alias such as `trait T { type U; }`
    #[must_use]
    pub const fn has_type_alias(&self) -> bool {
        matches!(self, Self::Type | Self::ImplementationType)
    }

    /// Checks if the symbol has a function signature.
    #[must_use]
    pub const fn has_function_signature(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::TraitFunction
                | Self::ImplementationFunction
                | Self::ExternFunction
        )
    }

    /// Checks if the symbol has implied predicates component.
    #[must_use]
    pub const fn has_implied_predicates(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::TraitFunction
                | Self::ImplementationFunction
                | Self::ExternFunction
        )
    }

    /// Checks if the symbol has elided lifetimes component.
    #[must_use]
    pub const fn has_elided_lifetimes(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::TraitFunction
                | Self::ImplementationFunction
                | Self::ExternFunction
        )
    }

    /// Checks if the symbol has a variance map component.
    #[must_use]
    pub const fn has_variance_map(&self) -> bool {
        matches!(self, Self::Struct | Self::Enum)
    }

    /// Checks if the symbol has a function body
    #[must_use]
    pub const fn has_function_body(&self) -> bool {
        matches!(self, Self::Function | Self::ImplementationFunction)
    }

    /// Checks if the symbol has a variant associated type
    #[must_use]
    pub const fn has_variant_associated_type(&self) -> bool {
        matches!(self, Self::Variant)
    }

    /// Checks if the symbol has fields
    #[must_use]
    pub const fn has_fields(&self) -> bool { matches!(self, Self::Struct) }
}
