//!  Contains the definition of the [`Kind`] enum.

use std::{fmt::Debug, hash::Hash};

use pernixc_target::Global;
use qbice::{Decode, Encode, Identifiable, Query, StableHash};

use crate::SymbolID;

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
    Encode,
    Decode,
    Default,
    StableHash,
    Identifiable,
)]
#[allow(missing_docs)]
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
    TraitAssociatedType,
    TraitAssociatedFunction,
    TraitAssociatedConstant,
    TraitAssociatedInstance,
    Effect,
    EffectOperation,
    Marker,
    PositiveImplementation,
    NegativeImplementation,
    ImplementationAssociatedType,
    ImplementationAssociatedFunction,
    ImplementationAssociatedConstant,
    Instance,
    InstanceAssociatedType,
    InstanceAssociatedFunction,
    InstanceAssociatedConstant,
    InstanceAssociatedInstance,
}

/// The key type used with [`TrackedEngine`] to access the kind of a symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    Query,
    StableHash,
)]
#[value(Kind)]
#[extend(name = get_kind, by_val)]
pub struct Key {
    /// The global ID of the symbol to get the kind for.
    pub symbol_id: Global<SymbolID>,
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
                | Self::Instance
        )
    }

    /// Checks if this kind of symbol can be a member of a module.
    #[must_use]
    pub const fn is_module_member(&self) -> bool {
        matches!(
            self,
            Self::Module
                | Self::Struct
                | Self::Enum
                | Self::Function
                | Self::Constant
                | Self::Effect
                | Self::Trait
                | Self::Marker
                | Self::Instance
        )
    }

    /// Checks if this kind of symbol has a [`Implemented`] component.
    #[must_use]
    pub const fn has_implemented(&self) -> bool {
        matches!(self, Self::Enum | Self::Struct | Self::Marker)
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
            Self::TraitAssociatedType => "trait associated type",
            Self::TraitAssociatedFunction => "trait associated function",
            Self::TraitAssociatedConstant => "trait associated constant",
            Self::TraitAssociatedInstance => "trait associated instance",
            Self::ExternFunction => "extern function",
            Self::PositiveImplementation => "implementation",
            Self::NegativeImplementation => "negative implementation",
            Self::ImplementationAssociatedFunction => {
                "implementation associated function"
            }
            Self::ImplementationAssociatedType => {
                "implementation associated type"
            }
            Self::ImplementationAssociatedConstant => {
                "implementation associated constant"
            }
            Self::Marker => "marker",
            Self::Effect => "effect",
            Self::EffectOperation => "effect operation",
            Self::Instance => "instance",
            Self::InstanceAssociatedType => "instance associated type",
            Self::InstanceAssociatedFunction => "instance associated function",
            Self::InstanceAssociatedConstant => "instance associated constant",
            Self::InstanceAssociatedInstance => "instance associated instance",
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
                | Self::TraitAssociatedType
                | Self::TraitAssociatedFunction
                | Self::TraitAssociatedConstant
                | Self::TraitAssociatedInstance
                | Self::Marker
                | Self::ExternFunction
                | Self::PositiveImplementation
                | Self::NegativeImplementation
                | Self::ImplementationAssociatedType
                | Self::ImplementationAssociatedFunction
                | Self::ImplementationAssociatedConstant
                | Self::Effect
                | Self::EffectOperation
                | Self::Instance
                | Self::InstanceAssociatedType
                | Self::InstanceAssociatedFunction
                | Self::InstanceAssociatedConstant
                | Self::InstanceAssociatedInstance
        )
    }

    /// Checks if the symbol kind has an instance associated value.
    #[must_use]
    pub const fn has_instance_associated_value(&self) -> bool {
        matches!(self, Self::InstanceAssociatedInstance)
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
                | Self::TraitAssociatedType
                | Self::TraitAssociatedFunction
                | Self::TraitAssociatedConstant
                | Self::TraitAssociatedInstance
                | Self::Marker
                | Self::ExternFunction
                | Self::PositiveImplementation
                | Self::NegativeImplementation
                | Self::ImplementationAssociatedType
                | Self::ImplementationAssociatedFunction
                | Self::ImplementationAssociatedConstant
                | Self::Effect
                | Self::EffectOperation
                | Self::Instance
                | Self::InstanceAssociatedType
                | Self::InstanceAssociatedFunction
                | Self::InstanceAssociatedConstant
                | Self::InstanceAssociatedInstance
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
        matches!(
            self,
            Self::Type
                | Self::ImplementationAssociatedType
                | Self::InstanceAssociatedType
        )
    }

    /// Checks if the symbol has a function signature.
    #[must_use]
    pub const fn has_function_signature(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::TraitAssociatedFunction
                | Self::ImplementationAssociatedFunction
                | Self::ExternFunction
                | Self::EffectOperation
                | Self::InstanceAssociatedFunction
        )
    }

    /// Checks if the symbol has a trait reference.
    #[must_use]
    pub const fn has_trait_ref(&self) -> bool {
        matches!(
            self,
            Self::Instance
                | Self::TraitAssociatedInstance
                | Self::InstanceAssociatedInstance
        )
    }

    /// Checks if the symbol has a `do Effects` clause.
    #[must_use]
    pub const fn has_capabilities(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::TraitAssociatedFunction
                | Self::ImplementationAssociatedFunction
                | Self::ExternFunction
                | Self::EffectOperation
        )
    }

    /// Checks if the symbol has implied predicates component.
    #[must_use]
    pub const fn has_implied_predicates(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::TraitAssociatedFunction
                | Self::ImplementationAssociatedFunction
                | Self::ExternFunction
                | Self::EffectOperation
        )
    }

    /// Checks if the symbol has elided lifetimes component.
    #[must_use]
    pub const fn has_elided_lifetimes(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::TraitAssociatedFunction
                | Self::ImplementationAssociatedFunction
                | Self::ExternFunction
                | Self::EffectOperation
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
        matches!(
            self,
            Self::Function
                | Self::ImplementationAssociatedFunction
                | Self::InstanceAssociatedFunction
        )
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
