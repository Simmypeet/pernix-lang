//!  Contains the definition of the [`Kind`] enum.

use std::hash::Hash;

use pernixc_query::{runtime::executor::CyclicError, TrackedEngine, Value};
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
    PositiveTraitImplementation,
    NegativeTraitImplementation,
    TraitImplementationFunction,
    TraitImplementationType,
    TraitImplementationConstant,
    AdtImplementation,
    AdtImplementationFunction,
    Marker,
    PositiveMarkerImplementation,
    NegativeMarkerImplementation,
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
                | Self::AdtImplementation
                | Self::PositiveTraitImplementation
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
            Self::PositiveTraitImplementation => {
                "positive trait implementation"
            }
            Self::NegativeTraitImplementation => {
                "negative trait implementation"
            }
            Self::TraitImplementationFunction => {
                "trait implementation function"
            }
            Self::TraitImplementationType => "trait implementation type",
            Self::TraitImplementationConstant => {
                "trait implementation constant"
            }
            Self::AdtImplementation => "implementation",
            Self::AdtImplementationFunction => "implementation function",
            Self::Marker => "marker",
            Self::PositiveMarkerImplementation => {
                "positive marker implementation"
            }
            Self::NegativeMarkerImplementation => {
                "negative marker implementation"
            }
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
                | Self::PositiveTraitImplementation
                | Self::NegativeTraitImplementation
                | Self::TraitImplementationFunction
                | Self::TraitImplementationType
                | Self::TraitImplementationConstant
                | Self::AdtImplementation
                | Self::AdtImplementationFunction
                | Self::Marker
                | Self::PositiveMarkerImplementation
                | Self::NegativeMarkerImplementation
                | Self::ExternFunction
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
                | Self::PositiveTraitImplementation
                | Self::NegativeTraitImplementation
                | Self::TraitImplementationFunction
                | Self::TraitImplementationType
                | Self::TraitImplementationConstant
                | Self::AdtImplementation
                | Self::AdtImplementationFunction
                | Self::Marker
                | Self::PositiveMarkerImplementation
                | Self::NegativeMarkerImplementation
                | Self::ExternFunction
        )
    }

    /// Checks if the symbol is a kind of `implements SYMBOL`.
    #[must_use]
    pub const fn is_implementation(&self) -> bool {
        matches!(
            self,
            Self::PositiveTraitImplementation
                | Self::NegativeTraitImplementation
                | Self::PositiveMarkerImplementation
                | Self::NegativeMarkerImplementation
                | Self::AdtImplementation
        )
    }

    /// Checks if the symbol has a type alias definition such as `type T =
    /// TYPE_ALIAS`.
    ///
    /// Trait type symbol is not included becuase it doesn't have the definition
    /// of the type alias such as `trait T { type U; }`
    #[must_use]
    pub const fn has_type_alias(&self) -> bool {
        matches!(self, Self::Type | Self::TraitImplementationType)
    }

    /// Checks if the symbol has a function signature.
    #[must_use]
    pub const fn has_function_signature(&self) -> bool {
        matches!(
            self,
            Self::Function
                | Self::TraitFunction
                | Self::TraitImplementationFunction
                | Self::AdtImplementationFunction
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
                | Self::TraitImplementationFunction
                | Self::AdtImplementationFunction
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
                | Self::TraitImplementationFunction
                | Self::AdtImplementationFunction
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
        matches!(
            self,
            Self::Function
                | Self::TraitImplementationFunction
                | Self::AdtImplementationFunction
        )
    }

    /// Checks if the symbol has a `unsafe`ness attribute.
    #[must_use]
    pub const fn has_function_unsafeness(&self) -> bool {
        matches!(self, Self::Function | Self::AdtImplementationFunction)
    }

    /// Checks if the symbol has a `const`ness attribute.
    #[must_use]
    pub const fn has_function_constness(&self) -> bool {
        matches!(self, Self::Function | Self::AdtImplementationFunction)
    }
}

#[pernixc_query::executor(key(Key), name(Executor))]
pub fn executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<Kind, CyclicError> {
    let map = engine.query(&crate::MapKey(key.0.target_id))?;
    let node_key = map
        .keys_by_symbol_id
        .get(&key.0.id)
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", key.0.id))
        .as_ref()
        .map_or_else(
            || crate::Key::Root(key.0.target_id),
            |x| crate::Key::Submodule {
                external_submodule: x.clone(),
                target_id: key.0.target_id,
            },
        );

    let table = engine.query(&crate::TableKey(node_key))?;

    Ok(table
        .kinds
        .get(&key.0.id)
        .copied()
        .unwrap_or_else(|| panic!("invalid symbol ID: {:?}", key.0.id)))
}
