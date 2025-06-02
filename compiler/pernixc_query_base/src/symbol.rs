//! Contains the definitions related to the symbols.
use std::hash::{Hash, Hasher};

use pernixc_query::{Engine, Key};
use pernixc_target::{Global, TargetID};
use serde::{Deserialize, Serialize};

/// Represents a unique identifier for the symbols in the compilation target.
/// This ID is only unique within the context of a single target. If wants to
/// use identifier across multiple targets, it should be combined with the
/// [`Global`]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct ID(pub u64);

/// A key used to retrieve [`Kind`]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
)]
#[value(Kind)]
pub struct KindKey(pub Global<ID>);

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

/// An extension trait for the [`Engine`] that provides additional methods
/// for working with symbols and their kinds.
pub trait Ext {
    /// Retrieves the kind of the symbol identified by the given [`Global<ID>`].
    fn symbol_kind(&self, id: Global<ID>) -> Kind;
}

impl Ext for Engine {
    fn symbol_kind(&self, id: Global<ID>) -> Kind {
        self.query(&KindKey(id)).expect(
            "should have no cyclic dependencies since this is a base query",
        )
    }
}

pub(super) fn generate_id<'a>(
    database: &pernixc_query::database::Database,
    paths: impl IntoIterator<Item = &'a str>,
    target_id: TargetID,
) -> ID {
    let mut hasher = fnv::FnvHasher::default();
    // write all paths to the hasher
    for path in paths {
        path.hash(&mut hasher);
    }

    // encode attempts to the hasher
    let mut attempt = 0;
    loop {
        let mut attempt_hasher = fnv::FnvHasher::with_key(hasher.finish());
        attempt.hash(&mut attempt_hasher);

        let id = ID(attempt_hasher.finish());

        if database.contains_key(&KindKey(target_id.make_global(id))) {
            // try again with a new attempt
            attempt += 1;
        } else {
            return id;
        }
    }
}
