//!

use std::collections::{HashMap, HashSet};

use derive_more::{Deref, DerefMut};
use pernixc_base::source_file::Span;
use serde::{Deserialize, Serialize};

use crate::table::{GlobalID, ID};

/// Describes the relationship between two symbols in the hierarchy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HierarchyRelationship {
    /// The first symbol is the parent of the second symbol.
    Parent,

    /// The first symbol is the child of the second symbol.
    Child,

    /// Both symbols are two equivalent symbols.
    Equivalent,

    /// Both symbols are defined in different hierarchy scope.
    Unrelated,
}

/// Represents an accessibility of a symbol.
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
pub enum Accessibility {
    /// The symbol is accessible from anywhere.
    #[default]
    Public,

    /// The symbol is accessible from the given module and its children.
    Scoped(ID),
}

/// Represents a name of a symbol.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
)]
pub struct Name(pub String);

/// A component for the symbols that can be implemented such as traits, structs,
/// enums, and markers.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
)]
pub struct Implemented(pub HashSet<GlobalID>);

/// A component for storing the symbols that are the defined in the scope of the
/// current symbol.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
)]
pub struct Member(pub HashMap<String, ID>);

/// The ID of the parent of the current symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
)]
pub struct Parent(pub ID);

/// The span of where the symbol is defined.
///
/// This is mainly used for diagnostics reporting.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut,
)]
pub struct LocationSpan(pub Span);

/// The component of the `implements` symbol.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
)]
pub struct Implements(pub GlobalID);

/// An enumeration of all the kinds of symbols that can be defined.
///
/// Every symbol should have a kind.
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
)]
#[allow(missing_docs)]
pub enum SymbolKind {
    Module,
    Struct,
    Trait,
    Enum,
    Type,
    Constant,
    Function,
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

impl SymbolKind {
    /// Checks if this kind of symbol has a [`Member`] component.
    pub fn has_member(&self) -> bool {
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
    pub fn has_implemented(&self) -> bool {
        matches!(self, Self::Trait | Self::Enum | Self::Struct | Self::Marker)
    }

    /// Checks if the symbol is either a struct or an enum.
    pub fn is_adt(&self) -> bool { matches!(self, Self::Struct | Self::Enum) }

    /// Gets the description string of the kind.
    pub fn kind_str(&self) -> &'static str {
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
}

/// The external linkage of a symbol.
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
)]
pub enum Extern {
    /// The function is an external function that is implemented in the c call
    /// convention.
    C,

    /// Unknown external linkage.
    Unknown,
}

/// Represents the using of a module member.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Using {
    /// The ID of the module member that is being used.
    pub id: GlobalID,

    /// The span of the using statement.
    pub span: Span,
}

/// Represents the import of a module.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct Import(HashMap<String, Using>);

/// A component for tagging an implementation as the final implementation.
///
/// The trait and marker implementation can only have this component.
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
pub struct FinalImplementation;

/// A component for tagging a positive trait implementation as a constant
/// implementation.
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
pub struct ConstTraitImplementation;
