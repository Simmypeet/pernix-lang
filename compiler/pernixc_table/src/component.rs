//! Contains all the definition of the components that can be attached to a
//! symbol.
//!
//! # Classification of Components
//!
//! ## Local vs Presistent Components
//!
//! The components are said to be **local** if they are only used during the
//! compilation of the current target and is not being serialized.
//!
//! On the other hand, the components are said to be **persistent** if they can
//! be serialized and deserialized.
//!
//! ## Input vs Derived Components
//!
//! The **input** components are the starting components that are used to derive
//! other components from them.

use std::collections::{HashMap, HashSet};

use derive_more::{Deref, DerefMut};
use pernixc_source_file::Span;
use serde::{Deserialize, Serialize};

use crate::{GlobalID, ID};

/// Represents a component that can be later added to the table by being built
/// by the [`crate::query::Builder`] trait.
pub trait Derived {
    /// Returns the name of the component; used for debugging and diagnostics.
    fn component_name() -> &'static str;
}

/// A maker trait for the **input** components.
pub trait Input {}

/// A maker trait for the **input** components that can be mutated.
pub trait InputMut: Input {}

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

/// A **presistent-input** component representing the accessibility of a
/// symbol.
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

impl Input for Accessibility {}

/// A **presistent-input** component representing a name of a symbol.
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

impl Input for Name {}

/// A **presistent-input** component for the symbols that can be implemented
/// such as traits, structs, enums, and markers.
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

impl Input for Implemented {}

/// A **presistent-input** component for storing the symbols that are the
/// defined in the scope of the current symbol.
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

impl Input for Member {}

/// A **presistent-input** component for storing the parent of the symbol.
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

impl Input for Parent {}

/// A **local-input** component for storing the span of the symbol.
///
/// This is mainly used for diagnostics reporting.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut,
)]
pub struct LocationSpan(pub Span);

impl Input for LocationSpan {}

/// A **presistent-input** component for storing the symbol that is being
/// implemented by the current symbol.
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

impl Input for Implements {}

/// A **presistent-input** component representing an enumeration of the
/// different kinds of symbols.
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

impl Input for SymbolKind {}

impl SymbolKind {
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

/// A **presistent-input** component representing the external linkage of a
/// [`SymbolKind::Function`] symbol.
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

impl Input for Extern {}

/// Represents the using of a module member.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Using {
    /// The ID of the module member that is being used.
    pub id: GlobalID,

    /// The span of the using statement.
    pub span: Span,
}

/// A **local-input** component for storing the import statements of the module.
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

impl Input for Import {}

/// A **presistent-input** component for tagging various trait implementations
/// as the final implementation.
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

impl Input for FinalImplementation {}

/// A **presistent-input** component for tagging the
/// [`SymbolKind::PositiveTraitImplementation`] as a constant implementation.
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

impl Input for ConstTraitImplementation {}
