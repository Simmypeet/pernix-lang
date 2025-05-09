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

use std::{
    any::Any,
    collections::{HashMap, HashSet},
};

use derive_more::{Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use pernixc_source_file::Span;
use serde::{Deserialize, Serialize};

use crate::{GlobalAccessibility, GlobalID, TargetID, ID};

pub mod syntax_tree;

/// Represents a component that can be later added to the table by being built
/// by the [`crate::query::Builder`] trait.
pub trait Derived: Any + Send + Sync {
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
    EnumAsInner,
)]
pub enum Accessibility {
    /// The symbol is accessible from anywhere.
    #[default]
    Public,

    /// The symbol is accessible from the given module and its children.
    Scoped(ID),
}

impl Accessibility {
    /// Converts the accessibility into a [`GlobalAccessibility`].
    #[must_use]
    pub fn into_global(self, target_id: TargetID) -> GlobalAccessibility {
        match self {
            Self::Public => GlobalAccessibility::Public,
            Self::Scoped(id) => {
                GlobalAccessibility::Scoped(GlobalID::new(target_id, id))
            }
        }
    }
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
pub struct Parent {
    /// The ID of the parent symbol. This only be `None` if the symbol is a
    /// root module symbol.
    pub parent: Option<ID>,
}

impl Input for Parent {}

/// A **presistent-input** component for storing the span of the symbol.
///
/// This is mainly used for diagnostics reporting.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Deref,
    DerefMut,
)]
pub struct LocationSpan {
    /// The span of the symbol.
    #[serde(skip)]
    pub span: Option<Span>,
}

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

/// Information about the extern `C` function.
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
pub struct ExternC {
    /// `true` if the function is variadic.
    pub var_args: bool,
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
    C(ExternC),

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
pub struct Import(pub HashMap<String, Using>);

impl Input for Import {}

/// A **presistent-input** component exclusively for both positive and negative
/// trait implementations; primarily used for indentifying whether the
/// implementation is `final`
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
pub struct TraitImplementation {
    /// `true` if the implementation is final.
    pub is_final: bool,
}

impl Input for TraitImplementation {}

/// A **presistent-input** component for tagging the
/// [`SymbolKind::PositiveTraitImplementation`] exclusively as a constant
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
pub struct PositiveTraitImplementation {
    /// `true` if the implementation is a constant implementation.
    pub is_const: bool,
}

impl Input for PositiveTraitImplementation {}

/// The **presistent-input** component for storing the order in which the
/// variants are declared in an enum.
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
pub struct VariantDeclarationOrder {
    /// The order of the variant declaration.
    pub order: usize,
}

impl Input for VariantDeclarationOrder {}

/// The **presistent-input** component for determining whether the function is
/// defined with `const` or not.
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
    Deref,
    DerefMut,
)]
pub struct FunctionConstness(pub bool);

impl Input for FunctionConstness {}

/// The **presistent-input** component for determining whether the function is
/// defined with `unsafe` or not
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
    Deref,
    DerefMut,
)]
pub struct FunctionUnsafeness(pub bool);

impl Input for FunctionUnsafeness {}
