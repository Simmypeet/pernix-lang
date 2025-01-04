//!

use std::collections::HashMap;

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
