//! Contains the definition of patterns

use std::collections::HashMap;

use diagnostic::AlreadyBoundName;
use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_source_file::{SourceElement, Span};

use crate::{
    component::derived::{
        fields::Field,
        ir::{address::Address, instruction::SwitchValue},
    },
    diagnostic::Diagnostic,
    table::GlobalID,
    term,
    term::r#type::Qualifier,
};

pub mod diagnostic;

/// An integer literal pattern
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Integer {
    /// The value of the ingteger literal.
    pub value: SwitchValue,

    /// The span of the integer literal.
    pub span: Span,
}

/// A boolean literal pattern
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Boolean {
    /// The value of the boolean literal.
    pub value: bool,

    /// The span of the boolean literal.
    pub span: Span,
}

/// A pattern where the value is bound to a name
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named {
    /// The name of the pattern.
    pub name: String,

    /// Whether the binding is mutable or not.
    pub is_mutable: bool,

    /// If `Some` the binding is a reference binding with the qualifier.
    /// Otherwise, the binding is a value binding.
    pub reference_binding: Option<Qualifier>,

    /// The span to the identifier of the name binding.
    pub span: Span,
}

/// A refutable pattern specifying a variant of an enum.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    /// The ID of the variant that the pattern matches.
    pub variant_id: GlobalID,

    /// The pattern binding for the variant.
    pub pattern: Option<Box<Refutable>>,

    /// The span of the enum variant pattern.
    pub span: Span,
}

/// A pattern bound to an element in a tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleElement<T> {
    /// The pattern binding for the element.
    pub pattern: T,

    /// Whether the element is unpacked or not.
    pub is_packed: bool,
}

impl<T> TupleElement<T> {
    /// Creates a new **packed** tuple element.
    #[must_use]
    pub const fn new_packed(pattern: T) -> Self {
        Self { pattern, is_packed: true }
    }

    /// Creates a new **non-packed** tuple element.
    #[must_use]
    pub const fn new_non_packed(pattern: T) -> Self {
        Self { pattern, is_packed: false }
    }
}

/// A pattern bound to a tuple type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Tuple<T> {
    /// The pattern binding for each element in the tuple.
    pub elements: Vec<TupleElement<T>>,

    /// The span of the whole tuple pattern.
    pub span: Span,
}

/// A pattern that matches on a struct with fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Structural<T> {
    /// The ID of the struct that the pattern matches.
    pub struct_id: GlobalID,

    /// Mapping from each field to the pattern that the field must match.
    pub patterns_by_field_id: HashMap<ID<Field>, T>,

    /// The span of the whole structural pattern.
    pub span: Span,
}

/// A pattern that discards the value
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Wildcard {
    /// The span of the wildcard.
    pub span: Span,
}

/// A pattern that cannot be refuted (always matches)
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, derive_more::From)]
#[allow(missing_docs)]
pub enum Irrefutable {
    Named(Named),
    Tuple(Tuple<Self>),
    Structural(Structural<Self>),
    Wildcard(Wildcard),
}

impl SourceElement for Irrefutable {
    fn span(&self) -> Span {
        match self {
            Self::Named(named) => named.span.clone(),
            Self::Tuple(tuple) => tuple.span.clone(),
            Self::Structural(structural) => structural.span.clone(),
            Self::Wildcard(wildcard) => wildcard.span.clone(),
        }
    }
}

/// A pattern that can be refuted (may not always match)
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, derive_more::From)]
pub enum Refutable {
    Boolean(Boolean),
    Integer(Integer),
    Named(Named),
    Enum(Enum),
    Tuple(Tuple<Self>),
    Structural(Structural<Self>),
    Wildcard(Wildcard),
}

impl SourceElement for Refutable {
    fn span(&self) -> Span {
        match self {
            Self::Boolean(boolean) => boolean.span.clone(),
            Self::Integer(numeric) => numeric.span.clone(),
            Self::Named(named) => named.span.clone(),
            Self::Enum(r#enum) => r#enum.span.clone(),
            Self::Tuple(tuple) => tuple.span.clone(),
            Self::Structural(structural) => structural.span.clone(),
            Self::Wildcard(wildcard) => wildcard.span.clone(),
        }
    }
}

/// Represents an lvalue binding in the pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NameBinding<M: term::Model> {
    /// Whether the binding is mutable or not.
    pub mutable: bool,

    /// The address where the value is stored.
    pub load_address: Address<M>,

    /// The span of the identifier of the name binding.
    pub span: Span,
}

/// Contains all the named bindings in the patterns.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct NameBindingPoint<M: term::Model> {
    /// Mapping from the name of the binding to the named pattern.
    pub named_patterns_by_name: HashMap<String, NameBinding<M>>,
}

impl<M: term::Model> NameBindingPoint<M> {
    /// Inserts a name binding into the point.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] with the passed name binding if the name is already
    /// bound.
    pub fn insert(
        &mut self,
        name: String,
        name_binding: NameBinding<M>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<(), NameBinding<M>> {
        match self.named_patterns_by_name.entry(name) {
            std::collections::hash_map::Entry::Occupied(entry) => {
                handler.receive(Box::new(AlreadyBoundName {
                    already_bound_identifier_span: entry.get().span.clone(),
                    new_binding_span: name_binding.span.clone(),
                }));

                Err(name_binding)
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(name_binding);
                Ok(())
            }
        }
    }
}
