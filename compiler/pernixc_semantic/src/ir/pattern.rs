//! Contains the definition of patterns

use std::collections::{hash_map::Entry, HashMap};

use pernixc_base::{handler::Handler, source_file::Span};

use super::address::Address;
use crate::{
    arena::ID,
    error::{AlreadyBoundName, Error},
    symbol::{Field, Struct},
    type_system::model::Model,
};

/// A trait that is implemented by [`Refutable`] and [`Irrefutable`].
pub trait Pattern {}

/// A numeric value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum NumericValue {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
}

/// A numeric literal pattern
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    /// The value of the numeric literal.
    pub value: NumericValue,
}

/// A boolean literal pattern
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Boolean {
    /// The value of the boolean literal.
    pub value: bool,
}

/// A pattern where the value is bound to a name
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named<M: Model> {
    /// The name of the pattern.
    pub name: String,

    /// The address to the location where the value is stored with this name
    /// binding.
    pub load_address: Address<M>,

    /// Determined if the underlying value is mutable or not.
    pub mutable: bool,

    /// The span to the identifier of the name binding.
    pub span: Option<Span>,
}

/// A pattern bound to a tuple type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Tuple<T: Pattern> {
    /// The pattern binding for each element in the tuple.
    pub elements: Vec<T>,
}

/// A pattern that matches on a struct with fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Structural<T: Pattern> {
    /// The ID of the struct that the pattern matches.
    pub struct_id: ID<Struct>,

    /// Mapping from each field to the pattern that the field must match.
    pub patterns_by_field_id: HashMap<ID<Field>, T>,
}

/// A pattern that discards the value
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Wildcard;

/// A pattern that cannot be refuted (always matches)
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Irrefutable<M: Model> {
    Named(Named<M>),
    Tuple(Tuple<Self>),
    Structural(Structural<Irrefutable<M>>),
    Wildcard(Wildcard),
}

/// A pattern that can be refuted (may not always match)
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Refutable<M: Model> {
    Boolean(Boolean),
    Numeric(Numeric),
    Named(Named<M>),
    Tuple(Tuple<Refutable<M>>),
    Structural(Structural<Refutable<M>>),
    Wildcard(Wildcard),
}

impl<M: Model> Pattern for Irrefutable<M> {}

impl<M: Model> Pattern for Refutable<M> {}

/// Contains all the named bindings in the patterns.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct NameBindingPoint<M: Model> {
    /// Mapping from the name of the binding to the named pattern.
    pub named_patterns_by_name: HashMap<String, Named<M>>,
}

impl<M: Model> NameBindingPoint<M> {
    /// Adds all the named binding occurrences in the pattern to this binding
    /// point.
    pub fn add_irrefutable_binding(
        &mut self,
        irrefutable: &Irrefutable<M>,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        match irrefutable {
            Irrefutable::Named(named) => {
                match self.named_patterns_by_name.entry(named.name.clone()) {
                    Entry::Occupied(entry) => {
                        if let (Some(first), Some(later)) =
                            (&entry.get().span, &named.span)
                        {
                            handler.receive(Box::new(AlreadyBoundName {
                                already_bound_identifier_span: first.clone(),
                                new_binding_span: later.clone(),
                            }));
                        }
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(named.clone());
                    }
                }
            }

            Irrefutable::Tuple(tuple) => {
                for element in &tuple.elements {
                    self.add_irrefutable_binding(element, handler);
                }
            }

            Irrefutable::Structural(structural) => {
                for pattern in structural.patterns_by_field_id.values() {
                    self.add_irrefutable_binding(pattern, handler);
                }
            }
            Irrefutable::Wildcard(_) => {}
        }
    }
}
