//! Contains the definition of [`Register`] and its variants.

use pernixc_base::source_file::Span;

use super::{Inspect, InvalidValueError, Value};
use crate::{
    arena::ID,
    ir::{address::Address, representation::Representation},
    semantic::{
        model::Model,
        term::{
            self,
            lifetime::Lifetime,
            r#type::{Qualifier, Reference, Type},
        },
    },
    symbol::table::{self, Table},
};

/// Represents an element of a [`Tuple`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TupleElement<M: Model> {
    Regular(Value<M>),
    Unpacked(Value<M>),
}

impl<M: Model> TupleElement<M> {
    /// Returns a reference to the value.
    #[must_use]
    pub const fn as_value(&self) -> &Value<M> {
        match self {
            Self::Regular(value) | Self::Unpacked(value) => value,
        }
    }

    /// Returns a mutable reference to the value.
    #[must_use]
    pub fn as_value_mut(&mut self) -> &mut Value<M> {
        match self {
            Self::Regular(value) | Self::Unpacked(value) => value,
        }
    }

    /// Consumes the element and returns the value.
    #[must_use]
    pub fn into_value(self) -> Value<M> {
        match self {
            Self::Regular(value) | Self::Unpacked(value) => value,
        }
    }
}

/// Represents a tuple of values.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<M: Model> {
    /// The elements of kthe tuple.
    pub elements: Vec<TupleElement<M>>,

    /// The span where the tuple is created.
    pub span: Option<Span>,
}

impl<M: Model> Inspect<M> for Tuple<M> {
    fn type_of(
        &self,
        ir: &Representation<M>,
        table: &Table<impl table::State>,
    ) -> Result<Type<M>, InvalidValueError> {
        let tuple_elements = self
            .elements
            .iter()
            .map(|element| match element {
                TupleElement::Regular(regular) => {
                    regular.type_of(ir, table).map(term::TupleElement::Regular)
                }
                TupleElement::Unpacked(unpacked) => unpacked
                    .type_of(ir, table)
                    .map(term::TupleElement::Unpacked),
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Type::Tuple(term::Tuple { elements: tuple_elements }))
    }

    fn get_span(
        &self,
        _: &Representation<M>,
        _: &Table<impl table::State>,
    ) -> Result<Option<Span>, InvalidValueError> {
        Ok(self.span.clone())
    }
}

/// An enumeration of either moving or copying loads.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LoadKind {
    /// The value is memcpy'd from the address and the value in the address is
    /// invalidated.
    Move,

    /// The value is copied from the address via `Copy` trait.
    Copy,
}

/// Represents a load/read from an address in memory.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Load<M: Model> {
    /// The address where the value is stored and will be read from.
    pub address: Address<M>,

    /// The type of the value stored at the address.
    pub address_type: Type<M>,

    /// The kind of load.
    pub kind: LoadKind,

    /// The span where the load is created.
    pub span: Option<Span>,
}

impl<M: Model> Inspect<M> for Load<M> {
    fn type_of(
        &self,
        _: &Representation<M>,
        _: &Table<impl table::State>,
    ) -> Result<Type<M>, InvalidValueError> {
        Ok(self.address_type.clone())
    }

    fn get_span(
        &self,
        _: &Representation<M>,
        _: &Table<impl table::State>,
    ) -> Result<Option<Span>, InvalidValueError> {
        Ok(self.span.clone())
    }
}

/// Obtains a reference at the given address.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferenceOf<M: Model> {
    /// The address to the value.
    pub address: Address<M>,

    /// The type of the value stored at the address.
    pub address_type: Type<M>,

    /// The qualfier of the reference.
    pub qualifier: Qualifier,

    /// The span where the reference is created.
    pub span: Option<Span>,

    /// The lifetime produced by the reference.
    pub lifetime: Lifetime<M>,
}

impl<M: Model> Inspect<M> for ReferenceOf<M> {
    fn type_of(
        &self,
        _: &Representation<M>,
        _: &Table<impl table::State>,
    ) -> Result<Type<M>, InvalidValueError> {
        Ok(Type::Reference(Reference {
            qualifier: self.qualifier,
            lifetime: self.lifetime.clone(),
            pointee: Box::new(self.address_type.clone()),
        }))
    }

    fn get_span(
        &self,
        _: &Representation<M>,
        _: &Table<impl table::State>,
    ) -> Result<Option<Span>, InvalidValueError> {
        Ok(self.span.clone())
    }
}

/// An enumeration of the different kinds of registers.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Register<M: Model> {
    Tuple(Tuple<M>),
    Load(Load<M>),
    ReferenceOf(ReferenceOf<M>),
}

impl<M: Model> Inspect<M> for ID<Register<M>> {
    fn type_of(
        &self,
        ir: &Representation<M>,
        table: &Table<impl table::State>,
    ) -> Result<Type<M>, InvalidValueError> {
        let register = ir.registers().get(*self).ok_or(InvalidValueError)?;

        match register {
            Register::Tuple(reg) => reg.type_of(ir, table),
            Register::Load(reg) => reg.type_of(ir, table),
            Register::ReferenceOf(reg) => reg.type_of(ir, table),
        }
    }

    fn get_span(
        &self,
        ir: &Representation<M>,
        table: &Table<impl table::State>,
    ) -> Result<Option<Span>, InvalidValueError> {
        let register = ir.registers().get(*self).ok_or(InvalidValueError)?;

        match register {
            Register::Tuple(reg) => reg.get_span(ir, table),
            Register::Load(reg) => reg.get_span(ir, table),
            Register::ReferenceOf(reg) => reg.get_span(ir, table),
        }
    }
}
