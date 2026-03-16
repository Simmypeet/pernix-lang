//! Contains the definition of the [`Struct`] register.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::fields::Field;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::Symbol, instantiation::Instantiation, r#type::Type,
};
use pernixc_type_system::OverflowError;
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    transform::{ResolutionMut, Transformer},
    value::{TypeOf, Value, register::Register},
};

/// Represents a struct value.
#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode, StableHash)]
pub struct Struct {
    symbol: Symbol,
    initializers_by_field_id: HashMap<ID<Field>, Value>,
}

impl Struct {
    #[must_use]
    pub const fn new(
        symbol: Symbol,
        initializers_by_field_id: HashMap<ID<Field>, Value>,
    ) -> Self {
        Self { symbol, initializers_by_field_id }
    }

    /// Returns the list of registers that are used in the struct.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.initializers_by_field_id
            .values()
            .filter_map(|x| x.as_register().copied())
            .collect()
    }

    #[must_use]
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Instantiation {
        self.symbol.create_instantiation(engine).await
    }

    #[must_use]
    pub const fn struct_id(&self) -> Global<pernixc_symbol::ID> {
        self.symbol.id()
    }
}

impl crate::visitor::Element for Struct {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for value in self.initializers_by_field_id.values() {
            visitor.visit_value(std::borrow::Cow::Borrowed(value));
        }
    }
}

pub(super) async fn transform_struct<T: Transformer>(
    st: &mut Struct,
    transformer: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) {
    for value in st.initializers_by_field_id.values_mut() {
        if let Some(literal) = value.as_literal_mut() {
            literal.transform(transformer).await;
        }
    }

    transformer.transform(ResolutionMut::Symbol(&mut st.symbol), span).await;
}

impl TypeOf<&Struct> for Values {
    async fn type_of<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        value: &Struct,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, OverflowError> {
        Ok(environment
            .type_environment
            .simplify(Type::Symbol(value.symbol.clone()))
            .await?
            .deref()
            .clone())
    }
}
