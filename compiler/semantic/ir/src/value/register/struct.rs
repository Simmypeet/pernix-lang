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
    resolution_visitor::{
        Abort, MutableResolutionVisitor, Resolution, ResolutionMut,
        ResolutionVisitor,
    },
    value::{TypeOf, Value, register::Register},
};

macro_rules! visit_struct {
    (
        $st:expr,
        $visitor:expr,
        $span:expr,
        $values_method:ident,
        $literal_accessor:ident,
        $accept_method:ident,
        $visit_method:ident,
        $resolution_ctor:ident,
        $symbol_expr:expr
    ) => {{
        for value in $st.initializers_by_field_id.$values_method() {
            if let Some(literal) = value.$literal_accessor() {
                literal.$accept_method($visitor).await?;
            }
        }

        $visitor
            .$visit_method($resolution_ctor::Symbol($symbol_expr), $span)
            .await?;
        Ok(())
    }};
}

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

    #[must_use]
    pub fn get_initializer_by_field_id(&self, field_id: ID<Field>) -> &Value {
        self.initializers_by_field_id.get(&field_id).unwrap()
    }
}

impl crate::visitor::Element for Struct {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for value in self.initializers_by_field_id.values() {
            visitor.visit_value(std::borrow::Cow::Borrowed(value));
        }
    }
}

pub(super) async fn transform_struct<T: MutableResolutionVisitor>(
    st: &mut Struct,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_struct!(
        st,
        visitor,
        span,
        values_mut,
        as_literal_mut,
        accept_mut,
        visit_mut,
        ResolutionMut,
        &mut st.symbol
    )
}

pub(super) async fn inspect_struct<T: ResolutionVisitor>(
    st: &Struct,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_struct!(
        st, visitor, span, values, as_literal, accept, visit, Resolution,
        &st.symbol
    )
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
