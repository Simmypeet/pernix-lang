//! Contains the definition of the [`Struct`] register.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_semantic_element::fields::Field;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, Symbol},
    lifetime::Lifetime,
    r#type::Type,
};

use crate::{
    transform::Transformer,
    value::{
        register::{transform_generic_arguments, Register},
        TypeOf, Value,
    },
    Values,
};

/// Represents a struct value.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct Struct {
    /// The struct ID of the struct.
    pub struct_id: Global<pernixc_symbol::ID>,

    /// The field initializers of the struct.
    pub initializers_by_field_id: HashMap<ID<Field>, Value>,

    /// The generic arguments supplied to the struct.
    pub generic_arguments: GenericArguments,
}

impl Struct {
    /// Returns the list of registers that are used in the struct.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.initializers_by_field_id
            .values()
            .filter_map(|x| x.as_register().copied())
            .collect()
    }
}

impl crate::visitor::Element for Struct {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for value in self.initializers_by_field_id.values() {
            visitor.visit_value(std::borrow::Cow::Borrowed(value));
        }
    }
}

pub(super) async fn transform_struct<
    T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
>(
    st: &mut Struct,
    transformer: &mut T,
    span: Option<pernixc_lexical::tree::RelativeSpan>,
    engine: &TrackedEngine,
) -> Result<(), CyclicError> {
    for value in st.initializers_by_field_id.values_mut() {
        if let Some(literal) = value.as_literal_mut() {
            literal.transform(transformer).await?;
        }
    }

    transform_generic_arguments(
        transformer,
        st.struct_id,
        span,
        engine,
        &mut st.generic_arguments,
    )
    .await
}

impl TypeOf<&Struct> for Values {
    async fn type_of<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        value: &Struct,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, pernixc_type_system::Error>
    {
        Ok(environment
            .type_environment
            .simplify(Type::Symbol(Symbol {
                id: value.struct_id,
                generic_arguments: value.generic_arguments.clone(),
            }))
            .await?
            .deref()
            .clone())
    }
}
