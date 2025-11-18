//! Contains the definition of a [`Tuple`] register assignment.

use std::{collections::BTreeSet, ops::Deref};

use pernixc_arena::ID;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::r#type::Type;
use pernixc_type_system::Succeeded;

use crate::{
    transform::Transformer,
    value::{register::Register, TypeOf, Value},
    Values,
};

/// Represents an element of a [`Tuple`].
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
    StableHash,
)]
pub struct TupleElement {
    /// The value of the tuple element.
    pub value: Value,

    /// Whether the tuple element is unpacked.
    pub is_unpacked: bool,
}

/// Represents a tuple of values.
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
    StableHash,
)]
pub struct Tuple {
    /// The elements of kthe tuple.
    pub elements: Vec<TupleElement>,
}

impl Tuple {
    /// Returns the list of registers that are used in the tuple.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.elements
            .iter()
            .filter_map(|x| x.value.as_register().copied())
            .collect()
    }
}

impl crate::visitor::Element for Tuple {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for element in &self.elements {
            visitor.visit_value(std::borrow::Cow::Borrowed(&element.value));
        }
    }
}

pub(super) async fn transform_tuple<
    T: Transformer<pernixc_term::lifetime::Lifetime>
        + Transformer<Type>
        + Transformer<pernixc_term::constant::Constant>,
>(
    tuple: &mut Tuple,
    transformer: &mut T,
    _span: Option<pernixc_lexical::tree::RelativeSpan>,
    _engine: &pernixc_query::TrackedEngine,
) -> Result<(), CyclicError> {
    for element in
        tuple.elements.iter_mut().filter_map(|x| x.value.as_literal_mut())
    {
        element.transform(transformer).await?;
    }

    Ok(())
}

impl TypeOf<&Tuple> for Values {
    async fn type_of<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        value: &Tuple,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, pernixc_type_system::Error>
    {
        let mut constraints = BTreeSet::new();
        let mut elements = Vec::new();

        for element in &value.elements {
            let Succeeded { result: ty, constraints: new_constraint } =
                Box::pin(self.type_of(&element.value, environment)).await?;

            constraints.extend(new_constraint);

            if element.is_unpacked {
                match ty {
                    Type::Tuple(ty) => elements.extend(ty.elements),
                    ty => elements.push(pernixc_term::tuple::Element {
                        term: ty,
                        is_unpacked: true,
                    }),
                }
            } else {
                elements.push(pernixc_term::tuple::Element {
                    term: ty,
                    is_unpacked: false,
                });
            }
        }

        let tuple_ty = Type::Tuple(pernixc_term::tuple::Tuple { elements });
        let mut simplified_ty = environment
            .type_environment
            .simplify(tuple_ty)
            .await?
            .deref()
            .clone();
        simplified_ty.constraints.extend(constraints);
        Ok(simplified_ty)
    }
}
