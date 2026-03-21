//! Contains the definition of the [`Array`] register.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_term::{constant::Constant, r#type::Type};
use pernixc_type_system::{OverflowError, Succeeded, normalizer::Normalizer};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    resolution_visitor::{
        self, Abort, Resolution, ResolutionMut, ResolutionVisitor,
    },
    value::{Environment, TypeOf, Value, register::Register},
};

macro_rules! visit_array_literals {
    ($values:expr, $literal_accessor:ident, $visitor:expr, $accept_method:ident) => {{
        for value in $values {
            if let Some(literal) = value.$literal_accessor() {
                literal.$accept_method($visitor).await?;
            }
        }
        Ok(())
    }};
}

macro_rules! visit_array_type {
    ($visitor:expr, $visit_method:ident, $resolution_ctor:ident, $element_type:expr, $span:expr) => {{
        $visitor
            .$visit_method($resolution_ctor::Type($element_type), $span)
            .await?;
        Ok(())
    }};
}

/// Represents an array of values.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct Array {
    /// The elements of the array.
    pub elements: Vec<Value>,

    /// The type of the element in the array.
    ///
    /// The type must be declared separately as the element values can have
    /// different lifetime values; thus, the type of the array can't be solely
    /// determined by one of the element values.
    pub element_type: Type,
}

impl Array {
    /// Returns the list of registers that are used in the array.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.elements.iter().filter_map(|x| x.as_register().copied()).collect()
    }
}

impl crate::visitor::Element for Array {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for element in &self.elements {
            visitor.visit_value(std::borrow::Cow::Borrowed(element));
        }
    }
}

pub(super) async fn transform_array<
    T: resolution_visitor::MutableResolutionVisitor,
>(
    array: &mut Array,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_array_literals!(
        &mut array.elements,
        as_literal_mut,
        visitor,
        accept_mut
    )?;
    visit_array_type!(
        visitor,
        visit_mut,
        ResolutionMut,
        &mut array.element_type,
        span
    )
}

pub(super) async fn inspect_array<T: ResolutionVisitor>(
    array: &Array,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_array_literals!(&array.elements, as_literal, visitor, accept)?;
    visit_array_type!(visitor, visit, Resolution, &array.element_type, span)
}

impl TypeOf<&Array> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        array: &Array,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, OverflowError> {
        Ok(environment
            .type_environment
            .simplify(Type::Array(pernixc_term::r#type::Array {
                r#type: Box::new(array.element_type.clone()),
                length: Constant::Primitive(
                    pernixc_term::constant::Primitive::Usize(
                        array.elements.len() as u64,
                    ),
                ),
            }))
            .await?
            .deref()
            .clone())
    }
}
