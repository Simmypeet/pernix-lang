//! Contains the definition of the [`Phi`] register.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_term::r#type::Type;
use pernixc_type_system::{OverflowError, Succeeded, normalizer::Normalizer};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    control_flow_graph::Block,
    resolution_visitor::{
        Abort, MutableResolutionVisitor, Resolution, ResolutionMut,
        ResolutionVisitor,
    },
    value::{Environment, TypeOf, Value, register::Register},
};

macro_rules! visit_phi {
    (
        $phi:expr,
        $visitor:expr,
        $span:expr,
        $values_method:ident,
        $literal_accessor:ident,
        $accept_method:ident,
        $visit_method:ident,
        $resolution_ctor:ident,
        $type_expr:expr
    ) => {{
        for value in $phi.incoming_values.$values_method() {
            if let Some(literal) = value.$literal_accessor() {
                literal.$accept_method($visitor).await?;
            }
        }

        $visitor
            .$visit_method($resolution_ctor::Type($type_expr), $span)
            .await?;
        Ok(())
    }};
}

/// Represents a phi node in the SSA form.
///
/// A phi node is used to determine the value based on the flow of the
/// execution. This is typcially used in the control flow related expressions
/// such as `if` and `match`.
#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode, StableHash)]
pub struct Phi {
    /// Maps the incoming block to the value.
    pub incoming_values: HashMap<ID<Block>, Value>,

    /// The type of the phi node.
    ///
    /// The type must be declared separately as the incoming values can have
    /// different lifetime values; thus, the type of the phi node can't be
    /// solely determined by one of the incoming values.
    pub r#type: Type,
}

impl Phi {
    /// Returns the list of registers that are used in the phi node.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.incoming_values
            .values()
            .filter_map(|x| x.as_register().copied())
            .collect()
    }
}

impl crate::visitor::Element for Phi {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for value in self.incoming_values.values() {
            visitor.visit_value(std::borrow::Cow::Borrowed(value));
        }
    }
}

pub(super) async fn transform_phi<T: MutableResolutionVisitor>(
    phi: &mut Phi,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_phi!(
        phi,
        visitor,
        span,
        values_mut,
        as_literal_mut,
        accept_mut,
        visit_mut,
        ResolutionMut,
        &mut phi.r#type
    )
}

pub(super) async fn inspect_phi<T: ResolutionVisitor>(
    phi: &Phi,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_phi!(
        phi,
        visitor,
        span,
        values,
        as_literal,
        accept,
        visit,
        Resolution,
        &phi.r#type
    )
}

impl TypeOf<&Phi> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        phi: &Phi,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, OverflowError> {
        Ok(environment
            .type_environment
            .simplify(phi.r#type.clone())
            .await?
            .deref()
            .clone())
    }
}
