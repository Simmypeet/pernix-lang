//! Defines the [`ResumeCall`] register.

use std::ops::Deref;

use getset::{CopyGetters, Getters};
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};
use pernixc_type_system::Error;
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    handling_scope::{HandlerClauseID, OperationHandlerID},
    transform::Transformer,
    value::{TypeOf, Value},
    visitor,
};

/// Represents a `reumse(value)` expression found inside operation handler
/// clause.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Getters,
    CopyGetters,
)]
pub struct ResumeCall {
    /// The value to be resumed.
    value: Value,

    /// The operation handler clause ID where this `resume` call is located.
    #[get_copy = "pub"]
    operation_handler_id: OperationHandlerID,
}

impl ResumeCall {
    /// Creates a new [`ResumeCall`].
    #[must_use]
    pub const fn new(
        value: Value,
        operation_handler_id: OperationHandlerID,
    ) -> Self {
        Self { value, operation_handler_id }
    }

    /// Returns the handler clause ID where this `resume` call is located.
    #[must_use]
    pub fn handler_clause_id(&self) -> HandlerClauseID {
        self.operation_handler_id.handler_clause_id()
    }

    /// Returns the handling scope ID where this `resume` call is located.
    #[must_use]
    pub const fn handling_scope_id(
        &self,
    ) -> pernixc_arena::ID<crate::handling_scope::HandlingScope> {
        self.operation_handler_id.handling_scope_id()
    }
}

pub(super) async fn transform_resume_call<
    T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
>(
    resume_call: &mut ResumeCall,
    transformer: &mut T,
) {
    if let Value::Literal(literal) = &mut resume_call.value {
        literal.transform(transformer).await;
    }
}

impl visitor::Element for ResumeCall {
    fn accept(&self, visitor: &mut impl visitor::Visitor) {
        visitor.visit_value(std::borrow::Cow::Borrowed(&self.value));
    }
}

impl TypeOf<&ResumeCall> for Values {
    async fn type_of<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        value: &ResumeCall,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, Error> {
        let handling_scope =
            &environment.handling_scopes[value.handling_scope_id()];

        Ok(environment
            .type_environment
            .simplify(handling_scope.return_type().clone())
            .await?
            .deref()
            .clone())
    }
}
