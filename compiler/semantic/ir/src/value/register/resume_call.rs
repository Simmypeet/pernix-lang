//! Defines the [`ResumeCall`] register.

use std::ops::Deref;

use getset::{CopyGetters, Getters};
use pernixc_term::r#type::Type;
use pernixc_type_system::OverflowError;
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    handling_scope::{HandlerClauseID, OperationHandlerID},
    resolution_visitor::{Abort, MutableResolutionVisitor, ResolutionVisitor},
    value::{TypeOf, Value},
    visitor,
};

macro_rules! visit_resume_literal {
    ($resume_call:expr, $visitor:expr, $literal_accessor:ident, $accept_method:ident) => {{
        if let Some(literal) = $resume_call.value.$literal_accessor() {
            literal.$accept_method($visitor).await?;
        }
        Ok(())
    }};
}

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

pub(super) async fn transform_resume_call<T: MutableResolutionVisitor>(
    resume_call: &mut ResumeCall,
    visitor: &mut T,
) -> Result<(), Abort> {
    visit_resume_literal!(resume_call, visitor, as_literal_mut, accept_mut)
}

pub(super) async fn inspect_resume_call<T: ResolutionVisitor>(
    resume_call: &ResumeCall,
    visitor: &mut T,
) -> Result<(), Abort> {
    visit_resume_literal!(resume_call, visitor, as_literal, accept)
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
        environment: &crate::value::ValueEnvironment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, OverflowError> {
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
