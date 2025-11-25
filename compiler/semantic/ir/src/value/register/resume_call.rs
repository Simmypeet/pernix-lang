//! Defines the [`ResumeCall`] register.

use std::ops::Deref;

use getset::Getters;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};
use pernixc_type_system::Error;

use crate::{
    Values,
    handling_scope::HandlerClauseID,
    transform::{Transformer, TypeTermSource},
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
    Serialize,
    Deserialize,
    Getters,
)]
pub struct ResumeCall {
    /// The value to be resumed.
    value: Value,
    handler_clause_id: HandlerClauseID,
    operation_symbol_id: pernixc_symbol::ID,

    /// The return type of the `resume` calling.
    #[get = "pub"]
    return_type: Type,
}

pub(super) async fn transform_resume_call<
    T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
>(
    resume_call: &mut ResumeCall,
    transformer: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), CyclicError> {
    if let Value::Literal(literal) = &mut resume_call.value {
        literal.transform(transformer).await?;
    }

    transformer
        .transform(
            &mut resume_call.return_type,
            TypeTermSource::ResumeCall,
            span,
        )
        .await
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
        Ok(environment
            .type_environment
            .simplify(value.return_type().clone())
            .await?
            .deref()
            .clone())
    }
}
