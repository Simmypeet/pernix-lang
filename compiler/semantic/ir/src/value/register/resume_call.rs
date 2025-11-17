//! Defines the [`ResumeCall`] register.

use getset::Getters;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};

use crate::{
    handling_scope::HandlerClauseID,
    transform::{Transformer, TypeTermSource},
    value::Value,
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
    span: Option<pernixc_lexical::tree::RelativeSpan>,
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
