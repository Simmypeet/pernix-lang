use pernixc_lexical::tree::RelativeSpan;

use crate::{
    bind::{expression::postfix::BindState, Bind, Expression, Guidance},
    binder::Error,
};

pub(super) async fn bind_method_call(
    binder: &mut crate::binder::Binder<'_>,
    current_state: BindState,
    current_span: RelativeSpan,
    access: &pernixc_syntax::expression::postfix::MethodCall,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<(Expression, RelativeSpan), Error> {
    let expression = match current_state {
        BindState::Initial(unit) => {
            binder.bind(&unit, &Guidance::builder().build(), handler).await?
        }
        BindState::Bound(expression) => expression,
    };

    todo!()
}
