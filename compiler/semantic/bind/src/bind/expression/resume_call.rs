use std::ops::Deref;

use pernixc_handler::Handler;
use pernixc_ir::value::register::{Assignment, ResumeCall};
use pernixc_semantic_element::return_type::get_return_type;
use pernixc_source_file::SourceElement;
use pernixc_term::{
    generic_parameters::{LifetimeParameterID, get_generic_parameters},
    instantiation::get_instantiation,
    lifetime::Lifetime,
};

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{Binder, BindingError, Error},
    diagnostic::{Diagnostic, ResumeCallOutsideOperationHandler},
};

impl Bind<&pernixc_syntax::expression::unit::ResumeCall> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::ResumeCall,
        _guidance: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        // extract the resume value expression
        let Some(resume_val) =
            syntax_tree.single_call().and_then(|x| x.expression())
        else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let Some(operation_handler_id) = self.current_operation_handler_id()
        else {
            handler.receive(Diagnostic::ResumeCallOutsideOperationHandler(
                ResumeCallOutsideOperationHandler { span: syntax_tree.span() },
            ));

            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let handler_clause =
            self.get_handler_clause(operation_handler_id.handler_clause_id());

        let operation_symbol_id = handler_clause
            .effect_id()
            .target_id
            .make_global(operation_handler_id.operation_id());

        let mut handler_instantiation = self
            .engine()
            .get_instantiation(
                handler_clause.effect_id(),
                handler_clause.generic_arguments().clone(),
            )
            .await?
            .expect("should've been a valid instantiation");

        // erase the lifetime of the operation generic parameters
        let operation_generic_parameters =
            self.engine().get_generic_parameters(operation_symbol_id).await?;

        handler_instantiation.lifetimes.extend(
            operation_generic_parameters.lifetimes().ids().map(|x| {
                (
                    Lifetime::Parameter(LifetimeParameterID::new(
                        operation_symbol_id,
                        x,
                    )),
                    Lifetime::Erased,
                )
            }),
        );

        // the return type of the operation is the resuming value.
        let mut return_type = self
            .engine()
            .get_return_type(operation_symbol_id)
            .await?
            .deref()
            .clone();

        handler_instantiation.instantiate(&mut return_type);

        // bind the resume value expression
        let resume_value = Box::pin(self.bind_value_or_error(
            &resume_val,
            Some(&return_type),
            handler,
        ))
        .await?;

        let register_id = self.create_register_assignment(
            Assignment::ResumeCall(ResumeCall::new(
                resume_value,
                operation_handler_id,
            )),
            syntax_tree.span(),
        );

        Ok(Expression::register_id_value(register_id))
    }
}
