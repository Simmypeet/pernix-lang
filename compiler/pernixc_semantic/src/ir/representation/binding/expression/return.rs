use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression};
use crate::{
    error::{self, ReturnIsNotAllowed},
    ir::{
        self,
        control_flow_graph::InsertTerminatorError,
        instruction::{self, Instruction, ScopePop, Terminator},
        representation::binding::{
            infer::{self, InferenceVariable},
            stack::Scope,
            Binder, Error, SemanticError,
        },
        value::{
            literal::{self, Literal, Unit},
            Value,
        },
    },
    symbol::{
        table::{self, resolution},
        CallableID,
    },
    type_system::{
        self,
        model::Model,
        term::r#type::{Constraint, Expected, Type},
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Bind<&syntax_tree::expression::Return> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Return,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let Some(callable_id) = CallableID::try_from(self.current_site).ok()
        else {
            self.create_handler_wrapper(handler).receive(Box::new(
                ReturnIsNotAllowed { span: syntax_tree.span() },
            ));

            return Err(Error::Semantic(SemanticError(syntax_tree.span())));
        };

        let callable = self.table.get_callable(callable_id).unwrap();
        let return_type =
            infer::Model::from_default_type(callable.return_type().clone());

        let value = syntax_tree.binary().as_ref().map_or_else(
            || {
                Ok(Value::Literal(Literal::Unit(Unit {
                    span: syntax_tree.span(),
                })))
            },
            |syn| self.bind_value_or_error(syn, handler),
        )?;
        let value_ty = self.type_of_value(&value)?;

        // do type check
        let _ = self.type_check(
            &value_ty,
            Expected::Known(return_type),
            syntax_tree.span(),
            true,
            handler,
        )?;

        // pop all the needed scopes
        for popping_scope in
            self.stack.scopes().iter().map(Scope::scope_id).rev()
        {
            let _ = self
                .intermediate_representation
                .control_flow_graph
                .get_block_mut(self.current_block_id)
                .unwrap()
                .insert_instruction(Instruction::ScopePop(ScopePop(
                    popping_scope,
                )));
        }

        // insert the return instruction
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Return(instruction::Return { value }),
            )
        {
            panic!("invalid block id");
        }

        let value =
            Value::Literal(Literal::Unreachable(literal::Unreachable {
                r#type: {
                    let inference = InferenceVariable::new();

                    assert!(self
                        .inference_context
                        .register(inference, Constraint::All(true)));

                    Type::Inference(inference)
                },
                span: syntax_tree.span(),
            }));

        Ok(Expression::RValue(value))
    }
}
