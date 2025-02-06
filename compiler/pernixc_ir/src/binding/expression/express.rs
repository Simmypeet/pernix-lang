use std::collections::hash_map::Entry;

use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;
use pernixc_table::diagnostic::Diagnostic;
use pernixc_term::r#type::Type;

use super::{Bind, Config, Expression};
use crate::{
    binding::{
        diagnostic::{BlockWithGivenLableNameNotFound, ExpressOutsideBlock},
        infer::Expected,
        stack::Scope,
        Binder, Error, SemanticError,
    },
    instruction::{Instruction, Jump, ScopePop, Terminator, UnconditionalJump},
    model::Constraint,
    value::{
        literal::{self, Literal},
        Value,
    },
};

impl Bind<&syntax_tree::expression::Express> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Express,
        _: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let label = syntax_tree
            .label()
            .as_ref()
            .map(|x| x.identifier().span.str().to_owned());

        let value = syntax_tree
            .binary()
            .as_ref()
            .map(|x| self.bind_value_or_error(x, handler))
            .transpose()?;

        let mut scope_id = None;

        // find the block state
        for scope in self.stack.scopes().iter().rev() {
            let Some(get_block_state) =
                self.block_states_by_scope_id.get(&scope.scope_id())
            else {
                continue;
            };

            if let Some(label) = &label {
                if get_block_state.label.as_ref() != Some(label) {
                    continue;
                }
            }

            scope_id = Some(scope.scope_id());
            break;
        }

        // block state not found report the error
        let Some(scope_id) = scope_id else {
            if label.is_some() {
                self.create_handler_wrapper(handler).receive(Box::new(
                    BlockWithGivenLableNameNotFound {
                        span: syntax_tree
                            .label()
                            .as_ref()
                            .map(|x| x.identifier().span.clone())
                            .unwrap(),
                    },
                ));
            } else {
                self.create_handler_wrapper(handler).receive(Box::new(
                    ExpressOutsideBlock { span: syntax_tree.span() },
                ));
            };

            return Err(Error::Semantic(SemanticError(syntax_tree.span())));
        };

        let value_type = value.as_ref().map_or_else(
            || Ok(Type::Tuple(pernixc_term::Tuple { elements: Vec::new() })),
            |x| self.type_of_value(x),
        )?;

        if let Some(express_type) =
            &self.block_states_by_scope_id.get(&scope_id).unwrap().express_type
        {
            let _ = self.type_check(
                &value_type,
                Expected::Known(express_type.clone()),
                syntax_tree.span(),
                true,
                handler,
            )?;
        } else {
            // have no express before, gets to decide the type.
            self.block_states_by_scope_id
                .get_mut(&scope_id)
                .unwrap()
                .express_type = Some(value_type);
        };

        if let Entry::Vacant(entry) = self
            .block_states_by_scope_id
            .get_mut(&scope_id)
            .unwrap()
            .incoming_values
            .entry(self.current_block_id)
        {
            entry.insert(value.unwrap_or(Value::Literal(Literal::Unit(
                literal::Unit { span: Some(syntax_tree.span()) },
            ))));
        }

        // pop all the needed scopes
        for popping_scope in self
            .stack
            .scopes()
            .iter()
            .rev()
            .map(Scope::scope_id)
            .take_while(|x| *x != scope_id)
            .chain(std::iter::once(scope_id))
        {
            let _ = self
                .intermediate_representation
                .control_flow_graph
                .get_block_mut(self.current_block_id)
                .unwrap()
                .add_instruction(Instruction::ScopePop(ScopePop(
                    popping_scope,
                )));
        }

        // jump to the block
        self.intermediate_representation.control_flow_graph.insert_terminator(
            self.current_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: self
                    .block_states_by_scope_id
                    .get(&scope_id)
                    .unwrap()
                    .successor_block_id,
            })),
        );

        let value =
            Value::Literal(Literal::Unreachable(literal::Unreachable {
                r#type: {
                    let inference =
                        self.create_type_inference(Constraint::All(true));

                    Type::Inference(inference)
                },
                span: Some(syntax_tree.span()),
            }));

        Ok(Expression::RValue(value))
    }
}

#[cfg(test)]
mod test;
