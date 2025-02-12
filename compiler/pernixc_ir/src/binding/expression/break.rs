use std::collections::hash_map::Entry;

use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree::{self, Label};
use pernixc_table::diagnostic::Diagnostic;
use pernixc_term::r#type::Type;

use super::{Bind, Config, Expression};
use crate::{
    binding::{
        diagnostic::LoopControlFlow,
        infer::{Expected, InferenceVariable},
        stack::Scope,
        Binder, Error, LoopKind,
    },
    instruction::{Instruction, Jump, ScopePop, Terminator, UnconditionalJump},
    model::Constraint,
    value::{
        literal::{self, Literal, Unreachable},
        Value,
    },
};

impl Bind<&syntax_tree::expression::Break> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Break,
        _: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let value = syntax_tree
            .binary()
            .as_ref()
            .map(|x| self.bind_value_or_error(x, handler))
            .transpose()?;

        let loop_scope_id = self.find_loop_scope_id(
            LoopControlFlow::Break,
            syntax_tree.label().as_ref().map(Label::identifier),
            syntax_tree.span(),
            handler,
        )?;

        let value_type = value.as_ref().map_or_else(
            || Ok(Type::Tuple(pernixc_term::Tuple { elements: Vec::new() })),
            |x| self.type_of_value(x, handler),
        )?;

        match &self.loop_states_by_scope_id.get(&loop_scope_id).unwrap().kind {
            // can only be unit type
            LoopKind::While => {
                self.type_check(
                    &value_type,
                    Expected::Known(Type::Tuple(pernixc_term::Tuple {
                        elements: Vec::new(),
                    })),
                    syntax_tree.binary().as_ref().map_or_else(
                        || syntax_tree.span(),
                        SourceElement::span,
                    ),
                    handler,
                )?;
            }

            // can use any type
            LoopKind::Loop { break_type, .. } => {
                if let Some(break_type) = break_type {
                    let _ = self.type_check(
                        &value_type,
                        Expected::Known(break_type.clone()),
                        syntax_tree.span(),
                        handler,
                    )?;
                } else {
                    *self
                        .loop_states_by_scope_id
                        .get_mut(&loop_scope_id)
                        .unwrap()
                        .kind
                        .as_loop_mut()
                        .unwrap()
                        .1 = Some(value_type);
                }

                // insert incoming value
                if let Entry::Vacant(entry) = self
                    .loop_states_by_scope_id
                    .get_mut(&loop_scope_id)
                    .unwrap()
                    .kind
                    .as_loop_mut()
                    .unwrap()
                    .0
                    .entry(self.current_block_id)
                {
                    entry.insert(value.unwrap_or(Value::Literal(
                        Literal::Unit(literal::Unit {
                            span: Some(syntax_tree.span()),
                        }),
                    )));
                }
            }
        }

        // pop all the needed scopes
        for popping_scope in self
            .stack
            .scopes()
            .iter()
            .rev()
            .map(Scope::scope_id)
            .take_while(|x| *x != loop_scope_id)
            .chain(std::iter::once(loop_scope_id))
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

        // jump to the exit block
        self.intermediate_representation.control_flow_graph.insert_terminator(
            self.current_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: self
                    .loop_states_by_scope_id
                    .get(&loop_scope_id)
                    .unwrap()
                    .exit_block_id,
            })),
        );

        let value = Value::Literal(Literal::Unreachable(Unreachable {
            r#type: {
                let inference = InferenceVariable::new();

                assert!(self
                    .inference_context
                    .register(inference, Constraint::All(true)));

                Type::Inference(inference)
            },
            span: Some(syntax_tree.span()),
        }));

        Ok(Expression::RValue(value))
    }
}
