use std::num::NonZeroUsize;

use pernixc_handler::Handler;
use pernixc_semantic::diagnostic::Diagnostic;
use pernixc_source_file::{SourceElement, Span};
use pernixc_syntax::syntax_tree;
use pernixc_term::r#type::{Primitive, Qualifier, Type};

use super::{Bind, Config, Expression, LValue, Target};
use crate::{
    binding::{
        diagnostic::{AssignToNonMutable, InvalidRelationalOperation},
        infer::{self, Expected},
        Binder, BindingError, Error,
    },
    instruction::{
        self, ConditionalJump, Instruction, Jump, ScopePop, ScopePush, Store,
        Terminator,
    },
    model::Constraint,
    value::{
        literal::{self, Boolean, Literal},
        register::{
            ArithmeticOperator, Assignment, Binary, BinaryOperator,
            BitwiseOperator, Load, Phi,
        },
        Value,
    },
};

impl Bind<&syntax_tree::expression::binary::Binary> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::binary::Binary,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        // transform the syntax tree into a binary tree
        let binary_node = to_binary_tree(syntax_tree);

        self.bind(&binary_node, config, handler)
    }
}

enum BinaryNode<'a> {
    Binary(Box<BinaryTree<'a>>),
    Expression(&'a syntax_tree::expression::binary::BinaryNode),
}

impl SourceElement for BinaryNode<'_> {
    fn span(&self) -> Span {
        match self {
            BinaryNode::Binary(tree) => tree.span(),
            BinaryNode::Expression(exp) => exp.span(),
        }
    }
}

struct BinaryTree<'a> {
    left: BinaryNode<'a>,
    right: BinaryNode<'a>,
    operator: &'a syntax_tree::expression::binary::BinaryOperator,
}

impl SourceElement for BinaryTree<'_> {
    fn span(&self) -> Span { self.left.span().join(&self.right.span()) }
}

const fn operator_precedence(
    operator: &syntax_tree::expression::binary::BinaryOperator,
) -> u32 {
    use syntax_tree::expression::binary::BinaryOperator;
    match operator {
        BinaryOperator::Multiply(_)
        | BinaryOperator::Divide(_)
        | BinaryOperator::Modulo(_) => 10,

        BinaryOperator::Add(_) | BinaryOperator::Subtract(_) => 9,

        BinaryOperator::BitwiseLeftShift(..)
        | BinaryOperator::BitwiseRightShift(..) => 8,

        BinaryOperator::LessThan(_)
        | BinaryOperator::LessThanOrEqual(..)
        | BinaryOperator::GreaterThan(_)
        | BinaryOperator::GreaterThanOrEqual(..) => 7,

        BinaryOperator::Equal(..) | BinaryOperator::NotEqual(..) => 6,

        BinaryOperator::BitwiseAnd(_) => 5,

        BinaryOperator::BitwiseXor(_) => 4,

        BinaryOperator::BitwiseOr(_) => 3,

        BinaryOperator::LogicalAnd(_) => 2,

        BinaryOperator::LogicalOr(_) => 1,

        BinaryOperator::Assign(_)
        | BinaryOperator::CompoundAdd(..)
        | BinaryOperator::CompoundSubtract(..)
        | BinaryOperator::CompoundMultiply(..)
        | BinaryOperator::CompoundDivide(..)
        | BinaryOperator::CompoundModulo(..)
        | BinaryOperator::CompoundBitwiseAnd(..)
        | BinaryOperator::CompoundBitwiseOr(..)
        | BinaryOperator::CompoundBitwiseXor(..)
        | BinaryOperator::CompoundBitwiseLeftShift(..)
        | BinaryOperator::CompoundBitwiseRightShift(..) => 0,
    }
}

fn to_binary_tree(
    syntax_tree: &syntax_tree::expression::binary::Binary,
) -> BinaryNode {
    let mut first = BinaryNode::Expression(&syntax_tree.first);
    let mut expressions = syntax_tree
        .chain
        .iter()
        .map(|(op, exp)| (op, Some(BinaryNode::Expression(exp))))
        .collect::<Vec<_>>();

    while !expressions.is_empty() {
        let candidate_operator_precedence = expressions
            .iter()
            .map(|(op, _)| operator_precedence(op))
            .max()
            .unwrap();

        // every operators except the assignments are left associative
        let is_left_associative = candidate_operator_precedence != 1;

        let fold_index = if is_left_associative {
            expressions.iter().position(|(op, _)| {
                operator_precedence(op) == candidate_operator_precedence
            })
        } else {
            expressions.iter().rposition(|(op, _)| {
                operator_precedence(op) == candidate_operator_precedence
            })
        }
        .unwrap();

        // replace the first expression with the binary tree
        if fold_index == 0 {
            let (operator, right_expression) = expressions.remove(0);

            // replace the first expression with the binary tree
            first = BinaryNode::Binary(Box::new(BinaryTree {
                left: first,
                right: right_expression.unwrap(),
                operator,
            }));
        } else {
            let (operator, right_expression) = expressions.remove(fold_index);

            // replace the first expression with the binary tree
            expressions[fold_index - 1].1 =
                Some(BinaryNode::Binary(Box::new(BinaryTree {
                    left: expressions[fold_index - 1].1.take().unwrap(),
                    right: right_expression.unwrap(),
                    operator,
                })));
        }
    }

    first
}

const fn into_binary_operator(
    syntax_tree: &syntax_tree::expression::binary::BinaryOperator,
) -> Result<
    (BinaryOperator, bool),
    &syntax_tree::expression::binary::BinaryOperator,
> {
    use syntax_tree::expression::binary::BinaryOperator as BinOpSyn;

    use crate::value::register::{
        ArithmeticOperator as ArithOp, BinaryOperator as BinOp,
        BitwiseOperator as BitOp, RelationalOperator as RelaOp,
    };

    match syntax_tree {
        BinOpSyn::Add(_) => Ok((BinOp::Arithmetic(ArithOp::Add), false)),
        BinOpSyn::Subtract(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Subtract), false))
        }
        BinOpSyn::Multiply(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Multiply), false))
        }
        BinOpSyn::Divide(_) => Ok((BinOp::Arithmetic(ArithOp::Divide), false)),
        BinOpSyn::Modulo(_) => Ok((BinOp::Arithmetic(ArithOp::Modulo), false)),

        BinOpSyn::Equal(_, _) => Ok((BinOp::Relational(RelaOp::Equal), false)),
        BinOpSyn::NotEqual(_, _) => {
            Ok((BinOp::Relational(RelaOp::NotEqual), false))
        }
        BinOpSyn::LessThan(_) => {
            Ok((BinOp::Relational(RelaOp::LessThan), false))
        }
        BinOpSyn::LessThanOrEqual(_, _) => {
            Ok((BinOp::Relational(RelaOp::LessThanOrEqual), false))
        }
        BinOpSyn::GreaterThan(_) => {
            Ok((BinOp::Relational(RelaOp::GreaterThan), false))
        }
        BinOpSyn::GreaterThanOrEqual(_, _) => {
            Ok((BinOp::Relational(RelaOp::GreaterThanOrEqual), false))
        }

        BinOpSyn::BitwiseAnd(_) => Ok((BinOp::Bitwise(BitOp::And), false)),
        BinOpSyn::BitwiseOr(_) => Ok((BinOp::Bitwise(BitOp::Or), false)),
        BinOpSyn::BitwiseXor(_) => Ok((BinOp::Bitwise(BitOp::Xor), false)),
        BinOpSyn::BitwiseLeftShift(_, _) => {
            Ok((BinOp::Bitwise(BitOp::LeftShift), false))
        }
        BinOpSyn::BitwiseRightShift(_, _) => {
            Ok((BinOp::Bitwise(BitOp::RightShift), false))
        }

        BinOpSyn::CompoundAdd(_, _) => {
            Ok((BinOp::Arithmetic(ArithOp::Add), true))
        }
        BinOpSyn::CompoundSubtract(_, _) => {
            Ok((BinOp::Arithmetic(ArithOp::Subtract), true))
        }
        BinOpSyn::CompoundMultiply(_, _) => {
            Ok((BinOp::Arithmetic(ArithOp::Multiply), true))
        }
        BinOpSyn::CompoundDivide(_, _) => {
            Ok((BinOp::Arithmetic(ArithOp::Divide), true))
        }
        BinOpSyn::CompoundModulo(_, _) => {
            Ok((BinOp::Arithmetic(ArithOp::Modulo), true))
        }

        BinOpSyn::CompoundBitwiseAnd(_, _) => {
            Ok((BinOp::Bitwise(BitOp::And), true))
        }
        BinOpSyn::CompoundBitwiseOr(_, _) => {
            Ok((BinOp::Bitwise(BitOp::Or), true))
        }
        BinOpSyn::CompoundBitwiseLeftShift(_, _, _) => {
            Ok((BinOp::Bitwise(BitOp::LeftShift), true))
        }
        BinOpSyn::CompoundBitwiseRightShift(_, _, _) => {
            Ok((BinOp::Bitwise(BitOp::RightShift), true))
        }
        BinOpSyn::CompoundBitwiseXor(_, _) => {
            Ok((BinOp::Bitwise(BitOp::Xor), true))
        }

        BinOpSyn::Assign(_)
        | BinOpSyn::LogicalAnd(_)
        | BinOpSyn::LogicalOr(_) => Err(syntax_tree),
    }
}

impl Binder<'_> {
    fn bind_assignment(
        &mut self,
        tree: &BinaryTree,
        config: Config,
        lhs_address: LValue,
        rhs_value: Value<infer::Model>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let lhs_ty = self.type_of_address(&lhs_address.address, handler)?;
        let rhs_ty = self.type_of_value(&rhs_value, handler)?;

        let _ = self.type_check(
            &rhs_ty,
            Expected::Known(lhs_ty),
            tree.right.span(),
            handler,
        )?;

        if lhs_address.qualifier != Qualifier::Mutable {
            handler.receive(Box::new(AssignToNonMutable { span: tree.span() }));
        }

        let _ = self.current_block_mut().add_instruction(Instruction::Store(
            Store {
                address: lhs_address.address.clone(),
                value: rhs_value,
                span: Some(tree.span()),
            },
        ));

        Ok(match config.target {
            Target::RValue => {
                let register_id = self.create_register_assignmnet(
                    Assignment::Load(Load { address: lhs_address.address }),
                    tree.span(),
                );

                Expression::RValue(Value::Register(register_id))
            }

            // qualifier is not checked here since the address is already bound
            // as Qualifier::Unique, which has the highest priority
            Target::Statement | Target::LValue { .. } => {
                Expression::LValue(LValue {
                    address: lhs_address.address,
                    span: tree.span(),
                    qualifier: lhs_address.qualifier,
                })
            }
        })
    }

    #[allow(clippy::too_many_lines)]
    fn bind_normal_binary(
        &mut self,
        syntax_tree: &BinaryTree,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let (op, is_compound) =
            into_binary_operator(syntax_tree.operator).unwrap();

        let (lhs_address, lhs_value) = 'out: {
            if is_compound {
                let lhs_lvalue = match self.bind_as_lvalue(
                    &syntax_tree.left,
                    false,
                    handler,
                ) {
                    Ok(address) => address,

                    Err(Error::Binding(BindingError(span))) => {
                        let inference =
                            self.create_type_inference(Constraint::All(false));

                        break 'out (
                            None,
                            Value::Literal(Literal::Error(literal::Error {
                                r#type: Type::Inference(inference),
                                span: Some(span),
                            })),
                        );
                    }

                    Err(Error::Unrecoverable(internal_error)) => {
                        return Err(Error::Unrecoverable(internal_error))
                    }
                };

                let lhs_register = self.create_register_assignmnet(
                    Assignment::Load(Load {
                        address: lhs_lvalue.address.clone(),
                    }),
                    syntax_tree.left.span(),
                );

                (Some(lhs_lvalue), Value::Register(lhs_register))
            } else {
                (None, self.bind_value_or_error(&syntax_tree.left, handler)?)
            }
        };

        let rhs_value =
            self.bind_value_or_error(&syntax_tree.right, handler)?;

        let lhs_register_ty = self.type_of_value(&lhs_value, handler)?;

        // left shift and right shift doesn't necessarily require the same type
        // for both operands
        if !(matches!(
            op,
            BinaryOperator::Bitwise(
                BitwiseOperator::LeftShift | BitwiseOperator::RightShift
            )
        ) || (matches!(
            op,
            BinaryOperator::Arithmetic(
                ArithmeticOperator::Add | ArithmeticOperator::Subtract
            )
        )) && matches!(lhs_register_ty, Type::Pointer(_)))
        {
            let rhs_register_ty = self.type_of_value(&rhs_value, handler)?;

            let _ = self.type_check(
                &rhs_register_ty,
                Expected::Known(lhs_register_ty.clone()),
                syntax_tree.right.span(),
                handler,
            )?;
        }

        match op {
            BinaryOperator::Arithmetic(arith) => {
                let rhs_register_ty =
                    self.type_of_value(&rhs_value, handler)?;

                if matches!(lhs_register_ty, Type::Pointer(_))
                    && matches!(
                        arith,
                        ArithmeticOperator::Add | ArithmeticOperator::Subtract
                    )
                {
                    let _ = self.type_check(
                        &rhs_register_ty,
                        Expected::Known(Type::Primitive(Primitive::Isize)),
                        syntax_tree.right.span(),
                        handler,
                    );
                } else {
                    let _ = self.type_check(
                        &lhs_register_ty,
                        Expected::Constraint(Constraint::Number),
                        syntax_tree.left.span(),
                        handler,
                    )?;
                }
            }
            BinaryOperator::Relational(_) => {
                let lhs_register_ty = self
                    .create_environment()
                    .simplify(lhs_register_ty)
                    .map_err(|x| {
                        x.report_overflow(|x| {
                            x.report_as_type_calculating_overflow(
                                syntax_tree.left.span(),
                                handler,
                            )
                        })
                    })?;

                let valid = match lhs_register_ty.result {
                    Type::Pointer(_) | Type::Primitive(_) => true,

                    Type::Inference(inference) => {
                        let constraint_id = *self
                            .inference_context
                            .get_inference(inference)
                            .unwrap()
                            .as_inferring()
                            .unwrap();

                        let constraint = *self
                            .inference_context
                            .get_constraint::<Type<_>>(constraint_id)
                            .unwrap();

                        match constraint {
                            Constraint::Number
                            | Constraint::Integer
                            | Constraint::SignedInteger
                            | Constraint::Signed
                            | Constraint::UnsignedInteger
                            | Constraint::Floating => true,

                            Constraint::All(_) => false,
                        }
                    }
                    _ => false,
                };

                if !valid {
                    handler.receive(Box::new(InvalidRelationalOperation {
                        found_type: self
                            .inference_context
                            .transform_type_into_constraint_model(
                                lhs_register_ty.result.clone(),
                                syntax_tree.left.span(),
                                self.table,
                                handler,
                            )?,
                        span: syntax_tree.span(),
                    }));
                }
            }
            BinaryOperator::Bitwise(bitwise) => match bitwise {
                BitwiseOperator::And
                | BitwiseOperator::Or
                | BitwiseOperator::Xor => {
                    let lhs_register_ty = self
                        .create_environment()
                        .simplify(lhs_register_ty)
                        .map_err(|x| {
                            x.report_overflow(|x| {
                                x.report_as_type_calculating_overflow(
                                    syntax_tree.left.span(),
                                    handler,
                                )
                            })
                        })?;

                    let valid = match &lhs_register_ty.result {
                        Type::Primitive(_) => true,
                        Type::Inference(inference) => {
                            let constraint_id = *self
                                .inference_context
                                .get_inference(*inference)
                                .unwrap()
                                .as_inferring()
                                .unwrap();

                            let constraint = *self
                                .inference_context
                                .get_constraint::<Type<_>>(constraint_id)
                                .unwrap();

                            match constraint {
                                Constraint::Number
                                | Constraint::Integer
                                | Constraint::SignedInteger
                                | Constraint::UnsignedInteger
                                | Constraint::Signed => true,

                                Constraint::Floating | Constraint::All(_) => {
                                    false
                                }
                            }
                        }
                        _ => false,
                    };

                    if !valid {
                        handler.receive(Box::new(InvalidRelationalOperation {
                            found_type: self
                                .inference_context
                                .transform_type_into_constraint_model(
                                    lhs_register_ty.result.clone(),
                                    syntax_tree.left.span(),
                                    self.table,
                                    handler,
                                )?,
                            span: syntax_tree.span(),
                        }));
                    }
                }

                BitwiseOperator::LeftShift | BitwiseOperator::RightShift => {
                    let _ = self.type_check(
                        &lhs_register_ty,
                        Expected::Constraint(Constraint::Integer),
                        syntax_tree.left.span(),
                        handler,
                    )?;

                    let rhs_value_ty =
                        self.type_of_value(&rhs_value, handler)?;

                    let _ = self.type_check(
                        &rhs_value_ty,
                        Expected::Constraint(Constraint::UnsignedInteger),
                        syntax_tree.right.span(),
                        handler,
                    )?;
                }
            },
        }

        let binary_register = self.create_register_assignmnet(
            Assignment::Binary(Binary {
                operator: op,
                lhs: lhs_value,
                rhs: rhs_value,
            }),
            syntax_tree.span(),
        );

        if let (Some(lhs_address), true) = (lhs_address, is_compound) {
            self.bind_assignment(
                syntax_tree,
                config,
                lhs_address,
                Value::Register(binary_register),
                handler,
            )
        } else {
            Ok(Expression::RValue(Value::Register(binary_register)))
        }
    }
}

impl Bind<&BinaryTree<'_>> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &BinaryTree<'_>,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        match syntax_tree.operator {
            syntax_tree::expression::binary::BinaryOperator::Assign(_) => {
                let rhs =
                    self.bind_value_or_error(&syntax_tree.right, handler)?;
                let lhs =
                    self.bind_as_lvalue(&syntax_tree.left, false, handler)?;

                self.bind_assignment(syntax_tree, config, lhs, rhs, handler)
            }

            syntax_tree::expression::binary::BinaryOperator::LogicalAnd(_)
            | syntax_tree::expression::binary::BinaryOperator::LogicalOr(_) => {
                let successor_block_id = self
                    .intermediate_representation
                    .control_flow_graph
                    .new_block();

                let lhs =
                    self.bind_value_or_error(&syntax_tree.left, handler)?;

                // must be a boolean
                let lhs_ty = self.type_of_value(&lhs, handler)?;
                let _ = self.type_check(
                    &lhs_ty,
                    Expected::Known(Type::Primitive(Primitive::Bool)),
                    syntax_tree.left.span(),
                    handler,
                )?;

                let true_block_id = self
                    .intermediate_representation
                    .control_flow_graph
                    .new_block();
                let false_block_id = self
                    .intermediate_representation
                    .control_flow_graph
                    .new_block();

                let _ = self
                    .intermediate_representation
                    .control_flow_graph
                    .insert_terminator(
                        self.current_block_id,
                        Terminator::Jump(Jump::Conditional(ConditionalJump {
                            condition: lhs,
                            true_target: true_block_id,
                            false_target: false_block_id,
                        })),
                    );

                let (true_branch, false_branch) = {
                    let result = self
                        .intermediate_representation
                        .scope_tree
                        .new_child_branch(
                            self.stack.current_scope().scope_id(),
                            NonZeroUsize::new(2).unwrap(),
                        )
                        .unwrap();

                    (result[0], result[1])
                };

                // true branch
                let (true_branch_value, true_branch_last_block_id) = {
                    self.current_block_id = true_block_id;

                    // push the scope
                    self.stack.push_scope(true_branch, false);
                    let _ = self.current_block_mut().add_instruction(
                        Instruction::ScopePush(ScopePush(true_branch)),
                    );

                    let value = if matches!(
                        syntax_tree.operator,
                        syntax_tree::expression::binary::BinaryOperator::LogicalOr(_)
                    ) {
                        Value::Literal(Literal::Boolean(Boolean {
                            value: true,
                            span: Some(syntax_tree.left.span()),
                        }))
                    } else {
                        let rhs = self
                            .bind_value_or_error(&syntax_tree.right, handler)?;

                        let rhs_ty = self.type_of_value(&rhs, handler)?;
                        let _ = self.type_check(
                            &rhs_ty,
                            Expected::Known(Type::Primitive(Primitive::Bool)),
                            syntax_tree.right.span(),
                            handler,
                        )?;

                        rhs
                    };

                    // pop the scope
                    assert_eq!(
                        self.stack.pop_scope().map(|x| x.scope_id()),
                        Some(true_branch)
                    );
                    let _ = self.current_block_mut().add_instruction(
                        Instruction::ScopePop(ScopePop(true_branch)),
                    );

                    // jump to the successor block
                    let _ = self
                        .intermediate_representation
                        .control_flow_graph
                        .insert_terminator(
                            self.current_block_id,
                            Terminator::Jump(Jump::Unconditional(
                                instruction::UnconditionalJump {
                                    target: successor_block_id,
                                },
                            )),
                        );

                    (value, self.current_block_id)
                };

                // false block
                let (false_branch_value, false_branch_last_block_id) = {
                    self.current_block_id = false_block_id;

                    // push the scope
                    self.stack.push_scope(false_branch, false);
                    let _ = self.current_block_mut().add_instruction(
                        Instruction::ScopePush(ScopePush(false_branch)),
                    );

                    let value = if matches!(
                        syntax_tree.operator,
                        syntax_tree::expression::binary::BinaryOperator::LogicalAnd(_)
                    ) {
                        Value::Literal(Literal::Boolean(Boolean {
                            value: false,
                            span: Some(syntax_tree.left.span()),
                        }))
                    } else {
                        let rhs = self
                            .bind_value_or_error(&syntax_tree.right, handler)?;

                        let rhs_ty = self.type_of_value(&rhs, handler)?;
                        let _ = self.type_check(
                            &rhs_ty,
                            Expected::Known(Type::Primitive(Primitive::Bool)),
                            syntax_tree.right.span(),
                            handler,
                        )?;

                        rhs
                    };

                    // pop the scope
                    assert_eq!(
                        self.stack.pop_scope().map(|x| x.scope_id()),
                        Some(false_branch)
                    );
                    let _ = self.current_block_mut().add_instruction(
                        Instruction::ScopePop(ScopePop(false_branch)),
                    );

                    // jump to the successor block
                    let _ = self
                        .intermediate_representation
                        .control_flow_graph
                        .insert_terminator(
                            self.current_block_id,
                            Terminator::Jump(Jump::Unconditional(
                                instruction::UnconditionalJump {
                                    target: successor_block_id,
                                },
                            )),
                        );

                    (value, self.current_block_id)
                };

                // set the current block to the successor block
                self.current_block_id = successor_block_id;

                // shouldn't have more than 2 predecessors
                assert!(self.current_block().predecessors().len() <= 2);

                let value = match self.current_block().predecessors().len() {
                    0 => {
                        // unreachable
                        Value::Literal(Literal::Unreachable(
                            literal::Unreachable {
                                r#type: Type::Primitive(Primitive::Bool),
                                span: Some(syntax_tree.span()),
                            },
                        ))
                    }

                    1 => {
                        // only one predecessor
                        if self
                            .current_block()
                            .predecessors()
                            .contains(&true_branch_last_block_id)
                        {
                            true_branch_value
                        } else {
                            assert!(self
                                .current_block()
                                .predecessors()
                                .contains(&false_branch_last_block_id));

                            false_branch_value
                        }
                    }

                    2 => {
                        // both predecessors

                        let register_id = self.create_register_assignmnet(
                            Assignment::Phi(Phi {
                                r#type: Type::Primitive(Primitive::Bool),
                                incoming_values: [
                                    (
                                        true_branch_last_block_id,
                                        true_branch_value,
                                    ),
                                    (
                                        false_branch_last_block_id,
                                        false_branch_value,
                                    ),
                                ]
                                .into_iter()
                                .collect(),
                            }),
                            syntax_tree.span(),
                        );

                        Value::Register(register_id)
                    }

                    _ => unreachable!(),
                };

                Ok(Expression::RValue(value))
            }

            _ => self.bind_normal_binary(syntax_tree, config, handler),
        }
    }
}

impl Bind<&BinaryNode<'_>> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &BinaryNode<'_>,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            BinaryNode::Binary(binary) => self.bind(&**binary, config, handler),
            BinaryNode::Expression(prefixable) => {
                self.bind(*prefixable, config, handler)
            }
        }
    }
}

#[cfg(test)]
mod test;
