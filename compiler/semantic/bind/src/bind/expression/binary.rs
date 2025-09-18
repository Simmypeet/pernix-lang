use pernixc_handler::Handler;
use pernixc_ir::{
    instruction::{Instruction, Store},
    value::{
        literal::{self, Literal},
        register::{
            ArithmeticOperator, Assignment, Binary, BinaryOperator,
            BitwiseOperator, Load,
        },
        Value,
    },
};
use pernixc_lexical::tree::RelativeLocation;
use pernixc_source_file::SourceElement;
use pernixc_syntax::expression::binary::Operator as BinaryOperatorSyn;
use pernixc_term::r#type::{Primitive, Qualifier, Type};

use crate::{
    bind::{Bind, Expression, Guidance, LValue},
    binder::{type_check::Expected, Binder, BindingError, Error},
    diagnostic::{
        AssignToNonMutable, BinaryOperatorKind, Diagnostic,
        InvalidTypeInBinaryOperator,
    },
    inference_context::constraint,
};

impl Bind<&pernixc_syntax::expression::binary::Binary> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::binary::Binary,
        guidance: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        // transform the syntax tree into a binary tree
        let Some(binary_node) = to_binary_tree(syntax_tree) else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        self.bind(&binary_node, guidance, handler).await
    }
}

enum BinaryNode {
    Binary(Box<BinaryTree>),
    Expression(pernixc_syntax::expression::binary::Node),
}

impl SourceElement for BinaryNode {
    type Location = RelativeLocation;

    fn span(&self) -> pernixc_source_file::Span<Self::Location> {
        match self {
            Self::Binary(tree) => tree.span(),
            Self::Expression(exp) => exp.span(),
        }
    }
}

struct BinaryTree {
    left: BinaryNode,
    right: BinaryNode,
    operator: BinaryOperatorSyn,
}

impl SourceElement for BinaryTree {
    type Location = RelativeLocation;

    fn span(&self) -> pernixc_source_file::Span<Self::Location> {
        self.left.span().join(&self.right.span())
    }
}

const fn operator_precedence(operator: &BinaryOperatorSyn) -> u32 {
    match operator {
        BinaryOperatorSyn::Multiply(_)
        | BinaryOperatorSyn::Divide(_)
        | BinaryOperatorSyn::Modulus(_) => 10,

        BinaryOperatorSyn::Add(_) | BinaryOperatorSyn::Subtract(_) => 9,

        BinaryOperatorSyn::BitwiseLeftShift(..)
        | BinaryOperatorSyn::BitwiseRightShift(..) => 8,

        BinaryOperatorSyn::LessThan(_)
        | BinaryOperatorSyn::LessThanOrEqual(..)
        | BinaryOperatorSyn::GreaterThan(_)
        | BinaryOperatorSyn::GreaterThanOrEqual(..) => 7,

        BinaryOperatorSyn::Equal(..) | BinaryOperatorSyn::NotEqual(..) => 6,

        BinaryOperatorSyn::BitwiseAnd(_) => 5,

        BinaryOperatorSyn::BitwiseXor(_) => 4,

        BinaryOperatorSyn::BitwiseOr(_) => 3,

        BinaryOperatorSyn::LogicalAnd(_) => 2,

        BinaryOperatorSyn::LogicalOr(_) => 1,

        BinaryOperatorSyn::Assign(_)
        | BinaryOperatorSyn::CompoundAdd(..)
        | BinaryOperatorSyn::CompoundSubtract(..)
        | BinaryOperatorSyn::CompoundMultiply(..)
        | BinaryOperatorSyn::CompoundDivide(..)
        | BinaryOperatorSyn::CompoundModulus(..)
        | BinaryOperatorSyn::CompoundBitwiseAnd(..)
        | BinaryOperatorSyn::CompoundBitwiseOr(..)
        | BinaryOperatorSyn::CompoundBitwiseXor(..)
        | BinaryOperatorSyn::CompoundBitwiseLeftShift(..)
        | BinaryOperatorSyn::CompoundBitwiseRightShift(..) => 0,
    }
}

fn to_binary_tree(
    syntax_tree: &pernixc_syntax::expression::binary::Binary,
) -> Option<BinaryNode> {
    let mut first = BinaryNode::Expression(syntax_tree.first()?);
    let mut expressions = syntax_tree
        .chain()
        .filter_map(|sub| {
            Some((sub.operator()?, Some(BinaryNode::Expression(sub.node()?))))
        })
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

    Some(first)
}

const fn into_binary_operator(
    syntax_tree: &BinaryOperatorSyn,
) -> Result<(BinaryOperator, bool), &BinaryOperatorSyn> {
    use pernixc_ir::value::register::{
        ArithmeticOperator as ArithOp, BinaryOperator as BinOp,
        BitwiseOperator as BitOp, RelationalOperator as RelaOp,
    };

    match syntax_tree {
        BinaryOperatorSyn::Add(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Add), false))
        }
        BinaryOperatorSyn::Subtract(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Subtract), false))
        }
        BinaryOperatorSyn::Multiply(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Multiply), false))
        }
        BinaryOperatorSyn::Divide(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Divide), false))
        }
        BinaryOperatorSyn::Modulus(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Modulo), false))
        }

        BinaryOperatorSyn::Equal(_) => {
            Ok((BinOp::Relational(RelaOp::Equal), false))
        }
        BinaryOperatorSyn::NotEqual(_) => {
            Ok((BinOp::Relational(RelaOp::NotEqual), false))
        }
        BinaryOperatorSyn::LessThan(_) => {
            Ok((BinOp::Relational(RelaOp::LessThan), false))
        }
        BinaryOperatorSyn::LessThanOrEqual(_) => {
            Ok((BinOp::Relational(RelaOp::LessThanOrEqual), false))
        }
        BinaryOperatorSyn::GreaterThan(_) => {
            Ok((BinOp::Relational(RelaOp::GreaterThan), false))
        }
        BinaryOperatorSyn::GreaterThanOrEqual(_) => {
            Ok((BinOp::Relational(RelaOp::GreaterThanOrEqual), false))
        }

        BinaryOperatorSyn::BitwiseAnd(_) => {
            Ok((BinOp::Bitwise(BitOp::And), false))
        }
        BinaryOperatorSyn::BitwiseOr(_) => {
            Ok((BinOp::Bitwise(BitOp::Or), false))
        }
        BinaryOperatorSyn::BitwiseXor(_) => {
            Ok((BinOp::Bitwise(BitOp::Xor), false))
        }
        BinaryOperatorSyn::BitwiseLeftShift(_) => {
            Ok((BinOp::Bitwise(BitOp::LeftShift), false))
        }
        BinaryOperatorSyn::BitwiseRightShift(_) => {
            Ok((BinOp::Bitwise(BitOp::RightShift), false))
        }

        BinaryOperatorSyn::CompoundAdd(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Add), true))
        }
        BinaryOperatorSyn::CompoundSubtract(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Subtract), true))
        }
        BinaryOperatorSyn::CompoundMultiply(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Multiply), true))
        }
        BinaryOperatorSyn::CompoundDivide(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Divide), true))
        }
        BinaryOperatorSyn::CompoundModulus(_) => {
            Ok((BinOp::Arithmetic(ArithOp::Modulo), true))
        }

        BinaryOperatorSyn::CompoundBitwiseAnd(_) => {
            Ok((BinOp::Bitwise(BitOp::And), true))
        }
        BinaryOperatorSyn::CompoundBitwiseOr(_) => {
            Ok((BinOp::Bitwise(BitOp::Or), true))
        }
        BinaryOperatorSyn::CompoundBitwiseLeftShift(_) => {
            Ok((BinOp::Bitwise(BitOp::LeftShift), true))
        }
        BinaryOperatorSyn::CompoundBitwiseRightShift(_) => {
            Ok((BinOp::Bitwise(BitOp::RightShift), true))
        }
        BinaryOperatorSyn::CompoundBitwiseXor(_) => {
            Ok((BinOp::Bitwise(BitOp::Xor), true))
        }

        BinaryOperatorSyn::Assign(_)
        | BinaryOperatorSyn::LogicalAnd(_)
        | BinaryOperatorSyn::LogicalOr(_) => Err(syntax_tree),
    }
}

impl Binder<'_> {
    fn bind_assignment(
        &mut self,
        tree: &BinaryTree,
        lhs_address: LValue,
        rhs_value: Value,
        handler: &dyn Handler<Diagnostic>,
    ) -> Expression {
        if lhs_address.qualifier != Qualifier::Mutable {
            handler.receive(AssignToNonMutable { span: tree.span() }.into());
        }

        self.push_instruction(Instruction::Store(Store {
            address: lhs_address.address.clone(),
            value: rhs_value,
            span: Some(tree.span()),
        }));

        Expression::LValue(LValue {
            address: lhs_address.address,
            span: tree.span(),
            qualifier: lhs_address.qualifier,
        })
    }

    #[allow(clippy::too_many_lines)]
    async fn bind_normal_binary(
        &mut self,
        syntax_tree: &BinaryTree,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let (op, is_compound) =
            into_binary_operator(&syntax_tree.operator).unwrap();

        let (lhs_address, lhs_value) = 'out: {
            if is_compound {
                let lhs_lvalue = match Box::pin(self.bind_as_lvalue(
                    &syntax_tree.left,
                    false,
                    None,
                    handler,
                ))
                .await
                {
                    Ok(address) => address,

                    Err(Error::Binding(BindingError(span))) => {
                        let inference = self.create_type_inference(
                            constraint::Type::All(false),
                        );

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

                let lhs_register = self.create_register_assignment(
                    Assignment::Load(Load {
                        address: lhs_lvalue.address.clone(),
                    }),
                    syntax_tree.left.span(),
                );

                (Some(lhs_lvalue), Value::Register(lhs_register))
            } else {
                (
                    None,
                    Box::pin(self.bind_value_or_error(
                        &syntax_tree.left,
                        None,
                        handler,
                    ))
                    .await?,
                )
            }
        };

        let lhs_register_ty = self.type_of_value(&lhs_value, handler).await?;

        let rhs_value = match op {
            BinaryOperator::Arithmetic(arith) => {
                if matches!(lhs_register_ty, Type::Pointer(_))
                    && matches!(
                        arith,
                        ArithmeticOperator::Add | ArithmeticOperator::Subtract
                    )
                {
                    Box::pin(self.bind_value_or_error(
                        syntax_tree,
                        Some(&Type::Primitive(Primitive::Isize)),
                        handler,
                    ))
                    .await?
                } else {
                    let rhs = Box::pin(self.bind_value_or_error(
                        &syntax_tree.right,
                        Some(&lhs_register_ty),
                        handler,
                    ))
                    .await?;

                    if self
                        .type_check_as_diagnostic(
                            &lhs_register_ty,
                            Expected::Constraint(constraint::Type::Number),
                            syntax_tree.left.span(),
                            handler,
                        )
                        .await?
                        .is_some()
                    {
                        handler.receive(
                            InvalidTypeInBinaryOperator {
                                lhs_span: syntax_tree.left.span(),
                                operator_kind: BinaryOperatorKind::Arithmetic,
                                lhs_type: lhs_register_ty.clone(),
                                type_inference_map: self
                                    .type_inference_rendering_map(),
                                constant_inference_map: self
                                    .constant_inference_rendering_map(),
                            }
                            .into(),
                        );
                    }

                    rhs
                }
            }
            BinaryOperator::Relational(_) => {
                let rhs = Box::pin(self.bind_value_or_error(
                    &syntax_tree.right,
                    Some(&lhs_register_ty),
                    handler,
                ))
                .await?;

                if self
                    .type_check_as_diagnostic(
                        &lhs_register_ty,
                        Expected::Constraint(constraint::Type::Number),
                        syntax_tree.left.span(),
                        handler,
                    )
                    .await?
                    .is_some()
                {
                    handler.receive(
                        InvalidTypeInBinaryOperator {
                            lhs_span: syntax_tree.left.span(),
                            operator_kind: BinaryOperatorKind::Bitwise,
                            lhs_type: lhs_register_ty.clone(),
                            type_inference_map: self
                                .type_inference_rendering_map(),
                            constant_inference_map: self
                                .constant_inference_rendering_map(),
                        }
                        .into(),
                    );
                }

                rhs
            }
            BinaryOperator::Bitwise(bitwise) => match bitwise {
                BitwiseOperator::And
                | BitwiseOperator::Or
                | BitwiseOperator::Xor => {
                    let rhs = Box::pin(self.bind_value_or_error(
                        &syntax_tree.right,
                        Some(&lhs_register_ty),
                        handler,
                    ))
                    .await?;

                    let valid = match self
                        .simplify_type(
                            lhs_register_ty.clone(),
                            syntax_tree.left.span(),
                            handler,
                        )
                        .await?
                        .result
                    {
                        Type::Pointer(_) | Type::Primitive(_) => true,

                        Type::Inference(inference) => {
                            let constraint_id = *self
                                .inference_context()
                                .type_table()
                                .get_inference(inference)
                                .unwrap()
                                .as_inferring()
                                .unwrap();

                            let constraint = *self
                                .inference_context()
                                .type_table()
                                .get_constraint(constraint_id)
                                .unwrap();

                            match constraint {
                                constraint::Type::Number
                                | constraint::Type::Integer
                                | constraint::Type::SignedInteger
                                | constraint::Type::Signed
                                | constraint::Type::UnsignedInteger
                                | constraint::Type::Floating => true,

                                constraint::Type::All(_) => false,
                            }
                        }
                        _ => false,
                    };

                    if !valid {
                        handler.receive(
                            InvalidTypeInBinaryOperator {
                                lhs_span: syntax_tree.left.span(),
                                operator_kind: BinaryOperatorKind::Relational,
                                lhs_type: lhs_register_ty.clone(),
                                type_inference_map: self
                                    .type_inference_rendering_map(),
                                constant_inference_map: self
                                    .constant_inference_rendering_map(),
                            }
                            .into(),
                        );
                    }

                    rhs
                }

                BitwiseOperator::LeftShift | BitwiseOperator::RightShift => {
                    self.type_check(
                        &lhs_register_ty,
                        Expected::Constraint(constraint::Type::Integer),
                        syntax_tree.left.span(),
                        handler,
                    )
                    .await?;

                    let expected_type =
                        Type::Inference(self.create_type_inference(
                            constraint::Type::UnsignedInteger,
                        ));

                    Box::pin(self.bind_value_or_error(
                        &syntax_tree.right,
                        Some(&expected_type),
                        handler,
                    ))
                    .await?
                }
            },
        };

        let binary_register = self.create_register_assignment(
            Assignment::Binary(Binary {
                operator: op,
                lhs: lhs_value,
                rhs: rhs_value,
            }),
            syntax_tree.span(),
        );

        if let (Some(lhs_address), true) = (lhs_address, is_compound) {
            Ok(self.bind_assignment(
                syntax_tree,
                lhs_address,
                Value::Register(binary_register),
                handler,
            ))
        } else {
            Ok(Expression::RValue(Value::Register(binary_register)))
        }
    }
}

impl Bind<&BinaryTree> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    async fn bind(
        &mut self,
        syntax_tree: &BinaryTree,
        _config: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree.operator {
            BinaryOperatorSyn::Assign(_) => {
                let lhs = Box::pin(self.bind_as_lvalue(
                    &syntax_tree.left,
                    false,
                    None,
                    handler,
                ))
                .await?;
                let rhs = Box::pin(self.bind_value_or_error(
                    &syntax_tree.right,
                    Some(&self.type_of_address(&lhs.address, handler).await?),
                    handler,
                ))
                .await?;

                Ok(self.bind_assignment(syntax_tree, lhs, rhs, handler))
            }

            BinaryOperatorSyn::LogicalAnd(_)
            | BinaryOperatorSyn::LogicalOr(_) => {
                todo!()
                /*
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
                */
            }

            _ => self.bind_normal_binary(syntax_tree, handler).await,
        }
    }
}

impl Bind<&BinaryNode> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &BinaryNode,
        config: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            BinaryNode::Binary(binary) => {
                self.bind(&**binary, config, handler).await
            }
            BinaryNode::Expression(prefixable) => {
                self.bind(prefixable, config, handler).await
            }
        }
    }
}
