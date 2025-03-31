//! Contains the code for binding the expression syntax tree.

use std::collections::HashMap;

use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_lexical::token;
use pernixc_source_file::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{self, Label};
use pernixc_semantic::diagnostic::Diagnostic;
use pernixc_term::r#type::{Qualifier, Type};

use super::{
    diagnostic::{
        CannotDereference, ExpectedLValue, LoopControlFlow,
        LoopControlFlowOutsideLoop, LoopWithGivenLabelNameNotFound,
        NotAllFlowPathsExpressValue,
    },
    infer, Abort, Binder, BindingError, BlockState, Error,
};
use crate::{
    address::{Address, Memory, Reference},
    binding,
    binding::infer::InferenceVariable,
    control_flow_graph::Block,
    instruction::{
        Instruction, Jump, ScopePop, ScopePush, Store, Terminator,
        UnconditionalJump,
    },
    model::Constraint,
    value::{
        literal::{self, Literal, Unit, Unreachable},
        register::{Assignment, Load, Phi},
        Value,
    },
};

/// An enumeration describes the intended purpose of binding the expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Target {
    /// Binds the expression syntax tree for the r-value.
    ///
    /// All the expressions can be bound as a r-value.
    RValue,

    /// Binds the syntax tree for the underlying address (l-value).
    ///
    /// This is a *request* to bind the expression as an l-value not strictly
    /// required. If the expression cannot be bound as an l-value, the r-value
    /// is returned instead.
    LValue,

    /// The expression is being bound for a statement, therefore the produced
    /// value will be discarded right away.
    Statement,
}

/// The configuration object for binding the expression syntax tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Config {
    /// The intended purpose of binding the expression.
    pub target: Target,
}

/// The result of binding the expression as an l-value. (The value has an
/// address where it is stored.)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LValue {
    /// The address of the l-value.
    pub address: Address<infer::Model>,

    /// The span of the expression that produces this l-value.
    pub span: Span,

    /// The qualifier of the l-value.
    pub qualifier: Qualifier,
}

/// The result of binding the expression syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    /// The expression is bound as an r-value.
    RValue(Value<infer::Model>),

    /// The expression is bound as an l-value.
    LValue(LValue),
}

/// The trait for binding the expression syntax tree.
pub trait Bind<T> {
    /// Binds the given syntax tree to the [`Expression`].
    ///
    /// # Errors
    ///
    /// If an error occurs during the binding process, an [`Error`] is returned
    /// with the span of the syntax tree that caused the error.
    fn bind(
        &mut self,
        syntax_tree: T,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error>;
}

mod array;
mod binary;
mod boolean;
mod r#break;
mod character;
mod r#continue;
mod express;
mod if_else;
mod r#loop;
mod r#match;
mod numeric;
mod panic;
mod parenthesized;
mod phantom;
mod postfix;
mod prefix;
mod qualified_identifier;
mod r#return;
mod scope;
mod string;
mod r#struct;
mod r#while;

impl Binder<'_> {
    /// Binds the given syntax tree as an address.
    ///
    /// If the expression cannot be bound as an address, a variable will be
    /// created an the value is stored in the variable; the address of the
    /// variable is returned.
    fn bind_as_lvalue<'a, T>(
        &mut self,
        syntax_tree: &'a T,
        create_temporary: bool,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<LValue, Error>
    where
        T: SourceElement,
        Self: Bind<&'a T>,
    {
        match self.bind(
            syntax_tree,
            Config { target: Target::LValue },
            handler,
        )? {
            Expression::RValue(value) => {
                if create_temporary {
                    let type_of_value = self.type_of_value(&value, handler)?;

                    let alloca_id =
                        self.create_alloca(type_of_value, syntax_tree.span());

                    // initialize
                    let _ = self.current_block_mut().add_instruction(
                        Instruction::Store(Store {
                            address: Address::Memory(Memory::Alloca(alloca_id)),
                            span: Some(syntax_tree.span()),
                            value,
                        }),
                    );

                    Ok(LValue {
                        address: Address::Memory(Memory::Alloca(alloca_id)),
                        span: syntax_tree.span(),
                        qualifier: Qualifier::Mutable,
                    })
                } else {
                    handler.receive(Box::new(ExpectedLValue {
                        expression_span: syntax_tree.span(),
                    }));

                    Err(Error::Binding(BindingError(syntax_tree.span())))
                }
            }
            Expression::LValue(lvalue) => Ok(lvalue),
        }
    }
}

impl Bind<&syntax_tree::expression::prefix::Prefixable> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::prefix::Prefixable,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::prefix::Prefixable::Postfixable(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::prefix::Prefixable::Prefix(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl Binder<'_> {
    fn bind_dereference<'a, T>(
        &mut self,
        dereference: &'a T,
        config: Config,
        final_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error>
    where
        Self: Bind<&'a T>,
        T: SourceElement,
    {
        let operand = self.bind_as_lvalue(dereference, true, handler)?;

        // expected a reference type
        let operand_type = self.type_of_address(&operand.address, handler)?;

        let reference_type = match operand_type {
            Type::Reference(reference) => reference,
            found_type => {
                handler.receive(Box::new(CannotDereference {
                    found_type: self
                        .inference_context
                        .transform_type_into_constraint_model(
                            found_type,
                            dereference.span(),
                            self.table,
                            handler,
                        )?,
                    span: dereference.span(),
                }));
                return Err(Error::Binding(BindingError(dereference.span())));
            }
        };

        match config.target {
            Target::RValue | Target::Statement => {
                let register_id = self.create_register_assignmnet(
                    Assignment::Load(Load {
                        address: Address::Reference(Reference {
                            qualifier: reference_type.qualifier,
                            reference_address: Box::new(operand.address),
                        }),
                    }),
                    final_span,
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }
            Target::LValue => {
                let new_qualifier = reference_type.qualifier.min(
                    if operand.address.is_behind_reference() {
                        operand.qualifier
                    } else {
                        Qualifier::Mutable
                    },
                );

                Ok(Expression::LValue(LValue {
                    address: Address::Reference(Reference {
                        qualifier: reference_type.qualifier,
                        reference_address: Box::new(operand.address),
                    }),
                    span: final_span,
                    qualifier: new_qualifier,
                }))
            }
        }
    }
}

impl Bind<&syntax_tree::expression::postfix::Postfixable> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::postfix::Postfixable,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::postfix::Postfixable::Unit(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::postfix::Postfixable::Postfix(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl Bind<&syntax_tree::expression::unit::Unit> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::unit::Unit,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::unit::Unit::Boolean(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::unit::Unit::Numeric(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::unit::Unit::QualifiedIdentifier(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::unit::Unit::Parenthesized(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::unit::Unit::Struct(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::unit::Unit::Array(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::unit::Unit::Phantom(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::unit::Unit::String(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::unit::Unit::Character(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::unit::Unit::Panic(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl Binder<'_> {
    /// Finds a [`ID<scope::Scope>`] to operate a control flow on based on the
    /// location and label.
    fn find_loop_scope_id(
        &self,
        control_flow: LoopControlFlow,
        label: Option<&token::Identifier>,
        syntax_tree_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<ID<binding::scope::Scope>, Error> {
        let mut loop_scope_id = None;

        // find the loop state
        for scope in self.stack.scopes().iter().rev() {
            let Some(get_loop_state) =
                self.loop_states_by_scope_id.get(&scope.scope_id())
            else {
                continue;
            };

            if let Some(label) = label.as_ref() {
                if get_loop_state.label.as_deref() != Some(label.span.str()) {
                    continue;
                }
            }

            loop_scope_id = Some(scope.scope_id());
            break;
        }

        // loop state not found report the error
        let Some(loop_scope_id) = loop_scope_id else {
            if let Some(label) = label {
                handler.receive(Box::new(LoopWithGivenLabelNameNotFound {
                    span: label.span.clone(),
                }));
            } else {
                handler.receive(Box::new(LoopControlFlowOutsideLoop {
                    span: syntax_tree_span.clone(),
                    control_flow,
                }));
            };

            return Err(Error::Binding(BindingError(syntax_tree_span)));
        };

        Ok(loop_scope_id)
    }
}

impl Bind<&syntax_tree::expression::terminator::Terminator> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::terminator::Terminator,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::terminator::Terminator::Return(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::terminator::Terminator::Continue(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::terminator::Terminator::Express(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::terminator::Terminator::Break(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl Bind<&syntax_tree::expression::block::Block> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::block::Block,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::block::Block::Scope(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::block::Block::IfElse(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::block::Block::Loop(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::block::Block::Match(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::block::Block::While(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl Bind<&syntax_tree::expression::Expression> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Expression,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::Expression::Binary(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::Expression::Terminator(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl Bind<&syntax_tree::expression::binary::BinaryNode> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::binary::BinaryNode,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        match syntax_tree {
            syntax_tree::expression::binary::BinaryNode::Prefixable(syn) => {
                self.bind(syn, config, handler)
            }
            syntax_tree::expression::binary::BinaryNode::Brace(syn) => {
                self.bind(syn, config, handler)
            }
        }
    }
}

impl Bind<BlockState> for Binder<'_> {
    fn bind(
        &mut self,
        mut block_state: BlockState,
        _: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let value = if let Some(express_type) = block_state.express_type {
            // the block id that doesn't have incoming values
            let missing_value_block_ids = self
                .current_block()
                .predecessors()
                .iter()
                .copied()
                .filter(|block_id| {
                    !block_state.incoming_values.contains_key(block_id)
                })
                .collect::<Vec<_>>();

            // filter out the incoming values that are unreachable
            #[allow(clippy::needless_collect)]
            for remove in block_state
                .incoming_values
                .keys()
                .copied()
                .filter(|x| !self.current_block().predecessors().contains(x))
                .collect::<Vec<_>>()
            {
                block_state.incoming_values.remove(&remove);
            }

            // add the missing values
            for block_id in missing_value_block_ids.iter().copied() {
                assert!(block_state
                    .incoming_values
                    .insert(
                        block_id,
                        Value::Literal(Literal::Error(literal::Error {
                            r#type: express_type.clone(),
                            span: Some(block_state.span.clone()),
                        })),
                    )
                    .is_none());
            }

            if !missing_value_block_ids.is_empty() {
                handler.receive(Box::new(NotAllFlowPathsExpressValue {
                    span: block_state.span.clone(),
                }));
            }

            match block_state.incoming_values.len() {
                // no incoming values, unreachable
                0 => Value::Literal(Literal::Unreachable(Unreachable {
                    r#type: express_type,
                    span: Some(block_state.span.clone()),
                })),

                // only one incoming value, just return it
                1 => block_state.incoming_values.into_iter().next().unwrap().1,

                // multiple incoming values, create a phi node
                _ => {
                    let phi_register_id = self.create_register_assignmnet(
                        Assignment::Phi(Phi {
                            r#type: express_type,
                            incoming_values: block_state.incoming_values,
                        }),
                        block_state.span.clone(),
                    );

                    Value::Register(phi_register_id)
                }
            }
        } else {
            // should have had no incoming values
            assert!(block_state.incoming_values.is_empty());

            if self.current_block().is_unreachable_or_terminated() {
                let inference_variable = InferenceVariable::new();

                assert!(self
                    .inference_context
                    .register(inference_variable, Constraint::All(true)));

                Value::Literal(Literal::Unreachable(Unreachable {
                    r#type: Type::Inference(inference_variable),
                    span: Some(block_state.span.clone()),
                }))
            } else {
                Value::Literal(Literal::Unit(Unit {
                    span: Some(block_state.span.clone()),
                }))
            }
        };

        Ok(Expression::RValue(value))
    }
}

impl Binder<'_> {
    /// Binds the basic block of the given syntax tree.
    ///
    /// When binding a block, a new scope push instruction will be inserted
    /// right away in the `current_block`. Then, the list of instructions will
    /// be bound. Finally, a scope pop instruction will be followed as well
    /// as a jump instruction to the successor block.
    ///
    /// The function returns the [`BlockState`] that can be bound as a value by
    /// calling the bind block state function.
    #[allow(clippy::too_many_arguments)]
    fn bind_block<'a>(
        &mut self,
        label: Option<&Label>,
        statements: impl IntoIterator<Item = &'a syntax_tree::statement::Statement>,
        span: Span,
        scope_id: ID<binding::scope::Scope>,
        successor_block_id: ID<Block<infer::Model>>,
        is_unsafe: bool,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<BlockState, Abort> {
        // add the scope push instruction
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePush(ScopePush(scope_id)));

        self.block_states_by_scope_id.insert(scope_id, BlockState {
            label: label.map(|x| x.identifier.span.str().to_owned()),
            incoming_values: HashMap::new(),
            successor_block_id,
            express_type: None,
            span,
        });

        // push a new scope
        self.stack.push_scope(scope_id, is_unsafe);

        // bind list of statements
        for statement in statements {
            self.bind_statement(statement, handler)?;
        }

        // ends the scope
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(scope_id)
        );
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePop(ScopePop(scope_id)));

        self.intermediate_representation.control_flow_graph.insert_terminator(
            self.current_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: successor_block_id,
            })),
        );

        // set the current block to the successor block
        self.current_block_id = successor_block_id;

        // return the block state
        Ok(self.block_states_by_scope_id.remove(&scope_id).unwrap())
    }
}

impl Binder<'_> {
    /// Binds the given syntax tree as a value. In case of an error, an error
    /// register is returned.
    ///
    /// # Errors
    ///
    /// Returns [`Abort`] that is returned by the [`Bind::bind()`]
    /// function.
    pub fn bind_value_or_error<T>(
        &mut self,
        syntax_tree: T,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Value<infer::Model>, Abort>
    where
        Self: Bind<T>,
    {
        match self.bind(syntax_tree, Config { target: Target::RValue }, handler)
        {
            Ok(value) => Ok(value.into_r_value().unwrap()),
            Err(Error::Binding(semantic_error)) => {
                let inference =
                    self.create_type_inference(Constraint::All(false));

                Ok(Value::Literal(Literal::Error(literal::Error {
                    r#type: Type::Inference(inference),
                    span: Some(semantic_error.0),
                })))
            }
            Err(Error::Unrecoverable(internal_error)) => Err(internal_error),
        }
    }
}
