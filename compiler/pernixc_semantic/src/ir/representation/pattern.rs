use pernixc_base::source_file::SourceElement;
use pernixc_syntax::syntax_tree::{self, pattern::Binding};

use super::Representation;
use crate::{
    arena::{Reserve, ID},
    ir::{
        address::{Address, Stack},
        alloca::Alloca,
        cfg::Block,
        instruction::{self, RegisterAssignment},
        value::register::{AddressOfReference, Load, LoadKind, Register},
    },
    pattern::{Irrefutable, Named, Wildcard},
    semantic::{
        session::{self, Cached, ExceedLimitError, Limit, Session},
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Qualifier, Reference, Type},
        },
        Environment,
    },
    table::State,
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum CreatePatternError {
    #[error(transparent)]
    ExceedLimitError(#[from] ExceedLimitError),

    #[error("The given block ID is not in the control flow graph")]
    InvalidBlockID,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SideEffects<'a> {
    required_registers: Reserve<'a, Register>,
    required_allocas: Reserve<'a, Alloca>,
    instructions: Vec<instruction::Basic>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferenceBinding {
    pub lifetime: Lifetime,
    pub qualifier: Qualifier,
}

impl<T> Representation<T> {
    /// Creates irrefutable patterns
    pub fn create_irrefutable_pattern(
        &mut self,
        syntax_tree: &syntax_tree::pattern::Irrefutable,
        pattern_type: &Type,
        address: &Address,
        block_id: ID<Block>,
        environment: &Environment<impl State>,
        limit: &mut Limit<
            impl Session<Lifetime> + Session<Type> + Session<Constant>,
        >,
    ) -> Result<Option<Irrefutable>, CreatePatternError> {
        let Some(block) = self.control_flow_graph.get_block_mut(block_id)
        else {
            return Err(CreatePatternError::InvalidBlockID);
        };

        let mut storage = session::Storage::new();
        let mut pattern_limit = Limit::new(&mut storage);

        let pattern = Self::create_irrefutable_pattern_internal(
            SideEffects {
                required_registers: Reserve::new(&self.registers),
                required_allocas: Reserve::new(&self.allocas),
                instructions: Vec::new(),
            },
            syntax_tree,
            pattern_type,
            address,
            environment,
            None,
            limit,
            &mut pattern_limit,
        );

        todo!()
    }

    /// Creates irrefutable patterns
    fn create_irrefutable_pattern_internal<'a>(
        mut side_effects: SideEffects<'a>,
        syntax_tree: &syntax_tree::pattern::Irrefutable,
        mut pattern_type: &Type,
        address: &Address,
        environment: &Environment<impl State>,
        reference_binding: Option<ReferenceBinding>,
        limit: &mut Limit<
            impl Session<Lifetime> + Session<Type> + Session<Constant>,
        >,
        pattern_limit: &mut Limit<session::Storage<Type, (), ()>>,
    ) -> Result<Option<(Irrefutable, SideEffects<'a>)>, CreatePatternError>
    {
        match pattern_limit.mark_as_in_progress(pattern_type.clone(), ())? {
            Some(Cached::InProgress(())) => return Ok(None),
            Some(Cached::Done(())) => unreachable!(),
            None => {}
        }

        let result = match syntax_tree {
            syntax_tree::pattern::Irrefutable::Structural(_) => todo!(),

            syntax_tree::pattern::Irrefutable::Named(named) => {
                match named.binding() {
                    Binding::Ref(ref_binding) => todo!(),

                    Binding::Value { mutable_keyword } => match address {
                        // no additional instructions are needed, refer to the
                        // stack address
                        Address::Stack(stack_address) => {
                            assert!(reference_binding.is_none());

                            Some(Irrefutable::Named(Named {
                                name: named.span().str().to_owned(),
                                address: *stack_address,
                            }))
                        }

                        address => {
                            let alloca_id =
                                side_effects.required_allocas.reserve(Alloca {
                                    r#type: reference_binding.map_or_else(
                                        || pattern_type.clone(),
                                        |binding| {
                                            Type::Reference(Reference {
                                                qualifier: binding.qualifier,
                                                pointee: Box::new(
                                                    pattern_type.clone(),
                                                ),
                                                lifetime: todo!(),
                                            });
                                        },
                                    ),
                                    span: Some(named.span()),
                                    mutable: mutable_keyword.is_some(),
                                });

                            let register_id = if reference_binding.is_some() {
                                // simply store the address in the alloca
                                side_effects.required_registers.reserve(
                                    Register::AddressOfReference(
                                        AddressOfReference {
                                            address: address.clone(),
                                        },
                                    ),
                                )
                            } else {
                                // move the value to the alloca
                                side_effects.required_registers.reserve(
                                    Register::Load(Load {
                                        address: address.clone(),
                                        kind: LoadKind::Move,
                                    }),
                                )
                            };
                            side_effects.instructions.push(
                                instruction::Basic::RegisterAssignment(
                                    RegisterAssignment { id: register_id },
                                ),
                            );

                            Some(Irrefutable::Named(Named {
                                name: named.identifier().span.str().to_owned(),
                                address: Stack::Alloca(alloca_id),
                            }))
                        }
                    },
                }
            }

            syntax_tree::pattern::Irrefutable::Tuple(_) => todo!(),

            syntax_tree::pattern::Irrefutable::Wildcard(_) => {
                Some(Irrefutable::Wildcard(Wildcard))
            }
        };

        pattern_limit.clear_query(pattern_type.clone());
        Ok(None)
    }
}
