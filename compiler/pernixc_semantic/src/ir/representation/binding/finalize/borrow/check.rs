use super::{
    cache::{RegionVariances, RegisterInfos},
    invalidate::Checker,
    subset::Subset,
};
use crate::{
    error::TypeSystemOverflow,
    ir::{
        self,
        address::Memory,
        control_flow_graph::{Point, Reachability},
        instruction::{AccessMode, Instruction, Read},
        representation::{
            binding::HandlerWrapper, borrow::Model as BorrowModel,
            Representation,
        },
        value::register::Assignment,
    },
    symbol::{table, CallableID, GlobalID},
    type_system::{
        environment::Environment, normalizer::Normalizer, observer::Observer,
        term::r#type::Qualifier,
    },
};

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Checker<'a, S, N, O>
{
    pub fn borrow_check(
        &self,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        for (point, inst) in self
            .representation()
            .control_flow_graph
            .traverse()
            .flat_map(|(block_id, block)| {
                block.instructions().iter().enumerate().map(move |(i, inst)| {
                    (Point { instruction_index: i, block_id }, inst)
                })
            })
        {
            match inst {
                Instruction::Store(store) => self.handle_access(
                    &store.address,
                    AccessMode::Write(store.span.clone()),
                    point,
                    handler,
                )?,
                Instruction::RegisterAssignment(register_assignment) => {
                    let register = self
                        .representation()
                        .values
                        .registers
                        .get(register_assignment.id)
                        .unwrap();

                    match &register.assignment {
                        Assignment::Load(load) => self.handle_access(
                            &load.address,
                            AccessMode::Read(Read {
                                qualifier: Qualifier::Immutable,
                                span: register.span.clone(),
                            }),
                            point,
                            handler,
                        )?,
                        Assignment::Borrow(borrow) => {
                            self.handle_access(
                                &borrow.address,
                                AccessMode::Read(Read {
                                    qualifier: borrow.qualifier,
                                    span: register.span.clone(),
                                }),
                                point,
                                handler,
                            )?;
                        }

                        Assignment::Tuple(_)
                        | Assignment::Prefix(_)
                        | Assignment::Struct(_)
                        | Assignment::Variant(_)
                        | Assignment::FunctionCall(_)
                        | Assignment::Binary(_)
                        | Assignment::Array(_)
                        | Assignment::Phi(_)
                        | Assignment::Cast(_)
                        | Assignment::VariantNumber(_) => {}
                    }
                }
                Instruction::RegisterDiscard(register_discard) => {}
                Instruction::TuplePack(tuple_pack) => {}
                Instruction::ScopePush(scope_push) => {}
                Instruction::ScopePop(scope_pop) => {
                    let mut dropped_memories = self
                        .representation()
                        .values
                        .allocas
                        .iter()
                        .filter_map(|(id, x)| {
                            (x.declared_in_scope_id == scope_pop.0)
                                .then_some(Memory::Alloca(id))
                        })
                        .collect::<Vec<_>>();

                    // add parameters to the dropped memories
                    if let Ok(callable_id) =
                        CallableID::try_from(self.current_site())
                    {
                        if scope_pop.0
                            == self.representation().scope_tree.root_scope_id()
                        {
                            let callable = self
                                .environment()
                                .table()
                                .get_callable(callable_id)
                                .unwrap();

                            dropped_memories.extend(
                                callable
                                    .parameters()
                                    .ids()
                                    .map(Memory::Parameter),
                            );
                        }
                    }

                    for memory in dropped_memories {
                        self.handle_drop_memory(memory, point, handler)?;
                    }
                }
                Instruction::DropUnpackTuple(drop_unpack_tuple) => {}
                Instruction::Drop(drop) => {}
            }
        }

        Ok(())
    }
}

impl Representation<BorrowModel> {
    /// The pass where the error checking is done.
    pub(super) fn borrow_check_internal<S: table::State>(
        &self,
        subset: &Subset,
        register_infos: &RegisterInfos,
        region_variances: &RegionVariances,
        reachability: &Reachability<BorrowModel>,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let checker = Checker::new(
            self,
            environment,
            reachability,
            register_infos,
            region_variances,
            current_site,
            subset,
        );

        checker.borrow_check(handler)
    }
}
