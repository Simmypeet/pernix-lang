use super::{cache::RegisterTypes, invalidate::Checker, subset::Subset};
use crate::{
    error::TypeSystemOverflow,
    ir::{
        self,
        control_flow_graph::{Point, Reachability},
        instruction::{AccessMode, Instruction, Read},
        representation::{
            binding::HandlerWrapper, borrow::Model as BorrowModel,
            Representation,
        },
        value::register::Assignment,
    },
    symbol::{table, GlobalID},
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
                Instruction::ScopePop(scope_pop) => {}
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
        register_types: &RegisterTypes,
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
            register_types,
            current_site,
            subset,
        );

        checker.borrow_check(handler)
    }
}
