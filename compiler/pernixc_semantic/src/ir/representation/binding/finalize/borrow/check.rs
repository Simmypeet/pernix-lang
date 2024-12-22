use std::borrow::Borrow;

use super::{
    cache::RegisterTypes, invalidate::Checker, liveness, subset::Subset,
};
use crate::{
    arena::ID,
    error::{TypeSystemOverflow, Usage},
    ir::{
        self,
        address::{Address, Memory},
        control_flow_graph::{Point, Reachability},
        instruction::{AccessMode, Instruction},
        representation::{
            binding::HandlerWrapper,
            borrow::{LocalRegion, Model as BorrowModel},
            Representation,
        },
        value::register::Register,
    },
    symbol::{table, GlobalID},
    type_system::{
        environment::Environment,
        normalizer::Normalizer,
        observer::Observer,
        term::{
            r#type::{Qualifier, Type},
            Term,
        },
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
                Instruction::RegisterAssignment(register_assignment) => {}
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
