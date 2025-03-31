use pernixc_abort::Abort;
use pernixc_handler::Handler;
use pernixc_ir::{
    address::{Address, Memory, Offset, Tuple},
    control_flow_graph::{Point, Reachability},
    instruction::{AccessMode, Instruction, Read},
    value::register::{Assignment, Load},
    Representation,
};
use pernixc_semantic::{
    component::{
        derived::function_signature::FunctionSignature, input::SymbolKind,
    },
    diagnostic::Diagnostic,
    table::GlobalID,
    term::{
        generic_arguments::GenericArguments,
        lifetime::Lifetime,
        predicate::{Outlives, PositiveMarker, Predicate},
        r#type::Qualifier,
    },
};
use pernixc_source_file::Span;
use pernixc_type_system::{
    diagnostic::{UndecidablePredicate, UnsatisfiedPredicate},
    environment::Environment,
    normalizer::Normalizer,
    well_formedness,
};

use crate::{
    cache::{RegionVariances, RegisterInfos},
    invalidate::Checker,
    subset::{RegionAt, Subset},
    Model as BorrowModel,
};

impl<N: Normalizer<BorrowModel>> Checker<'_, N> {
    fn handle_load(
        &self,
        load: &Load<BorrowModel>,
        register_span: &Span,
        point: Point<BorrowModel>,
    ) -> Result<(), Abort> {
        let ty = self
            .representation()
            .values
            .type_of_address(
                &load.address,
                self.current_site(),
                self.environment(),
            )
            .map_err(|x| {
                x.report_overflow(|x| {
                    x.report_as_type_calculating_overflow(
                        register_span.clone(),
                        self.handler(),
                    )
                })
            })?;

        self.handle_access(
            &load.address,
            &AccessMode::Read(Read {
                qualifier: Qualifier::Immutable,
                span: Some(register_span.clone()),
            }),
            point,
        )?;

        // has been checked previously
        'out: {
            if load.address.get_reference_qualifier()
                == Some(Qualifier::Immutable)
                || load.address.is_behind_index()
            {
                // TODO: check copy marker
            } else {
                let copy_marker = self
                    .environment()
                    .table()
                    .get_by_qualified_name(["core", "Copy"])
                    .unwrap();

                // no need to move
                if well_formedness::predicate_satisfied(
                    Predicate::PositiveMarker(PositiveMarker::new(
                        copy_marker,
                        GenericArguments {
                            lifetimes: Vec::new(),
                            types: vec![ty.result],
                            constants: Vec::new(),
                        },
                    )),
                    None,
                    false,
                    self.environment(),
                )?
                .1
                .is_empty()
                {
                    break 'out;
                }

                self.handle_moved_memory(&load.address, register_span, point)?;
            }
        };
        Ok(())
    }

    pub fn universal_region_check(&self) -> Result<(), Abort> {
        for (from, to_universal, span) in
            self.subset().direct_subset_relations().iter().filter_map(
                |(from, to, span)| {
                    let (from, RegionAt::Universal(to_universal), Some(span)) =
                        (from, to, span)
                    else {
                        return None;
                    };

                    Some((from, to_universal, span))
                },
            )
        {
            let universal_regions =
                self.subset().get_universal_regions_containing(*from);

            // use type_system's outlive checker
            for universal_region in universal_regions {
                let outlives = Outlives::<Lifetime<BorrowModel>>::new(
                    universal_region.into(),
                    to_universal.region.into(),
                );

                match self.environment().query(&outlives) {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        self.handler().receive(Box::new(
                            UnsatisfiedPredicate::<pernixc_ir::model::Model> {
                                predicate: Predicate::LifetimeOutlives(
                                    Outlives::new(
                                        universal_region.into(),
                                        to_universal.region.into(),
                                    ),
                                ),
                                instantiation_span: span.clone(),
                                predicate_declaration_span: None,
                            },
                        ));
                    }
                    Err(pernixc_type_system::Error::Overflow(error)) => {
                        self.handler().receive(Box::new(
                            UndecidablePredicate::<pernixc_ir::model::Model> {
                                predicate: Predicate::LifetimeOutlives(
                                    Outlives::new(
                                        universal_region.into(),
                                        to_universal.region.into(),
                                    ),
                                ),
                                predicate_declaration_span: None,
                                instantiation_span: span.clone(),
                                overflow_error: error,
                            },
                        ));
                    }
                    Err(pernixc_type_system::Error::Abort(error)) => {
                        return Err(error)
                    }
                }
            }
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub fn invalidate_check(&self) -> Result<(), Abort> {
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
                    &AccessMode::Write(store.span.clone()),
                    point,
                )?,
                Instruction::RegisterAssignment(register_assignment) => {
                    let register = self
                        .representation()
                        .values
                        .registers
                        .get(register_assignment.id)
                        .unwrap();

                    match &register.assignment {
                        Assignment::Load(load) => self.handle_load(
                            load,
                            register.span.as_ref().unwrap(),
                            point,
                        )?,
                        Assignment::Borrow(borrow) => {
                            self.handle_access(
                                &borrow.address,
                                &AccessMode::Read(Read {
                                    qualifier: borrow.qualifier,
                                    span: register.span.clone(),
                                }),
                                point,
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
                Instruction::TuplePack(tuple_pack) => {
                    self.handle_access(
                        &Address::Tuple(Tuple {
                            tuple_address: Box::new(
                                tuple_pack.tuple_address.clone(),
                            ),
                            offset: Offset::Unpacked,
                        }),
                        &AccessMode::Read(Read {
                            qualifier: Qualifier::Immutable,
                            span: tuple_pack.packed_tuple_span.clone(),
                        }),
                        point,
                    )?;

                    self.handle_access(
                        &tuple_pack.store_address,
                        &AccessMode::Write(
                            tuple_pack.packed_tuple_span.clone(),
                        ),
                        point,
                    )?;
                }
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
                    let symbol_kind = *self
                        .environment()
                        .table()
                        .get::<SymbolKind>(self.current_site());

                    if symbol_kind.has_function_signature()
                        && scope_pop.0
                            == self.representation().scope_tree.root_scope_id()
                    {
                        let callable = self
                            .environment()
                            .table()
                            .query::<FunctionSignature>(self.current_site())
                            .unwrap();

                        dropped_memories.extend(
                            callable.parameters.ids().map(Memory::Parameter),
                        );
                    }

                    for memory in dropped_memories {
                        self.handle_drop_memory(memory, point)?;
                    }
                }

                Instruction::RegisterDiscard(_)
                | Instruction::ScopePush(_)
                | Instruction::DropUnpackTuple(_)
                | Instruction::Drop(_) => {}
            }
        }

        Ok(())
    }
}

/// The pass where the error checking is done.
#[allow(clippy::too_many_arguments)]
pub(super) fn borrow_check_internal(
    ir: &Representation<BorrowModel>,
    subset: &Subset,
    register_infos: &RegisterInfos,
    region_variances: &RegionVariances,
    reachability: &Reachability<BorrowModel>,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<(), Abort> {
    let checker = Checker::new(
        ir,
        environment,
        reachability,
        register_infos,
        region_variances,
        current_site,
        subset,
        handler,
    );

    checker.invalidate_check()?;
    checker.universal_region_check()
}
