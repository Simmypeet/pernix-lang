use pernixc_ir::{
    address::{Address, Memory, Offset, Tuple},
    control_flow_graph::Point,
    instruction::{AccessMode, Instruction, Read},
    value::{
        register::{Assignment, Load},
        TypeOf,
    },
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::parameter::get_parameters;
use pernixc_symbol::{kind::get_kind, name::get_by_qualified_name};
use pernixc_term::{
    generic_arguments::GenericArguments,
    lifetime::Lifetime,
    predicate::{Outlives, PositiveMarker, Predicate},
    r#type::Qualifier,
};
use pernixc_type_system::{
    diagnostic::{PredicateSatisfiabilityOverflow, UnsatisfiedPredicate},
    normalizer::Normalizer,
    UnrecoverableError,
};

use crate::{
    context,
    invalidate::Checker,
    subset::{RegionAt, Subset},
};

impl<N: Normalizer> Checker<'_, N> {
    async fn handle_load(
        &self,
        load: &Load,
        register_span: &RelativeSpan,
        point: Point,
    ) -> Result<(), UnrecoverableError> {
        let ty = self
            .values()
            .type_of(&load.address, self.current_site(), self.environment())
            .await
            .map_err(|x| {
                x.report_as_type_calculating_overflow(
                    *register_span,
                    &self.handler(),
                )
            })?;

        self.handle_access(
            &load.address,
            &AccessMode::Read(Read {
                qualifier: Qualifier::Immutable,
                span: Some(*register_span),
            }),
            point,
        )
        .await?;

        // has been checked previously
        'out: {
            if load.address.get_reference_qualifier()
                == Some(Qualifier::Immutable)
                || load.address.is_behind_index()
            {
                // TODO: check copy marker
            } else {
                let copy_marker = self
                    .tracked_engine()
                    .get_by_qualified_name(
                        pernixc_corelib::copy::MARKER_SEQUENCE,
                    )
                    .await
                    .unwrap();

                if self
                    .context()
                    .environment()
                    .predicate_satisfied_as_diagnostics(
                        Predicate::PositiveMarker(PositiveMarker::new(
                            copy_marker,
                            GenericArguments {
                                lifetimes: Vec::new(),
                                types: vec![ty.result],
                                constants: Vec::new(),
                            },
                        )),
                        *register_span,
                        None,
                        false,
                        &self.handler(),
                    )
                    .await?
                    .0
                    .is_empty()
                {
                    break 'out;
                }

                self.handle_moved_memory(&load.address, register_span, point)
                    .await?;
            }
        };
        Ok(())
    }

    pub async fn universal_region_check(
        &self,
    ) -> Result<(), UnrecoverableError> {
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
                let outlives = Outlives::<Lifetime>::new(
                    universal_region.into(),
                    to_universal.region.into(),
                );

                match self.environment().query(&outlives).await {
                    Ok(Some(_)) => {}
                    Ok(None) => {
                        self.handler().receive(
                            UnsatisfiedPredicate {
                                predicate: Predicate::LifetimeOutlives(
                                    Outlives::new(
                                        universal_region.into(),
                                        to_universal.region.into(),
                                    ),
                                ),
                                instantiation_span: *span,
                                predicate_declaration_span: None,
                            }
                            .into(),
                        );
                    }
                    Err(pernixc_type_system::Error::Overflow(error)) => {
                        self.handler().receive(
                            PredicateSatisfiabilityOverflow {
                                predicate: Predicate::LifetimeOutlives(
                                    Outlives::new(
                                        universal_region.into(),
                                        to_universal.region.into(),
                                    ),
                                ),
                                predicate_declaration_span: None,
                                instantiation_span: *span,
                                overflow_error: error,
                            }
                            .into(),
                        );

                        return Err(UnrecoverableError::Reported);
                    }
                    Err(pernixc_type_system::Error::CyclicDependency(
                        error,
                    )) => return Err(error.into()),
                }
            }
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    async fn invalid_instuction(
        &self,
        point: Point,
        inst: &Instruction,
    ) -> Result<(), UnrecoverableError> {
        match inst {
            Instruction::Store(store) => {
                self.handle_access(
                    &store.address,
                    &AccessMode::Write(store.span),
                    point,
                )
                .await
            }
            Instruction::RegisterAssignment(register_assignment) => {
                let register = self
                    .values()
                    .registers
                    .get(register_assignment.id)
                    .unwrap();

                match &register.assignment {
                    Assignment::Load(load) => {
                        self.handle_load(
                            load,
                            register.span.as_ref().unwrap(),
                            point,
                        )
                        .await
                    }
                    Assignment::Borrow(borrow) => {
                        self.handle_access(
                            &borrow.address,
                            &AccessMode::Read(Read {
                                qualifier: borrow.qualifier,
                                span: register.span,
                            }),
                            point,
                        )
                        .await
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
                    | Assignment::VariantNumber(_) => Ok(()),
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
                        span: tuple_pack.packed_tuple_span,
                    }),
                    point,
                )
                .await?;

                self.handle_access(
                    &tuple_pack.store_address,
                    &AccessMode::Write(tuple_pack.packed_tuple_span),
                    point,
                )
                .await
            }
            Instruction::ScopePop(scope_pop) => {
                let mut dropped_memories = self
                    .values()
                    .allocas
                    .iter()
                    .filter_map(|(id, x)| {
                        (x.declared_in_scope_id == scope_pop.0)
                            .then_some(Memory::Alloca(id))
                    })
                    .collect::<Vec<_>>();

                // add parameters to the dropped memories
                let symbol_kind =
                    self.tracked_engine().get_kind(self.current_site()).await;

                if symbol_kind.has_function_signature()
                    && scope_pop.0
                        == self
                            .context()
                            .ir()
                            .control_flow_graph
                            .root_scope_id()
                {
                    let callable = self
                        .tracked_engine()
                        .get_parameters(self.current_site())
                        .await?;

                    dropped_memories.extend(
                        callable.parameters.ids().map(Memory::Parameter),
                    );
                }

                for memory in dropped_memories {
                    self.handle_drop_memory(memory, point).await?;
                }

                Ok(())
            }

            Instruction::RegisterDiscard(_)
            | Instruction::ScopePush(_)
            | Instruction::DropUnpackTuple(_)
            | Instruction::Drop(_) => Ok(()),
        }
    }

    pub async fn invalidate_check(&self) -> Result<(), UnrecoverableError> {
        for (block_id, block) in self.control_flow_graph().traverse() {
            for (point, inst) in
                block.instructions().iter().enumerate().map(move |(i, inst)| {
                    (Point { instruction_index: i, block_id }, inst)
                })
            {
                self.invalid_instuction(point, inst).await?;
            }
        }

        Ok(())
    }
}

/// The pass where the error checking is done.
#[allow(clippy::too_many_arguments)]
pub(super) async fn borrow_check<'a, N: Normalizer>(
    context: &'a context::Context<'a, N>,
    subset: &'a Subset,
) -> Result<(), UnrecoverableError> {
    let checker = Checker::new(context, subset);

    checker.invalidate_check().await?;
    checker.universal_region_check().await
}
