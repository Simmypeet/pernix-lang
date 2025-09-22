use getset::{CopyGetters, Getters};
use pernixc_handler::Handler;
use pernixc_hash::HashSet;
use pernixc_ir::{address::Address, value::TypeOf, IR};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_target::Global;
use pernixc_term::{r#type::Qualifier, visitor::RecursiveIterator};
use pernixc_type_system::{
    environment::Environment, normalizer::Normalizer, UnrecoverableError,
};

use crate::{
    cache::{RegionVariances, RegisterInfos},
    diagnostic::Diagnostic,
    Region,
};

/// A struct holding all relevant information for borrow checking
#[derive(Clone, Getters, CopyGetters)]
pub struct Context<'a, N> {
    /// The IR being borrow checked
    #[get_copy = "pub"]
    ir: &'a IR,

    /// The environment for type system operations
    #[get_copy = "pub"]
    environment: &'a Environment<'a, N>,

    /// The current site for type checking operations
    #[get_copy = "pub"]
    current_site: Global<pernixc_symbol::ID>,

    /// Cached information about reachability
    #[get = "pub"]
    reachability: pernixc_ir::control_flow_graph::Reachability,

    /// Cached information about registers
    #[get = "pub"]
    register_infos: RegisterInfos,

    /// Cached information about region's variance
    #[get = "pub"]
    region_variances: RegionVariances,

    /// The handler for reporting diagnostics
    #[get_copy = "pub"]
    handler: &'a dyn Handler<Diagnostic>,
}

impl<N: std::fmt::Debug> std::fmt::Debug for Context<'_, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Context")
            .field("ir", &self.ir)
            .field("environment", &self.environment)
            .field("current_site", &self.current_site)
            .field("reachability", &self.reachability)
            .field("register_infos", &self.register_infos)
            .field("region_variances", &self.region_variances)
            .finish_non_exhaustive()
    }
}

impl<'a, N: Normalizer> Context<'a, N> {
    /// Create a new borrow checking context
    pub async fn new(
        ir: &'a IR,
        environment: &'a Environment<'a, N>,
        current_site: Global<pernixc_symbol::ID>,
        handler: &'a dyn Handler<Diagnostic>,
    ) -> Result<Self, UnrecoverableError> {
        let register_infos =
            RegisterInfos::new(ir, current_site, environment, handler).await?;

        let region_variances = RegionVariances::new(
            ir,
            current_site,
            environment.tracked_engine(),
        )
        .await?;

        let reachability = ir.control_flow_graph.reachability();

        Ok(Self {
            ir,
            environment,
            current_site,
            reachability,
            register_infos,
            region_variances,
            handler,
        })
    }

    /// Gets the tracked engine from the environment
    pub fn tracked_engine(&self) -> &'a pernixc_query::TrackedEngine {
        self.environment.tracked_engine()
    }

    /// Gets the values from the IR
    #[must_use]
    pub const fn values(&self) -> &'a pernixc_ir::Values { &self.ir.values }

    /// Gets the control flow graph from the IR
    #[must_use]
    pub const fn control_flow_graph(
        &self,
    ) -> &'a pernixc_ir::control_flow_graph::ControlFlowGraph {
        &self.ir.control_flow_graph
    }

    /// Gets all the regions that appear in the given address type (including
    /// in dereferenced references if `include_deref` is true).
    pub async fn get_regions_in_address(
        &self,
        mut address: &Address,
        address_span: RelativeSpan,
        include_deref: bool,
    ) -> Result<HashSet<Region>, UnrecoverableError> {
        let address_ty = self
            .ir
            .values
            .type_of(address, self.current_site, self.environment)
            .await
            .map_err(|x| {
                x.report_as_type_calculating_overflow(
                    address_span,
                    &self.handler,
                )
            })?
            .result;

        let mut regions = HashSet::default();

        // let mut regions = RecursiveIterator::new(&address_ty)
        //     .filter_map(|x| x.0.into_lifetime().ok())
        //     .filter_map(|x| Region::try_from(*x).ok())
        //     .collect::<HashSet<_>>();

        if include_deref {
            loop {
                match address {
                    Address::Memory(_) => break,

                    Address::Field(field) => {
                        address = &field.struct_address;
                    }
                    Address::Tuple(tuple) => {
                        address = &tuple.tuple_address;
                    }
                    Address::Index(index) => {
                        address = &index.array_address;
                    }
                    Address::Variant(variant) => {
                        address = &variant.enum_address;
                    }

                    Address::Reference(reference) => {
                        let pointee_ty = self
                            .ir
                            .values
                            .type_of(
                                &*reference.reference_address,
                                self.current_site,
                                self.environment,
                            )
                            .await
                            .map_err(|x| {
                                x.report_as_type_calculating_overflow(
                                    address_span,
                                    &self.handler,
                                )
                            })?
                            .result;

                        let pointee_reference_ty =
                            pointee_ty.into_reference().unwrap();

                        regions.extend(
                            Region::try_from(pointee_reference_ty.lifetime)
                                .ok(),
                        );

                        if pointee_reference_ty.qualifier
                            == Qualifier::Immutable
                        {
                            break;
                        }

                        address = &reference.reference_address;
                    }
                }
            }
        }

        Ok(regions)
    }
}
