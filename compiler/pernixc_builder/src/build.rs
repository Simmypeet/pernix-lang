use std::sync::Arc;

use pernixc_component::{
    fields::Fields, function_signature::FunctionSignature,
    implementation::Implementation, implied_predicates::ImpliedPredicates,
    late_bound::LateBound, type_alias::TypeAlias, variance_map::VarianceMap,
    variant::Variant,
};
use pernixc_ir::IR;
use pernixc_table::{
    component::{Derived, SymbolKind},
    GlobalID, Table, TargetID,
};
use pernixc_term::{
    elided_lifetimes::ElidedLifetimes, forall_lifetime,
    generic_parameter::GenericParameters, where_clause::WhereClause,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use typed_builder::TypedBuilder;

use crate::{
    builder::Builder,
    extern_function_check::extern_function_check,
    function,
    implementation_coherence::{
        check_implementation_member, check_implemented_instantiation,
        check_orphan_rule, check_overlapping, check_unused_generic_parameters,
    },
    occurrences::{self, check_occurrences},
    variance_map, where_clause_check,
};

/// A struct for starting the building of a target.
#[derive(TypedBuilder)]
pub struct Compilation<'a> {
    /// The table where the local target is stored.
    pub table: &'a mut Table,

    /// The target to build.
    pub target_id: TargetID,

    /// The callback that is invoked when a symbol is started to be built.
    #[builder(default, setter(strip_option))]
    pub on_start: Option<Arc<SymbolCallback>>,

    /// The callback that is invoked when a symbol is finished building.
    #[builder(default, setter(strip_option))]
    pub on_done: Option<Arc<SymbolCallback>>,

    /// The callback that is invoked when a symbol's component is started to be
    /// built.
    #[builder(default, setter(strip_option))]
    pub on_start_building_component: Option<Arc<ComponentCallback>>,

    /// The callback that is invoked when a symbol's component is finished
    /// building.
    #[builder(default, setter(strip_option))]
    pub on_finish_building_component: Option<Arc<ComponentCallback>>,
}

impl std::fmt::Debug for Compilation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Compilation")
            .field("table", &"Table")
            .field("target_id", &self.target_id)
            .field("on_start", &self.on_start.is_some())
            .field("on_done", &self.on_done.is_some())
            .field(
                "on_start_building_component",
                &self.on_start_building_component.is_some(),
            )
            .field(
                "on_finish_building_component",
                &self.on_finish_building_component.is_some(),
            )
            .finish()
    }
}

/// Invokes the building of a component.
fn build_component<T: Derived>(table: &Table, global_id: GlobalID) {
    // query the component, if it haven't been built, this will invoke the
    // builder for the component
    let _ = table.query::<T>(global_id);
}

/// A callback that is invoked when a symbol is started/finished to be built.
pub type SymbolCallback = dyn Fn(&Table, GlobalID) + Send + Sync;

/// A callback that is invoked when a symbol's component is started/finished to
/// be built.
pub type ComponentCallback =
    dyn Fn(&Table, GlobalID, &'static str) + Send + Sync;

impl Compilation<'_> {
    /// Builds every symbol within the target.
    pub fn run(self) {
        let Compilation {
            table,
            target_id,
            on_start,
            on_done,
            on_start_building_component,
            on_finish_building_component,
        } = self;

        build(
            table,
            target_id,
            on_start,
            on_done,
            on_start_building_component,
            on_finish_building_component,
        );
    }
}

/// An entry point for building every symbol within the given target.
#[allow(clippy::needless_pass_by_value, clippy::too_many_lines)]
pub fn build(
    table: &mut Table,
    target_id: TargetID,
    on_start: Option<Arc<SymbolCallback>>,
    on_done: Option<Arc<SymbolCallback>>,
    on_start_building_component: Option<Arc<ComponentCallback>>,
    on_finish_building_component: Option<Arc<ComponentCallback>>,
) {
    table.set_builder_overwrite::<GenericParameters, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<WhereClause, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<Implementation, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<TypeAlias, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<function::Intermediate, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<FunctionSignature, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<ImpliedPredicates, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<ElidedLifetimes, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<LateBound, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<Variant, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<Fields, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<IR, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    ));
    table.set_builder_overwrite::<VarianceMap, _>(variance_map::Builder::new(
        Builder::new(on_start_building_component, on_finish_building_component),
    ));

    let symbols_to_build = table
        .get_target(target_id)
        .unwrap()
        .all_symbols()
        .map(|x| GlobalID::new(target_id, x))
        .collect::<Vec<_>>();

    for x in symbols_to_build.iter().copied() {
        assert!(table.add_component(x, occurrences::Occurrences::default()));
        assert!(table.add_component(x, forall_lifetime::Map::default()));
    }

    symbols_to_build.into_par_iter().panic_fuse().for_each(|x| {
        if let Some(callback) = on_start.as_ref() {
            (callback)(table, x);
        }

        let symbol_kind = *table.get::<SymbolKind>(x);

        if symbol_kind.has_generic_parameters() {
            build_component::<GenericParameters>(table, x);
        }

        if symbol_kind.has_where_clause() {
            build_component::<WhereClause>(table, x);
        }

        if symbol_kind.is_implementation() {
            build_component::<Implementation>(table, x);
        }

        if symbol_kind.has_type_alias() {
            build_component::<TypeAlias>(table, x);
        }

        if symbol_kind.has_function_signature() {
            build_component::<ElidedLifetimes>(table, x);
            build_component::<ImpliedPredicates>(table, x);
            build_component::<FunctionSignature>(table, x);
            build_component::<LateBound>(table, x);
        }

        if symbol_kind == SymbolKind::Variant {
            build_component::<Variant>(table, x);
        }
        if symbol_kind == SymbolKind::Struct {
            build_component::<Fields>(table, x);
        }
        if symbol_kind.has_variance_map() {
            build_component::<VarianceMap>(table, x);
        }

        if symbol_kind.has_where_clause()
            || symbol_kind.has_implied_predicates()
        {
            where_clause_check::check(table, x, &**table.handler());
        }

        // check the well-formedness of all occurrences so far
        check_occurrences(table, x, &**table.handler());

        if symbol_kind.is_implementation() {
            check_unused_generic_parameters(table, x, &**table.handler());
            check_implemented_instantiation(table, x, &**table.handler());
            check_orphan_rule(table, x, &**table.handler());
        }

        // check if the implementation is ambiguous or overrides another
        if matches!(
            symbol_kind,
            SymbolKind::PositiveMarkerImplementation
                | SymbolKind::NegativeMarkerImplementation
                | SymbolKind::PositiveTraitImplementation
                | SymbolKind::NegativeTraitImplementation
        ) {
            check_overlapping(table, x, &**table.handler());
        }

        if matches!(
            symbol_kind,
            SymbolKind::TraitImplementationFunction
                | SymbolKind::TraitImplementationConstant
                | SymbolKind::TraitImplementationType
        ) {
            check_implementation_member(table, x, &**table.handler());
        }

        // check the extern function definition
        if matches!(symbol_kind, SymbolKind::ExternFunction) {
            let _ = extern_function_check(table, x, &**table.handler());
        }

        if symbol_kind.has_function_body() {
            build_component::<IR>(table, x);
        }

        if let Some(callback) = on_done.as_ref() {
            (callback)(table, x);
        }
    });
}
