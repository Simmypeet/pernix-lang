use std::sync::Arc;

use pernixc_component::{
    fields::Fields, function_signature::FunctionSignature,
    implementation::Implementation, implied_predicates::ImpliedPredicates,
    type_alias::TypeAlias, variance_map::VarianceMap, variant::Variant,
};
use pernixc_table::{
    component::{Derived, SymbolKind},
    GlobalID, Table, TargetID,
};
use pernixc_term::{
    elided_lifetimes::ElidedLifetimes, generic_parameter::GenericParameters,
    where_clause::WhereClause,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use typed_builder::TypedBuilder;

use crate::{builder::Builder, function, occurrences, variance_map};

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
    table.query::<T>(global_id);
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
#[allow(clippy::needless_pass_by_value)]
pub fn build(
    table: &mut Table,
    target_id: TargetID,
    on_start: Option<Arc<SymbolCallback>>,
    on_done: Option<Arc<SymbolCallback>>,
    on_start_building_component: Option<Arc<ComponentCallback>>,
    on_finish_building_component: Option<Arc<ComponentCallback>>,
) {
    assert!(table.set_builder::<GenericParameters, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    )));
    assert!(table.set_builder::<WhereClause, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    )));
    assert!(table.set_builder::<Implementation, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    )));
    assert!(table.set_builder::<TypeAlias, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    )));
    assert!(table.set_builder::<function::Intermediate, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    )));
    assert!(table.set_builder::<FunctionSignature, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    )));
    assert!(table.set_builder::<ImpliedPredicates, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    )));
    assert!(table.set_builder::<ElidedLifetimes, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    )));
    assert!(table.set_builder::<Variant, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    )));
    assert!(table.set_builder::<Fields, _>(Builder::new(
        on_start_building_component.clone(),
        on_finish_building_component.clone(),
    )));
    assert!(table.set_builder::<VarianceMap, _>(variance_map::Builder::new(
        Builder::new(on_start_building_component, on_finish_building_component)
    )));

    let symbols_to_build = table
        .get_target(target_id)
        .unwrap()
        .all_symbols()
        .map(|x| GlobalID::new(target_id, x))
        .collect::<Vec<_>>();

    symbols_to_build.into_par_iter().for_each(|x| {
        if let Some(callback) = on_start.as_ref() {
            (callback)(table, x);
        }

        let symbol_kind = *table.get::<SymbolKind>(x);
        assert!(table.add_component(x, occurrences::Occurrences::default()));

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

        // check the well-formedness of all occurrences so far
        occurrences::check_occurrences(table, x, &**table.handler());

        if let Some(callback) = on_done.as_ref() {
            (callback)(table, x);
        }
    });
}
