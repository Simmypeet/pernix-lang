use std::sync::Arc;

use pernixc_component::{
    implementation::Implementation, type_alias::TypeAlias,
};
use pernixc_table::{
    component::{Derived, SymbolKind},
    query, GlobalID, Table, TargetID,
};
use pernixc_term::{
    generic_parameter::GenericParameters, where_clause::WhereClause,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::builder::Builder;

/// Invokes the building of a component.
fn build_component<T: Derived>(table: &Table, global_id: GlobalID) {
    // query the component, if it haven't been built, this will invoke the
    // builder for the component
    match table.query::<T>(global_id) {
        Ok(_) => { /*do nothing */ }
        Err(query::Error::CyclicDependency(error)) => {
            table.handler().receive(Box::new(error));
        }
        Err(error) => {
            panic!(
                "unexpected error: {error:?} when building {} for {:?}",
                std::any::type_name::<T>(),
                table.get_qualified_name(global_id)
            );
        }
    }
}

/// An entry point for building every symbol within the given target.
#[allow(clippy::needless_pass_by_value)]
pub fn build(
    table: &mut Table,
    target_id: TargetID,
    on_done: impl Fn(&Table, GlobalID) + Send + Sync,
    on_start_building: impl Fn(&Table, GlobalID, &'static str)
        + Send
        + Sync
        + 'static,
    on_finish_building: impl Fn(&Table, GlobalID, &'static str)
        + Send
        + Sync
        + 'static,
) {
    let on_start_building = Arc::new(on_start_building);
    let on_finish_building = Arc::new(on_finish_building);

    assert!(table.set_builder::<GenericParameters, _>(Builder::new(
        on_start_building.clone(),
        on_finish_building.clone(),
    )));
    assert!(table.set_builder::<WhereClause, _>(Builder::new(
        on_start_building.clone(),
        on_finish_building.clone(),
    )));
    assert!(table.set_builder::<Implementation, _>(Builder::new(
        on_start_building.clone(),
        on_finish_building.clone(),
    )));
    assert!(table.set_builder::<TypeAlias, _>(Builder::new(
        on_start_building,
        on_finish_building
    )));

    let symbols_to_build = table
        .get_target(target_id)
        .unwrap()
        .all_symbols()
        .map(|x| GlobalID::new(target_id, x))
        .collect::<Vec<_>>();

    symbols_to_build.into_par_iter().for_each(|x| {
        let symbol_kind = *table.get::<SymbolKind>(x).unwrap();

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

        on_done(table, x);
    });
}
