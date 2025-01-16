use std::sync::Arc;

use pernixc_table::{component::SymbolKind, query, GlobalID, Table, TargetID};
use pernixc_term::{
    generic_parameter::GenericParameters, where_clause::WhereClause,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::builder::Builder;

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
        on_start_building,
        on_finish_building,
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
            match table.query::<GenericParameters>(x) {
                Ok(_) => { /*do nothing */ }
                Err(query::Error::CyclicDependency(error)) => {
                    table.handler().receive(Box::new(error));
                }
                Err(error) => {
                    panic!("unexpected error: {error:?}");
                }
            }
        }

        if symbol_kind.has_where_clause() {
            match table.query::<WhereClause>(x) {
                Ok(_) => { /*do nothing */ }
                Err(query::Error::CyclicDependency(error)) => {
                    table.handler().receive(Box::new(error));
                }
                Err(error) => {
                    panic!("unexpected error: {error:?}");
                }
            }
        }

        on_done(table, x);
    });
}
