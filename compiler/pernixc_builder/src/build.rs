use pernixc_table::{component::SymbolKind, query, GlobalID, Table, TargetID};
use pernixc_term::generic_parameter::GenericParameters;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::Builder;

/// An entry point for building every symbol within the given target.
pub fn build(table: &mut Table, target_id: TargetID) {
    assert!(table.set_builder::<GenericParameters, _>(Builder));

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
    });
}
