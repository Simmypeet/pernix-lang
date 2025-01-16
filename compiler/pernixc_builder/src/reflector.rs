//! Contains the function used for creating the [`Reflector`] instance.

use pernixc_storage::{
    serde::{MergerFn, Reflector},
    ArcTrait,
};
use pernixc_table::{GlobalID, Table};
use pernixc_term::{
    generic_parameter::GenericParameters, where_clause::WhereClause,
};

/// Gets the reflector instance that can be used to serialize all the derived
/// and input components.
#[must_use]
pub fn get() -> Reflector<GlobalID, ArcTrait, String, String> {
    let mut reflector = Table::input_reflector();

    assert!(reflector.register_type_with_merger::<GenericParameters>(
        "GenericParameters".to_owned(),
        &((|a, b| {
            if *a == b {
                Ok(())
            } else {
                Err("Incompatible generic parameter".to_owned())
            }
        }) as MergerFn<GenericParameters, String>)
    ));
    assert!(reflector.register_type_with_merger::<WhereClause>(
        "WhereClause".to_owned(),
        &((|a, b| {
            if *a == b {
                Ok(())
            } else {
                Err("Incompatible where clause".to_owned())
            }
        }) as MergerFn<WhereClause, String>)
    ));

    reflector
}
