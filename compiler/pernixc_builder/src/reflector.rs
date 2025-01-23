//! Contains the function used for creating the [`Reflector`] instance.

use pernixc_component::{
    fields::Fields, function_signature::FunctionSignature,
    implementation::Implementation, implied_predicates::ImpliedPredicates,
    type_alias::TypeAlias, variant::Variant,
};
use pernixc_storage::{serde::Reflector, ArcTrait};
use pernixc_table::{GlobalID, Table};
use pernixc_term::{
    elided_lifetimes::ElidedLifetimes, generic_parameter::GenericParameters,
    where_clause::WhereClause,
};

/// Gets the reflector instance that can be used to serialize all the derived
/// and input components.
#[must_use]
pub fn get() -> Reflector<GlobalID, ArcTrait, String, String> {
    let mut reflector = Table::input_reflector();

    assert!(reflector
        .register_type::<GenericParameters>("GenericParameters".to_owned()));
    assert!(reflector.register_type::<WhereClause>("WhereClause".to_owned()));
    assert!(
        reflector.register_type::<Implementation>("Implementation".to_owned())
    );
    assert!(reflector.register_type::<TypeAlias>("TypeAlias".to_owned(),));
    assert!(reflector
        .register_type::<FunctionSignature>("FunctionSignature".to_owned(),));
    assert!(reflector
        .register_type::<ImpliedPredicates>("ImpliedPredicates".to_owned(),));
    assert!(reflector
        .register_type::<ElidedLifetimes>("ElidedLifetimes".to_owned(),));
    assert!(reflector.register_type::<Fields>("Fields".to_owned(),));
    assert!(reflector.register_type::<Variant>("Variant".to_owned(),));

    reflector
}
