//! Contains the function used for creating the [`Reflector`] instance.

use pernixc_component::{
    fields::Fields, function_signature::FunctionSignature,
    implementation::Implementation, implied_predicates::ImpliedPredicates,
    late_bound::LateBound, type_alias::TypeAlias, variance_map::VarianceMap,
    variant::Variant,
};
use pernixc_ir::FunctionBody;
use pernixc_storage::{
    serde::{MergerFn, Reflector},
    ArcTrait,
};
use pernixc_table::{
    component::{
        Accessibility, Extern, Implemented, Implements, LocationSpan, Member,
        Name, Parent, PositiveTraitImplementation, SymbolKind,
        TraitImplementation,
    },
    GlobalID,
};
use pernixc_term::{
    elided_lifetimes::ElidedLifetimes, forall_lifetime,
    generic_parameter::GenericParameters, where_clause::WhereClause,
};
use serde::{Deserialize, Serialize};

/// The enumeration tag for the components.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[serde(rename_all = "snake_case")]
#[allow(missing_docs)]
pub enum ComponentTag {
    Accessibility,
    Name,
    Member,
    Parent,
    Implements,
    SymbolKind,
    Extern,
    LocationSpan,
    Implemented,
    TraitImplementation,
    PositiveTraitImplementation,
    GenericParameters,
    WhereClause,
    Implementation,
    TypeAlias,
    FunctionSignature,
    ImpliedPredicates,
    ElidedLifetimes,
    Fields,
    Variant,
    VarianceMap,
    ForallLifetimeMap,
    LateBound,
    FunctionBody,
}

/// Gets the reflector instance that can be used to serialize all the derived
/// and input components.
#[must_use]
pub fn get() -> Reflector<GlobalID, ArcTrait, ComponentTag, String> {
    let mut reflector = Reflector::default();

    assert!(
        reflector.register_type::<Accessibility>(ComponentTag::Accessibility)
    );
    assert!(reflector.register_type::<Name>(ComponentTag::Name));
    assert!(reflector.register_type::<Member>(ComponentTag::Member));
    assert!(reflector.register_type::<Parent>(ComponentTag::Parent));
    assert!(reflector.register_type::<Implements>(ComponentTag::Implements));
    assert!(reflector.register_type::<SymbolKind>(ComponentTag::SymbolKind));
    assert!(reflector.register_type::<Extern>(ComponentTag::Extern));
    assert!(reflector.register_type::<LocationSpan>(ComponentTag::LocationSpan));
    assert!(reflector.register_type_with_merger::<Implemented>(
        ComponentTag::Implemented,
        &((|a, b| {
            a.extend(b.0.into_iter());

            Ok(())
        }) as MergerFn<Implemented, String>)
    ));
    assert!(reflector.register_type::<TraitImplementation>(
        ComponentTag::TraitImplementation
    ));
    assert!(reflector.register_type::<PositiveTraitImplementation>(
        ComponentTag::PositiveTraitImplementation
    ));

    assert!(reflector
        .register_type::<GenericParameters>(ComponentTag::GenericParameters));
    assert!(reflector.register_type::<WhereClause>(ComponentTag::WhereClause));
    assert!(
        reflector.register_type::<Implementation>(ComponentTag::Implementation)
    );
    assert!(reflector.register_type::<TypeAlias>(ComponentTag::TypeAlias));
    assert!(reflector
        .register_type::<FunctionSignature>(ComponentTag::FunctionSignature));
    assert!(reflector
        .register_type::<ImpliedPredicates>(ComponentTag::ImpliedPredicates));
    assert!(reflector
        .register_type::<ElidedLifetimes>(ComponentTag::ElidedLifetimes));
    assert!(reflector.register_type::<Fields>(ComponentTag::Fields));
    assert!(reflector.register_type::<Variant>(ComponentTag::Variant));
    assert!(reflector.register_type::<VarianceMap>(ComponentTag::VarianceMap));
    assert!(reflector.register_type::<forall_lifetime::Map>(
        ComponentTag::ForallLifetimeMap
    ));
    assert!(reflector.register_type::<LateBound>(ComponentTag::LateBound));
    assert!(reflector.register_type::<FunctionBody>(ComponentTag::FunctionBody));

    reflector
}
