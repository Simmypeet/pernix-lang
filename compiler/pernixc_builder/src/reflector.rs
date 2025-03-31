//! Contains the function used for creating the [`Reflector`] instance.

use pernixc_semantic::{
    component::{
        derived::{
            elided_lifetimes::ElidedLifetimes, fields::Fields,
            forall_lifetimes::ForallLifetimes,
            function_signature::FunctionSignature,
            generic_parameters::GenericParameters,
            implementation::Implementation,
            implied_predicates::ImpliedPredicates, ir::IR,
            late_bound::LateBound, type_alias::TypeAlias, variances::Variances,
            variant::Variant, where_clause::WhereClause,
        },
        input::{
            Accessibility, Extern, Implemented, Implements, LocationSpan,
            Member, Name, Parent, PositiveTraitImplementation, SymbolKind,
            TraitImplementation, VariantDeclarationOrder,
        },
    },
    table::GlobalID,
};
use pernixc_storage::{
    serde::{MergerFn, Reflector},
    ArcTrait,
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
    VariantDeclarationOrder,
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
    Variances,
    ForallLifetimeMap,
    LateBound,

    #[serde(rename = "ir")]
    IR,
}

/// Gets the reflector instance that can be used to serialize all the derived
/// and input components.
#[must_use]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
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
    assert!(reflector.register_type::<VariantDeclarationOrder>(
        ComponentTag::VariantDeclarationOrder
    ));
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
    assert!(reflector.register_type::<Variances>(ComponentTag::Variances));
    assert!(reflector
        .register_type::<ForallLifetimes>(ComponentTag::ForallLifetimeMap));
    assert!(reflector.register_type::<LateBound>(ComponentTag::LateBound));
    assert!(reflector.register_type::<IR>(ComponentTag::IR));

    reflector
}
