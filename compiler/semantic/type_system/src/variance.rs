//! Contains the [`get_variance_of`] function.

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::variance::{Variance, get_variances};
use pernixc_symbol::kind::{Kind, get_kind};
use pernixc_term::{
    generic_parameters::get_generic_parameters,
    sub_term::{Location, SubLifetimeLocation, SubTypeLocation, TermLocation},
    r#type::{self, Type},
};

/// Retrieves the variance of the term at the given location.
///
/// This function early returns the `parent_variance` if the variance is
/// [`Variance::Invariant`] or if the location is empty.
///
/// # Returns
///
/// [`None`] if encounters a cyclic dependency when querying the variance
/// map from the table.
///
/// # Errors
///
/// [`Abort`] errors mostly from the query functions.
#[extend]
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
pub async fn get_variance_of(
    self: &TrackedEngine,
    ty: &Type,
    parent_variance: Variance,
    mut locations: impl Iterator<Item = TermLocation>,
) -> Variance {
    let Some(location) = locations.next() else {
        return parent_variance;
    };

    // early return if the parent variance is invariant
    if parent_variance == Variance::Invariant {
        return Variance::Invariant;
    }

    match location {
        TermLocation::Lifetime(location) => {
            let SubLifetimeLocation::FromType(location) = location;

            match (location, ty) {
                // lifetime in the adt
                (
                    r#type::SubLifetimeLocation::Symbol(location),
                    Type::Symbol(symbol),
                ) => {
                    // there's no sub-term in the lifetime
                    assert!(locations.next().is_none());

                    let kind = self.get_kind(symbol.id).await;

                    if kind.is_adt() {
                        let id = *self
                            .get_generic_parameters(symbol.id)
                            .await
                            .lifetime_order()
                            .get(location.0)
                            .unwrap();

                        parent_variance.xfrom(
                            self.get_variances(symbol.id)
                                .await
                                .variances_by_lifetime_ids
                                .get(&id)
                                .copied()
                                .unwrap(),
                        )
                    } else {
                        parent_variance.xfrom(Variance::Invariant)
                    }
                }

                // lifetime in the member function and trait member
                (
                    r#type::SubLifetimeLocation::MemberSymbol(_),
                    Type::MemberSymbol(_),
                )
                | (
                    r#type::SubLifetimeLocation::TraitMember(_),
                    Type::TraitMember(_),
                ) => {
                    // there's no sub-term in the lifetime
                    assert!(locations.next().is_none());

                    parent_variance.xfrom(Variance::Invariant)
                }

                // lifetime in the reference
                (
                    r#type::SubLifetimeLocation::Reference,
                    Type::Reference(_),
                ) => {
                    // there's no sub-term in the lifetime
                    assert!(locations.next().is_none());

                    parent_variance.xfrom(Variance::Covariant)
                }

                _ => panic!("invalid location; found {location:?} {ty:?}"),
            }
        }

        TermLocation::Type(location) => {
            let SubTypeLocation::FromType(location) = location;

            match (location, ty) {
                (
                    r#type::SubTypeLocation::Symbol(location),
                    Type::Symbol(symbol),
                ) => {
                    let kind = self.get_kind(symbol.id).await;

                    if kind.is_adt() {
                        let id = *self
                            .get_generic_parameters(symbol.id)
                            .await
                            .type_order()
                            .get(location.0)
                            .unwrap();

                        let next_variance = parent_variance.xfrom(
                            self.get_variances(symbol.id)
                                .await
                                .variances_by_type_ids
                                .get(&id)
                                .copied()
                                .unwrap(),
                        );

                        let inner_term = symbol
                            .generic_arguments
                            .types
                            .get(location.0)
                            .unwrap();

                        Box::pin(self.get_variance_of(
                            inner_term,
                            next_variance,
                            locations,
                        ))
                        .await
                    } else if kind == Kind::Function {
                        parent_variance.xfrom(Variance::Invariant)
                    } else {
                        panic!("expected adt or function but found {kind:?}",);
                    }
                }

                (
                    r#type::SubTypeLocation::Reference,
                    Type::Reference(reference),
                ) => {
                    let current_variance =
                        parent_variance.xfrom(match reference.qualifier {
                            r#type::Qualifier::Immutable => Variance::Covariant,

                            r#type::Qualifier::Mutable => Variance::Invariant,
                        });

                    Box::pin(self.get_variance_of(
                        &reference.pointee,
                        current_variance,
                        locations,
                    ))
                    .await
                }

                (r#type::SubTypeLocation::Pointer, Type::Pointer(pointer)) => {
                    let current_variance =
                        parent_variance.xfrom(Variance::Bivariant);

                    Box::pin(self.get_variance_of(
                        &pointer.pointee,
                        current_variance,
                        locations,
                    ))
                    .await
                }

                (r#type::SubTypeLocation::Array, Type::Array(array)) => {
                    let current_variance =
                        parent_variance.xfrom(Variance::Covariant);

                    Box::pin(self.get_variance_of(
                        &array.r#type,
                        current_variance,
                        locations,
                    ))
                    .await
                }

                (r#type::SubTypeLocation::Phantom, Type::Phantom(phantom)) => {
                    let current_variance =
                        parent_variance.xfrom(Variance::Covariant);

                    Box::pin(self.get_variance_of(
                        &phantom.0,
                        current_variance,
                        locations,
                    ))
                    .await
                }

                (
                    location @ r#type::SubTypeLocation::FunctionSignature(_),
                    tuple @ Type::FunctionSignature(_),
                ) => {
                    use r#type::SubFunctionSignatureLocation::{
                        Parameter, ReturnType,
                    };

                    let current_variance =
                        match location.as_function_signature().unwrap() {
                            Parameter(_) => Variance::Contravariant,
                            ReturnType => Variance::Covariant,
                        };

                    let sub_term = location.get_sub_term(tuple).unwrap();

                    Box::pin(self.get_variance_of(
                        &sub_term,
                        parent_variance.xfrom(current_variance),
                        locations,
                    ))
                    .await
                }

                (
                    location @ r#type::SubTypeLocation::Tuple(_),
                    tuple @ Type::Tuple(_),
                ) => {
                    let sub_term =
                        location.get_sub_term(tuple).unwrap_or_else(|| {
                            panic!(
                                "failed to get sub term of tuple: {tuple:?} \
                                 at location: {location:?}"
                            )
                        });

                    let current_variance =
                        parent_variance.xfrom(Variance::Covariant);

                    Box::pin(self.get_variance_of(
                        &sub_term,
                        current_variance,
                        locations,
                    ))
                    .await
                }

                (
                    r#type::SubTypeLocation::MemberSymbol(location),
                    Type::MemberSymbol(symbol),
                ) => {
                    let inner_term = if location.from_parent {
                        symbol
                            .parent_generic_arguments
                            .types
                            .get(location.index)
                    } else {
                        symbol
                            .member_generic_arguments
                            .types
                            .get(location.index)
                    }
                    .unwrap();

                    let current_variance =
                        parent_variance.xfrom(Variance::Invariant);

                    Box::pin(self.get_variance_of(
                        inner_term,
                        current_variance,
                        locations,
                    ))
                    .await
                }

                (
                    r#type::SubTypeLocation::TraitMember(location),
                    Type::TraitMember(trait_member),
                ) => {
                    let inner_term = if location.0.from_parent {
                        trait_member
                            .0
                            .parent_generic_arguments
                            .types
                            .get(location.0.index)
                    } else {
                        trait_member
                            .0
                            .member_generic_arguments
                            .types
                            .get(location.0.index)
                    }
                    .unwrap();

                    let current_variance =
                        parent_variance.xfrom(Variance::Invariant);

                    Box::pin(self.get_variance_of(
                        inner_term,
                        current_variance,
                        locations,
                    ))
                    .await
                }

                _ => panic!("invalid location"),
            }
        }

        TermLocation::Constant(_) => panic!("no variance for constant"),
    }
}
