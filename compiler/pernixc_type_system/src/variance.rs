//! Contains the function [`Environment::get_variance_of()`]

use pernixc_abort::Abort;
use pernixc_table::component::SymbolKind;
use pernixc_term::{
    generic_parameter::GenericParameters,
    r#type::{self, Type},
    sub_term::{Location, SubLifetimeLocation, SubTypeLocation, TermLocation},
    variance::{Variance, Variances},
    Model,
};

use crate::{environment::Environment, normalizer::Normalizer};

impl<M: Model, N: Normalizer<M>> Environment<'_, M, N> {
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
    #[allow(clippy::too_many_lines)]
    pub fn get_variance_of(
        &self,
        ty: &Type<M>,
        parent_variance: Variance,
        mut locations: impl Iterator<Item = TermLocation>,
    ) -> Result<Variance, Abort> {
        let Some(location) = locations.next() else {
            return Ok(parent_variance);
        };

        // early return if the parent variance is invariant
        if parent_variance == Variance::Invariant {
            return Ok(Variance::Invariant);
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

                        let kind = *self.table().get::<SymbolKind>(symbol.id);

                        if kind.is_adt() {
                            let id = *self
                                .table()
                                .query::<GenericParameters>(symbol.id)?
                                .lifetime_order()
                                .get(location.0)
                                .unwrap();

                            Ok(parent_variance.xfrom(
                                self.table()
                                    .query::<Variances>(symbol.id)?
                                    .variances_by_lifetime_ids
                                    .get(&id)
                                    .copied()
                                    .unwrap(),
                            ))
                        } else {
                            Ok(parent_variance.xfrom(Variance::Invariant))
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

                        Ok(parent_variance.xfrom(Variance::Invariant))
                    }

                    // lifetime in the reference
                    (
                        r#type::SubLifetimeLocation::Reference,
                        Type::Reference(_),
                    ) => {
                        // there's no sub-term in the lifetime
                        assert!(locations.next().is_none());

                        Ok(parent_variance.xfrom(Variance::Covariant))
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
                        let kind = *self.table().get::<SymbolKind>(symbol.id);

                        if kind.is_adt() {
                            let id = *self
                                .table()
                                .query::<GenericParameters>(symbol.id)?
                                .type_order()
                                .get(location.0)
                                .unwrap();

                            let next_variance = parent_variance.xfrom(
                                self.table()
                                    .query::<Variances>(symbol.id)?
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

                            self.get_variance_of(
                                inner_term,
                                next_variance,
                                locations,
                            )
                        } else if kind == SymbolKind::Function {
                            Ok(parent_variance.xfrom(Variance::Invariant))
                        } else {
                            panic!(
                                "expected adt or function but found {kind:?}",
                            );
                        }
                    }

                    (
                        r#type::SubTypeLocation::Reference,
                        Type::Reference(reference),
                    ) => {
                        let current_variance =
                            parent_variance.xfrom(match reference.qualifier {
                                r#type::Qualifier::Immutable => {
                                    Variance::Covariant
                                }

                                r#type::Qualifier::Mutable => {
                                    Variance::Invariant
                                }
                            });

                        self.get_variance_of(
                            &reference.pointee,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        r#type::SubTypeLocation::Pointer,
                        Type::Pointer(pointer),
                    ) => {
                        let current_variance =
                            parent_variance.xfrom(Variance::Bivariant);

                        self.get_variance_of(
                            &pointer.pointee,
                            current_variance,
                            locations,
                        )
                    }

                    (r#type::SubTypeLocation::Array, Type::Array(array)) => {
                        let current_variance =
                            parent_variance.xfrom(Variance::Covariant);

                        self.get_variance_of(
                            &array.r#type,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        r#type::SubTypeLocation::Phantom,
                        Type::Phantom(phantom),
                    ) => {
                        let current_variance =
                            parent_variance.xfrom(Variance::Covariant);

                        self.get_variance_of(
                            &phantom.0,
                            current_variance,
                            locations,
                        )
                    }

                    (
                        location
                        @ r#type::SubTypeLocation::FunctionSignature(_),
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

                        self.get_variance_of(
                            &sub_term,
                            parent_variance.xfrom(current_variance),
                            locations,
                        )
                    }

                    (
                        location @ r#type::SubTypeLocation::Tuple(_),
                        tuple @ Type::Tuple(_),
                    ) => {
                        let sub_term = location.get_sub_term(tuple).unwrap();

                        let current_variance =
                            parent_variance.xfrom(Variance::Covariant);

                        self.get_variance_of(
                            &sub_term,
                            current_variance,
                            locations,
                        )
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

                        self.get_variance_of(
                            inner_term,
                            current_variance,
                            locations,
                        )
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

                        self.get_variance_of(
                            inner_term,
                            current_variance,
                            locations,
                        )
                    }

                    _ => panic!("invalid location"),
                }
            }

            TermLocation::Constant(_) => panic!("no variance for constant"),
        }
    }
}
