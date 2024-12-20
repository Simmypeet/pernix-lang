use std::collections::HashSet;

use pernixc_base::source_file::Span;

use crate::{
    error::{OverflowOperation, TypeSystemOverflow},
    ir::{self, address::Address, representation::Values},
    symbol::{table, GlobalID},
    type_system::{
        environment::Environment as TyEnvironment,
        model::Model,
        normalizer::Normalizer,
        observer::Observer,
        term::{lifetime::Lifetime, r#type::Qualifier},
        visitor::RecursiveIterator,
    },
};

mod check;
mod environment;
mod liveness;
mod local_region_generator;
mod transform;
mod transitive_closure;

/// Gets all the lifetimes included in the given address.
pub fn get_lifetimes_in_address<M: Model, S: table::State>(
    mut address: &Address<M>,
    span: &Span,
    values: &Values<M>,
    current_site: GlobalID,
    ty_environment: &TyEnvironment<
        M,
        S,
        impl Normalizer<M, S>,
        impl Observer<M, S>,
    >,
) -> Result<HashSet<Lifetime<M>>, TypeSystemOverflow<ir::Model>> {
    let mut lifetimes = HashSet::new();
    let address_ty = values
        .type_of_address(address, current_site, ty_environment)
        .map_err(|x| TypeSystemOverflow::<ir::Model> {
            operation: OverflowOperation::TypeOf,
            overflow_span: span.clone(),
            overflow_error: x.into_overflow().unwrap(),
        })?
        .result;

    lifetimes.extend(
        RecursiveIterator::new(&address_ty)
            .filter_map(|x| x.0.into_lifetime().ok())
            .cloned(),
    );

    loop {
        match address {
            Address::Memory(_) => break,

            Address::Field(field) => {
                address = &field.struct_address;
            }
            Address::Tuple(tuple) => {
                address = &tuple.tuple_address;
            }
            Address::Index(index) => {
                address = &index.array_address;
            }
            Address::Variant(variant) => {
                address = &variant.enum_address;
            }

            Address::Reference(reference) => {
                let pointee_ty = values
                    .type_of_address(
                        &reference.reference_address,
                        current_site,
                        ty_environment,
                    )
                    .map_err(|x| TypeSystemOverflow::<ir::Model> {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: span.clone(),
                        overflow_error: x.into_overflow().unwrap(),
                    })?
                    .result;
                let pointee_reference_ty = pointee_ty.as_reference().unwrap();

                lifetimes.insert(pointee_reference_ty.lifetime.clone());

                if pointee_reference_ty.qualifier == Qualifier::Immutable {
                    break;
                }

                address = &reference.reference_address;
            }
        }
    }

    Ok(lifetimes)
}

#[cfg(test)]
mod test;
