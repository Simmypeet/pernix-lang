use std::collections::HashSet;

use cache::RegisterTypes;
use pernixc_base::source_file::Span;
use transform::transform_to_borrow_model;

use crate::{
    error::{OverflowOperation, TypeSystemOverflow},
    ir::{
        self,
        address::Address,
        representation::{
            binding::HandlerWrapper,
            borrow::{Model as BorrowModel, Region},
            Representation, Values,
        },
    },
    symbol::{table, GlobalID},
    type_system::{
        environment::Environment, normalizer::Normalizer, observer::Observer,
        term::r#type::Qualifier, visitor::RecursiveIterator, Succeeded,
    },
};

mod cache;
mod check;
mod invalidate;
mod liveness;
mod local_region_generator;
mod subset;
mod transform;

impl Values<BorrowModel> {
    /// Gets the regions that got dereferenced when using the given address
    fn get_dereferenced_regions_in_address<S: table::State>(
        &self,
        mut address: &Address<BorrowModel>,
        span: &Span,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<HashSet<Region>, TypeSystemOverflow<ir::Model>> {
        let mut regions = HashSet::new();

        loop {
            match address {
                Address::Memory(_) => break Ok(regions),

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
                    let pointee_ty = self
                        .type_of_address(
                            &reference.reference_address,
                            current_site,
                            environment,
                        )
                        .map_err(|x| TypeSystemOverflow::<ir::Model> {
                            operation: OverflowOperation::TypeOf,
                            overflow_span: span.clone(),
                            overflow_error: x.into_overflow().unwrap(),
                        })?
                        .result;

                    let pointee_reference_ty =
                        pointee_ty.into_reference().unwrap();

                    regions.extend(
                        Region::try_from(pointee_reference_ty.lifetime).ok(),
                    );

                    address = &reference.reference_address;
                }
            }
        }
    }

    /// Gets the regions that appears when using the given address
    fn get_regions_in_address<S: table::State>(
        &self,
        mut address: &Address<BorrowModel>,
        span: &Span,
        include_deref: bool,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<HashSet<Region>, TypeSystemOverflow<ir::Model>> {
        let Succeeded { result: address_ty, .. } = self
            .type_of_address(address, current_site, environment)
            .map_err(|x| TypeSystemOverflow::<ir::Model> {
                operation: OverflowOperation::TypeOf,
                overflow_span: span.clone(),
                overflow_error: x.into_overflow().unwrap(),
            })?;

        let mut regions = RecursiveIterator::new(&address_ty)
            .filter_map(|x| x.0.into_lifetime().ok())
            .filter_map(|x| Region::try_from(x.clone()).ok())
            .collect::<HashSet<_>>();

        if include_deref {
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
                        let pointee_ty = self
                            .type_of_address(
                                &reference.reference_address,
                                current_site,
                                environment,
                            )
                            .map_err(|x| TypeSystemOverflow::<ir::Model> {
                                operation: OverflowOperation::TypeOf,
                                overflow_span: span.clone(),
                                overflow_error: x.into_overflow().unwrap(),
                            })?
                            .result;

                        let pointee_reference_ty =
                            pointee_ty.into_reference().unwrap();

                        regions.extend(
                            Region::try_from(pointee_reference_ty.lifetime)
                                .ok(),
                        );

                        if pointee_reference_ty.qualifier
                            == Qualifier::Immutable
                        {
                            break;
                        }

                        address = &reference.reference_address;
                    }
                }
            }

            Ok(regions)
        } else {
            Ok(regions)
        }
    }
}

impl Representation<ir::Model> {
    pub(super) fn borrow_check<S: table::State>(
        &self,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        // NOTE: we clone the whole ir here, is there a better way to do this?
        let (ir, _) =
            transform_to_borrow_model(self.clone(), environment.table());

        let register_types =
            RegisterTypes::new(&ir, current_site, environment)?;
        let reachability = ir.control_flow_graph.reachability();

        let subset =
            subset::analyze(&ir, &register_types, current_site, environment)?;

        ir.borrow_check_internal(
            &subset,
            &register_types,
            &reachability,
            current_site,
            environment,
            handler,
        )?;

        Ok(())
    }
}

#[cfg(test)]
mod test;
