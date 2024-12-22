use std::collections::{HashMap, HashSet};

use crate::{
    arena::ID,
    error::{OverflowOperation, TypeSystemOverflow},
    ir::{
        self,
        representation::{
            borrow::{Model as BorrowModel, Region},
            Values,
        },
        value::register::Register,
    },
    symbol::{table, GlobalID},
    type_system::{
        environment::Environment, normalizer::Normalizer, observer::Observer,
        term::r#type::Type, visitor::RecursiveIterator,
    },
};

/// The cache for `type_of` register operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegisterType {
    pub r#type: Type<BorrowModel>,
    pub regions: HashSet<Region>,
}

/// A map of register id to its type and regions.
#[derive(
    Debug, Clone, PartialEq, Eq, derive_more::Deref, derive_more::DerefMut,
)]
pub struct RegisterTypes(HashMap<ID<Register<BorrowModel>>, RegisterType>);

impl RegisterTypes {
    /// Create a new empty register types cache.
    pub fn new<S: table::State>(
        values: &Values<BorrowModel>,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<Self, TypeSystemOverflow<ir::Model>> {
        Ok(Self(
            values
                .registers
                .iter()
                .map(|(id, x)| {
                    Ok((id, {
                        let ty = values
                            .type_of_register(id, current_site, environment)
                            .map_err(|overflow_error| TypeSystemOverflow::<
                                ir::Model,
                            > {
                                operation: OverflowOperation::TypeOf,
                                overflow_span: x.span.clone(),
                                overflow_error: overflow_error
                                    .into_overflow()
                                    .unwrap(),
                            })?;

                        RegisterType {
                            regions: RecursiveIterator::new(&ty.result)
                                .filter_map(|x| x.0.into_lifetime().ok())
                                .filter_map(|x| x.clone().try_into().ok())
                                .collect(),
                            r#type: ty.result,
                        }
                    }))
                })
                .collect::<Result<_, TypeSystemOverflow<ir::Model>>>()?,
        ))
    }
}
