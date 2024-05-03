//! Contains the algorithm to get all the sub-values of an address or register.

use std::collections::HashSet;

use super::Representation;
use crate::{
    arena::ID,
    ir::{
        address::Address,
        value::{
            register::{Register, TupleElement},
            Value,
        },
    },
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum GetSubValuesError {
    #[error("contains an invalid register ID: {0:?}")]
    InvalidRegisterID(ID<Register>),
}

impl<T> Representation<T> {
    /// Gets all the sub-values of the given address recursively.
    ///
    /// # Errors
    ///
    /// See [`GetSubValuesError`] for the possible errors that can occur.
    pub fn sub_values_of_address<'a>(
        &'a self,
        address: &'a Address,
    ) -> Result<HashSet<&'a Value>, GetSubValuesError> {
        match address {
            Address::Parameter(_) | Address::Alloca(_) => Ok(HashSet::new()),

            Address::Field(field) => {
                self.sub_values_of_address(&field.struct_address)
            }

            Address::Tuple(field) => {
                self.sub_values_of_address(&field.tuple_address)
            }

            Address::Value(value) => {
                let mut result = HashSet::new();
                result.insert(value);

                if let Value::Register(register) = value {
                    result.extend(self.sub_values_of_register(*register)?);
                }

                Ok(result)
            }
        }
    }

    /// Gets all the sub-values of the given register recursively.
    ///
    /// # Errors
    ///
    /// See [`GetSubValuesError`] for the possible errors that can occur.
    pub fn sub_values_of_register(
        &self,
        register: ID<Register>,
    ) -> Result<HashSet<&Value>, GetSubValuesError> {
        let register = self
            .registers
            .get(register)
            .ok_or(GetSubValuesError::InvalidRegisterID(register))?;

        match register {
            Register::Tuple(tuple) => {
                let values = tuple
                    .elements
                    .iter()
                    .map(TupleElement::as_value)
                    .collect::<HashSet<_>>();
                let mut result = values.clone();

                for value in values {
                    if let Value::Register(register) = value {
                        result.extend(self.sub_values_of_register(*register)?);
                    }
                }

                Ok(result)
            }
            Register::Load(load) => self.sub_values_of_address(&load.address),
            Register::ReferenceOf(ref_of) => {
                self.sub_values_of_address(&ref_of.address)
            }
        }
    }
}
