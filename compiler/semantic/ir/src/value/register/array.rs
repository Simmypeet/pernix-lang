//! Contains the definition of the [`Array`] register.

use pernixc_arena::ID;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::{constant::Constant, r#type::Type};
use pernixc_type_system::{normalizer::Normalizer, Error, Succeeded};

use crate::{
    transform::{Transformer, TypeTermSource},
    value::{register::Register, Environment, TypeOf, Value},
    Values,
};

/// Represents an array of values.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Array {
    /// The elements of the array.
    pub elements: Vec<Value>,

    /// The type of the element in the array.
    ///
    /// The type must be declared separately as the element values can have
    /// different lifetime values; thus, the type of the array can't be solely
    /// determined by one of the element values.
    pub element_type: Type,
}

impl Array {
    /// Returns the list of registers that are used in the array.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.elements.iter().filter_map(|x| x.as_register().copied()).collect()
    }
}

pub(super) async fn transform_array<T: Transformer<Type>>(
    array: &mut Array,
    transformer: &mut T,
    span: Option<pernixc_lexical::tree::RelativeSpan>,
) -> Result<(), CyclicError> {
    for value in &mut array.elements {
        if let Some(literal) = value.as_literal_mut() {
            literal.transform(transformer).await?;
        }
    }

    transformer
        .transform(&mut array.element_type, TypeTermSource::Array, span)
        .await
}

impl TypeOf<&Array> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        array: &Array,
        _environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        Ok(Succeeded::new(Type::Array(pernixc_term::r#type::Array {
            r#type: Box::new(array.element_type.clone()),
            length: Constant::Primitive(
                pernixc_term::constant::Primitive::Usize(
                    array.elements.len() as u64
                ),
            ),
        })))
    }
}
