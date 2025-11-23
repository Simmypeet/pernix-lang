//! Contains the definition of the [`VariantNumber`] register.

use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::member::get_members;
use pernixc_target::Global;
use pernixc_term::r#type::{Primitive, Type};
use pernixc_type_system::{Succeeded, normalizer::Normalizer};

use crate::{
    Values,
    address::Address,
    transform::Transformer,
    value::{Environment, TypeOf},
};

/// Returns the variant number of the given address to the enum.
///
/// The variant number is supposed to be a unique identifier specifying which
/// variant is active in the enum. The number should correspond to the
/// declration order of it in the enum.
///
/// This is primarily used in the pattern matching.
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
pub struct VariantNumber {
    /// Address to the num to get the variant number of.
    pub address: Address,

    /// The enum ID of the enum.
    pub enum_id: Global<pernixc_symbol::ID>,
}

impl crate::visitor::Element for VariantNumber {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        visitor.visit_address(std::borrow::Cow::Borrowed(&self.address));
    }
}

pub(super) async fn transform_variant_number<T: Transformer<Type>>(
    variant_number: &mut VariantNumber,
    transformer: &mut T,
    _span: Option<pernixc_lexical::tree::RelativeSpan>,
) -> Result<(), CyclicError> {
    variant_number.address.transform(transformer).await
}

impl TypeOf<&VariantNumber> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        variant_number: &VariantNumber,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, pernixc_type_system::Error> {
        let mut answer = Primitive::Bool;
        let members = environment
            .tracked_engine()
            .get_members(variant_number.enum_id)
            .await;

        let variant_count =
            members.member_ids_by_name.len() + members.unnameds.iter().len();

        let int_capacity = |a: Primitive| match a {
            Primitive::Bool => 2u64,
            Primitive::Uint8 => 2u64.pow(8),
            Primitive::Uint16 => 2u64.pow(16),
            Primitive::Uint32 => 2u64.pow(32),
            Primitive::Uint64 => 2u64.pow(64),
            _ => unreachable!(),
        };

        let next = |a: Primitive| match a {
            Primitive::Bool => Primitive::Uint8,
            Primitive::Uint8 => Primitive::Uint16,
            Primitive::Uint16 => Primitive::Uint32,
            Primitive::Uint32 => Primitive::Uint64,
            _ => unreachable!(),
        };

        while int_capacity(answer) < variant_count as u64 {
            answer = next(answer);
        }

        Ok(Succeeded::new(Type::Primitive(answer)))
    }
}
