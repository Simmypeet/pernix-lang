//! Contains the definition of the [`VariantNumber`] register.

use pernixc_symbol::member::get_members;
use pernixc_target::Global;
use pernixc_term::r#type::{Primitive, Type};
use pernixc_type_system::{OverflowError, Succeeded, normalizer::Normalizer};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    address::Address,
    resolution_visitor::{Abort, MutableResolutionVisitor, ResolutionVisitor},
    value::{Environment, TypeOf},
};

macro_rules! visit_variant_number_address {
    ($variant_number:expr, $visitor:expr, $accept_method:ident) => {{
        $variant_number.address.$accept_method($visitor).await?;
        Ok(())
    }};
}

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
    Encode,
    Decode,
    StableHash,
)]
pub struct VariantNumber {
    /// Address to the num to get the variant number of.
    pub address: Address,

    /// The enum ID of the enum.
    pub enum_id: Global<pernixc_symbol::SymbolID>,
}

impl crate::visitor::Element for VariantNumber {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        visitor.visit_address(std::borrow::Cow::Borrowed(&self.address));
    }
}

pub(super) async fn transform_variant_number<T: MutableResolutionVisitor>(
    variant_number: &mut VariantNumber,
    visitor: &mut T,
) -> Result<(), Abort> {
    visit_variant_number_address!(variant_number, visitor, accept_mut)
}

pub(super) async fn inspect_variant_number<T: ResolutionVisitor>(
    variant_number: &VariantNumber,
    visitor: &mut T,
) -> Result<(), Abort> {
    visit_variant_number_address!(variant_number, visitor, accept)
}

impl TypeOf<&VariantNumber> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        variant_number: &VariantNumber,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, OverflowError> {
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
