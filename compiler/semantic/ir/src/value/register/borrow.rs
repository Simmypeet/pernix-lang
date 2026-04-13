//! Contains the definition of the [`Borrow`] register.

use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::{
    lifetime::Lifetime,
    r#type::{Qualifier, Type},
};
use pernixc_type_system::{OverflowError, Succeeded, normalizer::Normalizer};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    address::Address,
    resolution_visitor::{
        self, Abort, Resolution, ResolutionMut, ResolutionVisitor,
    },
    value::{ValueEnvironment, TypeOf},
};

macro_rules! visit_borrow_contents {
    (
        $borrow:expr,
        $visitor:expr,
        $span:expr,
        $address_method:ident,
        $visit_method:ident,
        $resolution_ctor:ident,
        $lifetime:expr
    ) => {{
        $borrow.address.$address_method($visitor).await?;

        $visitor
            .$visit_method($resolution_ctor::Lifetime($lifetime), $span)
            .await?;
        Ok(())
    }};
}

/// Obtains a reference at the given address.
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
pub struct Borrow {
    /// The address to the value.
    pub address: Address,

    /// The qualfier of the reference.
    pub qualifier: Qualifier,

    /// The lifetime introduces by the reference of operation.
    pub lifetime: Lifetime,
}

impl crate::visitor::Element for Borrow {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        visitor.visit_address(std::borrow::Cow::Borrowed(&self.address));
    }
}

pub(super) async fn transform_borrow<
    T: resolution_visitor::MutableResolutionVisitor,
>(
    borrow: &mut Borrow,
    visitor: &mut T,
    span: RelativeSpan,
) -> Result<(), Abort> {
    visit_borrow_contents!(
        borrow,
        visitor,
        span,
        accept_mut,
        visit_mut,
        ResolutionMut,
        &mut borrow.lifetime
    )
}

pub(super) async fn inspect_borrow<T: ResolutionVisitor>(
    borrow: &Borrow,
    visitor: &mut T,
    span: RelativeSpan,
) -> Result<(), Abort> {
    visit_borrow_contents!(
        borrow,
        visitor,
        span,
        accept,
        visit,
        Resolution,
        &borrow.lifetime
    )
}

impl TypeOf<&Borrow> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        reference_of: &Borrow,
        environment: &ValueEnvironment<'_, N>,
    ) -> Result<Succeeded<Type>, OverflowError> {
        self.type_of(&reference_of.address, environment).await.map(|x| {
            x.map(|x| {
                Type::Reference(pernixc_term::r#type::Reference {
                    qualifier: reference_of.qualifier,
                    lifetime: reference_of.lifetime.clone(),
                    pointee: Box::new(x),
                })
            })
        })
    }
}
