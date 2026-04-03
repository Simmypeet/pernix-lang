//! Contains the definition of the [`Load`] register.

use getset::{CopyGetters, Getters, MutGetters};
use pernixc_corelib::get_copy_marker_id;
use pernixc_term::{
    generic_arguments::GenericArguments,
    predicate::{PositiveMarker, Predicate},
    r#type::{Qualifier, Type},
};
use pernixc_type_system::{
    OverflowError, Succeeded, UnrecoverableError, constraints::Constraints,
    normalizer::Normalizer,
};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    address::Address,
    resolution_visitor::{Abort, MutableResolutionVisitor, ResolutionVisitor},
    value::{ValueEnvironment, TypeOf},
};

macro_rules! visit_load_address {
    ($load:expr, $visitor:expr, $accept_method:ident) => {{
        $load.address.$accept_method($visitor).await?;
        Ok(())
    }};
}

/// Indicates how a load is being used. This is used for improving diagnostics
/// related to use-after-move and similar errors.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub enum Purpose {
    /// A general purpose load.
    General,

    /// A load for moving the captured value into a capture structure.
    Capture,
}

/// Represents a load/read from an address in memory. (The type must be Copy)
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
    Getters,
    CopyGetters,
    MutGetters,
)]
pub struct Load {
    /// The address where the value is stored and will be read from.
    #[get = "pub"]
    #[get_mut = "pub(crate)"]
    address: Address,

    /// The purpose of this load.
    #[get_copy = "pub"]
    purpose: Purpose,
}

impl Load {
    /// Performs well-formedness checking on this tuple construction.
    ///
    /// It primarily searches for unpacking elements and checks whether their
    /// type satisfies the `tuple` bound.
    pub async fn wf_check<N, D>(
        &self,
        environment: &crate::value::ValueEnvironment<'_, N>,
        values: &crate::value::Values,
        register_span: pernixc_lexical::tree::RelativeSpan,
        handler: &dyn pernixc_handler::Handler<D>,
    ) -> Result<Constraints, UnrecoverableError>
    where
        N: pernixc_type_system::normalizer::Normalizer,
        D: pernixc_diagnostic::Report
            + From<pernixc_type_system::diagnostic::Diagnostic>,
    {
        if self.address().get_reference_qualifier()
            == Some(Qualifier::Immutable)
            || self.address().is_behind_index()
        {
            let ty = values
                .type_of(self.address(), environment)
                .await
                .map_err(|x| {
                    x.report_as_type_calculating_overflow(
                        register_span,
                        &handler,
                    )
                })?;

            let copy_marker =
                environment.tracked_engine().get_copy_marker_id().await;

            let predicate = Predicate::PositiveMarker(PositiveMarker::new(
                copy_marker,
                GenericArguments::new(
                    Vec::new(),
                    vec![ty.result.clone()],
                    Vec::new(),
                    Vec::new(),
                ),
            ));

            environment
                .type_environment
                .predicate_satisfied(predicate, &register_span, None, &handler)
                .await
        } else {
            Ok(Constraints::new())
        }
    }
}

impl Load {
    /// Creates a new general purpose load from the given address.
    #[must_use]
    pub const fn new(address: Address) -> Self {
        Self { address, purpose: Purpose::General }
    }

    /// Creates a new load from the given address with the given purpose.
    #[must_use]
    pub const fn with_purpose(address: Address, purpose: Purpose) -> Self {
        Self { address, purpose }
    }
}

impl crate::visitor::Element for Load {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        visitor.visit_address(std::borrow::Cow::Borrowed(&self.address));
    }
}

pub(super) async fn transform_load<T: MutableResolutionVisitor>(
    load: &mut Load,
    visitor: &mut T,
) -> Result<(), Abort> {
    visit_load_address!(load, visitor, accept_mut)
}

pub(super) async fn inspect_load<T: ResolutionVisitor>(
    load: &Load,
    visitor: &mut T,
) -> Result<(), Abort> {
    visit_load_address!(load, visitor, accept)
}

impl TypeOf<&Load> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        load: &Load,
        environment: &ValueEnvironment<'_, N>,
    ) -> Result<Succeeded<Type>, OverflowError> {
        self.type_of(&load.address, environment).await
    }
}
