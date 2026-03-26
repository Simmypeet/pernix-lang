//! Contains the definition of the [`Variant`] register.
use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    variance::Variance, variant::get_variant_associated_type,
};
use pernixc_target::Global;
use pernixc_term::{instantiation::Instantiation, r#type::Type};
use pernixc_type_system::{
    OverflowError, UnrecoverableError, constraints::Constraints,
};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    resolution_visitor::{
        Abort, MutableResolutionVisitor, Resolution, ResolutionMut,
        ResolutionVisitor,
    },
    value::{
        TypeOf, Value,
        register::{Register, subtype::Subtype},
    },
};

macro_rules! visit_variant {
    (
        $variant:expr,
        $visitor:expr,
        $span:expr,
        $associated_value:expr,
        $literal_accessor:ident,
        $accept_method:ident,
        $visit_method:ident,
        $resolution_ctor:ident,
        $symbol_expr:expr
    ) => {{
        if let Some(value) = $associated_value
            && let Some(literal) = value.$literal_accessor()
        {
            literal.$accept_method($visitor).await?;
        }

        $visitor
            .$visit_method($resolution_ctor::Variant($symbol_expr), $span)
            .await?;
        Ok(())
    }};
}

/// Represents a variant value.
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
pub struct Variant {
    symbol: pernixc_resolution::qualified_identifier::Variant,
    associated_value: Option<Value>,
}

impl<'a> From<&'a Variant> for Resolution<'a> {
    fn from(val: &'a Variant) -> Self { Resolution::Variant(&val.symbol) }
}

impl Variant {
    #[must_use]
    pub const fn new(
        symbol: pernixc_resolution::qualified_identifier::Variant,
        associated_value: Option<Value>,
    ) -> Self {
        Self { symbol, associated_value }
    }

    pub async fn parent_enum_id(
        &self,
        engine: &TrackedEngine,
    ) -> Global<pernixc_symbol::ID> {
        self.symbol.parent_enum_id(engine).await
    }

    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Instantiation {
        self.symbol.create_instantiation(engine).await
    }

    #[must_use]
    pub const fn variant_id(&self) -> Global<pernixc_symbol::ID> {
        self.symbol.variant_id()
    }

    #[must_use]
    pub const fn associated_value(&self) -> Option<&Value> {
        self.associated_value.as_ref()
    }

    /// Checks subtyping for this variant, ensuring the associated value is an
    /// expected subtype.
    pub async fn subtypes<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        values: &Values,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<Subtype, OverflowError> {
        if let Some(associated_value) = self.associated_value() {
            let engine = environment.type_environment.tracked_engine();
            let variant_sym =
                engine.get_variant_associated_type(self.variant_id()).await;

            let mut associated_type = (*variant_sym.unwrap()).clone();

            let instantiation = self.create_instantiation(engine).await;
            instantiation.instantiate(&mut associated_type);

            let mut constraints = Constraints::default();

            let val_succeeded =
                values.type_of(associated_value, environment).await?;
            constraints.extend(val_succeeded.constraints.into_iter());
            let val_type = val_succeeded.result;

            let result = environment
                .type_environment
                .subtypes(
                    associated_type.clone(),
                    val_type.clone(),
                    Variance::Covariant,
                )
                .await?;

            let Some(succeeded) = result else {
                return Ok(Subtype::Incompatible {
                    found_type: val_type,
                    expected_type: associated_type,
                });
            };

            if !succeeded.result.forall_lifetime_errors.is_empty() {
                return Ok(Subtype::ForallLifetimeError {
                    found_type: val_type,
                    expected_type: associated_type,
                });
            }

            constraints.extend(succeeded.constraints.iter().cloned());

            Ok(Subtype::Succeeded(constraints))
        } else {
            Ok(Subtype::Succeeded(Constraints::default()))
        }
    }

    /// Performs well-formedness checking on this variant construction.
    ///
    /// This validates that the variant symbol and all its generic arguments
    /// satisfy well-formedness constraints.
    pub async fn wf_check<N, D>(
        &self,
        environment: &crate::value::Environment<'_, N>,
        span: pernixc_lexical::tree::RelativeSpan,
        handler: &dyn pernixc_handler::Handler<D>,
    ) -> Result<Constraints, UnrecoverableError>
    where
        N: pernixc_type_system::normalizer::Normalizer,
        D: pernixc_diagnostic::Report
            + From<pernixc_type_system::diagnostic::Diagnostic>,
    {
        use crate::resolution_visitor::IntoResolutionWithSpan;

        let mut wf_check_visitor =
            crate::wf_check::WfCheckVisitor::new(environment, handler);

        // Recursively check all symbols within the variant
        let visitable = IntoResolutionWithSpan::new(self, span);
        wf_check_visitor.check_resolution(&visitable).await?;

        Ok(wf_check_visitor.into_constraints())
    }
}

impl Variant {
    /// Returns the list of registers that are used in the variant.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.associated_value
            .as_ref()
            .map(|x| x.as_register().copied())
            .into_iter()
            .flatten()
            .collect()
    }
}

impl crate::visitor::Element for Variant {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        if let Some(value) = &self.associated_value {
            visitor.visit_value(std::borrow::Cow::Borrowed(value));
        }
    }
}

pub(super) async fn transform_variant<T: MutableResolutionVisitor>(
    variant: &mut Variant,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_variant!(
        variant,
        visitor,
        span,
        variant.associated_value.as_mut(),
        as_literal_mut,
        accept_mut,
        visit_mut,
        ResolutionMut,
        &mut variant.symbol
    )
}

pub(super) async fn inspect_variant<T: ResolutionVisitor>(
    variant: &Variant,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_variant!(
        variant,
        visitor,
        span,
        variant.associated_value.as_ref(),
        as_literal,
        accept,
        visit,
        Resolution,
        &variant.symbol
    )
}

impl TypeOf<&Variant> for Values {
    async fn type_of<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        value: &Variant,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, OverflowError> {
        Ok(environment
            .type_environment
            .simplify(
                value
                    .symbol
                    .create_enum_type(environment.tracked_engine())
                    .await,
            )
            .await?
            .deref()
            .clone())
    }
}
