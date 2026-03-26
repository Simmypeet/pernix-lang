//! Contains the definition of the [`Struct`] register.
use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_hash::FxHashMap;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    fields::{Field, get_fields},
    variance::Variance,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::Symbol, instantiation::Instantiation, r#type::Type,
};
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

macro_rules! visit_struct {
    (
        $st:expr,
        $visitor:expr,
        $span:expr,
        $values_method:ident,
        $literal_accessor:ident,
        $accept_method:ident,
        $visit_method:ident,
        $resolution_ctor:ident,
        $symbol_expr:expr
    ) => {{
        for value in $st.initializers_by_field_id.$values_method() {
            if let Some(literal) = value.$literal_accessor() {
                literal.$accept_method($visitor).await?;
            }
        }

        $visitor
            .$visit_method($resolution_ctor::Symbol($symbol_expr), $span)
            .await?;
        Ok(())
    }};
}

/// Represents a struct value.
#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode, StableHash)]
pub struct Struct {
    symbol: Symbol,
    initializers_by_field_id: FxHashMap<ID<Field>, Value>,
}

impl<'a> From<&'a Struct> for Resolution<'a> {
    fn from(val: &'a Struct) -> Self { Resolution::Symbol(&val.symbol) }
}

impl Struct {
    #[must_use]
    pub const fn new(
        symbol: Symbol,
        initializers_by_field_id: FxHashMap<ID<Field>, Value>,
    ) -> Self {
        Self { symbol, initializers_by_field_id }
    }

    /// Returns the list of registers that are used in the struct.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.initializers_by_field_id
            .values()
            .filter_map(|x| x.as_register().copied())
            .collect()
    }

    #[must_use]
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Instantiation {
        self.symbol.create_instantiation(engine).await
    }

    #[must_use]
    pub const fn struct_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.symbol.id()
    }

    #[must_use]
    pub fn get_initializer_by_field_id(&self, field_id: ID<Field>) -> &Value {
        self.initializers_by_field_id.get(&field_id).unwrap()
    }

    /// Checks subtyping for this struct, ensuring all initializers are expected
    /// subtypes.
    pub async fn subtypes<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        values: &Values,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<Subtype, OverflowError> {
        let mut constraints = Constraints::default();

        let engine = environment.type_environment.tracked_engine();

        let instantiation = self.create_instantiation(engine).await;
        let fields = engine.get_fields(self.struct_id()).await;

        for field_id in fields.field_declaration_order.iter().copied() {
            let mut field_ty =
                fields.fields.get(field_id).unwrap().r#type.clone();
            instantiation.instantiate(&mut field_ty);

            let initializer = self.get_initializer_by_field_id(field_id);

            let val_succeeded =
                values.type_of(initializer, environment).await?;
            constraints.extend(val_succeeded.constraints.into_iter());
            let val_type = val_succeeded.result;

            let result = environment
                .type_environment
                .subtypes(
                    field_ty.clone(),
                    val_type.clone(),
                    Variance::Covariant,
                )
                .await?;

            let Some(succeeded) = result else {
                return Ok(Subtype::Incompatible {
                    found_type: val_type,
                    expected_type: field_ty,
                });
            };

            if !succeeded.result.forall_lifetime_errors.is_empty() {
                return Ok(Subtype::ForallLifetimeError {
                    found_type: val_type,
                    expected_type: field_ty,
                });
            }

            constraints.extend(succeeded.constraints.iter().cloned());
        }

        Ok(Subtype::Succeeded(constraints))
    }

    /// Performs well-formedness checking on this struct construction.
    ///
    /// This validates that the struct symbol and all its generic arguments
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

        // Recursively check all symbols within the struct
        let visitable = IntoResolutionWithSpan::new(self, span);
        wf_check_visitor.check_resolution(&visitable).await?;

        Ok(wf_check_visitor.into_constraints())
    }
}

impl crate::visitor::Element for Struct {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for value in self.initializers_by_field_id.values() {
            visitor.visit_value(std::borrow::Cow::Borrowed(value));
        }
    }
}

pub(super) async fn transform_struct<T: MutableResolutionVisitor>(
    st: &mut Struct,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_struct!(
        st,
        visitor,
        span,
        values_mut,
        as_literal_mut,
        accept_mut,
        visit_mut,
        ResolutionMut,
        &mut st.symbol
    )
}

pub(super) async fn inspect_struct<T: ResolutionVisitor>(
    st: &Struct,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_struct!(
        st, visitor, span, values, as_literal, accept, visit, Resolution,
        &st.symbol
    )
}

impl TypeOf<&Struct> for Values {
    async fn type_of<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        value: &Struct,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, OverflowError> {
        Ok(environment
            .type_environment
            .simplify(Type::Symbol(value.symbol.clone()))
            .await?
            .deref()
            .clone())
    }
}
