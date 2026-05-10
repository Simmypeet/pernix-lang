//! Contains the definition of the [`EffectOperationCall`] register.
use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    effect_annotation::get_effect_annotation, parameter::get_parameters,
    return_type::get_return_type, variance::Variance,
};
use pernixc_target::Global;
use pernixc_term::{self, r#type::Type};
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
        register::{
            Register, function_call::EffectHandlerArgument, subtype::Subtype,
        },
    },
};

/// Represents an effect operation call.
#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode, StableHash)]
pub struct EffectOperationCall {
    symbol: pernixc_resolution::qualified_identifier::ParentGenericSymbol,
    arguments: Vec<Value>,
    effect_arguments: EffectHandlerArgument,
}

impl<'a> From<&'a EffectOperationCall> for Resolution<'a> {
    fn from(call: &'a EffectOperationCall) -> Self {
        Resolution::ParentGenericSymbol(&call.symbol)
    }
}

impl EffectOperationCall {
    #[must_use]
    pub const fn new(
        symbol: pernixc_resolution::qualified_identifier::ParentGenericSymbol,
        arguments: Vec<Value>,
        effect_arguments: EffectHandlerArgument,
    ) -> Self {
        Self { symbol, arguments, effect_arguments }
    }

    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.arguments.iter().filter_map(|x| x.as_register().copied()).collect()
    }

    #[must_use]
    pub const fn symbol_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.symbol.symbol_id()
    }

    #[must_use]
    pub fn arguments(&self) -> &[Value] { &self.arguments }

    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_term::instantiation::Instantiation {
        self.symbol.create_instantiation(engine).await
    }

    pub async fn subtypes<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        values: &Values,
        environment: &crate::value::ValueEnvironment<'_, N>,
    ) -> Result<Subtype, OverflowError> {
        let engine = environment.type_environment.tracked_engine();
        let inst = self.create_instantiation(engine).await;
        let parameters = engine.get_parameters(self.symbol_id()).await;

        let mut constraints = Constraints::default();

        for (parameter, argument) in parameters
            .parameter_order
            .iter()
            .copied()
            .map(|x| parameters.parameters.get(x).unwrap())
            .zip(self.arguments.iter())
        {
            let mut parameter_ty = parameter.r#type.clone();
            inst.instantiate(&mut parameter_ty);

            let val_succeeded = values.type_of(argument, environment).await?;
            constraints.extend(val_succeeded.constraints.into_iter());
            let val_type = val_succeeded.result;

            let result = environment
                .type_environment
                .subtypes(
                    parameter_ty.clone(),
                    val_type.clone(),
                    Variance::Covariant,
                )
                .await?;

            let Some(succeeded) = result else {
                return Ok(Subtype::Incompatible {
                    found_type: val_type,
                    expected_type: parameter_ty,
                });
            };

            if !succeeded.result.forall_lifetime_errors.is_empty() {
                return Ok(Subtype::ForallLifetimeError {
                    found_type: val_type,
                    expected_type: parameter_ty,
                });
            }

            constraints.extend(succeeded.constraints.iter().cloned());
        }

        let current_capabilities =
            engine.get_effect_annotation(environment.current_site()).await;

        match &self.effect_arguments {
            EffectHandlerArgument::FromEffectAnnotation(capability_unit) => {
                let available_capability =
                    &current_capabilities[*capability_unit];

                let subtypable = environment
                    .type_environment
                    .subtypes_generic_arguments(
                        self.symbol.parent_generic_arguments(),
                        available_capability.generic_arguments(),
                    )
                    .await?;

                if let Some(subtypable) = subtypable {
                    assert!(
                        subtypable.result.forall_lifetime_errors.is_empty()
                    );

                    constraints.extend(subtypable.constraints.iter().cloned());
                }
            }

            #[allow(clippy::match_same_arms)]
            EffectHandlerArgument::FromEffectHandler(_) => {}

            EffectHandlerArgument::Unhandled => {}
        }

        Ok(Subtype::Succeeded(constraints))
    }

    pub async fn wf_check<N, D>(
        &self,
        environment: &crate::value::ValueEnvironment<'_, N>,
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

        let visitable = IntoResolutionWithSpan::new(self, span);
        wf_check_visitor.check_resolution(&visitable).await?;

        Ok(wf_check_visitor.into_constraints())
    }
}

impl crate::visitor::Element for EffectOperationCall {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for argument in &self.arguments {
            visitor.visit_value(std::borrow::Cow::Borrowed(argument));
        }
    }
}

pub(super) async fn transform_effect_operation_call<
    T: MutableResolutionVisitor,
>(
    effect_operation_call: &mut EffectOperationCall,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visitor
        .visit_mut(
            ResolutionMut::ParentGenericSymbol(
                &mut effect_operation_call.symbol,
            ),
            span,
        )
        .await?;

    for argument in &mut effect_operation_call.arguments {
        if let Some(literal) = argument.as_literal_mut() {
            literal.accept_mut(visitor).await?;
        }
    }

    Ok(())
}

pub(super) async fn inspect_effect_operation_call<T: ResolutionVisitor>(
    effect_operation_call: &EffectOperationCall,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visitor
        .visit(
            Resolution::ParentGenericSymbol(&effect_operation_call.symbol),
            span,
        )
        .await?;

    for argument in &effect_operation_call.arguments {
        if let Some(literal) = argument.as_literal() {
            literal.accept(visitor).await?;
        }
    }

    Ok(())
}

impl TypeOf<&EffectOperationCall> for Values {
    async fn type_of<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        value: &EffectOperationCall,
        environment: &crate::value::ValueEnvironment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, OverflowError> {
        let instantiation =
            value.create_instantiation(environment.tracked_engine()).await;

        let return_type = environment
            .tracked_engine()
            .get_return_type(value.symbol_id())
            .await
            .deref()
            .clone();

        let mut instantiated_return_type = return_type.clone();
        instantiation.instantiate(&mut instantiated_return_type);

        let simplified_type = environment
            .type_environment
            .simplify(instantiated_return_type)
            .await?;

        Ok(simplified_type.deref().clone())
    }
}
