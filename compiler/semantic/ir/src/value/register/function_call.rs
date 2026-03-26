//! Contains the definition of the [`FunctionCall`] register.
use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    parameter::get_parameters, return_type::get_return_type,
    trait_ref::create_instantiation, variance::Variance,
};
use pernixc_target::Global;
use pernixc_term::{
    effect,
    error::MakeError,
    generic_arguments::{AssociatedSymbol, Symbol},
    instance::InstanceAssociated,
    instantiation::Instantiation,
    lifetime::{ElidedLifetime, ElidedLifetimeID, Lifetime},
    r#type::Type,
};
use pernixc_type_system::{
    OverflowError, Succeeded, UnrecoverableError, constraints::Constraints,
};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    handling_scope::HandlerClauseID,
    resolution_visitor::{
        Abort, MutableResolutionVisitor, Resolution, ResolutionMut,
        ResolutionVisitor,
    },
    value::{
        TypeOf, Value,
        register::{Register, subtype::Subtype},
    },
};

macro_rules! visit_function_call {
    (
        $function_call:expr,
        $visitor:expr,
        $span:expr,
        $callee_ref:expr,
        $res_symbol_ctor:ident,
        $res_associated_ctor:ident,
        $res_instance_ctor:ident,
        $visit_method:ident,
        $arguments_iter:expr,
        $literal_accessor:ident,
        $accept_method:ident,
        $lifetimes_values:expr,
        $res_lifetime_ctor:ident
    ) => {{
        let res = match $callee_ref {
            Callee::Function(symbol) => $res_symbol_ctor::Symbol(symbol),
            Callee::AssociatedFunction(associated_symbol) => {
                $res_associated_ctor::AssociatedSymbol(associated_symbol)
            }
            Callee::InstanceAssociatedFunction(instance_associated) => {
                $res_instance_ctor::InstanceAssociated(instance_associated)
            }
        };

        $visitor.$visit_method(res, $span).await?;

        for argument in $arguments_iter {
            if let Some(literal) = argument.$literal_accessor() {
                literal.$accept_method($visitor).await?;
            }
        }

        for lt in $lifetimes_values {
            $visitor
                .$visit_method($res_lifetime_ctor::Lifetime(lt), $span)
                .await?;
        }
        Ok(())
    }};
}

/// Specifies how an effectful operation's capability arguments are supplied.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub enum EffectHandlerArgument {
    /// Uses the handler presented in the effect annotation.
    FromEffectAnnotation(ID<effect::Unit>),

    /// Uses the local effect handler defiend via `do-with` expression.
    FromEffectHandler(HandlerClauseID),

    /// The capability is unhandled, error should've been reported.
    Unhandled,
}

/// Represents the callee of a function call, which could be a normal function,
/// an ADT function, or a trait-associated function called through an instance.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Encode, Decode, StableHash,
)]
#[allow(missing_docs)]
pub enum Callee {
    Function(Symbol),
    AssociatedFunction(AssociatedSymbol),
    InstanceAssociatedFunction(InstanceAssociated),
}

impl<'a> From<&'a FunctionCall> for Resolution<'a> {
    fn from(callee: &'a FunctionCall) -> Self {
        match &callee.callee {
            Callee::Function(symbol) => Self::Symbol(symbol),
            Callee::AssociatedFunction(associated_symbol) => {
                Self::AssociatedSymbol(associated_symbol)
            }
            Callee::InstanceAssociatedFunction(instance_associated) => {
                Self::InstanceAssociated(instance_associated)
            }
        }
    }
}

impl Callee {
    /// Creates an [`Instantiation`] for this callee.
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Option<Instantiation> {
        match self {
            Self::Function(function_callee) => {
                Some(function_callee.create_instantiation(engine).await)
            }

            Self::AssociatedFunction(adt_function_callee) => {
                Some(adt_function_callee.create_instantiation(engine).await)
            }

            Self::InstanceAssociatedFunction(
                instance_associated_function_callee,
            ) => {
                instance_associated_function_callee
                    .create_instantiation(engine)
                    .await
            }
        }
    }

    /// Returns the symbol ID of this callee.
    #[must_use]
    pub const fn get_symbol_id(&self) -> Global<pernixc_symbol::ID> {
        match self {
            Self::Function(symbol) => symbol.id(),
            Self::AssociatedFunction(associated_symbol) => {
                associated_symbol.id()
            }
            Self::InstanceAssociatedFunction(instance_associated) => {
                instance_associated.trait_associated_symbol_id()
            }
        }
    }
}

/// Represents a function call.
#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode, StableHash)]
pub struct FunctionCall {
    callee: Callee,
    arguments: Vec<Value>,
    elided_lifetimes_instantiation: HashMap<ID<ElidedLifetime>, Lifetime>,
    effect_arguments: HashMap<ID<effect::Unit>, EffectHandlerArgument>,
}

impl FunctionCall {
    #[must_use]
    pub const fn new(
        callee: Callee,
        arguments: Vec<Value>,
        elided_lifetimes_instantiation: HashMap<ID<ElidedLifetime>, Lifetime>,
        effect_arguments: HashMap<ID<effect::Unit>, EffectHandlerArgument>,
    ) -> Self {
        Self {
            callee,
            arguments,
            elided_lifetimes_instantiation,
            effect_arguments,
        }
    }

    /// Returns the list of registers that are used in the function call.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.arguments.iter().filter_map(|x| x.as_register().copied()).collect()
    }

    #[must_use]
    pub const fn callee_symbol_id(&self) -> Global<pernixc_symbol::ID> {
        self.callee.get_symbol_id()
    }

    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Option<Instantiation> {
        let mut instantiation =
            self.callee.create_instantiation(engine).await?;

        instantiation.extend_lifetimes_mappings(
            self.elided_lifetimes_instantiation.iter().map(
                |(elided_lifetime_id, lifetime)| {
                    (
                        Lifetime::Elided(ElidedLifetimeID::new(
                            self.callee.get_symbol_id(),
                            *elided_lifetime_id,
                        )),
                        lifetime.clone(),
                    )
                },
            ),
        );

        Some(instantiation)
    }

    #[must_use]
    pub fn arguments(&self) -> &[Value] { &self.arguments }

    pub fn effect_arguments(
        &self,
    ) -> impl Iterator<Item = (ID<effect::Unit>, &EffectHandlerArgument)> {
        self.effect_arguments.iter().map(|x| (*x.0, x.1))
    }

    /// Checks subtyping for this function call, ensuring all arguments are
    /// expected subtypes.
    pub async fn subtypes<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        values: &Values,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<Subtype, OverflowError> {
        let engine = environment.type_environment.tracked_engine();
        let Some(inst) = self.create_instantiation(engine).await else {
            return Ok(Subtype::Succeeded(Constraints::default()));
        };
        let function_signature =
            engine.get_parameters(self.callee_symbol_id()).await;

        let mut constraints = Constraints::default();

        for (parameter, argument) in function_signature
            .parameter_order
            .iter()
            .copied()
            .map(|x| function_signature.parameters.get(x).unwrap())
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

        Ok(Subtype::Succeeded(constraints))
    }

    /// Performs well-formedness checking on this function call.
    ///
    /// This validates that the function callee and all its generic arguments
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

        // Recursively check all symbols within the function call callee
        let visitable = IntoResolutionWithSpan::new(self, span);
        wf_check_visitor.check_resolution(&visitable).await?;

        Ok(wf_check_visitor.into_constraints())
    }
}

impl crate::visitor::Element for FunctionCall {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for argument in &self.arguments {
            visitor.visit_value(std::borrow::Cow::Borrowed(argument));
        }
    }
}

#[allow(clippy::too_many_lines)]
pub(super) async fn transform_function_call<T: MutableResolutionVisitor>(
    function_call: &mut FunctionCall,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_function_call!(
        function_call,
        visitor,
        span,
        &mut function_call.callee,
        ResolutionMut,
        ResolutionMut,
        ResolutionMut,
        visit_mut,
        &mut function_call.arguments,
        as_literal_mut,
        accept_mut,
        function_call.elided_lifetimes_instantiation.values_mut(),
        ResolutionMut
    )
}

#[allow(clippy::too_many_lines)]
pub(super) async fn inspect_function_call<T: ResolutionVisitor>(
    function_call: &FunctionCall,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    visit_function_call!(
        function_call,
        visitor,
        span,
        &function_call.callee,
        Resolution,
        Resolution,
        Resolution,
        visit,
        &function_call.arguments,
        as_literal,
        accept,
        function_call.elided_lifetimes_instantiation.values(),
        Resolution
    )
}

impl TypeOf<&FunctionCall> for Values {
    async fn type_of<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        value: &FunctionCall,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, OverflowError> {
        let Some(mut instantiation) = value
            .callee
            .create_instantiation(environment.tracked_engine())
            .await
        else {
            return Ok(Succeeded::new(Type::new_error()));
        };

        instantiation.extend_lifetimes_mappings(
            value.elided_lifetimes_instantiation.iter().map(
                |(elided_lifetime_id, lifetime)| {
                    (
                        Lifetime::Elided(ElidedLifetimeID::new(
                            value.callee.get_symbol_id(),
                            *elided_lifetime_id,
                        )),
                        lifetime.clone(),
                    )
                },
            ),
        );

        let return_type = environment
            .tracked_engine()
            .get_return_type(value.callee.get_symbol_id())
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
