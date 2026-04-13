//! Contains the definition of the [`FunctionCall`] register.
use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_hash::FxHashMap;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    effect_annotation::get_effect_annotation, parameter::get_parameters,
    return_type::get_return_type, trait_ref::create_instantiation,
    variance::Variance,
};
use pernixc_symbol::instance_associated::{
    AssociatedKind, get_instance_associated_equivalent,
};
use pernixc_target::Global;
use pernixc_term::{
    effect,
    error::MakeError,
    generic_arguments::{AssociatedSymbol, Symbol},
    generic_parameters::get_generic_parameters,
    instance::InstanceAssociated,
    instantiation::Instantiation,
    lifetime::{ElidedLifetime, ElidedLifetimeID, Forall, Lifetime},
    r#type::Type,
    visitor::RecursiveIterator,
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

impl Callee {
    /// If the callee is an instance-associated function, return all of the
    /// "forall" lifetimes that appear in the instance's generic parameters.
    pub async fn get_instance_associated_forall_lifetimes(
        &self,
        mapped_lt: &Lifetime,
        engine: &TrackedEngine,
    ) -> Option<FxHashMap<Forall, Lifetime>> {
        let Self::InstanceAssociatedFunction(inst) = self else {
            return None;
        };

        let inst = inst.create_instantiation(engine).await?;
        let mut forall_lifetimes = FxHashMap::default();

        for term in inst.iter_all_term() {
            match term {
                // lifetime shouldn't appear in constant
                pernixc_term::TermRef::Constant(_) => {}

                pernixc_term::TermRef::Lifetime(lifetime) => {
                    if let Lifetime::Forall(forall) = lifetime {
                        forall_lifetimes
                            .insert(forall.clone(), mapped_lt.clone());
                    }
                }

                pernixc_term::TermRef::Type(ty) => {
                    for forall in RecursiveIterator::new(ty).filter_map(|x| {
                        x.0.as_lifetime().and_then(|x| x.as_forall())
                    }) {
                        forall_lifetimes
                            .insert(forall.clone(), mapped_lt.clone());
                    }
                }

                pernixc_term::TermRef::Instance(instance) => {
                    for forall in
                        RecursiveIterator::new(instance).filter_map(|x| {
                            x.0.as_lifetime().and_then(|x| x.as_forall())
                        })
                    {
                        forall_lifetimes
                            .insert(forall.clone(), mapped_lt.clone());
                    }
                }
            }
        }

        Some(forall_lifetimes)
    }
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
    pub const fn get_symbol_id(&self) -> Global<pernixc_symbol::SymbolID> {
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

    elided_lifetimes_instantiation: FxHashMap<ID<ElidedLifetime>, Lifetime>,
    forall_lifetimes_instantiation: FxHashMap<Forall, Lifetime>,

    effect_arguments: FxHashMap<ID<effect::Unit>, EffectHandlerArgument>,
}

#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode, StableHash)]
pub struct Monomorphization {
    callee_symbol_id: Global<pernixc_symbol::SymbolID>,
    instantiation: Instantiation,
}

impl Monomorphization {
    #[must_use]
    pub fn destruct(self) -> (Global<pernixc_symbol::SymbolID>, Instantiation) {
        (self.callee_symbol_id, self.instantiation)
    }
}

impl FunctionCall {
    #[must_use]
    pub const fn new(
        callee: Callee,
        arguments: Vec<Value>,
        elided_lifetimes_instantiation: FxHashMap<ID<ElidedLifetime>, Lifetime>,
        forall_lifetimes_instantiation: FxHashMap<Forall, Lifetime>,
        effect_arguments: FxHashMap<ID<effect::Unit>, EffectHandlerArgument>,
    ) -> Self {
        Self {
            callee,
            arguments,
            elided_lifetimes_instantiation,
            forall_lifetimes_instantiation,
            effect_arguments,
        }
    }

    /// Returns the list of registers that are used in the function call.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.arguments.iter().filter_map(|x| x.as_register().copied()).collect()
    }

    #[must_use]
    pub const fn callee_symbol_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.callee.get_symbol_id()
    }

    #[must_use]
    pub async fn create_monomorphization(
        &self,
        engine: &TrackedEngine,
        parent_monomorphization: &Instantiation,
    ) -> Monomorphization {
        match &self.callee {
            Callee::Function(symbol) => {
                let id = symbol.id();
                let mut inst = symbol.create_instantiation(engine).await;
                inst.instantiate_values(parent_monomorphization);

                Monomorphization { callee_symbol_id: id, instantiation: inst }
            }

            Callee::AssociatedFunction(associated_symbol) => {
                let id = associated_symbol.id();
                let mut inst =
                    associated_symbol.create_instantiation(engine).await;
                inst.instantiate_values(parent_monomorphization);

                Monomorphization { callee_symbol_id: id, instantiation: inst }
            }

            Callee::InstanceAssociatedFunction(instance_associated) => {
                let mut instance_associated = instance_associated.clone();
                instance_associated.instantiate(parent_monomorphization);

                let instance_symbol = instance_associated
                    .instance()
                    .as_symbol()
                    .expect("should've been monomorphized to a symbol vairant");

                let mut instantiation =
                    instance_symbol.create_instantiation(engine).await;

                let equiv_instance_associated = engine
                    .get_instance_associated_equivalent(
                        instance_symbol.id(),
                        instance_associated.trait_associated_symbol_id(),
                        AssociatedKind::Function,
                    )
                    .await
                    .expect("should've found the equivalent");

                {
                    let instance_associated_instance_generic_parameters =
                        engine
                            .get_generic_parameters(equiv_instance_associated)
                            .await;

                    instantiation.append_from_generic_arguments(
                        instance_associated
                            .associated_instance_generic_arguments(),
                        equiv_instance_associated,
                        &instance_associated_instance_generic_parameters,
                    );
                }

                instantiation.instantiate_values(parent_monomorphization);

                Monomorphization {
                    callee_symbol_id: equiv_instance_associated,
                    instantiation,
                }
            }
        }
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

        for (forall, lifetime) in &self.forall_lifetimes_instantiation {
            // NOTE: we need to compose the lifetime mapping to the existing
            // instantiation
            instantiation.insert_lifetime_mapping_compose(
                Lifetime::Forall(forall.clone()),
                lifetime.clone(),
            );
        }

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
        environment: &crate::value::ValueEnvironment<'_, N>,
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

        // Check effect handler subtyping
        let called_capabilities =
            engine.get_effect_annotation(self.callee_symbol_id()).await;

        let current_capabilities =
            engine.get_effect_annotation(environment.current_site()).await;

        for (required_id, argument) in self.effect_arguments() {
            let mut required_capability =
                called_capabilities[required_id].clone();

            // instantiate the generic arguments of the required capability
            required_capability.instantiate(&inst);

            match argument {
                EffectHandlerArgument::FromEffectAnnotation(
                    capability_unit,
                ) => {
                    // no need to instantiate, as the capability unit is
                    // already instantiated from the call site
                    let available_capability =
                        &current_capabilities[*capability_unit];

                    let subtypable = environment
                        .type_environment
                        .subtypes_generic_arguments(
                            required_capability.generic_arguments(),
                            available_capability.generic_arguments(),
                        )
                        .await?;

                    let Some(subtypable) = subtypable else {
                        // If the generic arguments are not subtypes, we should
                        // return an incompatible error. However, we don't have
                        // a good way to represent this in the current error
                        // structure. For now, we'll skip this case as the
                        // error should have been reported during semantic
                        // analysis.
                        continue;
                    };

                    assert!(
                        subtypable.result.forall_lifetime_errors.is_empty()
                    );

                    constraints.extend(subtypable.constraints.iter().cloned());
                }

                #[allow(clippy::match_same_arms)]
                EffectHandlerArgument::FromEffectHandler(_) => {
                    // TODO: extract the lifetime constraints
                }

                EffectHandlerArgument::Unhandled => {
                    // error should've been reported
                }
            }
        }

        Ok(Subtype::Succeeded(constraints))
    }

    /// Performs well-formedness checking on this function call.
    ///
    /// This validates that the function callee and all its generic arguments
    /// satisfy well-formedness constraints.
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
    let res = match &mut function_call.callee {
        Callee::Function(symbol) => ResolutionMut::Symbol(symbol),
        Callee::AssociatedFunction(associated_symbol) => {
            ResolutionMut::AssociatedSymbol(associated_symbol)
        }
        Callee::InstanceAssociatedFunction(instance_associated) => {
            ResolutionMut::InstanceAssociated(instance_associated)
        }
    };

    visitor.visit_mut(res, span).await?;

    for argument in &mut function_call.arguments {
        if let Some(literal) = argument.as_literal_mut() {
            literal.accept_mut(visitor).await?;
        }
    }

    for lt in function_call
        .elided_lifetimes_instantiation
        .values_mut()
        .chain(function_call.forall_lifetimes_instantiation.values_mut())
    {
        visitor.visit_mut(ResolutionMut::Lifetime(lt), span).await?;
    }

    Ok(())
}

#[allow(clippy::too_many_lines)]
pub(super) async fn inspect_function_call<T: ResolutionVisitor>(
    function_call: &FunctionCall,
    visitor: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) -> Result<(), Abort> {
    let res = match &function_call.callee {
        Callee::Function(symbol) => Resolution::Symbol(symbol),
        Callee::AssociatedFunction(associated_symbol) => {
            Resolution::AssociatedSymbol(associated_symbol)
        }
        Callee::InstanceAssociatedFunction(instance_associated) => {
            Resolution::InstanceAssociated(instance_associated)
        }
    };

    visitor.visit(res, span).await?;

    for argument in &function_call.arguments {
        if let Some(literal) = argument.as_literal() {
            literal.accept(visitor).await?;
        }
    }

    for lt in function_call
        .elided_lifetimes_instantiation
        .values()
        .chain(function_call.forall_lifetimes_instantiation.values())
    {
        visitor.visit(Resolution::Lifetime(lt), span).await?;
    }

    Ok(())
}

impl TypeOf<&FunctionCall> for Values {
    async fn type_of<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        value: &FunctionCall,
        environment: &crate::value::ValueEnvironment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, OverflowError> {
        let Some(instantiation) =
            value.create_instantiation(environment.tracked_engine()).await
        else {
            return Ok(Succeeded::new(Type::new_error()));
        };

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
