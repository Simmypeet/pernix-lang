//! Contains the definition of the [`FunctionCall`] register.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    return_type::get_return_type, trait_ref::create_instantiation,
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
use pernixc_type_system::{OverflowError, Succeeded};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    handling_scope::HandlerClauseID,
    transform::{ResolutionMut, Transformer},
    value::{TypeOf, Value, register::Register},
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
}

impl crate::visitor::Element for FunctionCall {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for argument in &self.arguments {
            visitor.visit_value(std::borrow::Cow::Borrowed(argument));
        }
    }
}

#[allow(clippy::too_many_lines)]
pub(super) async fn transform_function_call<T: Transformer>(
    function_call: &mut FunctionCall,
    transformer: &mut T,
    span: pernixc_lexical::tree::RelativeSpan,
) {
    let res = match &mut function_call.callee {
        Callee::Function(symbol) => ResolutionMut::Symbol(symbol),
        Callee::AssociatedFunction(associated_symbol) => {
            ResolutionMut::AssociatedSymbol(associated_symbol)
        }
        Callee::InstanceAssociatedFunction(instance_associated) => {
            ResolutionMut::InstanceAssociated(instance_associated)
        }
    };

    transformer.transform(res, span).await;

    for argument in &mut function_call.arguments {
        if let Some(literal) = argument.as_literal_mut() {
            literal.transform(transformer).await;
        }
    }

    for lt in function_call.elided_lifetimes_instantiation.values_mut() {
        transformer.transform(ResolutionMut::Lifetime(lt), span).await;
    }
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
