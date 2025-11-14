//! Contains the definition of the [`FunctionCall`] register.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_semantic_element::return_type::get_return_type;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant, effect, instantiation::Instantiation,
    lifetime::Lifetime, r#type::Type,
};
use pernixc_type_system::Error;

use crate::{
    handling_scope::HandlerClauseID,
    transform::{
        ConstantTermSource, LifetimeTermSource, Transformer, TypeTermSource,
    },
    value::{register::Register, Value},
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
    Serialize,
    Deserialize,
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

/// Represents a function call.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct FunctionCall {
    /// The ID of the function that is called.
    pub callable_id: Global<pernixc_symbol::ID>,

    /// The arguments supplied to the function.
    pub arguments: Vec<Value>,

    /// The generic instantiations of the function.
    pub instantiation: Instantiation,

    /// The capability arguments supplied to the function.
    pub effect_arguments: HashMap<ID<effect::Unit>, EffectHandlerArgument>,
}

impl FunctionCall {
    /// Returns the list of registers that are used in the function call.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.arguments.iter().filter_map(|x| x.as_register().copied()).collect()
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
pub(super) async fn transform_function_call<
    T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
>(
    function_call: &mut FunctionCall,
    transformer: &mut T,
    span: Option<pernixc_lexical::tree::RelativeSpan>,
) -> Result<(), CyclicError> {
    for argument in &mut function_call.arguments {
        if let Some(literal) = argument.as_literal_mut() {
            literal.transform(transformer).await?;
        }
    }

    for (lt_id, lt) in &mut function_call.instantiation.lifetimes {
        let source = match lt_id {
            Lifetime::Parameter(member_id) => {
                LifetimeTermSource::GenericParameter(*member_id)
            }
            Lifetime::Elided(member_id) => {
                LifetimeTermSource::ElidedLifetimeParameter(*member_id)
            }
            _ => unreachable!("should've either be parameter or elided"),
        };

        transformer.transform(lt, source, span).await?;
    }

    for (ty_id, ty) in &mut function_call.instantiation.types {
        transformer
            .transform(
                ty,
                TypeTermSource::GenericParameter(
                    ty_id
                        .clone()
                        .into_parameter()
                        .expect("should've been a type ID"),
                ),
                span,
            )
            .await?;
    }

    for (ct_id, ct) in &mut function_call.instantiation.constants {
        transformer
            .transform(
                ct,
                ConstantTermSource::GenericParameter(
                    ct_id
                        .clone()
                        .into_parameter()
                        .expect("should've been a constant ID"),
                ),
                span,
            )
            .await?;
    }

    Ok(())
}

pub(super) async fn type_of_function_call_assignment(
    function_call: &FunctionCall,
    engine: &TrackedEngine,
) -> Result<Type, Error> {
    let mut return_type = engine
        .get_return_type(function_call.callable_id)
        .await?
        .deref()
        .clone();

    function_call.instantiation.instantiate(&mut return_type);

    Ok(return_type)
}
