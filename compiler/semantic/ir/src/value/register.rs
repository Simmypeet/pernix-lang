//! Contains the definition of [`Register`] and [`Assignment`].
//!
//! The register is a place where SSA values are stored. The assignment is the
//! value that is stored in the register.

use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::MemberID;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant, generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters, lifetime::Lifetime,
    r#type::Type,
};
use pernixc_type_system::{normalizer::Normalizer, Error, Succeeded};

use crate::{
    transform::{
        self, ConstantTermSource, LifetimeTermSource, Transformer,
        TypeTermSource,
    },
    value::{Environment, TypeOf},
    Values,
};

pub mod array;
pub mod binary;
pub mod borrow;
pub mod cast;
pub mod do_with;
pub mod function_call;
pub mod load;
pub mod phi;
pub mod prefix;
pub mod r#struct;
pub mod tuple;
pub mod variant;
pub mod variant_number;

// Re-export commonly used types for convenience
pub use array::Array;
pub use binary::{
    ArithmeticOperator, Binary, BinaryOperator, BitwiseOperator,
    RelationalOperator,
};
pub use borrow::Borrow;
pub use cast::Cast;
pub use function_call::{EffectHandlerArgument, FunctionCall};
pub use load::{Load, Purpose as LoadPurpose};
pub use phi::Phi;
pub use prefix::{Prefix, PrefixOperator};
pub use r#struct::Struct;
pub use tuple::{Tuple, TupleElement};
pub use variant::Variant;
pub use variant_number::VariantNumber;

/// An enumeration of the different kinds of values that can be assigned in the
/// register.
#[derive(
    Debug, Clone, PartialEq, Eq, Serialize, Deserialize, EnumAsInner, StableHash,
)]
#[allow(missing_docs)]
pub enum Assignment {
    Tuple(Tuple),
    Load(Load),
    Borrow(Borrow),
    Prefix(Prefix),
    Struct(Struct),
    Variant(Variant),
    FunctionCall(FunctionCall),
    Binary(Binary),
    Array(Array),
    Phi(Phi),
    Cast(Cast),
    VariantNumber(VariantNumber),
    Do(do_with::DoWith),
}

impl Assignment {
    /// Returns the register that is used in the assignment.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        match self {
            Self::Tuple(tuple) => tuple.get_used_registers(),
            Self::Prefix(prefix) => prefix.get_used_registers(),
            Self::Struct(st) => st.get_used_registers(),
            Self::Variant(variant) => variant.get_used_registers(),
            Self::FunctionCall(function_call) => {
                function_call.get_used_registers()
            }
            Self::Binary(binary) => binary.get_used_registers(),
            Self::Array(array) => array.get_used_registers(),
            Self::Phi(phi) => phi.get_used_registers(),
            Self::Cast(cast) => cast.get_used_registers(),
            Self::Do(d) => d.get_used_registers(),

            Self::Load(_) | Self::Borrow(_) | Self::VariantNumber(_) => {
                Vec::new()
            }
        }
    }
}

/// Represents a register in the SSA from.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct Register {
    /// The value stored in the register.
    pub assignment: Assignment,

    /// The span where the value was defined.
    pub span: Option<RelativeSpan>,
}

impl TypeOf<ID<Register>> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        id: ID<Register>,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        let register = &self.registers[id];

        let ty = match &register.assignment {
            Assignment::Tuple(tuple) => {
                return self.type_of(tuple, environment).await
            }
            Assignment::Load(load) => {
                return self.type_of(load, environment).await
            }
            Assignment::Borrow(borrow) => {
                return self.type_of(borrow, environment).await
            }
            Assignment::Prefix(prefix) => {
                return self.type_of(prefix, environment).await
            }
            Assignment::Struct(st) => {
                Ok(r#struct::type_of_struct_assignment(st))
            }
            Assignment::Variant(variant) => {
                Ok(variant::type_of_variant_assignment(
                    variant,
                    environment.tracked_engine(),
                )
                .await)
            }
            Assignment::FunctionCall(function_call) => {
                function_call::type_of_function_call_assignment(
                    function_call,
                    environment.tracked_engine(),
                )
                .await
            }
            Assignment::Binary(binary) => {
                return self.type_of(binary, environment).await;
            }
            Assignment::Phi(phi_node) => Ok(phi_node.r#type.clone()),
            Assignment::Array(array) => {
                Ok(Type::Array(pernixc_term::r#type::Array {
                    r#type: Box::new(array.element_type.clone()),
                    length: Constant::Primitive(
                        pernixc_term::constant::Primitive::Usize(
                            array.elements.len() as u64,
                        ),
                    ),
                }))
            }
            Assignment::Cast(cast) => Ok(cast.r#type.clone()),
            Assignment::VariantNumber(variant) => {
                return self.type_of(variant, environment).await
            }
            Assignment::Do(d) => Ok(d.return_type().clone()),
        }?;

        Ok(environment.type_environment.simplify(ty).await?.deref().clone())
    }
}

impl transform::Element for Register {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        match &mut self.assignment {
            Assignment::Tuple(tuple) => {
                tuple::transform_tuple(tuple, transformer, self.span, engine)
                    .await
            }
            Assignment::Load(load) => {
                load::transform_load(load, transformer, self.span).await
            }
            Assignment::Borrow(borrow) => {
                borrow::transform_borrow(borrow, transformer, self.span).await
            }
            Assignment::Prefix(prefix) => {
                prefix::transform_prefix(prefix, transformer, self.span).await
            }
            Assignment::Struct(st) => {
                r#struct::transform_struct(st, transformer, self.span, engine)
                    .await
            }
            Assignment::Variant(variant) => {
                variant::transform_variant(
                    variant,
                    transformer,
                    self.span,
                    engine,
                )
                .await
            }
            Assignment::FunctionCall(function_call) => {
                function_call::transform_function_call(
                    function_call,
                    transformer,
                    self.span,
                )
                .await
            }
            Assignment::Binary(binary) => {
                binary::transform_binary(binary, transformer, self.span).await
            }
            Assignment::Array(array) => {
                array::transform_array(array, transformer, self.span).await
            }
            Assignment::Phi(phi) => {
                phi::transform_phi(phi, transformer, self.span).await
            }
            Assignment::Cast(cast) => {
                cast::transform_cast(cast, transformer, self.span).await
            }
            Assignment::VariantNumber(variant_number) => {
                variant_number::transform_variant_number(
                    variant_number,
                    transformer,
                    self.span,
                )
                .await
            }
            Assignment::Do(d) => d.transform(transformer, engine).await,
        }
    }
}

pub(super) async fn transform_generic_arguments<
    T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
>(
    transformer: &mut T,
    symbol_id: Global<pernixc_symbol::ID>,
    span: Option<RelativeSpan>,
    engine: &TrackedEngine,
    generic_arg: &mut GenericArguments,
) -> Result<(), CyclicError> {
    let generic_params = engine.get_generic_parameters(symbol_id).await?;

    for (lt_id, lt) in generic_params
        .lifetime_order()
        .iter()
        .copied()
        .zip(generic_arg.lifetimes.iter_mut())
    {
        transformer
            .transform(
                lt,
                LifetimeTermSource::GenericParameter(MemberID::new(
                    symbol_id, lt_id,
                )),
                span,
            )
            .await?;
    }

    for (ty_id, ty) in generic_params
        .type_order()
        .iter()
        .copied()
        .zip(generic_arg.types.iter_mut())
    {
        transformer
            .transform(
                ty,
                TypeTermSource::GenericParameter(MemberID::new(
                    symbol_id, ty_id,
                )),
                span,
            )
            .await?;
    }

    for (ct_id, ct) in generic_params
        .constant_order()
        .iter()
        .copied()
        .zip(generic_arg.constants.iter_mut())
    {
        transformer
            .transform(
                ct,
                ConstantTermSource::GenericParameter(MemberID::new(
                    symbol_id, ct_id,
                )),
                span,
            )
            .await?;
    }

    Ok(())
}
