//! Contains the definition of [`Register`] and [`Assignment`].
//!
//! The register is a place where SSA values are stored. The assignment is the
//! value that is stored in the register.

use std::borrow::Cow;

use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::MemberID;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant, generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters, lifetime::Lifetime,
    r#type::Type,
};
use pernixc_type_system::{Error, Succeeded, normalizer::Normalizer};

use crate::{
    Values,
    address::Address,
    transform::{
        self, ConstantTermSource, LifetimeTermSource, Transformer,
        TypeTermSource,
    },
    value::{Environment, TypeOf, Value},
    visitor,
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
pub mod resume_call;
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
pub use resume_call::ResumeCall;
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
    ResumeCall(ResumeCall),
}

impl visitor::Element for Assignment {
    fn accept(&self, visitor: &mut impl visitor::Visitor) {
        match self {
            Self::Tuple(tuple) => tuple.accept(visitor),
            Self::Load(load) => load.accept(visitor),
            Self::Borrow(borrow) => borrow.accept(visitor),
            Self::Prefix(prefix) => prefix.accept(visitor),
            Self::Struct(st) => st.accept(visitor),
            Self::Variant(variant) => variant.accept(visitor),
            Self::FunctionCall(function_call) => function_call.accept(visitor),
            Self::Binary(binary) => binary.accept(visitor),
            Self::Array(array) => array.accept(visitor),
            Self::Phi(phi) => phi.accept(visitor),
            Self::Cast(cast) => cast.accept(visitor),
            Self::VariantNumber(variant_number) => {
                variant_number.accept(visitor);
            }
            Self::Do(d) => d.accept(visitor),
            Self::ResumeCall(resume_call) => resume_call.accept(visitor),
        }
    }
}

impl Assignment {
    /// Returns the register that is used in the assignment.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        use visitor::Element;
        let mut visitor = RegisterVisitor::default();
        self.accept(&mut visitor);
        visitor.registers
    }
}

/// Represents a register in the SSA from.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct Register {
    /// The value stored in the register.
    pub assignment: Assignment,

    /// The span where the value was defined.
    pub span: RelativeSpan,
}

impl TypeOf<ID<Register>> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        id: ID<Register>,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, Error> {
        let register = &self.registers[id];

        match &register.assignment {
            Assignment::Tuple(tuple) => {
                return self.type_of(tuple, environment).await;
            }
            Assignment::Load(load) => {
                return self.type_of(load, environment).await;
            }
            Assignment::Borrow(borrow) => {
                return self.type_of(borrow, environment).await;
            }
            Assignment::Prefix(prefix) => {
                return self.type_of(prefix, environment).await;
            }
            Assignment::Struct(st) => {
                return self.type_of(st, environment).await;
            }
            Assignment::Variant(variant) => {
                return self.type_of(variant, environment).await;
            }
            Assignment::FunctionCall(function_call) => {
                return self.type_of(function_call, environment).await;
            }
            Assignment::ResumeCall(d) => {
                return self.type_of(d, environment).await
            }
            Assignment::Binary(binary) => {
                return self.type_of(binary, environment).await;
            }
            Assignment::Phi(phi_node) => {
                return self.type_of(phi_node, environment).await;
            }
            Assignment::Array(array) => {
                return self.type_of(array, environment).await;
            }
            Assignment::Cast(cast) => {
                return self.type_of(cast, environment).await;
            }
            Assignment::VariantNumber(variant) => {
                return self.type_of(variant, environment).await;
            }
            Assignment::Do(d) => return self.type_of(d, environment).await,
        }
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
            Assignment::Do(d) => {
                do_with::transform_do_with(d, transformer, engine, self.span)
                    .await
            }
            Assignment::ResumeCall(r) => {
                resume_call::transform_resume_call(r, transformer, self.span)
                    .await
            }
        }
    }
}

pub(super) async fn transform_generic_arguments<
    T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
>(
    transformer: &mut T,
    symbol_id: Global<pernixc_symbol::ID>,
    span: RelativeSpan,
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

/// A visitor that collects all register IDs from values.
#[derive(Debug, Default)]
struct RegisterVisitor {
    registers: Vec<ID<Register>>,
}

impl visitor::Visitor for RegisterVisitor {
    fn visit_value(&mut self, value: Cow<Value>) {
        if let Some(&register_id) = value.as_register() {
            self.registers.push(register_id);
        }
    }

    fn visit_address(&mut self, address: Cow<Address>) {
        if let Address::Index(index) = &*address
            && let Value::Register(register_id) = &index.indexing_value
        {
            self.registers.push(*register_id);
        }
    }
}
