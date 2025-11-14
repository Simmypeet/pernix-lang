//! Contains the definition of the [`Struct`] register.

use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_semantic_element::fields::Field;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::MemberID;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::get_generic_parameters,
    lifetime::Lifetime,
    r#type::Type,
};

use crate::{
    transform::{
        self, ConstantTermSource, LifetimeTermSource, Transformer,
        TypeTermSource,
    },
    value::{register::Register, Value},
};

/// Represents a struct value.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct Struct {
    /// The struct ID of the struct.
    pub struct_id: Global<pernixc_symbol::ID>,

    /// The field initializers of the struct.
    pub initializers_by_field_id: HashMap<ID<Field>, Value>,

    /// The generic arguments supplied to the struct.
    pub generic_arguments: GenericArguments,
}

impl Struct {
    /// Returns the list of registers that are used in the struct.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.initializers_by_field_id
            .values()
            .filter_map(|x| x.as_register().copied())
            .collect()
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

impl transform::Element for Struct {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        for value in self.initializers_by_field_id.values_mut() {
            if let Some(literal) = value.as_literal_mut() {
                literal.transform(transformer).await?;
            }
        }

        transform_generic_arguments(
            transformer,
            self.struct_id,
            None,
            engine,
            &mut self.generic_arguments,
        )
        .await
    }
}

pub(super) fn type_of_struct_assignment(st: &Struct) -> Type {
    Type::Symbol(Symbol {
        id: st.struct_id,
        generic_arguments: st.generic_arguments.clone(),
    })
}
