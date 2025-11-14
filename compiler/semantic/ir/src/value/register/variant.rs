//! Contains the definition of the [`Variant`] register.

use pernixc_arena::ID;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::parent::get_parent;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, Symbol},
    lifetime::Lifetime,
    r#type::Type,
};

use super::r#struct::transform_generic_arguments;
use crate::{
    transform::{self, Transformer},
    value::{register::Register, Value},
};

/// Represents a variant value.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Variant {
    /// The variant ID of the variant.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The field initializers of the variant.
    pub associated_value: Option<Value>,

    /// The generic arguments supplied to the enum.
    pub generic_arguments: GenericArguments,
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

impl transform::Element for Variant {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) -> Result<(), CyclicError> {
        if let Some(value) = self.associated_value.as_mut() {
            if let Some(literal) = value.as_literal_mut() {
                literal.transform(transformer).await?;
            }
        }

        transform_generic_arguments(
            transformer,
            self.variant_id
                .target_id
                .make_global(engine.get_parent(self.variant_id).await.unwrap()),
            None,
            engine,
            &mut self.generic_arguments,
        )
        .await
    }
}

pub(super) async fn type_of_variant_assignment(
    variant: &Variant,
    engine: &TrackedEngine,
) -> Type {
    let enum_id = engine.get_parent(variant.variant_id).await.unwrap();

    Type::Symbol(Symbol {
        id: Global::new(variant.variant_id.target_id, enum_id),
        generic_arguments: variant.generic_arguments.clone(),
    })
}
