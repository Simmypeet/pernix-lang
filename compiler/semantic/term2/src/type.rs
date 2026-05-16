use pernixc_qbice::TrackedEngine;
use qbice::{Decode, Encode, Identifiable, StableHash};

use crate::{
    generic_parameters::{GenericParameterID, get_generic_parameters},
    r#type::{
        bound::BoundVar, constructor::Application, context::TyContext,
        inference::InferenceVariable,
    },
};

pub mod bound;
pub mod constructor;
pub mod context;
pub mod inference;
pub mod kind;

/// The main representation of types in the compiler.
///
/// The representation is highly homogeneous, making easy to manipulate and
/// reason about them.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Identifiable,
)]
pub enum Type {
    GenericParameter(GenericParameterID),
    InferenceVariable(InferenceVariable),
    BoundVar(BoundVar),
    Application(Application),
}

impl Type {
    pub async fn kind(
        &self,
        engine: &TrackedEngine,
        ctx: &impl TyContext,
    ) -> kind::TyKind {
        match self {
            Self::GenericParameter(member_id) => engine
                .get_generic_parameters(member_id.parent_id())
                .await[member_id.id()]
            .kind(),

            Self::InferenceVariable(inference_variable) => {
                ctx.get_inference_variable_kind(*inference_variable).await
            }

            Self::Application(application) => application.kind(engine).await,

            Self::BoundVar(bound_var) => {
                ctx.get_bound_var_kind(bound_var).await
            }
        }
    }
}
