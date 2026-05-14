use qbice::{Decode, Encode, Identifiable, StableHash};

use crate::{
    generic_parameters::GenericParameterID,
    r#type::{
        constructor::Application, context::TyContext,
        inference::InferenceVariable,
    },
};

pub mod constructor;
pub mod context;
pub mod inference;
pub mod kind;

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
    Application(Application),
}

impl Type {
    pub async fn kind(&self, ctx: &impl TyContext) -> kind::TyKind {
        match self {
            Self::GenericParameter(member_id) => ctx
                .get_symbol_generic_parameters(member_id.parent_id())
                .await[member_id.id()]
            .kind(),

            Self::InferenceVariable(inference_variable) => {
                ctx.get_inference_variable_kind(*inference_variable).await
            }

            Self::Application(application) => application.kind(ctx).await,
        }
    }
}
