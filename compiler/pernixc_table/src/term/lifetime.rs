use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;
use serde::{Deserialize, Serialize};

use super::{generic_parameters::LifetimeParameterID, Error, Model};
use crate::GlobalID;

/// Represents a for-all quantified lifetime, denoted by `for['a]` syntax, used
/// in higher-ranked predicates.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Forall {
    /// The global ID where the forall lifetime was declared.
    pub global_id: GlobalID,

    /// The unique ID of the forall lifetime within the global ID scope.
    pub id: usize,

    /// The span where the forall lifetime was declared.
    ///
    /// This field doesn't influence the equality, ordering, and hashing of the
    /// this struct.
    #[serde(skip)]
    pub span: Option<Span>,
}

impl PartialEq for Forall {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.global_id == other.global_id
    }
}

impl Eq for Forall {}

impl PartialOrd for Forall {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Forall {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id).then(self.global_id.cmp(&other.global_id))
    }
}

impl std::hash::Hash for Forall {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.global_id.hash(state);
    }
}

/// Represents a lifetime annotation term.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    Serialize,
    Deserialize,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Lifetime<M: Model> {
    Static,
    #[from]
    Parameter(LifetimeParameterID),
    Inference(M::LifetimeInference),
    #[from]
    Forall(Forall),
    #[from]
    Error(Error),
}
