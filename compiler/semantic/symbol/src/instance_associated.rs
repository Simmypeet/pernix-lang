//! Defines the [`get_instance_associated_equivalent`] function.

use pernixc_extend::extend;
use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;

use crate::{
    kind::{Kind, get_kind},
    member::get_members,
    name::get_name,
};

/// Defines the kind of symbol that can be associated to a trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum AssociatedKind {
    Function,
    Type,
    Instance,
    Constant,
}

/// Retrieves the instance associated item that is equivalent to the given trait
/// associated item, if any.
#[extend]
pub async fn get_instance_associated_equivalent(
    self: &TrackedEngine,
    instance: Global<crate::SymbolID>,
    trait_associated: Global<crate::SymbolID>,
    associated_kind: AssociatedKind,
) -> Option<Global<crate::SymbolID>> {
    let trait_associated_name = self.get_name(trait_associated).await;
    let expected_instance_associated_kind = match associated_kind {
        AssociatedKind::Function => Kind::InstanceAssociatedFunction,
        AssociatedKind::Type => Kind::InstanceAssociatedType,
        AssociatedKind::Instance => Kind::InstanceAssociatedInstance,
        AssociatedKind::Constant => Kind::InstanceAssociatedConstant,
    };

    let instance_member = self.get_members(instance).await;
    let member_id = instance
        .target_id
        .make_global(instance_member.get_by_name(&trait_associated_name)?);

    if self.get_kind(member_id).await == expected_instance_associated_kind {
        Some(member_id)
    } else {
        None
    }
}
