//! Contains the definition of tyhe [`Accessibility`] enum.

use enum_as_inner::EnumAsInner;
use pernixc_extend::extend;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};

use crate::{
    name::get_qualified_name,
    parent::{
        get_closest_module_id, symbol_hierarchy_relationship,
        HierarchyRelationship,
    },
    ID,
};

/// The key type used with [`TrackedEngine`] to access the accessibility of a
/// symbol.
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
    pernixc_query::Key,
    StableHash,
)]
#[value(Accessibility<ID>)]
#[extend(method(get_accessibility), no_cyclic)]
pub struct Key(pub Global<ID>);

/// The accessibility defined to a symbol
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
    EnumAsInner,
    StableHash,
)]
pub enum Accessibility<ID> {
    /// The symbol is accessible from anywhere.
    #[default]
    Public,

    /// The symbol is accessible from the given module and its children.
    Scoped(ID),
}

impl Accessibility<ID> {
    /// Converts the accessibility into a [`Accessibility<Global<ID>>`].
    #[must_use]
    pub fn into_global(self, target_id: TargetID) -> Accessibility<Global<ID>> {
        match self {
            Self::Public => Accessibility::Public,
            Self::Scoped(id) => {
                Accessibility::Scoped(Global::new(target_id, id))
            }
        }
    }
}

/// Computes the [`HierarchyRelationship`] between the two given
/// accessibilities.
///
/// The returned [`HierarchyRelationship`] is based on the `first`
/// accessibility.
#[extend]
pub async fn accessibility_hierarchy_relationship(
    self: &TrackedEngine,
    target_id: TargetID,
    first: Accessibility<ID>,
    second: Accessibility<ID>,
) -> HierarchyRelationship {
    match (first, second) {
        (Accessibility::Public, Accessibility::Public) => {
            HierarchyRelationship::Equivalent
        }
        (Accessibility::Public, Accessibility::Scoped(_)) => {
            HierarchyRelationship::Parent
        }
        (Accessibility::Scoped(_), Accessibility::Public) => {
            HierarchyRelationship::Child
        }
        (Accessibility::Scoped(first), Accessibility::Scoped(second)) => {
            self.symbol_hierarchy_relationship(target_id, first, second).await
        }
    }
}

/// Checks if the `referred` is accessible from the `referring_site`.
///
/// # Returns
///
/// Returns `None` if `referred` or `referring_site` is not a valid ID.
#[extend]
pub async fn symbol_accessible(
    self: &TrackedEngine,
    referring_site: Global<ID>,
    referred: Global<ID>,
) -> bool {
    let referred_accessibility = self.get_accessibility(referred).await;

    self.is_accessible_from(
        referring_site.id,
        referred.target_id,
        referred_accessibility,
    )
    .await
}

/// Returns the user-friendly description of the accessibility.
#[extend]
pub async fn accessibility_description(
    self: &TrackedEngine,
    accessibility: Accessibility<Global<ID>>,
) -> String {
    match accessibility {
        Accessibility::Public => "publicly accessible".to_owned(),
        Accessibility::Scoped(module_id) => {
            let module_qualified_name =
                self.get_qualified_name(module_id).await;

            format!("accessible in `{module_qualified_name}`")
        }
    }
}

/// Determines whether the given `referred` is accessible from the
/// `referring_site` as if the `referred` has the given
/// `referred_accessibility`.
#[extend]
pub async fn is_accessible_from(
    self: &TrackedEngine,
    referring_site: ID,
    referred_target_id: TargetID,
    referred_accessibility: Accessibility<ID>,
) -> bool {
    match referred_accessibility {
        Accessibility::Public => true,

        Accessibility::Scoped(module_id) => {
            let referring_site_module_id = self
                .get_closest_module_id(Global::new(
                    referred_target_id,
                    referring_site,
                ))
                .await;

            matches!(
                self.symbol_hierarchy_relationship(
                    referred_target_id,
                    module_id,
                    referring_site_module_id,
                )
                .await,
                HierarchyRelationship::Parent
                    | HierarchyRelationship::Equivalent
            )
        }
    }
}
