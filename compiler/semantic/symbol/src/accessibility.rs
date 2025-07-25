//! Contains the definition of tyhe [`Accessibility`] enum.

use enum_as_inner::EnumAsInner;
use pernixc_extend::extend;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};

use crate::{
    get_table_of_symbol,
    kind::{get_kind, Kind},
    parent::{
        get_closest_module_id, get_parent, symbol_hierarchy_relationship,
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

#[pernixc_query::executor(key(Key), name(Executor))]
pub async fn executor(
    &Key(id): &Key,
    engine: &TrackedEngine,
) -> Result<Accessibility<ID>, CyclicError> {
    match engine.get_kind(id).await {
        Kind::Module
        | Kind::Struct
        | Kind::Trait
        | Kind::Enum
        | Kind::Type
        | Kind::Constant
        | Kind::TraitType
        | Kind::TraitFunction
        | Kind::TraitConstant
        | Kind::Marker
        | Kind::AdtImplementationFunction
        | Kind::ExternFunction
        | Kind::Function => {
            let table = engine.get_table_of_symbol(id).await;

            Ok(table.accessibilities.get(&id.id).copied().unwrap())
        }

        // based on the parent's accessibility
        Kind::TraitImplementationFunction
        | Kind::TraitImplementationType
        | Kind::TraitImplementationConstant
        | Kind::Variant => Ok(engine
            .get_accessibility(Global::new(
                id.target_id,
                engine.get_parent(id).await.unwrap(),
            ))
            .await),

        Kind::PositiveTraitImplementation
        | Kind::NegativeTraitImplementation
        | Kind::PositiveMarkerImplementation
        | Kind::NegativeMarkerImplementation
        | Kind::AdtImplementation => {
            // self.get_accessibility(self.get_implements(id))
            todo!()
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
