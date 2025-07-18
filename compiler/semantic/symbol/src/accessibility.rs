//! Contains the definition of tyhe [`Accessibility`] enum.

use enum_as_inner::EnumAsInner;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};

use crate::ID;

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

/*
/// An executor for the [`Key`] query that retrieves the accessibility of a
/// symbol with the given ID.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &TrackedEngine,
        &Key(id): &Key,
    ) -> Result<Accessibility<ID>, pernixc_query::runtime::executor::CyclicError>
    {
        let table = engine
            .query(&crate::Key(id.target_id))
            .expect("should have no cyclic dependencies");

        Ok(table
            .entries_by_id
            .get(&id.id)
            .expect("invalid symbol ID")
            .accessibility
            .expect(
                "the symbol doesn't have an explicit accessibility set, use \
                 the `get_accessibility` extension method to retrieve it",
            ))
    }
}

/// Computes the [`HierarchyRelationship`] between the two given
/// accessibilities.
///
/// The returned [`HierarchyRelationship`] is based on the `first`
/// accessibility.
#[extend]
pub fn accessibility_hierarchy_relationship(
    self: &TrackedEngine<'_>,
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
            self.symbol_hierarchy_relationship(target_id, first, second)
        }
    }
}

/// Gets the [`Accessibility`] of the given symbol.
#[extend]
pub fn get_accessibility(
    self: &TrackedEngine<'_>,
    id: Global<ID>,
) -> Accessibility<ID> {
    match self.get_kind(id) {
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
            self.query(&Key(id)).expect("should have no cyclic dependencies")
        }

        // based on the parent's accessibility
        Kind::TraitImplementationFunction
        | Kind::TraitImplementationType
        | Kind::TraitImplementationConstant
        | Kind::Variant => self.get_accessibility(Global::new(
            id.target_id,
            self.get_parent(id).unwrap(),
        )),

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

/// Checks if the `referred` is accessible from the `referring_site`.
///
/// # Returns
///
/// Returns `None` if `referred` or `referring_site` is not a valid ID.
#[extend]
pub fn symbol_accessible(
    self: &TrackedEngine<'_>,
    referring_site: Global<ID>,
    referred: Global<ID>,
) -> bool {
    let referred_accessibility = self.get_accessibility(referred);

    self.is_accessible_from(
        referring_site.id,
        referred.target_id,
        referred_accessibility,
    )
}

/// Determines whether the given `referred` is accessible from the
/// `referring_site` as if the `referred` has the given
/// `referred_accessibility`.
#[extend]
pub fn is_accessible_from(
    self: &TrackedEngine<'_>,
    referring_site: ID,
    referred_target_id: TargetID,
    referred_accessibility: Accessibility<ID>,
) -> bool {
    match referred_accessibility {
        Accessibility::Public => true,

        Accessibility::Scoped(module_id) => {
            let referring_site_module_id = self.get_closest_module_id(
                Global::new(referred_target_id, referring_site),
            );

            matches!(
                self.symbol_hierarchy_relationship(
                    referred_target_id,
                    module_id,
                    referring_site_module_id,
                ),
                HierarchyRelationship::Parent
                    | HierarchyRelationship::Equivalent
            )
        }
    }
}
*/
