use pernixc_table::{
    component::HierarchyRelationship, GlobalAccessibility, Table,
};

/// Checks if a private entity is leaked to the public interface.
pub fn check_private_entity_leakage(
    table: &Table,
    entity_accessibility: GlobalAccessibility,
    public_accessibility: GlobalAccessibility,
) -> bool {
    match (entity_accessibility, public_accessibility) {
        (
            GlobalAccessibility::Public,
            GlobalAccessibility::Public | GlobalAccessibility::Scoped(_),
        ) => false,

        (GlobalAccessibility::Scoped(_), GlobalAccessibility::Public) => true,

        (GlobalAccessibility::Scoped(ty), GlobalAccessibility::Scoped(sym)) => {
            assert_eq!(ty.target_id, sym.target_id);

            table.symbol_hierarchy_relationship(ty.target_id, ty.id, sym.id)
                == HierarchyRelationship::Child
        }
    }
}
