use crate::{instance::InstanceAssociated, r#type::Type};

/// A predicate payload equating an instance-associated type with a concrete
/// type.
pub type InstanceAssociatedTypeEquality = crate::predicate::Compatible<
    InstanceAssociated,
    qbice::storage::intern::Interned<Type>,
>;
