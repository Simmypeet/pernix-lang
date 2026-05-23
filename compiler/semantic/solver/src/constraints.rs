use pernixc_hash::FxHashSet;
use pernixc_type::{predicate::Outlives, r#type::Type};
use qbice::storage::intern::Interned;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Constraints(FxHashSet<Outlives>);

impl Constraints {
    #[must_use]
    pub fn new() -> Self { Self::default() }

    #[must_use]
    pub fn lifetimes_eq(a: Interned<Type>, b: Interned<Type>) -> Self {
        let mut lifetime_eq = Self::new();

        lifetime_eq.0.insert(Outlives::new(a.clone(), b.clone()));
        lifetime_eq.0.insert(Outlives::new(b, a));

        lifetime_eq
    }
}
