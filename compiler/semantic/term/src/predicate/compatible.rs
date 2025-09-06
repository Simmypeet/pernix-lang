use std::fmt::Write;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

/// A predicate representing compatible equality between two values.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    derive_new::new,
)]
#[allow(missing_docs)]
pub struct Compatible<T, U = T> {
    pub lhs: T,
    pub rhs: U,
}

impl<T: crate::display::Display, U: crate::display::Display>
    crate::display::Display for Compatible<T, U>
{
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        self.lhs.fmt(engine, formatter).await?;
        formatter.write_str(" = ")?;
        self.rhs.fmt(engine, formatter).await
    }
}
