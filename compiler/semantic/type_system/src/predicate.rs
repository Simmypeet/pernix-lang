//! Implements the [`crate::environment::Query`] for various
//! definition of [`pernixc_term::predicate`]

pub mod constant_type;
pub mod marker;
pub mod outlives;
// pub mod r#trait;
pub mod tuple;

// pub use marker::{
//     NegativeSatisfied as NegativeMarkerSatisfied,
//     PositiveSatisfied as PositiveMarkerSatisfied,
// };
// pub use r#trait::{
//     NegativeSatisfied as NegativeTraitSatisfied,
//     PositiveSatisfied as PositiveTraitSatisfied,
// };
