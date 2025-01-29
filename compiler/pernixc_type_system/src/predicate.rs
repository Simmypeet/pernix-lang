//! Implements the [`crate::environment::Query`] for various
//! definition of [`pernixc_term::predicate`]

mod constant_type;
mod marker;
mod outlives;
mod r#trait;
mod tuple;

pub use marker::{
    NegativeSatisfied as NegativeMarkerSatisfied,
    PositiveSatisfied as PositiveMarkerSatisfied,
};
pub use r#trait::{
    NegativeSatisfied as NegativeTraitSatisfied,
    PositiveSatisfied as PositiveTraitSatisfied,
};
