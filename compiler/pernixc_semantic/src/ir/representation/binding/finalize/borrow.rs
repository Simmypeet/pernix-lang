//! This module implements the borrow checker.
//!
//! ## Credits
//!
//! The model is based on the [`Polonius`](https://github.com/rust-lang/polonius).
//! Thanks to Niko Matsakis descrbing the algorithm in his blog post
//! [`Polonius revisited, part 1`](https://smallcultfollowing.com/babysteps/blog/2023/09/22/polonius-part-1/).
//! and [`Polonius revisited, part 2`](https://smallcultfollowing.com/babysteps/blog/2023/09/29/polonius-part-2/).

use pernixc_base::handler::Handler;

use super::memory;
use crate::{
    error::{self, TypeSystemOverflow},
    ir::{self, representation::borrow},
    symbol::{table, GlobalID},
    type_system::{
        environment::Environment, normalizer::Normalizer, observer::Observer,
    },
};

mod state;
mod transform;

/// Main entry point for the borrow checking phase.
pub(super) fn borrow_check<S: table::State>(
    ir: ir::Representation<ir::Model>,
    current_site: GlobalID,
    environment: &Environment<
        borrow::Model,
        S,
        impl Normalizer<borrow::Model, S>,
        impl Observer<borrow::Model, S>,
    >,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Result<(), TypeSystemOverflow<ir::Model>> {
    let (ir, origins) = transform::transform(ir, environment.table());
    let state = state::State::new(memory::state::Stack::new(), origins);

    Ok(())
}
