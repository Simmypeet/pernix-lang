//! The definition of the [`Database`].

use std::{
    any::{Any, TypeId},
    hash::Hash,
    sync::Arc,
};

use dashmap::DashMap;
use parking_lot::Mutex;
use pernixc_arena::ID;
use pernixc_target::Global;

use crate::{map::Map, Executor, Key};

mod call_graph;

#[cfg(test)]
mod test;

