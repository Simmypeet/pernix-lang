//! Contains the definitions of the syntax tree related to expressions

use binary::Binary;
use enum_as_inner::EnumAsInner;
use pernixc_parser::{abstract_tree, parser::ast};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use terminator::Terminator;

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

pub mod binary;
pub mod block;
pub mod postfix;
pub mod prefix;
pub mod terminator;
pub mod unit;

abstract_tree::abstract_tree! {
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
        EnumAsInner
    )]
    pub enum Expression {
        Binary(Binary = ast::<Binary>()),
        Terminator(Terminator = ast::<Terminator>()),
    }
}

#[cfg(test)]
mod test;
