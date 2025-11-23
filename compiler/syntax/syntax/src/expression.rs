//! Contains the definitions of the syntax tree related to expressions

use binary::Binary;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{Parser, ast},
};
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
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct Call {
        pub expressions: #[multi] Expression
            = ast::<Expression>().repeat_all_with_separator(','),
    }
}

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
