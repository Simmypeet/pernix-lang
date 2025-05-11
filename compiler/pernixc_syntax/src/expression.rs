//! Contains the definitions of the syntax tree related to expressions

use binary::Binary;
use enum_as_inner::EnumAsInner;
use pernixc_parser::{abstract_tree, parser::ast};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

pub mod binary;
pub mod postfix;
pub mod prefix;
pub mod unit;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Expression {
        Binary(Binary = ast::<Binary>()),
    }
}

#[cfg(test)]
mod test;
