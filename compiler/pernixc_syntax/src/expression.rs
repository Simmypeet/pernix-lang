//! Contains the definitions of the syntax tree related to expressions

use enum_as_inner::EnumAsInner;
use pernixc_parser::{abstract_tree, parser::ast};
use prefix::Prefixable;

pub mod postfix;
pub mod prefix;
pub mod unit;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Expression {
        Prefixable(Prefixable = ast::<Prefixable>()),
    }
}
