use std::collections::HashMap;

use pernixc_syntax::syntax_tree;

use crate::{arena::ID, symbol::Trait};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum State<T> {
    Drafted(T),
    Constructing,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(super) struct Manager {
    states_by_enum_id: HashMap<ID<Trait>, State<syntax_tree::item::Enum>>,
}
