use pernixc_parser::{
    abstract_tree::{self, AbstractTree},
    expect,
    parser::{ast, Parser as _},
};
use where_clause::WhereClause;

use crate::Passable;

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

pub mod r#enum;
pub mod generic_parameters;
pub mod r#struct;
pub mod where_clause;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Indentation}
    pub struct Body<T: 'static + AbstractTree> {
        pub where_clause: WhereClause = ast::<WhereClause>().optional(),
        pub members: #[multi] Passable<T>
            = ast::<Passable<T>>().line().repeat_all(),
    }
}
