use pernixc_parser::{
    abstract_tree, expect,
    parser::{Parser as _, ast},
};

use crate::{
    AccessModifier, Identifier, Keyword,
    item::{TrailingWhereClause, generic_parameters::GenericParameters},
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Signature {
        pub marker_keyword: Keyword = expect::Keyword::Marker,
        pub identifier: Identifier = expect::Identifier,
        pub generic_parameters: GenericParameters
            = ast::<GenericParameters>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Marker {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub signature: Signature = ast::<Signature>(),
        pub trailing_where_clause: TrailingWhereClause
            = ast::<TrailingWhereClause>().optional(),
    }
}

#[cfg(test)]
mod test;
