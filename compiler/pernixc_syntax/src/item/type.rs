use pernixc_parser::{
    abstract_tree, expect,
    parser::{ast, Parser as _},
};

use crate::{
    item::{generic_parameters::GenericParameters, TrailingWhereClause},
    r#type::Type as TypeTerm,
    AccessModifier, Identifier, Keyword, Punctuation,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Signature {
        pub type_keyword: Keyword = expect::Keyword::Type,
        pub identifier: Identifier = expect::Identifier,
        pub generic_parameters: GenericParameters
            = ast::<GenericParameters>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Body {
        pub equals: Punctuation = '=',
        pub r#type: TypeTerm = ast::<TypeTerm>(),
        pub trailing_where_clause: TrailingWhereClause
            = ast::<TrailingWhereClause>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Type {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub signature: Signature = ast::<Signature>(),
        pub body: Body = ast::<Body>(),
    }
}

#[cfg(test)]
mod test;
