use pernixc_parser::{
    abstract_tree::{self, AbstractTree},
    expect,
    parser::{Parser as _, ast},
};

use crate::{
    AccessModifier, Identifier, Keyword,
    item::{
        Body, TrailingWhereClause, constant, function,
        generic_parameters::GenericParameters, r#type,
    },
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Signature {
        pub trait_keyword: Keyword = expect::Keyword::Trait,
        pub identifier: Identifier = expect::Identifier,
        pub generic_parameters: GenericParameters
            = ast::<GenericParameters>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct MemberTemplate<T: 'static + AbstractTree> {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub signature: T = ast::<T>(),
        pub trailing_where_clause: TrailingWhereClause
            = ast::<TrailingWhereClause>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Member {
        Type(
            MemberTemplate<r#type::Signature>
                = ast::<MemberTemplate<r#type::Signature>>()
        ),
        Function(
            MemberTemplate<function::Signature>
                = ast::<MemberTemplate<function::Signature>>()
        ),
        Constant(
            MemberTemplate<constant::Signature>
                = ast::<MemberTemplate<constant::Signature>>()
        ),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Trait {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub signature: Signature = ast::<Signature>(),
        pub body: Body<Member> = ast::<Body<Member>>(),
    }
}

#[cfg(test)]
mod test;
