use pernixc_parser::{
    abstract_tree::{self, AbstractTree},
    expect,
    parser::{Parser as _, ast},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    AccessModifier, Keyword, QualifiedIdentifier,
    item::{
        TrailingWhereClause, constant, function,
        generic_parameters::GenericParameters, r#type,
    },
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Signature {
        pub final_keyword: Keyword = expect::Keyword::Final.optional(),
        pub const_keyword: Keyword = expect::Keyword::Const.optional(),
        pub implements_keyword: Keyword = expect::Keyword::Implements,
        pub generic_parameters: GenericParameters
            = ast::<GenericParameters>().optional(),
        pub qualified_identifier: QualifiedIdentifier
            = ast::<QualifiedIdentifier>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct MemberTemplate<
        S: 'static + AbstractTree,
        B: 'static + AbstractTree
    > {
        pub access_modifier: AccessModifier = ast::<AccessModifier>().optional(),
        pub signature: S = ast::<S>(),
        pub body: B = ast::<B>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct FunctionSignature {
        pub const_keyword: Keyword = expect::Keyword::Const.optional(),
        pub signature: function::Signature = ast::<function::Signature>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Member {
        Constant(
            MemberTemplate<constant::Signature, constant::Body>
                = ast::<MemberTemplate<constant::Signature, constant::Body>>()
        ),
        Function(
            MemberTemplate<FunctionSignature, function::Body>
                = ast::<MemberTemplate<FunctionSignature, function::Body>>()
        ),
        Type(
            MemberTemplate<r#type::Signature, r#type::Body>
                = ast::<MemberTemplate<r#type::Signature, r#type::Body>>()
        ),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct NegativeBody {
        pub delete_keyword: Keyword = expect::Keyword::Delete,
        pub trailing_where_clause: TrailingWhereClause
            = ast::<TrailingWhereClause>().optional(),
    }
}

pub type PositiveBody = super::Body<Member>;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Body {
        Negative(NegativeBody = ast::<NegativeBody>()),
        Positive(PositiveBody = ast::<PositiveBody>()),
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
        Deserialize
    )]
    pub struct Implements {
        pub signature: Signature = ast::<Signature>(),
        pub body: Body = ast::<Body>().optional(),
    }
}

#[cfg(test)]
mod test;
