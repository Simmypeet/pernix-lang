use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree::{self, AbstractTree},
    expect,
    parser::{ast, Parser as _},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    expression::Expression, predicate::TypeBound, r#type::Type, Identifier,
    Keyword, LifetimeParameter, Punctuation,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Default<T: 'static + AbstractTree> {
        pub equals: Punctuation = '=',
        pub value: T = ast::<T>()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TypeParameterBound {
        pub colon: Punctuation = ':',
        pub bounds: #[multi] TypeBound
            = ast::<TypeBound>().repeat_with_separator_at_least_once('+')
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TypeParameter {
        pub identifier: Identifier = expect::Identifier,
        pub bound: TypeParameterBound = ast::<TypeParameterBound>().optional(),
        pub default: Default<Type> = ast::<Default<Type>>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ConstantParameter {
        pub const_keyword: Keyword = expect::Keyword::Const,
        pub identifier: Identifier = expect::Identifier,
        pub colon: Punctuation = ':',
        pub r#type: Type = ast::<Type>(),
        pub default: Default<Expression>
            = ast::<Default<Expression>>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum GenericParameter {
        Lifetime(LifetimeParameter = ast::<LifetimeParameter>()),
        Type(TypeParameter = ast::<TypeParameter>()),
        Constant(ConstantParameter = ast::<ConstantParameter>())
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
        Serialize,
        Deserialize,
        StableHash,
    )]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    pub struct GenericParameters {
        pub parameters: #[multi] GenericParameter
            = ast::<GenericParameter>().repeat_all_with_separator(','),
    }
}

#[cfg(test)]
mod test;
