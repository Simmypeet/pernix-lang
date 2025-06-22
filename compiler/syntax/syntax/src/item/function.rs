use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree,
    expect::{self, Ext as _},
    parser::{ast, Parser as _},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    item::generic_parameters::GenericParameters, pattern::Irrefutable,
    r#type::Type, statement::Statement, AccessModifier, Identifier, Keyword,
    Punctuation,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Variadic {
        pub first_dot = '.',
        pub second_dot = '.'.no_prior_insignificant(),
        pub third_dot = '.'.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Parameter {
        pub irrefutable_pattern: Irrefutable = ast::<Irrefutable>(),
        pub colon: Punctuation = ':',
        pub r#type: Type = ast::<Type>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum ParameterKind {
        Regular(Parameter = ast::<Parameter>()),
        Variadic(Variadic = ast::<Variadic>()),
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
    )]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct Parameters {
        pub parameters: #[multi] ParameterKind
            = ast::<ParameterKind>().repeat_all_with_separator(','),
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
    )]
    pub struct ReturnType {
        pub dash = '-',
        pub greater_than = '>'.no_prior_insignificant(),
        pub r#type: Type = ast::<Type>(),
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
    )]
    pub struct Signature {
        pub function_keyword: Keyword = expect::Keyword::Function,
        pub identifier: Identifier = expect::Identifier,
        pub generic_parameters: GenericParameters
            = ast::<GenericParameters>().optional(),
        pub parameters: Parameters = ast::<Parameters>(),
        pub return_type: ReturnType = ast::<ReturnType>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Function {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub unsafe_keyword: Keyword = expect::Keyword::Unsafe.optional(),
        pub const_keyword: Keyword = expect::Keyword::Const.optional(),
        pub signature: Signature = ast::<Signature>(),
        pub body: super::Body<Statement> = ast::<super::Body<Statement>>(),
    }
}

pub type Body = super::Body<Statement>;

#[cfg(test)]
mod test;
