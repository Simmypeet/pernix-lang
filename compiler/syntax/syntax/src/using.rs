use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{ParserExt, ast},
};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Identifier, Keyword, Punctuation, QualifiedIdentifier,
    predicate::HigherRankedLifetimes,
};

abstract_tree::abstract_tree! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        Encode,
        Decode,
        StableHash,
    )]
    pub struct ContextParameterName {
        pub identifier: Identifier = expect::Identifier,
        pub colon: Punctuation = ':',
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
        Encode,
        Decode,
        StableHash,
    )]
    pub struct Trait {
        pub higher_ranked_lifetimes: HigherRankedLifetimes
            = ast::<HigherRankedLifetimes>().optional(),
        pub qualified_identifier: QualifiedIdentifier
            = ast::<QualifiedIdentifier>(),
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
        Encode,
        Decode,
        StableHash,
    )]
    pub struct ContextParameter {
        pub name: ContextParameterName = ast::<ContextParameterName>()
            .optional(),
        pub r#trait: Trait = ast::<Trait>(),
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
        Encode,
        Decode,
        StableHash,
    )]
    pub struct ContextParameters {
        pub parameters: #[multi] ContextParameter
            = ast::<ContextParameter>().repeat_with_separator_at_least_once(','),
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
        Encode,
        Decode,
        StableHash,
    )]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct ParenthesizedContextParameters {
        pub parameters: #[multi] ContextParameter
            = ast::<ContextParameter>().repeat_with_separator_at_least_once(','),
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
        Encode,
        Decode,
        StableHash,
    )]
    pub enum ContextParametersKind {
        Regular(ContextParameters = ast::<ContextParameters>()),
        Parenthesized(
            ParenthesizedContextParameters = ast::<ParenthesizedContextParameters>()
        ),
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
        Encode,
        Decode,
        StableHash,
    )]
    pub struct UsingClause {
        pub using_keyword: Keyword = expect::Keyword::Using,
        pub context_parameters: ContextParametersKind
            = ast::<ContextParametersKind>(),
    }
}

