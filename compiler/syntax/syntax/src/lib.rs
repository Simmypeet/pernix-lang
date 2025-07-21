#![allow(missing_docs)]

//! Contains all the definitions of the syntax tree

use std::{path::Path, sync::Arc};

use enum_as_inner::EnumAsInner;
use expression::Expression;
use pernixc_lexical::{
    kind,
    token::Token,
    tree::{DelimiterKind, RelativeLocation},
};
use pernixc_parser::{
    abstract_tree::{self, AbstractTree},
    expect::{self, Ext as _},
    parser::{ast, Parser as _},
};
use pernixc_query::{
    runtime::{
        executor::CyclicError,
        persistence::{serde::DynamicRegistry, Persistence},
    },
    TrackedEngine,
};
use pernixc_serialize::{
    de::Deserializer, ser::Serializer, Deserialize, Serialize,
};
use pernixc_stable_hash::StableHash;
use pernixc_target::TargetID;
use r#type::Type;

pub mod expression;
pub mod item;
pub mod pattern;
pub mod predicate;
pub mod statement;
pub mod r#type;

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// Registers all the required executors to run the queries.
pub fn register_executors(
    executor: &mut pernixc_query::runtime::executor::Registry,
) {
    executor.register(Arc::new(Executor));
    executor.register(Arc::new(DiagnosticExecutor));
}

/// Registers all the necessary runtime information for the query engine.
pub fn register_serde<
    S: Serializer<Registry>,
    D: Deserializer<Registry>,
    Registry: DynamicRegistry<S, D> + Send + Sync,
>(
    serde_registry: &mut Registry,
) where
    S::Error: Send + Sync,
{
    serde_registry.register::<Key>();
    serde_registry.register::<DiagnosticKey>();
}

/// Registers the keys that should be skipped during serialization and
/// deserialization in the query engine's persistence layer
pub const fn skip_persistence(_persistence: &mut Persistence) {}

/// Type alias for [`Token`] categorized as a [`kind::Keyword`].
pub type Keyword = Token<kind::Keyword, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::NewLine`].
pub type NewLine = Token<kind::NewLine, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::Character`].
pub type Character = Token<kind::Character, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::String`].
pub type String = Token<kind::String, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::Identifier`].
pub type Identifier = Token<kind::Identifier, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::Punctuation`].
pub type Punctuation = Token<kind::Punctuation, RelativeLocation>;

/// Type alias for [`Token`] categorized as a [`kind::Numeric`].
pub type Numeric = Token<kind::Numeric, RelativeLocation>;

abstract_tree::abstract_tree! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        EnumAsInner,
        Serialize,
        Deserialize,
        StableHash
    )]
    pub enum AccessModifier {
        Public(Keyword = expect::Keyword::Public),
        Private(Keyword = expect::Keyword::Private),
        Internal(Keyword = expect::Keyword::Internal)
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ScopeSeparator {
        pub first_colon = ':',
        pub second_colon = ':'.no_prior_insignificant()
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
    pub struct Elided {
        pub first_dot = '.',
        pub second_dot = '.'.no_prior_insignificant()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum LifetimeIdentifier {
        Identifier(Identifier = expect::Identifier),
        Static(Keyword = expect::Keyword::Static),
        Elided(Elided = ast::<Elided>())
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Lifetime {
        pub apostrophe: Punctuation = '\'',
        pub identifier: LifetimeIdentifier = ast::<LifetimeIdentifier>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum GenericArgument {
        Lifetime(Lifetime = ast::<Lifetime>()),
        Type(Type = ast::<Type>()),
        Constant(ConstantArgument = ast::<ConstantArgument>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    pub struct GenericArguments {
        pub arguments: #[multi] GenericArgument
            = ast::<GenericArgument>().repeat_all_with_separator(',')
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct GenericIdentifier {
        pub identifier: Identifier = expect::Identifier,
        pub generic_arguments: GenericArguments
            = ast::<GenericArguments>().optional()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct LifetimeParameter {
        pub apostrophe: Punctuation = '\'',
        pub identifier: Identifier = expect::Identifier,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum SimplePathRoot {
        Target(Keyword = expect::Keyword::Target),
        Identifier(Identifier = expect::Identifier),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct SimplePathSubsequent {
        pub scope_separator: ScopeSeparator = ast::<ScopeSeparator>(),
        pub identifier: Identifier = expect::Identifier,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct SimplePath {
        pub root: SimplePathRoot = ast::<SimplePathRoot>(),
        pub subsequences: #[multi] SimplePathSubsequent
            = ast::<SimplePathSubsequent>().repeat()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum QualifiedIdentifierRoot {
        Target(Keyword = expect::Keyword::Target),
        This(Keyword = expect::Keyword::This),
        GenericIdentifier(GenericIdentifier = ast::<GenericIdentifier>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct QualifiedIdentifierSubsequent {
        pub scope_separator: ScopeSeparator = ast::<ScopeSeparator>(),
        pub generic_identifier: GenericIdentifier
            = ast::<GenericIdentifier>(),
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
    pub struct QualifiedIdentifier {
        pub root: QualifiedIdentifierRoot = ast::<QualifiedIdentifierRoot>(),
        pub subsequences: #[multi] QualifiedIdentifierSubsequent
            = ast::<QualifiedIdentifierSubsequent>().repeat()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Label {
        pub apostrophe: Punctuation = '\'',
        pub identifier: Identifier = expect::Identifier,
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ReferenceOf {
        pub ampersand: Punctuation = '&',
        pub mut_keyword: Keyword = expect::Keyword::Mut.optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Ellipsis {
        pub first_dot: Punctuation = '.',
        pub second_dot: Punctuation = '.'.no_prior_insignificant(),
        pub third_dot: Punctuation = '.'.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Brace)}
    pub enum ConstantArgument {
        Expression(Expression =  ast::<Expression>()),
        Elided(Elided = ast::<Elided>()),
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
        EnumAsInner
    )]
    pub enum Passable<T: 'static + AbstractTree> {
        Pass(Keyword = expect::Keyword::Pass),
        Line(T = ast::<T>())
    }
}

/// Query for parsing a token tree from the given source file path.
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
    pernixc_query::Key,
)]
#[value(
    Result<
        (Option<item::module::Content>, Arc<[pernixc_parser::error::Error]>),
        pernixc_source_file::Error
    >
)]
pub struct Key {
    /// The path to load the source file.
    pub path: Arc<Path>,

    /// The target ID that requested the source file parsing.
    pub target_id: TargetID,
}

#[pernixc_query::executor(key(Key), name(Executor))]
#[allow(clippy::type_complexity)]
pub fn parse_executor(
    key: &Key,
    engine: &TrackedEngine,
) -> Result<
    Result<
        (Option<item::module::Content>, Arc<[pernixc_parser::error::Error]>),
        pernixc_source_file::Error,
    >,
    CyclicError,
> {
    // load the token tree
    let token_tree = match engine.query(&pernixc_lexical::Key {
        path: key.path.clone(),
        target_id: key.target_id,
    })? {
        Ok(source_code) => source_code,
        Err(error) => return Ok(Err(error)),
    };

    let (module, errors) = item::module::Content::parse(&token_tree.0);

    Ok(Ok((module, Arc::from(errors.into_boxed_slice()))))
}

/// A key for retrieving the diagnostics that occurred while parsing the token
/// tree from the source file.
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
    pernixc_query::Key,
)]
#[value(Result<Arc<[pernixc_parser::error::Error]>, pernixc_source_file::Error>)]
pub struct DiagnosticKey(pub Key);

#[pernixc_query::executor(key(DiagnosticKey), name(DiagnosticExecutor))]
#[allow(clippy::type_complexity)]
pub fn diagnostic_executor(
    DiagnosticKey(key): &DiagnosticKey,
    engine: &TrackedEngine,
) -> Result<
    Result<Arc<[pernixc_parser::error::Error]>, pernixc_source_file::Error>,
    CyclicError,
> {
    let token_tree = match engine.query(key)? {
        Ok(token_tree) => token_tree,
        Err(error) => return Ok(Err(error)),
    };

    Ok(Ok(token_tree.1))
}

#[cfg(test)]
mod test;
