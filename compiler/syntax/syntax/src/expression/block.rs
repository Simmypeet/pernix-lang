use enum_as_inner::EnumAsInner;
use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{ast, Parser as _},
};

use crate::{
    expression::{binary::Binary, Expression},
    pattern::{Irrefutable, Refutable},
    statement::Statements,
    Identifier, Keyword, Label, Passable, Punctuation, QualifiedIdentifier,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Block {
        Scope(Scope = ast::<Scope>()),
        IfElse(IfElse = ast::<IfElse>()),
        Loop(Loop = ast::<Loop>()),
        Match(Match = ast::<Match>()),
        While(While = ast::<While>()),
        Do(Do = ast::<Do>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Do {
        pub do_keyword: Keyword = expect::Keyword::Do,
        pub label: Label = ast::<Label>().optional(),
        pub statements: Statements = ast::<Statements>(),
        pub with: With = ast::<With>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct With {
        pub with_keyword: Keyword = expect::Keyword::With
            .new_line_significant(false),
        pub effect: QualifiedIdentifier =
            ast::<QualifiedIdentifier>(),
        pub body: WithBody = ast::<WithBody>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct HandlerArguments {
        pub irrefutable_patterns: #[multi] Irrefutable =
            ast::<Irrefutable>().repeat_all_with_separator(','),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Handler {
        pub identifier: Identifier = expect::Identifier,
        pub arguments: HandlerArguments = ast::<HandlerArguments>(),
        pub statements: Statements = ast::<Statements>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Indentation}
    pub struct WithBody {
        pub handlers: #[multi] Passable<Handler> =
            ast::<Passable<Handler>>().line().repeat_all(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Scope {
        pub unsafe_keyword: Keyword = expect::Keyword::Unsafe.optional(),
        pub scope_keyword: Keyword = expect::Keyword::Scope,
        pub label: Label = ast::<Label>().optional(),
        pub statements: Statements = ast::<Statements>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct IndentedGroup {
        pub unsafe_keyword: Keyword = expect::Keyword::Unsafe.optional(),
        pub label: Label = ast::<Label>().optional(),
        pub statements: Statements = ast::<Statements>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct InlineExpression {
        pub colon: Punctuation = ':',
        pub expression: Expression = ast::<Expression>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Group {
        Indented(IndentedGroup = ast::<IndentedGroup>()),
        Inline(InlineExpression = ast::<InlineExpression>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct IfElse {
        pub if_keyword: Keyword = expect::Keyword::If,
        pub binary: Binary = ast::<Binary>(),
        pub then: Group = ast::<Group>(),
        pub r#else: Else = ast::<Else>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum GroupOrIfElse {
        Group(Group = ast::<Group>()),
        IfElse(IfElse = ast::<IfElse>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Else {
        pub else_keyword: Keyword
            = expect::Keyword::Else.new_line_significant(false),
        pub group_or_if_else: GroupOrIfElse = ast::<GroupOrIfElse>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct MatchArm {
        pub refutable_pattern: Refutable = ast::<Refutable>(),
        pub group: Group = ast::<Group>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Indentation}
    pub struct MatchBody {
        pub arms: #[multi] Passable<MatchArm> = ast::<Passable<MatchArm>>()
            .line()
            .repeat_all(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Match {
        pub match_keyword: Keyword = expect::Keyword::Match,
        pub binary: Binary = ast::<Binary>(),
        pub body: MatchBody = ast::<MatchBody>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct While {
        pub while_keyword: Keyword = expect::Keyword::While,
        pub binary: Binary = ast::<Binary>(),
        pub group: IndentedGroup = ast::<IndentedGroup>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Loop {
        pub loop_keyword: Keyword = expect::Keyword::Loop,
        pub group: IndentedGroup = ast::<IndentedGroup>(),
    }
}
