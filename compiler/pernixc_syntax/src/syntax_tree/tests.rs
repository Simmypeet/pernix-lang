use std::{
    fmt::{Debug, Display, Write},
    str::FromStr,
};

use enum_as_inner::EnumAsInner;
use pernixc_base::{diagnostic::Storage, source_file::SourceFile};
use pernixc_lexical::{
    token::{self, KeywordKind, Punctuation},
    token_stream::TokenStream,
};
use pernixc_tests::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::{expression::tests::Expression, r#type::tests::Type};
use crate::{
    error::{self},
    parser::Parser,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    pub string: String,
}

impl Arbitrary for Identifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        "[a-zA-Z_][a-zA-Z0-9_]*"
            .prop_map(|string| Self { string })
            .prop_filter_map("filter out keywords", |x| {
                if KeywordKind::from_str(&x.string).is_ok() {
                    None
                } else {
                    Some(x)
                }
            })
            .boxed()
    }
}

impl Input<&pernixc_lexical::token::Identifier> for &Identifier {
    fn assert(self, output: &pernixc_lexical::token::Identifier) -> TestCaseResult {
        prop_assert_eq!(&self.string, output.span.str());
        Ok(())
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str(&self.string) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeIdentifier {
    Static,
    Identifier(Identifier),
}

impl Arbitrary for LifetimeIdentifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Static),
            Identifier::arbitrary().prop_map(Self::Identifier)
        ]
        .boxed()
    }
}
impl Display for LifetimeIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static => f.write_str("static"),
            Self::Identifier(ident) => Display::fmt(ident, f),
        }
    }
}

impl Input<&super::LifetimeIdentifier> for &LifetimeIdentifier {
    fn assert(self, output: &super::LifetimeIdentifier) -> TestCaseResult {
        match (self, output) {
            (LifetimeIdentifier::Static, super::LifetimeIdentifier::Static(..)) => Ok(()),
            (LifetimeIdentifier::Identifier(i), super::LifetimeIdentifier::Identifier(o)) => {
                i.assert(o)
            }
            (i, o) => Err(TestCaseError::fail(
                format!("Expected {i:?} but got {o:?}",),
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lifetime {
    pub identifier: LifetimeIdentifier,
}

impl Arbitrary for Lifetime {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        LifetimeIdentifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

impl Input<&super::Lifetime> for &Lifetime {
    fn assert(self, output: &super::Lifetime) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        Ok(())
    }
}

impl Display for Lifetime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('\'')?;
        Display::fmt(&self.identifier, f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ConstantPunctuation<const CHAR: char>;

impl<const CHAR: char> Arbitrary for ConstantPunctuation<CHAR> {
    type Parameters = ();
    type Strategy = Just<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy { Just(Self) }
}

impl<const CHAR: char> Input<&Punctuation> for &ConstantPunctuation<CHAR> {
    fn assert(self, output: &Punctuation) -> TestCaseResult {
        prop_assert_eq!(CHAR, output.punctuation);
        Ok(())
    }
}

impl<const CHAR: char> Display for ConstantPunctuation<CHAR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_char(CHAR) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConnectedList<T, U> {
    pub first: T,
    pub rest: Vec<(U, T)>,
    pub trailing_separator: Option<U>,
}

impl<T: Debug, U: Debug, V: Debug, W: Debug> Input<&super::ConnectedList<T, U>>
    for &ConnectedList<V, W>
where
    for<'a, 'b> &'a V: Input<&'b T>,
    for<'a, 'b> &'a W: Input<&'b U>,
{
    fn assert(self, output: &super::ConnectedList<T, U>) -> TestCaseResult {
        self.first.assert(output.first())?;
        self.rest.assert(output.rest())?;
        self.trailing_separator
            .as_ref()
            .assert(output.trailing_separator().as_ref())?;

        Ok(())
    }
}

impl<T: Display, U: Display> Display for ConnectedList<T, U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.first, f)?;

        for (separator, element) in &self.rest {
            Display::fmt(separator, f)?;
            f.write_char(' ')?;
            Display::fmt(element, f)?;
        }

        if let Some(separator) = self.trailing_separator.as_ref() {
            Display::fmt(separator, f)?;
        }

        Ok(())
    }
}

impl<T: Debug, U: Debug> ConnectedList<T, U> {
    pub fn arbitrary_with(
        element_strategy: impl Strategy<Value = T> + Clone,
        punctuation: impl Strategy<Value = U> + Clone,
    ) -> impl Strategy<Value = Self> {
        (
            element_strategy.clone(),
            proptest::collection::vec((punctuation.clone(), element_strategy), 0..=7),
            proptest::option::of(punctuation),
        )
            .prop_map(|(first, rest, trailing_separator)| Self {
                first,
                rest,
                trailing_separator,
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantArgument {
    pub expression: Box<Expression>,
}

impl Arbitrary for ConstantArgument {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        args.unwrap_or_else(|| Expression::arbitrary().boxed())
            .prop_map(|expression| Self {
                expression: Box::new(expression),
            })
            .boxed()
    }
}

impl Input<&super::ConstantArgument> for &ConstantArgument {
    fn assert(self, output: &super::ConstantArgument) -> TestCaseResult {
        self.expression.assert(output.expression())?;
        Ok(())
    }
}

impl Display for ConstantArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{{}}}", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum GenericArgument {
    Lifetime(Lifetime),
    Type(Box<Type>),
    Constant(ConstantArgument),
}

impl Arbitrary for GenericArgument {
    type Parameters = (
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Expression>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((type_arg, expr_arg): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Lifetime::arbitrary().prop_map(Self::Lifetime),
            type_arg
                .clone()
                .unwrap_or_else(|| Type::arbitrary_with((expr_arg.clone(), None)))
                .prop_map(|x| Self::Type(Box::new(x))),
            expr_arg
                .unwrap_or_else(|| Expression::arbitrary_with((type_arg, None, None)))
                .prop_map(|x| Self::Constant(ConstantArgument {
                    expression: Box::new(x)
                })),
        ]
        .boxed()
    }
}

impl Input<&super::GenericArgument> for &GenericArgument {
    fn assert(self, output: &super::GenericArgument) -> TestCaseResult {
        match (self, output) {
            (GenericArgument::Lifetime(i), super::GenericArgument::Lifetime(o)) => i.assert(o),
            (GenericArgument::Type(i), super::GenericArgument::Type(o)) => i.assert(o),
            (GenericArgument::Constant(i), super::GenericArgument::Constant(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

impl Display for GenericArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lifetime(i) => Display::fmt(i, f),
            Self::Type(i) => Display::fmt(i, f),
            Self::Constant(i) => Display::fmt(i, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArguments {
    pub argument_list: Option<ConnectedList<GenericArgument, ConstantPunctuation<','>>>,
}

impl Arbitrary for GenericArguments {
    type Parameters = <GenericArgument as Arbitrary>::Parameters;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            GenericArgument::arbitrary_with(args),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(move |argument_list| Self { argument_list })
        .boxed()
    }
}

impl Input<&super::GenericArguments> for &GenericArguments {
    fn assert(self, output: &super::GenericArguments) -> TestCaseResult {
        self.argument_list
            .as_ref()
            .assert(output.argument_list().as_ref())?;

        Ok(())
    }
}

impl Display for GenericArguments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;
        if let Some(argument_list) = self.argument_list.as_ref() {
            Display::fmt(argument_list, f)?;
        }
        f.write_char(']')?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericIdentifier {
    pub identifier: Identifier,
    pub generic_arguments: Option<GenericArguments>,
}

impl Arbitrary for GenericIdentifier {
    type Parameters = (
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Expression>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(GenericArguments::arbitrary_with(args)),
        )
            .prop_map(|(identifier, generic_arguments)| Self {
                identifier,
                generic_arguments,
            })
            .boxed()
    }
}

impl Input<&super::GenericIdentifier> for &GenericIdentifier {
    fn assert(self, output: &super::GenericIdentifier) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_arguments
            .as_ref()
            .assert(output.generic_arguments().as_ref())
    }
}

impl Display for GenericIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.identifier, f)?;

        if let Some(generic_arguments) = self.generic_arguments.as_ref() {
            Display::fmt(generic_arguments, f)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Qualifier {
    Mutable,
    Restrict,
}

impl Arbitrary for Qualifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![Just(Self::Mutable), Just(Self::Restrict),].boxed()
    }
}

impl Input<&super::Qualifier> for &Qualifier {
    fn assert(self, output: &super::Qualifier) -> TestCaseResult {
        match (self, output) {
            (Qualifier::Mutable, super::Qualifier::Mutable(..))
            | (Qualifier::Restrict, super::Qualifier::Restrict(..)) => Ok(()),

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

impl Display for Qualifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mutable => f.write_str("mutable"),
            Self::Restrict => f.write_str("restrict"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QualifiedIdentifier {
    pub leading_scope_separator: bool,
    pub first: GenericIdentifier,
    pub rest: Vec<GenericIdentifier>,
}

impl Arbitrary for QualifiedIdentifier {
    type Parameters = (
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Expression>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let generic_identifier_strategy = GenericIdentifier::arbitrary_with(args).boxed();
        (
            proptest::bool::ANY,
            generic_identifier_strategy.clone(),
            proptest::collection::vec(generic_identifier_strategy, 0..=6),
        )
            .prop_map(|(leading_scope_separator, first, rest)| Self {
                leading_scope_separator,
                first,
                rest,
            })
            .boxed()
    }
}

impl Input<&super::QualifiedIdentifier> for &QualifiedIdentifier {
    fn assert(self, output: &super::QualifiedIdentifier) -> TestCaseResult {
        prop_assert_eq!(
            self.leading_scope_separator,
            output.leading_scope_separator().is_some()
        );

        self.first.assert(output.first())?;

        prop_assert_eq!(self.rest.len(), output.rest().len());

        for (input, (_, output)) in self.rest.iter().zip(output.rest().iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}

impl Display for QualifiedIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.leading_scope_separator {
            f.write_str("::")?;
        }

        Display::fmt(&self.first, f)?;
        for identifier in &self.rest {
            f.write_str("::")?;
            Display::fmt(identifier, f)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum AccessModifier {
    Public,
    Private,
    Internal,
}

impl Input<&super::AccessModifier> for &AccessModifier {
    fn assert(self, output: &super::AccessModifier) -> TestCaseResult {
        match (self, output) {
            (AccessModifier::Public, super::AccessModifier::Public(k)) => {
                prop_assert_eq!(k.kind, pernixc_lexical::token::KeywordKind::Public);
            }
            (AccessModifier::Private, super::AccessModifier::Private(k)) => {
                prop_assert_eq!(k.kind, pernixc_lexical::token::KeywordKind::Private);
            }
            (AccessModifier::Internal, super::AccessModifier::Internal(k)) => {
                prop_assert_eq!(k.kind, pernixc_lexical::token::KeywordKind::Internal);
            }
            _ => {
                return Err(TestCaseError::fail(format!(
                    "Expected {self:?}, got {output:?}"
                )))
            }
        }

        Ok(())
    }
}

impl Arbitrary for AccessModifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Public),
            Just(Self::Private),
            Just(Self::Internal),
        ]
        .boxed()
    }
}

impl Display for AccessModifier {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Public => write!(formatter, "public"),
            Self::Private => write!(formatter, "private"),
            Self::Internal => write!(formatter, "internal"),
        }
    }
}

pub fn parse<T, F>(source: &str, f: F) -> Result<T, TestCaseError>
where
    F: FnOnce(&mut Parser, &Storage<error::Error>) -> Option<T>,
{
    let source_file = SourceFile::temp(source.to_string())?;

    let storage: Storage<pernixc_lexical::error::Error> = Storage::new();

    let token_stream = TokenStream::tokenize(&source_file, &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::reject(format!(
            "found lexical error(s): {:#?};\nsource: {source}",
            storage.as_vec(),
        )));
    }

    let mut parser = Parser::new(&token_stream);

    let storage: Storage<error::Error> = Storage::new();
    let output = f(&mut parser, &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::fail(format!(
            "found syntax error(s): {:#?};\nsource: {source}",
            storage.as_vec(),
        )));
    }

    output.map_or_else(
        || {
            Err(TestCaseError::fail(format!(
                "failed to parse the source code: {source}",
            )))
        },
        |output| Ok(output),
    )
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label {
    pub identifier: Identifier,
}

impl Input<&super::Label> for &Label {
    fn assert(self, output: &super::Label) -> TestCaseResult {
        self.identifier.assert(output.identifier())
    }
}

impl Arbitrary for Label {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.identifier)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    pub integer: u64,
}

impl Input<&token::Numeric> for &Numeric {
    fn assert(self, output: &token::Numeric) -> TestCaseResult {
        output.span.str().parse::<u64>().map_or_else(
            |_| {
                Err(TestCaseError::fail(format!(
                    "expected {self:?}, got {output:?}",
                )))
            },
            |x| {
                prop_assert_eq!(self.integer, x);
                Ok(())
            },
        )
    }
}

impl Arbitrary for Numeric {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::num::u64::ANY
            .prop_map(|integer| Self { integer })
            .boxed()
    }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.integer, f)
    }
}

proptest! {
    #[allow(clippy::ignored_unit_patterns)]
    #[test]
    fn qualified_identifier_test(
        qualified_identifier_input in QualifiedIdentifier::arbitrary(),
    ) {
        let source = qualified_identifier_input.to_string();
        let qualified_identifier = parse(
            &source,
            |parser, handler| parser.parse_qualified_identifier(handler)
        )?;

        qualified_identifier_input.assert(&qualified_identifier)?;
    }
}
