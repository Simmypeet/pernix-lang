use std::{
    fmt::{Debug, Display, Write},
    str::FromStr,
};

use enum_as_inner::EnumAsInner;
use pernixc_lexical::{token::KeywordKind, token_stream::TokenStream};
use pernixc_source::SourceFile;
use pernixc_system::diagnostic::Storage;
use pernixc_tests::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use super::{expression::tests::Expression, ty::tests::Type};
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

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
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

impl Input for Identifier {
    type Output = pernixc_lexical::token::Identifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(&self.string, output.span.str());
        Ok(())
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str(&self.string) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeArgumentIdentifier {
    Static,
    Identifier(Identifier),
}

impl Arbitrary for LifetimeArgumentIdentifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Static),
            Identifier::arbitrary().prop_map(Self::Identifier)
        ]
        .boxed()
    }
}
impl Display for LifetimeArgumentIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static => f.write_str("static"),
            Self::Identifier(ident) => Display::fmt(ident, f),
        }
    }
}

impl Input for LifetimeArgumentIdentifier {
    type Output = super::LifetimeArgumentIdentifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Static, super::LifetimeArgumentIdentifier::Static(..)) => Ok(()),
            (Self::Identifier(i), super::LifetimeArgumentIdentifier::Identifier(o)) => i.assert(o),
            (i, o) => Err(TestCaseError::fail(
                format!("Expected {i:?} but got {o:?}",),
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LifetimeArgument {
    pub lifetime_argument_identifier: LifetimeArgumentIdentifier,
}

impl Arbitrary for LifetimeArgument {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        LifetimeArgumentIdentifier::arbitrary()
            .prop_map(|identifier| Self {
                lifetime_argument_identifier: identifier,
            })
            .boxed()
    }
}

impl Input for LifetimeArgument {
    type Output = super::LifetimeArgument;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.lifetime_argument_identifier
            .assert(output.identifier())?;
        Ok(())
    }
}

impl Display for LifetimeArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('\'')?;
        Display::fmt(&self.lifetime_argument_identifier, f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ConstantPunctuation<const CHAR: char>;

impl<const CHAR: char> Arbitrary for ConstantPunctuation<CHAR> {
    type Parameters = ();
    type Strategy = Just<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy { Just(Self) }
}

impl<const CHAR: char> Input for ConstantPunctuation<CHAR> {
    type Output = pernixc_lexical::token::Punctuation;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
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

impl<T: Input, U: Input + Debug> Input for ConnectedList<T, U>
where
    U::Output: Debug,
{
    type Output = super::ConnectedList<T::Output, U::Output>;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.first.assert(output.first())?;

        prop_assert_eq!(self.rest.len(), output.rest().len());

        for (input, output) in self.rest.iter().zip(output.rest().iter()) {
            input.0.assert(&output.0)?;
            input.1.assert(&output.1)?;
        }

        self.trailing_separator
            .assert(output.trailing_separator())?;

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
            proptest::collection::vec((punctuation.clone(), element_strategy), 0..=6),
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
pub struct ConstArgument {
    pub expression: Box<Expression>,
}

impl Arbitrary for ConstArgument {
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

impl Input for ConstArgument {
    type Output = super::ConstantArgument;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.expression.assert(output.expression())?;
        Ok(())
    }
}

impl Display for ConstArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{{}}}", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum GenericArgument {
    Lifetime(LifetimeArgument),
    Type(Box<Type>),
    Const(ConstArgument),
}

impl Arbitrary for GenericArgument {
    type Parameters = <Type as Arbitrary>::Parameters;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            LifetimeArgument::arbitrary().prop_map(Self::Lifetime),
            Type::arbitrary_with(args.clone()).prop_map(|x| Self::Type(Box::new(x))),
            ConstArgument::arbitrary_with(args.1).prop_map(Self::Const),
        ]
        .boxed()
    }
}

impl Input for GenericArgument {
    type Output = super::GenericArgument;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Lifetime(i), super::GenericArgument::Lifetime(o)) => i.assert(o),
            (Self::Type(i), super::GenericArgument::Type(o)) => i.assert(o),
            (Self::Const(i), super::GenericArgument::Constant(o)) => i.assert(o),
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
            Self::Const(i) => Display::fmt(i, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArguments {
    pub turbofish: bool,

    pub argument_list: ConnectedList<GenericArgument, ConstantPunctuation<','>>,
}

impl Arbitrary for GenericArguments {
    type Parameters = (bool, <GenericArgument as Arbitrary>::Parameters);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (ConnectedList::arbitrary_with(
            GenericArgument::arbitrary_with(args.1),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(move |argument_list| Self {
            turbofish: args.0,
            argument_list,
        })
        .boxed()
    }
}

impl Input for GenericArguments {
    type Output = super::GenericArguments;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.turbofish, output.colon().is_some());

        self.argument_list.assert(output.argument_list())?;

        Ok(())
    }
}

impl Display for GenericArguments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.turbofish {
            f.write_char(':')?;
        }

        f.write_char('<')?;
        Display::fmt(&self.argument_list, f)?;
        f.write_char('>')?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericIdentifier {
    pub identifier: Identifier,

    pub generic_arguments: Option<GenericArguments>,
}

impl Arbitrary for GenericIdentifier {
    type Parameters = <GenericArguments as Arbitrary>::Parameters;
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

impl Input for GenericIdentifier {
    type Output = super::GenericIdentifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.identifier.assert(output.identifier())?;
        self.generic_arguments.assert(output.generic_arguments())
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QualifiedIdentifier {
    pub leading_scope_separator: bool,

    pub first: GenericIdentifier,

    pub rest: Vec<GenericIdentifier>,
}

impl Arbitrary for QualifiedIdentifier {
    type Parameters = (bool, Option<BoxedStrategy<Expression>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = (proptest::bool::ANY, Identifier::arbitrary()).prop_map(
            |(leading_separator, identifier)| Self {
                leading_scope_separator: leading_separator,
                first: GenericIdentifier {
                    identifier,
                    generic_arguments: None,
                },
                rest: Vec::new(),
            },
        );

        leaf.prop_recursive(4, 24, 6, move |inner| {
            let generic_identifier_strategy =
                GenericIdentifier::arbitrary_with((args.0, (Some(inner), args.1.clone()))).boxed();

            (
                proptest::bool::ANY,
                generic_identifier_strategy.clone(),
                proptest::collection::vec(generic_identifier_strategy, 0..=6),
            )
                .prop_map(|(leading_separator, first, rest)| Self {
                    leading_scope_separator: leading_separator,
                    first,
                    rest,
                })
        })
        .boxed()
    }
}

impl Input for QualifiedIdentifier {
    type Output = super::QualifiedIdentifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
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

impl Input for AccessModifier {
    type Output = super::AccessModifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Public, super::AccessModifier::Public(k)) => {
                prop_assert_eq!(k.keyword, pernixc_lexical::token::KeywordKind::Public);
            }
            (Self::Private, super::AccessModifier::Private(k)) => {
                prop_assert_eq!(k.keyword, pernixc_lexical::token::KeywordKind::Private);
            }
            (Self::Internal, super::AccessModifier::Internal(k)) => {
                prop_assert_eq!(k.keyword, pernixc_lexical::token::KeywordKind::Internal);
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

impl Input for Label {
    type Output = super::Label;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
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

proptest! {
    #[test]
    fn qualified_identifier_test(
        qualified_identifier_input in QualifiedIdentifier::arbitrary_with((true, None)),
    ) {
        let source = qualified_identifier_input.to_string();
        let qualified_identifier = parse(
            &source,
            |parser, handler| parser.parse_qualified_identifier(true, handler)
        )?;

        qualified_identifier_input.assert(&qualified_identifier)?;
    }
}
