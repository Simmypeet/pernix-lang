use std::fmt::{Display, Write};

use enum_as_inner::EnumAsInner;
use pernixc_tests::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    self,
    expression::tests::Expression,
    tests::{
        ConnectedList, ConstantPunctuation, GenericArgument, LifetimeArgument, QualifiedIdentifier,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Qualifier {
    Mutable,
    Restrict,
}

impl Arbitrary for Qualifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![Just(Self::Mutable), Just(Self::Restrict),].boxed()
    }
}

impl Input for Qualifier {
    type Output = super::Qualifier;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Mutable, super::Qualifier::Mutable(..))
            | (Self::Restrict, super::Qualifier::Restrict(..)) => Ok(()),

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
pub struct Reference {
    pub lifetime_argument: Option<LifetimeArgument>,
    pub qualifier: Option<Qualifier>,
    pub operand_type: Box<Type>,
}

impl Arbitrary for Reference {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(LifetimeArgument::arbitrary()),
            proptest::option::of(Qualifier::arbitrary()),
            args.unwrap_or_else(Type::arbitrary),
        )
            .prop_map(|(lifetime_argument, qualifier, operand_type)| Self {
                lifetime_argument,
                qualifier,
                operand_type: Box::new(operand_type),
            })
            .boxed()
    }
}

impl Input for Reference {
    type Output = super::Reference;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.lifetime_argument.assert(output.lifetime_argument())?;
        self.qualifier.assert(output.qualifier())?;
        self.operand_type.assert(output.operand())
    }
}

impl Display for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('&')?;

        if let Some(lifetime_argument) = &self.lifetime_argument {
            Display::fmt(lifetime_argument, f)?;
            f.write_char(' ')?;
        }

        if let Some(qualifier) = &self.qualifier {
            Display::fmt(qualifier, f)?;
            f.write_char(' ')?;
        }

        Display::fmt(&self.operand_type, f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Primitive {
    Bool,
    Float32,
    Float64,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Usize,
    Isize,
}

impl Arbitrary for Primitive {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Bool),
            Just(Self::Float32),
            Just(Self::Float64),
            Just(Self::Int8),
            Just(Self::Int16),
            Just(Self::Int32),
            Just(Self::Int64),
            Just(Self::Uint8),
            Just(Self::Uint16),
            Just(Self::Uint32),
            Just(Self::Uint64),
            Just(Self::Usize),
            Just(Self::Isize),
        ]
        .boxed()
    }
}

impl Input for Primitive {
    type Output = super::Primitive;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Bool, super::Primitive::Bool(..))
            | (Self::Float32, super::Primitive::Float32(..))
            | (Self::Float64, super::Primitive::Float64(..))
            | (Self::Int8, super::Primitive::Int8(..))
            | (Self::Int16, super::Primitive::Int16(..))
            | (Self::Int32, super::Primitive::Int32(..))
            | (Self::Int64, super::Primitive::Int64(..))
            | (Self::Uint8, super::Primitive::Uint8(..))
            | (Self::Uint16, super::Primitive::Uint16(..))
            | (Self::Uint32, super::Primitive::Uint32(..))
            | (Self::Uint64, super::Primitive::Uint64(..))
            | (Self::Usize, super::Primitive::Usize(..))
            | (Self::Isize, super::Primitive::Isize(..)) => Ok(()),

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Float32 => write!(f, "float32"),
            Self::Float64 => write!(f, "float64"),
            Self::Int8 => write!(f, "int8"),
            Self::Int16 => write!(f, "int16"),
            Self::Int32 => write!(f, "int32"),
            Self::Int64 => write!(f, "int64"),
            Self::Uint8 => write!(f, "uint8"),
            Self::Uint16 => write!(f, "uint16"),
            Self::Uint32 => write!(f, "uint32"),
            Self::Uint64 => write!(f, "uint64"),
            Self::Usize => write!(f, "usize"),
            Self::Isize => write!(f, "isize"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array {
    pub operand: Box<Type>,
    pub expression: Box<Expression>,
}

impl Arbitrary for Array {
    type Parameters = (
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<Expression>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            args.0
                .clone()
                .unwrap_or_else(|| Type::arbitrary_with((None, args.1.clone()))),
            args.1.unwrap_or_else(|| Expression::arbitrary_with(args.0)),
        )
            .prop_map(|(type_specifier, expression)| Self {
                operand: Box::new(type_specifier),
                expression: Box::new(expression),
            })
            .boxed()
    }
}

impl Input for Array {
    type Output = super::Array;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.operand.assert(output.operand())?;
        self.expression.assert(output.expression())
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;
        Display::fmt(&self.operand, f)?;
        f.write_str(": ")?;
        Display::fmt(&self.expression, f)?;
        f.write_char(']')?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer {
    pub operand: Box<Type>,
    pub qualifier: Option<Qualifier>,
}

impl Input for Pointer {
    type Output = super::Pointer;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.operand.assert(output.operand())?;
        self.qualifier.assert(output.qualifier())?;
        Ok(())
    }
}

impl Arbitrary for Pointer {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            args.unwrap_or_else(Type::arbitrary),
            proptest::option::of(Qualifier::arbitrary()),
        )
            .prop_map(|(operand, qualifier)| Self {
                operand: Box::new(operand),
                qualifier,
            })
            .boxed()
    }
}

impl Display for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('*')?;

        if let Some(qualifier) = &self.qualifier {
            Display::fmt(qualifier, f)?;
            f.write_char(' ')?;
        }

        Display::fmt(&self.operand, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unpackable {
    pub ellipsis: bool,
    pub ty: Box<Type>,
}

impl Input for Unpackable {
    type Output = super::Unpackable;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        prop_assert_eq!(self.ellipsis, output.ellipsis().is_some());
        self.ty.assert(output.ty())
    }
}

impl Arbitrary for Unpackable {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let ty = args.unwrap_or_else(Type::arbitrary);

        (proptest::bool::ANY, ty.prop_map(Box::new))
            .prop_map(|(ellipsis, ty)| Self { ellipsis, ty })
            .boxed()
    }
}

impl Display for Unpackable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ellipsis {
            f.write_str("...")?;
        }

        Display::fmt(&self.ty, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple {
    pub unpackable_list: Option<ConnectedList<Unpackable, ConstantPunctuation<','>>>,
}

impl Input for Tuple {
    type Output = super::Tuple;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        self.unpackable_list.assert(output.unpackable_list())
    }
}

impl Arbitrary for Tuple {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            Unpackable::arbitrary_with(args),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(|type_specifier_list| Self {
            unpackable_list: type_specifier_list,
        })
        .boxed()
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('(')?;
        if let Some(type_specifier_list) = &self.unpackable_list {
            Display::fmt(type_specifier_list, f)?;
        }
        f.write_char(')')?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From)]
#[allow(missing_docs)]
pub enum Type {
    Primitive(Primitive),
    Reference(Reference),
    QualifiedIdentifier(QualifiedIdentifier),
    Array(Array),
    Pointer(Pointer),
    Tuple(Tuple),
}

impl Input for Type {
    type Output = super::Type;

    fn assert(&self, output: &Self::Output) -> TestCaseResult {
        match (self, output) {
            (Self::Primitive(i), super::Type::Primitive(o)) => i.assert(o),
            (Self::Reference(i), super::Type::Reference(o)) => i.assert(o),
            (Self::QualifiedIdentifier(i), super::Type::QualifiedIdentifier(o)) => i.assert(o),
            (Self::Array(i), super::Type::Array(o)) => i.assert(o),
            (Self::Pointer(i), super::Type::Pointer(o)) => i.assert(o),
            (Self::Tuple(i), super::Type::Tuple(o)) => i.assert(o),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

fn remove_turbo_fish(qualified_identifier: &mut QualifiedIdentifier) {
    for generic_identifier in
        std::iter::once(&mut qualified_identifier.first).chain(qualified_identifier.rest.iter_mut())
    {
        if let Some(generic_arguments) = &mut generic_identifier.generic_arguments {
            generic_arguments.turbofish = false;

            for generic_argument in std::iter::once(&mut generic_arguments.argument_list.first)
                .chain(
                    generic_arguments
                        .argument_list
                        .rest
                        .iter_mut()
                        .map(|(_, a)| a),
                )
            {
                let GenericArgument::Type(q) = generic_argument else {
                    continue;
                };

                let Type::QualifiedIdentifier(ref mut q) = q.as_mut() else {
                    continue;
                };

                remove_turbo_fish(q);
            }
        }
    }
}

impl Arbitrary for Type {
    type Parameters = (
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Expression>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = Primitive::arbitrary().prop_map(Type::Primitive);

        leaf.prop_recursive(4, 24, 6, move |inner| {
            prop_oneof![
                Reference::arbitrary_with(Some(inner.clone())).prop_map(Type::Reference),
                args.0
                    .clone()
                    .unwrap_or_else(|| QualifiedIdentifier::arbitrary_with((false, args.1.clone())))
                    .prop_map(|mut x| {
                        remove_turbo_fish(&mut x);
                        Self::QualifiedIdentifier(x)
                    }),
                Array::arbitrary_with((Some(inner.clone()), args.1.clone())).prop_map(Type::Array),
                Pointer::arbitrary_with(Some(inner.clone())).prop_map(Type::Pointer),
                Tuple::arbitrary_with(Some(inner)).prop_map(Type::Tuple),
            ]
        })
        .boxed()
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(i) => Display::fmt(i, f),
            Self::Reference(i) => Display::fmt(i, f),
            Self::QualifiedIdentifier(i) => Display::fmt(i, f),
            Self::Array(i) => Display::fmt(i, f),
            Self::Pointer(i) => Display::fmt(i, f),
            Self::Tuple(i) => Display::fmt(i, f),
        }
    }
}

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn type_specifier_test(
        type_specifier_input in Type::arbitrary(),
    ) {
        let source = type_specifier_input.to_string();
        let type_specifier = syntax_tree::tests::parse(
            &source,
            |parser, handler| parser.parse_type(handler)
        )?;

        type_specifier_input.assert(&type_specifier)?;
    }
}
