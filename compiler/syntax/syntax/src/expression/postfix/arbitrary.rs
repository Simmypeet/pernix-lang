use std::fmt::{Display, Write};

use enum_as_inner::EnumAsInner;
use pernixc_lexical::kind::{self, arbitrary::Numeric};
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Strategy as _},
    prop_oneof,
};

use crate::{
    arbitrary::{GenericIdentifier, IndentDisplay, QualifiedIdentifier},
    expression::{
        arbitrary::{Call, Expression},
        unit::arbitrary::Unit,
    },
    reference,
    r#type::arbitrary::Type,
};

reference! {
    #[derive(Debug, Clone)]
    pub struct Postfix for super::Postfix {
        pub unit (Unit),
        pub operators (Vec<Operator>),
    }
}

impl IndentDisplay for Postfix {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        self.unit.indent_fmt(f, indent)?;

        for operator in &self.operators {
            operator.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

impl Arbitrary for Postfix {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((expr, ty, qi): Self::Parameters) -> Self::Strategy {
        let unit_strategy =
            Unit::arbitrary_with((expr.clone(), ty.clone(), qi.clone()));

        let operators_strategy = proptest::collection::vec(
            Operator::arbitrary_with((expr, ty, qi)),
            0..4,
        );

        (unit_strategy, operators_strategy)
            .prop_map(|(unit, operators)| Self { unit, operators })
            .prop_filter(
                "filter out possible of numeric with no decimal with tuple \
                 dot access",
                |x| {
                    let unit_is_numeric =
                        x.unit.as_numeric().is_some_and(|x| {
                            x.decimal.is_none() && x.decimal.is_none()
                        });

                    let is_tuple_dot_access = x
                        .operators
                        .first()
                        .and_then(|x| x.as_access())
                        .is_some_and(|x| {
                            x.kind.as_tuple_index().is_some_and(|x| !x.minus)
                        });

                    // 123.456 this can be interpreted as a decimal number
                    // or a numeric `123` in tuple access at index `456`
                    !(unit_is_numeric && is_tuple_dot_access)
                },
            )
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Cast for super::Cast {
        pub r#type (Type)
    }
}

impl IndentDisplay for Cast {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str(" as ")?;
        self.r#type.indent_fmt(f, indent)
    }
}

impl Arbitrary for Cast {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((expr, ty, qi): Self::Parameters) -> Self::Strategy {
        let type_strategy =
            ty.unwrap_or_else(|| Type::arbitrary_with((expr, qi)));

        type_strategy.prop_map(|r#type| Self { r#type }).boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{}{index}",
        if *minus { "-" } else { "" },
    )]
    pub struct TupleIndex for super::TupleIndex {
        pub minus (bool),

        #{map_input_assert(index, &index.kind)}
        pub index (Numeric)
    }
}

impl Arbitrary for TupleIndex {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (bool::arbitrary(), Numeric::arbitrary())
            .prop_map(|(minus, index)| Self { minus, index })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct ArrayIndex for super::ArrayIndex {
        pub expression (Expression)
    }
}

impl IndentDisplay for ArrayIndex {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('[')?;
        self.expression.indent_fmt(f, indent)?;
        f.write_char(']')
    }
}

impl Arbitrary for ArrayIndex {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(expr: Self::Parameters) -> Self::Strategy {
        let expr = expr.unwrap_or_else(Expression::arbitrary);

        expr.prop_map(|expression| Self { expression }).boxed()
    }
}

reference! {
    #[derive(Debug, Clone, EnumAsInner)]
    pub enum AccessKind for super::AccessKind {
        #{map_input_assert(StructField, &StructField.kind)}
        StructField(kind::Identifier),
        TupleIndex(TupleIndex),
        ArrayIndex(ArrayIndex),
    }
}

impl IndentDisplay for AccessKind {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::StructField(x) => x.fmt(f),
            Self::TupleIndex(x) => x.fmt(f),
            Self::ArrayIndex(x) => x.indent_fmt(f, indent),
        }
    }
}

impl Arbitrary for AccessKind {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(expression: Self::Parameters) -> Self::Strategy {
        let expression = expression.unwrap_or_else(Expression::arbitrary);
        prop_oneof![
            kind::Identifier::arbitrary().prop_map(Self::StructField),
            TupleIndex::arbitrary().prop_map(Self::TupleIndex),
            ArrayIndex::arbitrary_with(Some(expression))
                .prop_map(Self::ArrayIndex),
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Access for super::Access {
        pub kind (AccessKind),
    }
}

impl IndentDisplay for Access {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('.')?;
        self.kind.indent_fmt(f, indent)
    }
}

impl Arbitrary for Access {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(expression: Self::Parameters) -> Self::Strategy {
        AccessKind::arbitrary_with(expression)
            .prop_map(|kind| Self { kind })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct MethodCall for super::MethodCall {
        pub generic_identifier (GenericIdentifier),
        pub call (Call),
    }
}

impl IndentDisplay for MethodCall {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('.')?;
        self.generic_identifier.indent_fmt(f, indent)?;
        self.call.indent_fmt(f, indent)
    }
}

impl Arbitrary for MethodCall {
    type Parameters =
        (Option<BoxedStrategy<Expression>>, Option<BoxedStrategy<Type>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((expression, ty): Self::Parameters) -> Self::Strategy {
        (
            GenericIdentifier::arbitrary_with((ty, expression.clone())),
            Call::arbitrary_with(expression),
        )
            .prop_map(|(generic_identifier, call)| Self {
                generic_identifier,
                call,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, EnumAsInner)]
    pub enum Operator for super::Operator {
        MethodCall(MethodCall),
        Cast(Cast),
        Access(Access),
    }
}

impl IndentDisplay for Operator {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::MethodCall(x) => x.indent_fmt(f, indent),
            Self::Cast(x) => x.indent_fmt(f, indent),
            Self::Access(x) => x.indent_fmt(f, indent),
        }
    }
}

impl Arbitrary for Operator {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(
        (expression, ty, qi): Self::Parameters,
    ) -> Self::Strategy {
        prop_oneof![
            MethodCall::arbitrary_with((expression.clone(), ty.clone()))
                .prop_map(Self::MethodCall),
            Cast::arbitrary_with((expression.clone(), ty, qi))
                .prop_map(Self::Cast),
            Access::arbitrary_with(expression).prop_map(Self::Access),
        ]
        .boxed()
    }
}
