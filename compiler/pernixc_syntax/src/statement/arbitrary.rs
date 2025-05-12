use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy},
    prop_oneof,
};

use crate::{
    arbitrary::{
        write_indent_line_for_indent_display, IndentDisplay, Passable,
        QualifiedIdentifier,
    },
    expression::arbitrary::Expression,
    pattern::arbitrary::Irrefutable,
    r#type::arbitrary::Type,
    reference,
};

reference! {
    #[derive(Debug, Clone)]
    pub struct TypeAnnotation for super::TypeAnnotation {
        pub r#type (Type)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct VariableDeclaration for super::VariableDeclaration {
        pub irrefutable_pattern (Irrefutable),
        pub type_annotation (Option<TypeAnnotation>),
        pub expression (Expression),
    }
}

impl Arbitrary for VariableDeclaration {
    type Parameters =
        (Option<BoxedStrategy<Expression>>, Option<BoxedStrategy<Type>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let expression = args.0.clone().unwrap_or_else(|| {
            Expression::arbitrary_with((args.1.clone(), None, None))
        });

        (
            Irrefutable::arbitrary(),
            proptest::option::of(args.1.unwrap_or_else(|| {
                Type::arbitrary_with((args.0.clone(), None))
            })),
            expression,
        )
            .prop_map(|(irrefutable_pattern, ty, expression)| Self {
                irrefutable_pattern,
                type_annotation: ty.map(|r#type| TypeAnnotation { r#type }),
                expression,
            })
            .boxed()
    }
}

impl IndentDisplay for VariableDeclaration {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        write!(f, "let {}", self.irrefutable_pattern)?;

        if let Some(type_annotation) = &self.type_annotation {
            f.write_str(": ")?;
            type_annotation.r#type.indent_fmt(f, indent)?;
        }

        f.write_str(" = ")?;
        self.expression.indent_fmt(f, indent)
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum Statement for super::Statement {
        VariableDeclaration(VariableDeclaration),
        Expression(Expression),
    }
}

impl Arbitrary for Statement {
    type Parameters =
        (Option<BoxedStrategy<Expression>>, Option<BoxedStrategy<Type>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            4 => args.0
                .clone()
                .unwrap_or_else(|| Expression::arbitrary_with((
                    args.1.clone(),
                    None,
                    None
                )))
                .prop_map(Self::Expression),
            1 => VariableDeclaration::arbitrary_with(args)
                .prop_map(Self::VariableDeclaration),
        ]
        .boxed()
    }
}

impl IndentDisplay for Statement {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::VariableDeclaration(v) => v.indent_fmt(f, indent),
            Self::Expression(e) => e.indent_fmt(f, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct Statements for super::Statements {
        pub statements (Vec<Passable<Statement>>)
    }
}

impl Arbitrary for Statements {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Statement>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let statement = args
            .3
            .unwrap_or_else(|| Statement::arbitrary_with((args.0, args.1)));

        proptest::collection::vec(
            prop_oneof![
                8 => statement.prop_map(Passable::Line),
                1 => Just(Passable::Pass)
            ],
            1..=10,
        )
        .prop_map(|statements| Self { statements })
        .boxed()
    }
}

impl IndentDisplay for Statements {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        writeln!(f, ":",)?;
        for statement in &self.statements {
            write_indent_line_for_indent_display(f, statement, indent + 1)?;
        }

        Ok(())
    }
}
