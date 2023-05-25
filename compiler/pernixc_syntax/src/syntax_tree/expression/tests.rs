use proptest::{
    prop_assert_eq, prop_oneof, proptest,
    strategy::{Just, Strategy},
    test_runner::TestCaseError,
};

use super::{
    BooleanLiteral, Expression, Functional, MemberAccess, NumericLiteral, Prefix, PrefixOperator,
};

#[derive(Debug, Clone, Copy)]
pub struct NumericLiteralInput {
    pub number: u64,
}

impl ToString for NumericLiteralInput {
    fn to_string(&self) -> String { self.number.to_string() }
}

#[derive(Debug, Clone)]
pub struct MemberAccessInput {
    pub identifier: String,
    pub operand: Box<ExpressionInput>,
}

impl ToString for MemberAccessInput {
    fn to_string(&self) -> String { format!("{}.{}", self.operand.to_string(), self.identifier) }
}

#[derive(Debug, Clone, Copy)]
pub struct BooleanLiteralInput {
    pub value: bool,
}

impl ToString for BooleanLiteralInput {
    fn to_string(&self) -> String { self.value.to_string() }
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOperatorInput {
    LogicalNot,
    Negate,
}
impl ToString for PrefixOperatorInput {
    fn to_string(&self) -> String {
        match self {
            Self::LogicalNot => "!",
            Self::Negate => "-",
        }
        .to_string()
    }
}

#[derive(Debug, Clone)]
pub struct PrefixInput {
    pub prefix_operator: PrefixOperatorInput,
    pub operand: Box<ExpressionInput>,
}

impl ToString for PrefixInput {
    fn to_string(&self) -> String {
        format!(
            "{}{}",
            self.prefix_operator.to_string(),
            self.operand.to_string()
        )
    }
}

#[derive(Debug, Clone)]
pub enum FunctionalInput {
    NumericLiteral(NumericLiteralInput),
    Prefix(PrefixInput),
    BooleanLiteral(BooleanLiteralInput),
    MemberAccessInput(MemberAccessInput),
}

impl ToString for FunctionalInput {
    fn to_string(&self) -> String {
        match self {
            Self::NumericLiteral(n) => n.to_string(),
            Self::Prefix(p) => p.to_string(),
            Self::BooleanLiteral(p) => p.to_string(),
            Self::MemberAccessInput(p) => p.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionInput {
    Functional(FunctionalInput),
}

impl ToString for ExpressionInput {
    fn to_string(&self) -> String {
        match self {
            Self::Functional(f) => f.to_string(),
        }
    }
}

pub fn expression_strategy() -> impl Strategy<Value = ExpressionInput> {
    prop_oneof![
        proptest::num::u64::ANY.prop_map(|number| ExpressionInput::Functional(
            FunctionalInput::NumericLiteral(NumericLiteralInput { number })
        )),
        proptest::bool::ANY.prop_map(|value| ExpressionInput::Functional(
            FunctionalInput::BooleanLiteral(BooleanLiteralInput { value })
        ))
    ]
    .prop_recursive(8, 24, 10, |x| {
        // prefix
        prop_oneof![
            (
                prop_oneof![
                    Just(PrefixOperatorInput::LogicalNot),
                    Just(PrefixOperatorInput::Negate)
                ],
                x.clone()
            )
                .prop_map(|(prefix_operator, expression)| {
                    ExpressionInput::Functional(FunctionalInput::Prefix(PrefixInput {
                        prefix_operator,
                        operand: Box::new(expression),
                    }))
                }),
            // member access
            (pernixc_lexical::token::strategy::identifier(), x).prop_map(
                |(identifier, operand)| {
                    ExpressionInput::Functional(FunctionalInput::MemberAccessInput(
                        MemberAccessInput {
                            identifier,
                            operand: Box::new(operand),
                        },
                    ))
                }
            )
        ]
    })
}

pub fn validate_numeric_literal(
    output: &NumericLiteral,
    input: NumericLiteralInput,
) -> Result<(), TestCaseError> {
    let output_number = output
        .numeric_literal_token
        .value_span
        .str()
        .parse::<u64>()?;

    prop_assert_eq!(output_number, input.number);

    Ok(())
}

pub fn validate_boolean_literal(
    output: &BooleanLiteral,
    input: BooleanLiteralInput,
) -> Result<(), TestCaseError> {
    match (output, input.value) {
        (BooleanLiteral::True(o), true) => {
            prop_assert_eq!(o.span.str(), "true");
            Ok(())
        }
        (BooleanLiteral::False(o), false) => {
            prop_assert_eq!(o.span.str(), "false");
            Ok(())
        }
        _ => Err(TestCaseError::fail("Boolean literal mismatch")),
    }
}

pub fn validate_prefix(output: &Prefix, input: &PrefixInput) -> Result<(), TestCaseError> {
    match (&output.prefix_operator, &input.prefix_operator) {
        (PrefixOperator::LogicalNot(o), PrefixOperatorInput::LogicalNot) => {
            prop_assert_eq!(o.span.str(), "!");
        }
        (PrefixOperator::Negate(o), PrefixOperatorInput::Negate) => {
            prop_assert_eq!(o.span.str(), "-");
        }
        _ => return Err(TestCaseError::fail("Prefix operator mismatch")),
    }

    validate_expression(&output.operand, &input.operand)
}

pub fn validate_member_access(
    output: &MemberAccess,
    input: &MemberAccessInput,
) -> Result<(), TestCaseError> {
    prop_assert_eq!(output.identifier.span.str(), &input.identifier);

    validate_expression(&output.operand, &input.operand)
}

pub fn validate_functional(
    output: &Functional,
    input: &FunctionalInput,
) -> Result<(), TestCaseError> {
    match (output, input) {
        (Functional::NumericLiteral(o), FunctionalInput::NumericLiteral(i)) => {
            validate_numeric_literal(o, *i)
        }
        (Functional::BooleanLiteral(o), FunctionalInput::BooleanLiteral(i)) => {
            validate_boolean_literal(o, *i)
        }
        (Functional::Prefix(o), FunctionalInput::Prefix(i)) => validate_prefix(o, i),
        (Functional::MemberAccess(o), FunctionalInput::MemberAccessInput(i)) => {
            validate_member_access(o, i)
        }

        _ => Err(TestCaseError::fail("Functional mismatch")),
    }
}

pub fn validate_expression(
    output: &Expression,
    input: &ExpressionInput,
) -> Result<(), TestCaseError> {
    match (output, input) {
        (Expression::Functional(o), ExpressionInput::Functional(i)) => validate_functional(o, i),
        _ => Err(TestCaseError::fail("Expression mismatch")),
    }
}

proptest! {
    #[test]
    fn expression_test(
        expression_input in expression_strategy()
    ) {
        let source = expression_input.to_string();
        println!("{source}");
        let expression = crate::syntax_tree::tests::parse(
            source,
            |parser, handler| parser.parse_primary_expression(handler)
        )?;

        validate_expression(&expression, &expression_input)?;
    }
}
