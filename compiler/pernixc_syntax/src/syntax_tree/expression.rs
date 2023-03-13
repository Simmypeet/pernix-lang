use pernixc_common::source_file::Span;
use pernixc_lexical::token::PunctuationToken;

use super::{statement::StatementSyntaxTree, SyntaxTree};

pub enum ExpressionSyntaxTree {}

impl SyntaxTree for ExpressionSyntaxTree {
    fn span(&self) -> Span { todo!() }
}

