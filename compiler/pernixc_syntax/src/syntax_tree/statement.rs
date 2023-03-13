use pernixc_common::source_file::Span;
use pernixc_lexical::token::PunctuationToken;

use super::SyntaxTree;

pub enum StatementSyntaxTree {}

impl SyntaxTree for StatementSyntaxTree {
    fn span(&self) -> Span { todo!() }
}



pub struct SemiColonStatementSyntaxTree {
    pub statement: StatementSyntaxTree,
    pub semicolon: PunctuationToken,
}

impl SyntaxTree for SemiColonStatementSyntaxTree {
    fn span(&self) -> Span { Span::new(self.statement.span().start, self.semicolon.span.end) }
}
