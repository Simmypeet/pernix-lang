use pernixc_syntactic_analysis::abstract_syntax_tree::{
    declaration::TypeAnnotationAST, PositionWrapper,
};

pub enum SemanticError<'ast, 'src> {
    DeclarationNameConflictWithNamespace {
        name_conflict: &'ast PositionWrapper<&'src str>,
    },
    SymbolRedeclaration {
        redeclaration_name: &'ast PositionWrapper<&'src str>,
    },
    TypeExpected {
        type_annotation_ast: &'ast PositionWrapper<TypeAnnotationAST<'src>>,
    },
    TypeNotFound {
        type_annotation_ast: &'ast PositionWrapper<TypeAnnotationAST<'src>>,
    },
    FunctionRedeclaration {
        redeclaration_name: &'ast PositionWrapper<&'src str>,
    },
    ClassFieldRedeclaration {
        redeclaration_name: &'ast PositionWrapper<&'src str>,
    },
}
