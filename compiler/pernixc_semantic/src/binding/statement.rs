use getset::{CopyGetters, Getters};

use super::expression::ExpressionBinding;
use crate::symbol::ty::TypeBinding;

#[derive(Debug, Clone, PartialEq)]
pub enum StatementBinding {
    DeclarationBinding(DeclarationBinding),
    ExpressionBinding(ExpressionBinding),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationBinding {
    VariableDeclarationBinding(VariableDeclarationBinding),
}

#[derive(Debug, Clone, PartialEq, Getters, CopyGetters)]
pub struct VariableDeclarationBinding {
    #[get_copy = "pub"]
    pub(super) type_binding_specifier: TypeBinding,
    #[get_copy = "pub"]
    pub(super) local_variable_id: usize,
    #[get = "pub"]
    pub(super) expression_binding: Option<Box<ExpressionBinding>>,
}
