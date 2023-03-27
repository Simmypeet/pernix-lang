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
    pub type_binding_specifier: TypeBinding,
    pub local_variable_id: usize,
    pub expression_binding: Option<Box<ExpressionBinding>>,
}
