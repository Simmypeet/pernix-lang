use super::{expression::ScopeStatement, PositiionWrapper, Type};

/// A declaration is a statement that declares a new name in the current scope.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declaration {
    /// Represents a namespace declaration of the form `namespace namespace_name { declarations* }`
    NamespaceDeclaration {
        namespace_name: PositiionWrapper<String>,
        declarations: Vec<PositiionWrapper<Declaration>>,
    },

    /// Represents a function declaration of the form `type function_name(parameters) { statements* }`
    FunctionDeclaration {
        function_name: PositiionWrapper<String>,
        parameters: Vec<PositiionWrapper<String>>,
        return_type: PositiionWrapper<Type>,
        body: Vec<PositiionWrapper<ScopeStatement>>,
    },

    /// Represents a namespace using declaration of the form `using namespace_name;`
    UsingDeclaration {
        namespace_name: PositiionWrapper<String>,
    },
}
