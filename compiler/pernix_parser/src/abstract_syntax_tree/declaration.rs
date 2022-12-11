use super::{expression::Statement, PositiionWrapper, Type};

/// A declaration is a statement that declares a new name in the current scope.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declaration<'a> {
    /// Represents a namespace declaration of the form `namespace namespace_name { declarations* }`
    NamespaceDeclaration {
        namespace_name: PositiionWrapper<&'a str>,
        declarations: Vec<PositiionWrapper<Declaration<'a>>>,
    },

    /// Represents a function declaration of the form `type function_name(parameters) { statements* }`
    FunctionDeclaration {
        function_name: PositiionWrapper<&'a str>,
        parameters: Vec<PositiionWrapper<&'a str>>,
        return_type: PositiionWrapper<Type<'a>>,
        body: Vec<PositiionWrapper<Statement<'a>>>,
    },

    /// Represents a namespace using statement of the form `using namespace_name;`
    UsingStatement {
        namespace_name: PositiionWrapper<&'a str>,
    },
}
