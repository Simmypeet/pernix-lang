#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub enum PrimitiveType {
    #[default]
    Void,
    Bool,
    Float32,
    Float64,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
    Primitive(PrimitiveType),
    UserDefinedSymbolIndex(usize),
}

impl Default for Type {
    fn default() -> Self { Type::Primitive(PrimitiveType::Void) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeBinding {
    pub ty: Type,
    pub is_mutable: bool,
}
