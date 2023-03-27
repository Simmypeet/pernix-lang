use enum_as_inner::EnumAsInner;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, EnumAsInner)]
pub enum Type {
    Primitive(PrimitiveType),
    UserDefinedSymbolIndex(usize),
}

impl Type {
    pub fn is_arithmetic(self) -> bool {
        matches!(
            self,
            Type::Primitive(PrimitiveType::Float32)
                | Type::Primitive(PrimitiveType::Float64)
                | Type::Primitive(PrimitiveType::Int8)
                | Type::Primitive(PrimitiveType::Int16)
                | Type::Primitive(PrimitiveType::Int32)
                | Type::Primitive(PrimitiveType::Int64)
                | Type::Primitive(PrimitiveType::Uint8)
                | Type::Primitive(PrimitiveType::Uint16)
                | Type::Primitive(PrimitiveType::Uint32)
                | Type::Primitive(PrimitiveType::Uint64)
        )
    }

    pub fn is_floating_point(self) -> bool {
        matches!(
            self,
            Type::Primitive(PrimitiveType::Float32) | Type::Primitive(PrimitiveType::Float64)
        )
    }
}

impl Default for Type {
    fn default() -> Self { Type::Primitive(PrimitiveType::Void) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeBinding {
    pub ty: Type,
    pub is_mutable: bool,
}
