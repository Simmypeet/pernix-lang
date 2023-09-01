use enum_as_inner::EnumAsInner;
use pernixc_system::arena::ID;

use crate::{
    symbol,
    ty::{self, TupleBoundable},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Irrefutable {
    Discard,
    Named(Named),
    Structural(Structural<Self>),
    Tuple(Tuple<Self>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NumericLiteral {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum<Pattern> {
    pub enum_id: ID<symbol::Enum>,
    pub variant_id: usize,
    pub pattern: Box<Pattern>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Refutable {
    BooleanLiteral(bool),
    NumericLiteral(NumericLiteral),
    Structural(Structural<Self>),
    Enum(Enum<Self>),
    Named(Named),
    Tuple(Tuple<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named {
    pub is_mutable: bool,
    pub ty: ty::Type,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Structural<Pattern> {
    pub struct_ty: ty::Struct,
    pub patterns: Vec<Pattern>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unpack {
    pub ty: TupleBoundable,
    pub name: String,
    pub mutable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Unpackable<Pattern> {
    Unpack(Unpack),
    Pattern(Pattern),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<Pattern> {
    pub elements: Vec<Unpackable<Pattern>>,
}
