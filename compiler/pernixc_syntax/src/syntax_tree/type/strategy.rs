use std::fmt::{Display, Write};

use enum_as_inner::EnumAsInner;
use pernixc_tests::input::Input;
use proptest::{
    bool,
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    expression::strategy::Expression,
    strategy::{
        ConnectedList, Constant, ConstantPunctuation, Lifetime,
        QualifiedIdentifier,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reference {
    pub lifetime: Option<Lifetime>,
    pub is_mutable: bool,
    pub operand_type: Box<Type>,
}

impl Arbitrary for Reference {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            proptest::option::of(Lifetime::arbitrary()),
            proptest::bool::ANY,
            args.unwrap_or_else(Type::arbitrary),
        )
            .prop_map(|(lifetime, is_mutable, operand_type)| Self {
                lifetime,
                is_mutable,
                operand_type: Box::new(operand_type),
            })
            
    }
}

impl Input<&super::Reference> for &Reference {
    fn assert(self, output: &super::Reference) -> TestCaseResult {
        prop_assert_eq!(self.is_mutable, output.mutable_keyword.is_some());
        self.lifetime.as_ref().assert(output.lifetime.as_ref())?;
        self.operand_type.assert(output.operand())
    }
}

impl Display for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&")?;

        if let Some(lifetime) = &self.lifetime {
            write!(f, "{lifetime} ")?;
        }

        if self.is_mutable {
            write!(f, "mutable ")?;
        }

        Display::fmt(&self.operand_type, f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Primitive {
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
    Usize,
    Isize,
}

impl Arbitrary for Primitive {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Bool),
            Just(Self::Float32),
            Just(Self::Float64),
            Just(Self::Int8),
            Just(Self::Int16),
            Just(Self::Int32),
            Just(Self::Int64),
            Just(Self::Uint8),
            Just(Self::Uint16),
            Just(Self::Uint32),
            Just(Self::Uint64),
            Just(Self::Usize),
            Just(Self::Isize),
        ]
        
    }
}

impl Input<&super::Primitive> for &Primitive {
    fn assert(self, output: &super::Primitive) -> TestCaseResult {
        match (self, output) {
            (Primitive::Bool, super::Primitive::Bool(..))
            | (Primitive::Float32, super::Primitive::Float32(..))
            | (Primitive::Float64, super::Primitive::Float64(..))
            | (Primitive::Int8, super::Primitive::Int8(..))
            | (Primitive::Int16, super::Primitive::Int16(..))
            | (Primitive::Int32, super::Primitive::Int32(..))
            | (Primitive::Int64, super::Primitive::Int64(..))
            | (Primitive::Uint8, super::Primitive::Uint8(..))
            | (Primitive::Uint16, super::Primitive::Uint16(..))
            | (Primitive::Uint32, super::Primitive::Uint32(..))
            | (Primitive::Uint64, super::Primitive::Uint64(..))
            | (Primitive::Usize, super::Primitive::Usize(..))
            | (Primitive::Isize, super::Primitive::Isize(..)) => Ok(()),

            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Float32 => write!(f, "float32"),
            Self::Float64 => write!(f, "float64"),
            Self::Int8 => write!(f, "int8"),
            Self::Int16 => write!(f, "int16"),
            Self::Int32 => write!(f, "int32"),
            Self::Int64 => write!(f, "int64"),
            Self::Uint8 => write!(f, "uint8"),
            Self::Uint16 => write!(f, "uint16"),
            Self::Uint32 => write!(f, "uint32"),
            Self::Uint64 => write!(f, "uint64"),
            Self::Usize => write!(f, "usize"),
            Self::Isize => write!(f, "isize"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array {
    pub operand: Box<Type>,
    pub constant: Constant,
}

impl Arbitrary for Array {
    type Parameters =
        (Option<BoxedStrategy<Type>>, Option<BoxedStrategy<Expression>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (
            args.0.unwrap_or_else(Type::arbitrary),
            Constant::arbitrary_with(args.1),
        )
            .prop_map(|(type_specifier, constant)| Self {
                operand: Box::new(type_specifier),
                constant,
            })
            
    }
}

impl Input<&super::Array> for &Array {
    fn assert(self, output: &super::Array) -> TestCaseResult {
        self.operand.assert(output.operand())?;
        self.constant.assert(output.constant())
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;
        Display::fmt(&self.operand, f)?;
        f.write_str(": ")?;
        Display::fmt(&self.constant, f)?;
        f.write_char(']')?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer {
    pub is_mutable: bool,
    pub operand: Box<Type>,
}

impl Input<&super::Pointer> for &Pointer {
    fn assert(self, output: &super::Pointer) -> TestCaseResult {
        prop_assert_eq!(self.is_mutable, output.mutable_keyword().is_some());
        self.operand.assert(output.operand())
    }
}

impl Arbitrary for Pointer {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        (proptest::bool::ANY, args.unwrap_or_else(Type::arbitrary))
            .prop_map(|(is_mutable, operand)| Self {
                is_mutable,
                operand: Box::new(operand),
            })
            
    }
}

impl Display for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('*')?;

        if self.is_mutable {
            write!(f, "mutable ")?;
        }

        Display::fmt(&self.operand, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unpackable {
    pub ellipsis: bool,
    pub ty: Box<Type>,
}

impl Input<&super::Unpackable> for &Unpackable {
    fn assert(self, output: &super::Unpackable) -> TestCaseResult {
        prop_assert_eq!(self.ellipsis, output.ellipsis().is_some());
        self.ty.assert(output.ty())
    }
}

impl Arbitrary for Unpackable {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let ty = args.unwrap_or_else(Type::arbitrary);

        (proptest::bool::ANY, ty.prop_map(Box::new))
            .prop_map(|(ellipsis, ty)| Self { ellipsis, ty })
            
    }
}

impl Display for Unpackable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ellipsis {
            f.write_str("...")?;
        }

        Display::fmt(&self.ty, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple {
    pub unpackable_list:
        Option<ConnectedList<Unpackable, ConstantPunctuation<','>>>,
}

impl Input<&super::Tuple> for &Tuple {
    fn assert(self, output: &super::Tuple) -> TestCaseResult {
        self.unpackable_list.as_ref().assert(output.unpackable_list().as_ref())
    }
}

impl Arbitrary for Tuple {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        proptest::option::of(ConnectedList::arbitrary_with(
            Unpackable::arbitrary_with(args),
            ConstantPunctuation::<','>::arbitrary(),
        ))
        .prop_map(|type_specifier_list| Self {
            unpackable_list: type_specifier_list,
        })
        
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('(')?;
        if let Some(type_specifier_list) = &self.unpackable_list {
            Display::fmt(type_specifier_list, f)?;
        }
        f.write_char(')')?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local {
    pub ty: Box<Type>,
}

impl Input<&super::Local> for &Local {
    fn assert(self, output: &super::Local) -> TestCaseResult {
        self.ty.assert(output.ty())
    }
}

impl Arbitrary for Local {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        args.unwrap_or_else(Type::arbitrary)
            .prop_map(|ty| Self { ty: Box::new(ty) })
            
    }
}

impl Display for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "local {}", &self.ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Phantom {
    r#type: Box<Type>,
}

impl Input<&super::Phantom> for &Phantom {
    fn assert(self, output: &super::Phantom) -> TestCaseResult {
        self.r#type.assert(output.r#type())
    }
}

impl Arbitrary for Phantom {
    type Parameters = Option<BoxedStrategy<Type>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        args.unwrap_or_else(Type::arbitrary)
            .prop_map(|r#type| Self { r#type: Box::new(r#type) })
            
    }
}

impl Display for Phantom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "phantom {}", &self.r#type)
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Type {
    Primitive(Primitive),
    Reference(Reference),
    QualifiedIdentifier(QualifiedIdentifier),
    Array(Array),
    Pointer(Pointer),
    Tuple(Tuple),
    Local(Local),
    Phantom(Phantom),
    Elided,
}

impl Input<&super::Type> for &Type {
    fn assert(self, output: &super::Type) -> TestCaseResult {
        match (self, output) {
            (Type::Primitive(i), super::Type::Primitive(o)) => i.assert(o),
            (Type::Reference(i), super::Type::Reference(o)) => i.assert(o),
            (Type::Local(i), super::Type::Local(o)) => i.assert(o),
            (
                Type::QualifiedIdentifier(i),
                super::Type::QualifiedIdentifier(o),
            ) => i.assert(o),
            (Type::Array(i), super::Type::Array(o)) => i.assert(o),
            (Type::Pointer(i), super::Type::Pointer(o)) => i.assert(o),
            (Type::Tuple(i), super::Type::Tuple(o)) => i.assert(o),
            (Type::Phantom(i), super::Type::Phantom(o)) => i.assert(o),
            (Type::Elided, super::Type::Elided(_)) => Ok(()),
            _ => Err(TestCaseError::fail(format!(
                "Expected {self:?} but got {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Type {
    type Parameters = (
        Option<BoxedStrategy<Expression>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let leaf = Primitive::arbitrary().prop_map(Type::Primitive);

        leaf.prop_recursive(4, 24, 6, move |inner| {
            prop_oneof![
                Reference::arbitrary_with(Some(inner.clone()))
                    .prop_map(Self::Reference),
                Local::arbitrary_with(Some(inner.clone()))
                    .prop_map(Self::Local),
                Pointer::arbitrary_with(Some(inner.clone()))
                    .prop_map(Self::Pointer),
                Tuple::arbitrary_with(Some(inner.clone()))
                    .prop_map(Self::Tuple),
                Array::arbitrary_with((Some(inner.clone()), args.0.clone()))
                    .prop_map(Self::Array),
                Phantom::arbitrary_with(Some(inner.clone()))
                    .prop_map(Self::Phantom),
                args.1
                    .clone()
                    .unwrap_or_else(|| {
                        QualifiedIdentifier::arbitrary_with((
                            Some(inner),
                            args.0.clone(),
                        ))
                    })
                    .prop_map(Self::QualifiedIdentifier),
                Just(Self::Elided),
            ]
        })
        
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(i) => Display::fmt(i, f),
            Self::Reference(i) => Display::fmt(i, f),
            Self::QualifiedIdentifier(i) => Display::fmt(i, f),
            Self::Array(i) => Display::fmt(i, f),
            Self::Pointer(i) => Display::fmt(i, f),
            Self::Tuple(i) => Display::fmt(i, f),
            Self::Local(i) => Display::fmt(i, f),
            Self::Phantom(i) => Display::fmt(i, f),
            Self::Elided => f.write_str(".."),
        }
    }
}
