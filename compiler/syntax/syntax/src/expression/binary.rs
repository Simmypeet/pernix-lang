use enum_as_inner::EnumAsInner;
use pernixc_parser::{
    abstract_tree,
    expect::Ext as _,
    parser::{ast, Parser as _},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use super::block::Block;
use crate::{expect::Keyword, expression::prefix::Prefixable};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        StableHash,
        Serialize,
        Deserialize,
    )]
    pub struct Binary {
        pub first: Node = ast::<Node>(),
        pub chain: #[multi] BinarySubsequent
            = ast::<BinarySubsequent>().repeat(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct BinarySubsequent {
        pub operator: Operator = ast::<Operator>(),
        pub node: Node = ast::<Node>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct CompoundAdd {
        pub plus = '+',
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct CompoundSubtract {
        pub minus = '-',
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct CompoundMultiply {
        pub asterisk = '*',
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct CompoundDivide {
        pub slash = '/',
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct CompoundModulus {
        pub percent = '%',
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Add {
        pub plus = '+',
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Subtract {
        pub minus = '-',
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Multiply {
        pub asterisk = '*',
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Divide {
        pub slash = '/',
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Modulus {
        pub percent = '%',
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Equal {
        pub equals_first = '=',
        pub equals_second = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct NotEqual {
        pub exclamation = '!',
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct GreaterThan {
        pub greater = '>',
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct LessThan {
        pub less = '<',
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct GreaterThanOrEqual {
        pub greater = '>',
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct LessThanOrEqual {
        pub less = '<',
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct BitwiseAnd {
        pub ampersand = '&',
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct BitwiseOr {
        pub pipe = '|',
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct BitwiseXor {
        pub caret = '^',
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct BitwiseLeftShift {
        pub less_first = '<',
        pub less_second = '<'.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct BitwiseRightShift {
        pub greater_first = '>',
        pub greater_second = '>'.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct CompoundBitwiseAnd {
        pub ampersand = '&',
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct CompoundBitwiseOr {
        pub pipe = '|',
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct CompoundBitwiseXor {
        pub caret = '^',
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct CompoundBitwiseLeftShift {
        pub less_first = '<',
        pub less_second = '<'.no_prior_insignificant(),
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct CompoundBitwiseRightShift {
        pub greater_first = '>',
        pub greater_second = '>'.no_prior_insignificant(),
        pub equals = '='.no_prior_insignificant(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Assign {
        pub equals = '=',
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct And {
        pub and_keyword = Keyword::And
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Or {
        pub or_keyword = Keyword::Or
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Node {
        Prefixable(Prefixable = ast::<Prefixable>()),
        Block(Block = ast::<Block>()),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Operator {
        CompoundBitwiseLeftShift(
            CompoundBitwiseLeftShift = ast::<CompoundBitwiseLeftShift>()
        ),
        CompoundBitwiseRightShift(
            CompoundBitwiseRightShift = ast::<CompoundBitwiseRightShift>()
        ),
        CompoundAdd(
            CompoundAdd = ast::<CompoundAdd>()
        ),
        CompoundSubtract(
            CompoundSubtract = ast::<CompoundSubtract>()
        ),
        CompoundMultiply(
            CompoundMultiply = ast::<CompoundMultiply>()
        ),
        CompoundDivide(
            CompoundDivide = ast::<CompoundDivide>()
        ),
        CompoundModulus(
            CompoundModulus = ast::<CompoundModulus>()
        ),
        CompoundBitwiseAnd(
            CompoundBitwiseAnd = ast::<CompoundBitwiseAnd>()
        ),
        CompoundBitwiseOr(
            CompoundBitwiseOr = ast::<CompoundBitwiseOr>()
        ),
        CompoundBitwiseXor(
            CompoundBitwiseXor = ast::<CompoundBitwiseXor>()
        ),
        Equal(
            Equal = ast::<Equal>()
        ),
        NotEqual(
            NotEqual = ast::<NotEqual>()
        ),
        LessThanOrEqual(
            LessThanOrEqual = ast::<LessThanOrEqual>()
        ),
        GreaterThanOrEqual(
            GreaterThanOrEqual = ast::<GreaterThanOrEqual>()
        ),
        BitwiseLeftShift(
            BitwiseLeftShift = ast::<BitwiseLeftShift>()
        ),
        BitwiseRightShift(
            BitwiseRightShift = ast::<BitwiseRightShift>()
        ),
        Add(
            Add = ast::<Add>()
        ),
        Subtract(
            Subtract = ast::<Subtract>()
        ),
        Multiply(
            Multiply = ast::<Multiply>()
        ),
        Divide(
            Divide = ast::<Divide>()
        ),
        Modulus(
            Modulus = ast::<Modulus>()
        ),
        BitwiseAnd(
            BitwiseAnd = ast::<BitwiseAnd>()
        ),
        BitwiseOr(
            BitwiseOr = ast::<BitwiseOr>()
        ),
        BitwiseXor(
            BitwiseXor = ast::<BitwiseXor>()
        ),
        GreaterThan(
            GreaterThan = ast::<GreaterThan>()
        ),
        LessThan(
            LessThan = ast::<LessThan>()
        ),
        Assign(
            Assign = ast::<Assign>()
        ),
        LogicalAnd(
            And = ast::<And>()
        ),
        LogicalOr(
            Or = ast::<Or>()
        ),
    }
}
