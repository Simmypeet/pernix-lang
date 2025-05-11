use enum_as_inner::EnumAsInner;
use pernixc_parser::{
    abstract_tree,
    expect::Ext as _,
    parser::{ast, Parser as _},
};

use crate::expression::prefix::Prefixable;

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Binary {
        pub first: Prefixable = ast::<Prefixable>(),
        pub chain: #[multi] BinarySubsequent
            = ast::<BinarySubsequent>().repeat(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct BinarySubsequent {
        pub operator: Operator = ast::<Operator>(),
        pub prefixable: Prefixable = ast::<Prefixable>(),
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
    }
}
