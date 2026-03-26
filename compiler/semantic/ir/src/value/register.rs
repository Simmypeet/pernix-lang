//! Contains the definition of [`Register`] and [`Assignment`].
//!
//! The register is a place where SSA values are stored. The assignment is the
//! value that is stored in the register.

use std::borrow::Cow;

use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::r#type::Type;
use pernixc_type_system::{OverflowError, Succeeded, normalizer::Normalizer};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    address::Address,
    resolution_visitor::{
        self, Abort, MutableResolutionVisitor, ResolutionVisitor,
    },
    value::{Environment, TypeOf, Value},
    visitor,
};

pub mod array;
pub mod binary;
pub mod borrow;
pub mod cast;
pub mod do_with;
pub mod function_call;
pub mod load;
pub mod phi;
pub mod prefix;
pub mod resume_call;
pub mod r#struct;
pub mod subtype;
pub mod tuple;
pub mod variant;
pub mod variant_number;

// Re-export commonly used types for convenience
pub use array::Array;
pub use binary::{
    ArithmeticOperator, Binary, BinaryOperator, BitwiseOperator,
    RelationalOperator,
};
pub use borrow::Borrow;
pub use cast::Cast;
pub use function_call::{EffectHandlerArgument, FunctionCall};
pub use load::{Load, Purpose as LoadPurpose};
pub use phi::Phi;
pub use prefix::{Prefix, PrefixOperator};
pub use resume_call::ResumeCall;
pub use r#struct::Struct;
pub use tuple::{Tuple, TupleElement};
pub use variant::Variant;
pub use variant_number::VariantNumber;

macro_rules! visit_assignment_with {
    (
        $assignment:expr,
        $visitor:expr,
        $span:expr,
        $tuple_fn:path,
        $load_fn:path,
        $borrow_fn:path,
        $prefix_fn:path,
        $struct_fn:path,
        $variant_fn:path,
        $function_call_fn:path,
        $binary_fn:path,
        $array_fn:path,
        $phi_fn:path,
        $cast_fn:path,
        $variant_number_fn:path,
        $do_with_fn:path,
        $resume_call_fn:path
    ) => {{
        match $assignment {
            Assignment::Tuple(tuple) => {
                $tuple_fn(tuple, $visitor).await?;
            }
            Assignment::Load(load) => {
                $load_fn(load, $visitor).await?;
            }
            Assignment::Borrow(borrow) => {
                $borrow_fn(borrow, $visitor, $span).await?;
            }
            Assignment::Prefix(prefix) => {
                $prefix_fn(prefix, $visitor, $span).await?;
            }
            Assignment::Struct(st) => {
                $struct_fn(st, $visitor, $span).await?;
            }
            Assignment::Variant(variant) => {
                $variant_fn(variant, $visitor, $span).await?;
            }
            Assignment::FunctionCall(function_call) => {
                $function_call_fn(function_call, $visitor, $span).await?;
            }
            Assignment::Binary(binary) => {
                $binary_fn(binary, $visitor).await?;
            }
            Assignment::Array(array) => {
                $array_fn(array, $visitor, $span).await?;
            }
            Assignment::Phi(phi) => {
                $phi_fn(phi, $visitor, $span).await?;
            }
            Assignment::Cast(cast) => {
                $cast_fn(cast, $visitor, $span).await?;
            }
            Assignment::VariantNumber(variant_number) => {
                $variant_number_fn(variant_number, $visitor).await?;
            }
            Assignment::Do(d) => {
                $do_with_fn(d, $visitor).await?;
            }
            Assignment::ResumeCall(r) => {
                $resume_call_fn(r, $visitor).await?;
            }
        }
        Ok(())
    }};
}

/// An enumeration of the different kinds of values that can be assigned in the
/// register.
#[derive(
    Debug, Clone, PartialEq, Eq, Encode, Decode, EnumAsInner, StableHash,
)]
#[allow(missing_docs)]
pub enum Assignment {
    Tuple(Tuple),
    Load(Load),
    Borrow(Borrow),
    Prefix(Prefix),
    Struct(Struct),
    Variant(Variant),
    FunctionCall(FunctionCall),
    Binary(Binary),
    Array(Array),
    Phi(Phi),
    Cast(Cast),
    VariantNumber(VariantNumber),
    Do(do_with::DoWith),
    ResumeCall(ResumeCall),
}

impl visitor::Element for Assignment {
    fn accept(&self, visitor: &mut impl visitor::Visitor) {
        match self {
            Self::Tuple(tuple) => tuple.accept(visitor),
            Self::Load(load) => load.accept(visitor),
            Self::Borrow(borrow) => borrow.accept(visitor),
            Self::Prefix(prefix) => prefix.accept(visitor),
            Self::Struct(st) => st.accept(visitor),
            Self::Variant(variant) => variant.accept(visitor),
            Self::FunctionCall(function_call) => function_call.accept(visitor),
            Self::Binary(binary) => binary.accept(visitor),
            Self::Array(array) => array.accept(visitor),
            Self::Phi(phi) => phi.accept(visitor),
            Self::Cast(cast) => cast.accept(visitor),
            Self::VariantNumber(variant_number) => {
                variant_number.accept(visitor);
            }
            Self::Do(d) => d.accept(visitor),
            Self::ResumeCall(resume_call) => resume_call.accept(visitor),
        }
    }
}

impl Assignment {
    /// Returns the register that is used in the assignment.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        use visitor::Element;
        let mut visitor = RegisterVisitor::default();
        self.accept(&mut visitor);
        visitor.registers
    }
}

/// Represents a register in the SSA from.
#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode, StableHash)]
pub struct Register {
    /// The value stored in the register.
    pub assignment: Assignment,

    /// The span where the value was defined.
    pub span: RelativeSpan,
}

impl TypeOf<ID<Register>> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        id: ID<Register>,
        environment: &Environment<'_, N>,
    ) -> Result<Succeeded<Type>, OverflowError> {
        let register = &self.registers[id];

        match &register.assignment {
            Assignment::Tuple(tuple) => {
                return self.type_of(tuple, environment).await;
            }
            Assignment::Load(load) => {
                return self.type_of(load, environment).await;
            }
            Assignment::Borrow(borrow) => {
                return self.type_of(borrow, environment).await;
            }
            Assignment::Prefix(prefix) => {
                return self.type_of(prefix, environment).await;
            }
            Assignment::Struct(st) => {
                return self.type_of(st, environment).await;
            }
            Assignment::Variant(variant) => {
                return self.type_of(variant, environment).await;
            }
            Assignment::FunctionCall(function_call) => {
                return self.type_of(function_call, environment).await;
            }
            Assignment::ResumeCall(d) => {
                return self.type_of(d, environment).await;
            }
            Assignment::Binary(binary) => {
                return self.type_of(binary, environment).await;
            }
            Assignment::Phi(phi_node) => {
                return self.type_of(phi_node, environment).await;
            }
            Assignment::Array(array) => {
                return self.type_of(array, environment).await;
            }
            Assignment::Cast(cast) => {
                return self.type_of(cast, environment).await;
            }
            Assignment::VariantNumber(variant) => {
                return self.type_of(variant, environment).await;
            }
            Assignment::Do(d) => return self.type_of(d, environment).await,
        }
    }
}

impl resolution_visitor::MutableResolutionVisitable for Register {
    async fn accept_mut<T: MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visit_assignment_with!(
            &mut self.assignment,
            visitor,
            self.span,
            tuple::transform_tuple,
            load::transform_load,
            borrow::transform_borrow,
            prefix::transform_prefix,
            r#struct::transform_struct,
            variant::transform_variant,
            function_call::transform_function_call,
            binary::transform_binary,
            array::transform_array,
            phi::transform_phi,
            cast::transform_cast,
            variant_number::transform_variant_number,
            do_with::transform_do_with,
            resume_call::transform_resume_call
        )
    }
}

impl resolution_visitor::ResolutionVisitable for Register {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visit_assignment_with!(
            &self.assignment,
            visitor,
            self.span,
            tuple::inspect_tuple,
            load::inspect_load,
            borrow::inspect_borrow,
            prefix::inspect_prefix,
            r#struct::inspect_struct,
            variant::inspect_variant,
            function_call::inspect_function_call,
            binary::inspect_binary,
            array::inspect_array,
            phi::inspect_phi,
            cast::inspect_cast,
            variant_number::inspect_variant_number,
            do_with::inspect_do_with,
            resume_call::inspect_resume_call
        )
    }
}

/// A visitor that collects all register IDs from values.
#[derive(Debug, Default)]
struct RegisterVisitor {
    registers: Vec<ID<Register>>,
}

impl visitor::Visitor for RegisterVisitor {
    fn visit_value(&mut self, value: Cow<Value>) {
        if let Some(&register_id) = value.as_register() {
            self.registers.push(register_id);
        }
    }

    fn visit_address(&mut self, address: Cow<Address>) {
        if let Address::Index(index) = &*address
            && let Value::Register(register_id) = &index.indexing_value
        {
            self.registers.push(*register_id);
        }
    }
}
