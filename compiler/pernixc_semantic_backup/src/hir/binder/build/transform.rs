use crate::{
    hir::{
        binder::IntermediateTypeID,
        instruction::{Backend, Basic, ConditionalJump, Return, Store},
        value::{
            binding::{
                Binary, Binding, Cast, FunctionCall, Load, MemberAccess, PhiNode, Prefix,
                StructLiteral,
            },
            Constant, NumericLiteral, Value,
        },
        Alloca, Container, Register,
    },
    infer::{Constraint, InferableType, InferenceContext},
    symbol::ty::{PrimitiveType, Type},
};

/// Changes the underlying type system of the [`Container`] into concrete [`Type`]s.
pub(in crate::hir::binder) fn transform_type_system(
    container: Container<IntermediateTypeID>,
    inference_context: &InferenceContext,
) -> Container<Type> {
    let control_flow_graph = container.control_flow_graph.map::<Backend<Type>>(
        |x| x,
        |x| Return {
            return_value: x
                .return_value
                .map(|x| transform_value(x, inference_context)),
        },
        |x| match x {
            Basic::RegisterAssignment(x) => Basic::RegisterAssignment(x),
            Basic::VariableDeclaration(x) => Basic::VariableDeclaration(x),
            Basic::Store(x) => Basic::Store(Store {
                address: x.address,
                value: transform_value(x.value, inference_context),
                span: x.span,
            }),
            Basic::ScopePush(x) => Basic::ScopePush(x),
            Basic::ScopePop(x) => Basic::ScopePop(x),
            Basic::Destruct(..) => unreachable!(
                "there shouldn't be any destruct instructions generated before `build`"
            ),
        },
        |x| ConditionalJump {
            condition_value: transform_value(x.condition_value, inference_context),
            true_jump_target: x.true_jump_target,
            false_jump_target: x.false_jump_target,
        },
    );

    let allocas = container.allocas.map(|source_alloca| Alloca {
        identifier_token: source_alloca.identifier_token,
        is_mutable: source_alloca.is_mutable,
        ty: transform_intermediate_type_id(source_alloca.ty, inference_context),
        scope_id: source_alloca.scope_id,
        declaration_order: source_alloca.declaration_order,
    });

    let registers = container.registers.map(|x| Register {
        binding: transform_binding(x.binding, inference_context),
    });

    Container {
        control_flow_graph,
        registers,
        allocas,
        table: container.table,
        overload_id: container.overload_id,
        parent_overload_set_id: container.parent_overload_set_id,
        parent_scoped_id: container.parent_scoped_id,
        scope_tree: container.scope_tree,
    }
}

fn transform_binding(
    binding: Binding<IntermediateTypeID>,
    inference_context: &InferenceContext,
) -> Binding<Type> {
    match binding {
        Binding::FunctionCall(binding) => Binding::FunctionCall(FunctionCall {
            span: binding.span,
            overload_id: binding.overload_id,
            arguments: binding
                .arguments
                .into_iter()
                .map(|x| transform_value(x, inference_context))
                .collect(),
        }),
        Binding::Prefix(binding) => Binding::Prefix(Prefix {
            span: binding.span,
            prefix_operator: binding.prefix_operator,
            operand: transform_value(binding.operand, inference_context),
        }),
        Binding::Load(binding) => Binding::Load(Load {
            span: binding.span,
            load_type: binding.load_type,
            address: binding.address,
        }),
        Binding::StructLiteral(binding) => Binding::StructLiteral(StructLiteral {
            span: binding.span,
            struct_id: binding.struct_id,
            initializations: binding
                .initializations
                .into_iter()
                .map(|(k, v)| (k, transform_value(v, inference_context)))
                .collect(),
        }),
        Binding::MemberAccess(binding) => Binding::MemberAccess(MemberAccess {
            span: binding.span,
            operand: transform_value(binding.operand, inference_context),
            field_id: binding.field_id,
        }),
        Binding::Binary(binding) => Binding::Binary(Binary {
            span: binding.span,
            left_operand: transform_value(binding.left_operand, inference_context),
            right_operand: transform_value(binding.right_operand, inference_context),
            binary_operator: binding.binary_operator,
        }),
        Binding::PhiNode(binding) => Binding::PhiNode(PhiNode {
            span: binding.span,
            values_by_predecessor: binding
                .values_by_predecessor
                .into_iter()
                .map(|(k, v)| (k, transform_value(v, inference_context)))
                .collect(),
            phi_node_source: binding.phi_node_source,
        }),
        Binding::Cast(binding) => Binding::Cast(Cast {
            operand: transform_value(binding.operand, inference_context),
            target_type: transform_intermediate_type_id(binding.target_type, inference_context),
            span: binding.span,
        }),
    }
}

fn transform_value(
    value: Value<IntermediateTypeID>,
    inference_context: &InferenceContext,
) -> Value<Type> {
    match value {
        Value::Register(register) => Value::Register(register),
        Value::Constant(constant) => match constant {
            Constant::NumericLiteral(literal) => {
                Value::Constant(Constant::NumericLiteral(NumericLiteral {
                    numeric_literal_syntax_tree: literal.numeric_literal_syntax_tree,
                    ty: transform_intermediate_type_id(literal.ty, inference_context),
                }))
            }
            Constant::BooleanLiteral(literal) => Value::Constant(Constant::BooleanLiteral(literal)),
            Constant::EnumLiteral(literal) => Value::Constant(Constant::EnumLiteral(literal)),
            Constant::VoidConstant(constant) => Value::Constant(Constant::VoidConstant(constant)),
        },
        Value::Placeholder(..) | Value::Unreachable(..) => unreachable!(),
    }
}

fn transform_intermediate_type_id(
    intermediate_type_id: IntermediateTypeID,
    inference_context: &InferenceContext,
) -> Type {
    match intermediate_type_id {
        IntermediateTypeID::InferenceID(inference) => {
            match inference_context.get_inferable_type(inference).unwrap() {
                InferableType::Type(ty) => ty,
                InferableType::Constraint(constraint) => match constraint {
                    Constraint::All | Constraint::PrimitiveType => unreachable!(),
                    Constraint::Number | Constraint::Signed => {
                        Type::PrimitiveType(PrimitiveType::Int32)
                    }
                    Constraint::Float => Type::PrimitiveType(PrimitiveType::Float64),
                },
            }
        }
        IntermediateTypeID::Type(ty) => ty,
    }
}
