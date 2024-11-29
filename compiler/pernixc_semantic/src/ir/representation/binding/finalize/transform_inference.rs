use std::{collections::HashSet, convert::Infallible};

use pernixc_base::{handler::Handler, source_file::Span};

use crate::{
    arena::{Key, ID},
    error::TypeAnnotationRequired,
    ir::{
        self,
        representation::binding::{
            infer::{self, ConstraintModel, NoConstraint},
            HandlerWrapper,
        },
        value::register::{self, Assignment, Register},
        Erased,
    },
    symbol::{
        table::{self, representation::Index, Table},
        AdtID, CallableID, ImplementationID,
    },
    type_system::{
        instantiation::Instantiation,
        model::Transform,
        sub_term::TermLocation,
        term::{
            self,
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Type},
            Error, GenericArguments, MemberSymbol, Never, Symbol, Term,
        },
        visitor::{self, MutableRecursive, RecursiveIterator},
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ReplaceInference;

impl MutableRecursive<Lifetime<ConstraintModel>> for ReplaceInference {
    fn visit(
        &mut self,
        _: &mut Lifetime<ConstraintModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}
impl MutableRecursive<Type<ConstraintModel>> for ReplaceInference {
    fn visit(
        &mut self,
        term: &mut Type<ConstraintModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            *term = Type::Error(Error);
        }

        true
    }
}
impl MutableRecursive<Constant<ConstraintModel>> for ReplaceInference {
    fn visit(
        &mut self,
        term: &mut Constant<ConstraintModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            *term = Constant::Error(Error);
        }

        true
    }
}

impl TryFrom<NoConstraint> for Erased {
    type Error = Infallible;

    fn try_from(_: NoConstraint) -> Result<Self, Self::Error> { Ok(Self) }
}

impl TryFrom<NoConstraint> for Never {
    type Error = Infallible;

    fn try_from(_: NoConstraint) -> Result<Self, Self::Error> {
        panic!("NoConstraint should never be converted to Never")
    }
}

impl TryFrom<r#type::Constraint> for Never {
    type Error = Infallible;

    fn try_from(_: r#type::Constraint) -> Result<Self, Self::Error> {
        panic!("Constraint should never be converted to Never")
    }
}

#[derive(Clone, Copy)]
pub struct Transformer<'a> {
    inference_context: &'a infer::Context,
    handler: &'a HandlerWrapper<'a>,
    should_report: bool,
}

impl Transform<Lifetime<infer::Model>> for Transformer<'_> {
    type Target = ir::Model;

    fn transform(
        &mut self,
        term: Lifetime<infer::Model>,
        _: Option<Span>,
    ) -> <Lifetime<infer::Model> as Term>::Rebind<Self::Target> {
        match term {
            Lifetime::Static => Lifetime::Static,
            Lifetime::Parameter(member_id) => Lifetime::Parameter(member_id),
            Lifetime::Inference(_) => Lifetime::Inference(Erased),
            Lifetime::Forall(forall) => Lifetime::Forall(forall),
            Lifetime::Error(error) => Lifetime::Error(error),
        }
    }
}

impl Transform<Type<infer::Model>> for Transformer<'_> {
    type Target = ir::Model;

    fn transform(
        &mut self,
        term: Type<infer::Model>,
        span: Option<Span>,
    ) -> <Type<infer::Model> as Term>::Rebind<Self::Target> {
        let mut ty =
            self.inference_context.into_constraint_model(term).unwrap();
        let found_inference = RecursiveIterator::new(&ty).any(|x| match x.0 {
            term::Kind::Lifetime(_) => false,
            term::Kind::Type(a) => a.is_inference(),
            term::Kind::Constant(a) => a.is_inference(),
        });

        if let Some(span) = span {
            if found_inference && self.should_report {
                self.handler.receive(Box::new(TypeAnnotationRequired {
                    span,
                    r#type: ty.clone(),
                }));
            }
        }

        let mut replace_inference = ReplaceInference;

        visitor::accept_recursive_mut(&mut ty, &mut replace_inference);

        Type::try_from_other_model(ty)
            .expect("all inference should've been replaced")
    }
}

fn transform_instantiation(
    instantiation: Instantiation<infer::Model>,
    inference_context: &infer::Context,
) -> Instantiation<ir::Model> {
    Instantiation {
        lifetimes: instantiation
            .lifetimes
            .into_iter()
            .map(|(k, v)| {
                (
                    match k {
                        Lifetime::Static => Lifetime::Static,
                        Lifetime::Parameter(member_id) => {
                            Lifetime::Parameter(member_id)
                        }
                        Lifetime::Inference(Erased) => {
                            Lifetime::Inference(Erased)
                        }
                        Lifetime::Forall(forall) => Lifetime::Forall(forall),
                        Lifetime::Error(error) => Lifetime::Error(error),
                    },
                    match v {
                        Lifetime::Static => Lifetime::Static,
                        Lifetime::Parameter(member_id) => {
                            Lifetime::Parameter(member_id)
                        }
                        Lifetime::Inference(Erased) => {
                            Lifetime::Inference(Erased)
                        }
                        Lifetime::Forall(forall) => Lifetime::Forall(forall),
                        Lifetime::Error(error) => Lifetime::Error(error),
                    },
                )
            })
            .collect(),
        types: instantiation
            .types
            .into_iter()
            .map(|(k, v)| {
                let mut k = inference_context.into_constraint_model(k).unwrap();
                let mut v = inference_context.into_constraint_model(v).unwrap();

                let mut replace_inference = ReplaceInference;

                visitor::accept_recursive_mut(&mut k, &mut replace_inference);
                visitor::accept_recursive_mut(&mut v, &mut replace_inference);

                (
                    Type::try_from_other_model(k).unwrap(),
                    Type::try_from_other_model(v).unwrap(),
                )
            })
            .collect(),
        constants: {
            instantiation
                .constants
                .into_iter()
                .map(|(k, v)| {
                    let mut k = inference_context
                        .constant_into_constraint_model(k)
                        .unwrap();
                    let mut v = inference_context
                        .constant_into_constraint_model(v)
                        .unwrap();

                    let mut replace_inference = ReplaceInference;

                    visitor::accept_recursive_mut(
                        &mut k,
                        &mut replace_inference,
                    );
                    visitor::accept_recursive_mut(
                        &mut v,
                        &mut replace_inference,
                    );

                    (
                        Constant::try_from_other_model(k).unwrap(),
                        Constant::try_from_other_model(v).unwrap(),
                    )
                })
                .collect()
        },
    }
}

impl Register<infer::Model> {
    /// Transforms the [`Register`] to another model using the given
    /// transformer.
    #[allow(clippy::too_many_lines)]
    fn transform_model(
        self,
        transformer: &mut Transformer,
        inference_context: &infer::Context,
        table: &Table<impl table::State>,
    ) -> Register<ir::Model> {
        Register {
            span: self.span.clone(),
            assignment: match self.assignment {
                Assignment::Tuple(tuple) => {
                    Assignment::Tuple(register::Tuple {
                        elements: tuple
                            .elements
                            .into_iter()
                            .map(|x| register::TupleElement {
                                value: x.value.transform_model(transformer),
                                is_unpacked: x.is_unpacked,
                            })
                            .collect(),
                    })
                }
                Assignment::Load(load) => Assignment::Load(register::Load {
                    address: load.address.transform_model(transformer),
                }),
                Assignment::ReferenceOf(reference_of) => {
                    Assignment::ReferenceOf(register::ReferenceOf {
                        address: reference_of
                            .address
                            .transform_model(transformer),
                        qualifier: reference_of.qualifier,
                        lifetime: transformer
                            .transform(reference_of.lifetime, self.span),
                    })
                }
                Assignment::Prefix(prefix) => {
                    Assignment::Prefix(register::Prefix {
                        operand: prefix.operand.transform_model(transformer),
                        operator: prefix.operator,
                    })
                }
                Assignment::Struct(st) => {
                    Assignment::Struct(register::Struct {
                        struct_id: st.struct_id,
                        initializers_by_field_id: st
                            .initializers_by_field_id
                            .into_iter()
                            .map(|(k, v)| (k, v.transform_model(transformer)))
                            .collect(),
                        generic_arguments: {
                            transformer
                                .transform(
                                    Type::Symbol(Symbol {
                                        id: r#type::SymbolID::Adt(
                                            AdtID::Struct(st.struct_id),
                                        ),
                                        generic_arguments: st.generic_arguments,
                                    }),
                                    self.span,
                                )
                                .into_symbol()
                                .unwrap()
                                .generic_arguments
                        },
                    })
                }
                Assignment::Variant(variant) => {
                    Assignment::Variant(register::Variant {
                        variant_id: variant.variant_id,
                        associated_value: variant
                            .associated_value
                            .map(|x| x.transform_model(transformer)),
                        generic_arguments: {
                            let enum_id = table
                                .get(variant.variant_id)
                                .unwrap()
                                .parent_enum_id();

                            transformer
                                .transform(
                                    Type::Symbol(Symbol {
                                        id: r#type::SymbolID::Adt(AdtID::Enum(
                                            enum_id,
                                        )),
                                        generic_arguments: variant
                                            .generic_arguments,
                                    }),
                                    self.span,
                                )
                                .into_symbol()
                                .unwrap()
                                .generic_arguments
                        },
                    })
                }

                // this is such a bad idea :(
                Assignment::FunctionCall(function_call) => {
                    Assignment::FunctionCall(register::FunctionCall {
                        callable_id: function_call.callable_id,
                        arguments: function_call
                            .arguments
                            .into_iter()
                            .map(|x| x.transform_model(transformer))
                            .collect(),
                        instantiation: match function_call.callable_id {
                            CallableID::Function(id) => {
                                let function_symbol = table.get(id).unwrap();

                                let generic_arguments = function_call
                                    .instantiation
                                    .create_generic_arguments(
                                        id.into(),
                                        &function_symbol
                                            .generic_declaration
                                            .parameters,
                                    )
                                    .unwrap();

                                transformer
                                    .transform(
                                        Type::Symbol(Symbol {
                                            id: r#type::SymbolID::Function(id),
                                            generic_arguments,
                                        }),
                                        self.span,
                                    )
                                    .into_symbol()
                                    .unwrap()
                                    .generic_arguments;

                                transform_instantiation(
                                    function_call.instantiation,
                                    inference_context,
                                )
                            }

                            CallableID::TraitFunction(id) => {
                                let trait_funciton = table.get(id).unwrap();
                                let parent_trait = trait_funciton.parent_id();

                                let parent_generic_arguments = function_call
                                    .instantiation
                                    .create_generic_arguments(
                                        parent_trait.into(),
                                        &&table
                                            .get(parent_trait)
                                            .unwrap()
                                            .generic_declaration
                                            .parameters,
                                    )
                                    .unwrap();
                                let member_generic_arguments = function_call
                                    .instantiation
                                    .create_generic_arguments(
                                        id.into(),
                                        &trait_funciton
                                            .generic_declaration
                                            .parameters,
                                    )
                                    .unwrap();

                                transformer.transform(
                                    Type::MemberSymbol(MemberSymbol {
                                        id: r#type::MemberSymbolID::Function(
                                            id.into(),
                                        ),
                                        member_generic_arguments,
                                        parent_generic_arguments,
                                    }),
                                    self.span,
                                );

                                transform_instantiation(
                                    function_call.instantiation,
                                    inference_context,
                                )
                            }

                            CallableID::TraitImplementationFunction(_)
                            | CallableID::AdtImplementationFunction(_) => {
                                let parent_implementation_id =
                                    match function_call.callable_id {
                                        CallableID::TraitImplementationFunction(id) => {
                                            ImplementationID::PositiveTrait(
                                                table.get(id).unwrap().parent_id(),
                                            )
                                        }
                                        CallableID::AdtImplementationFunction(id) => {
                                            ImplementationID::Adt(
                                                table.get(id).unwrap().parent_id(),
                                            )
                                        }
                                        _ => unreachable!()
                                    };

                                let mut parent_generic_arguments =
                                    GenericArguments::from_other_model(
                                        table
                                            .get_implementation(
                                                parent_implementation_id,
                                            )
                                            .unwrap()
                                            .arguments()
                                            .clone(),
                                    );

                                parent_generic_arguments
                                    .instantiate(&function_call.instantiation);

                                let member_generic_arguments = function_call
                                    .instantiation
                                    .create_generic_arguments(
                                        function_call.callable_id.into(),
                                        &table
                                            .get_generic(
                                                function_call
                                                    .callable_id
                                                    .into(),
                                            )
                                            .unwrap()
                                            .generic_declaration()
                                            .parameters,
                                    )
                                    .unwrap();

                                transformer.transform(
                                    Type::MemberSymbol(MemberSymbol {
                                        id: r#type::MemberSymbolID::Function(
                                            match function_call.callable_id {
                                                CallableID::TraitImplementationFunction(id) => id.into(),
                                                CallableID::AdtImplementationFunction(id) => id.into(),
                                                _ => unreachable!()
                                            }
                                        ),
                                        member_generic_arguments,
                                        parent_generic_arguments,
                                    }),
                                    self.span,
                                ).into_member_symbol().unwrap();

                                transform_instantiation(
                                    function_call.instantiation,
                                    inference_context,
                                )
                            }
                        },
                    })
                }
                Assignment::Binary(binary) => {
                    Assignment::Binary(register::Binary {
                        lhs: binary.lhs.transform_model(transformer),
                        rhs: binary.rhs.transform_model(transformer),
                        operator: binary.operator,
                    })
                }
                Assignment::Array(array) => {
                    Assignment::Array(register::Array {
                        elements: array
                            .elements
                            .into_iter()
                            .map(|x| x.transform_model(transformer))
                            .collect(),
                        element_type: transformer
                            .transform(array.element_type, self.span),
                    })
                }
                Assignment::Phi(phi) => Assignment::Phi(register::Phi {
                    incoming_values: phi
                        .incoming_values
                        .into_iter()
                        .map(|(k, v)| {
                            (
                                ID::from_index(k.into_index()),
                                v.transform_model(transformer),
                            )
                        })
                        .collect(),
                    r#type: transformer.transform(phi.r#type, self.span),
                }),
                Assignment::Cast(cast) => Assignment::Cast(register::Cast {
                    value: cast.value.transform_model(transformer),
                    r#type: transformer.transform(cast.r#type, self.span),
                }),
                Assignment::VariantNumber(variant_number) => {
                    Assignment::VariantNumber(register::VariantNumber {
                        address: variant_number
                            .address
                            .transform_model(transformer),
                    })
                }
            },
        }
    }
}

pub fn transform_inference(
    mut original: ir::Representation<infer::Model>,
    inference_context: &infer::Context,
    table: &Table<impl table::State>,
    handler: &HandlerWrapper,
) -> ir::Representation<ir::Model> {
    original.control_flow_graph.remove_unerachable_blocks();

    let used_allocas = original
        .control_flow_graph
        .traverse()
        .flat_map(|x| x.1.instructions())
        .filter_map(|x| x.as_alloca_declaration().map(|x| x.id))
        .collect::<HashSet<_>>();

    original.allocas.retain(|id, _| used_allocas.contains(&id));

    let used_registers = original
        .control_flow_graph
        .traverse()
        .flat_map(|x| x.1.instructions())
        .filter_map(|x| x.as_register_assignment().map(|x| x.id))
        .collect::<HashSet<_>>();

    original.registers.retain(|id, _| used_registers.contains(&id));

    let mut result = ir::Representation::<ir::Model>::default();

    let mut transformer =
        Transformer { inference_context, handler, should_report: false };

    result.allocas =
        original.allocas.map(|x| x.transform_model(&mut transformer));

    transformer.should_report = true;

    result.control_flow_graph =
        original.control_flow_graph.transform_model(&mut transformer);
    result.registers = original
        .registers
        .map(|x| x.transform_model(&mut transformer, inference_context, table));
    result.scope_tree = original.scope_tree;

    result
}

#[cfg(test)]
mod test;
