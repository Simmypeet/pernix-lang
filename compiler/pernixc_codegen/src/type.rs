//! Contains the logic mapping from the Pernix type to LLVM's type.

use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap},
    rc::Rc,
};

use getset::Getters;
use inkwell::{
    types::{BasicType, BasicTypeEnum, StructType},
    AddressSpace,
};
use pernixc_arena::ID;
use pernixc_component::fields::{Field, Fields};
use pernixc_ir::model::Erased;
use pernixc_table::component::SymbolKind;
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameter::GenericParameters,
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    r#type::{Primitive, Tuple, Type},
    sub_term::TermLocation,
    visitor::MutableRecursive,
    Model as _, Symbol,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    normalizer,
    term::Term,
};

use crate::{context::Context, zst::Zst, Model};

/// The signature of the struct in LLVM.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LlvmStructSignature<'ctx> {
    /// The LLVM struct type.
    pub llvm_struct_type: StructType<'ctx>,

    /// The LLVM types might not contains all the fields defined in the Pernix
    /// front-end, the zero-sized type fields are removed. Note that the order
    /// of the fields in this vector is still preserved.
    pub llvm_field_types: Vec<BasicTypeEnum<'ctx>>,

    /// The original struct ID in the Pernix table.
    ///
    /// If a particular field id does not have appear in this map, it means
    /// that the field is a ZST.
    pub llvm_field_indices_by_field_id: HashMap<ID<Field>, usize>,
}

/// The signature of the tuple in LLVM.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LlvmTupleSignature<'ctx> {
    /// The LLVM struct type of the tuple.
    pub llvm_tuple_type: StructType<'ctx>,

    /// The LLVM types of the elements in the tuple.
    pub llvm_element_types: Vec<BasicTypeEnum<'ctx>>,

    /// The original tuple index in the Pernix front-end.
    pub llvm_field_indices_by_tuple_idnex: HashMap<usize, usize>,
}

/// Represents the mapping between the Pernix type and the LLVM type.
#[derive(Debug, Default, Getters)]
pub struct Map<'ctx> {
    /// The mapping between the ADT instantiation and the LLVM type.
    ///
    /// If the value is `None`, it means that the struct is `Zero-sized type`.
    struct_sigantures:
        HashMap<Symbol<Model>, Option<Rc<LlvmStructSignature<'ctx>>>>,

    /// The mapping between the tuple and the LLVM type.
    ///
    /// If the value is `None`, it means that the tuple is `Zero-sized type`.
    tuple_signatures:
        HashMap<Tuple<Model>, Option<Rc<LlvmTupleSignature<'ctx>>>>,
}

/// A mutable visitor that erases all the lifetimes in the term.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct EraseLifetime;

impl MutableRecursive<Lifetime<Model>> for EraseLifetime {
    fn visit(
        &mut self,
        term: &mut Lifetime<Model>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        *term = Lifetime::Inference(Erased);

        true
    }
}

impl MutableRecursive<Type<Model>> for EraseLifetime {
    fn visit(
        &mut self,
        _: &mut Type<Model>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Constant<Model>> for EraseLifetime {
    fn visit(
        &mut self,
        _: &mut Constant<Model>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl<'ctx> Context<'_, 'ctx> {
    /// Performs [`Self::normalize_term`] on all the terms in the generic
    /// arguments.
    pub fn normalize_instantiation(
        &self,
        instantiation: &mut Instantiation<Model>,
    ) {
        let mut erase_lifetime = EraseLifetime;

        for lifetime in instantiation.lifetimes.values_mut() {
            pernixc_term::visitor::accept_recursive_mut(
                lifetime,
                &mut erase_lifetime,
            );
        }

        for ty in &mut instantiation.types.values_mut() {
            pernixc_term::visitor::accept_recursive_mut(
                ty,
                &mut erase_lifetime,
            );
            take_mut::take(ty, |ty| self.normalize_term(ty));
        }

        for constant in &mut instantiation.constants.values_mut() {
            pernixc_term::visitor::accept_recursive_mut(
                constant,
                &mut erase_lifetime,
            );
            take_mut::take(constant, |constant| self.normalize_term(constant));
        }
    }

    /// Performs [`Self::normalize_term`] on all the terms in the generic
    /// arguments.
    pub fn normalize_generic_arguments(
        &self,
        generic_arguments: &mut GenericArguments<Model>,
    ) {
        let mut erase_lifetime = EraseLifetime;

        for lifetime in &mut generic_arguments.lifetimes {
            pernixc_term::visitor::accept_recursive_mut(
                lifetime,
                &mut erase_lifetime,
            );
        }

        for ty in &mut generic_arguments.types {
            pernixc_term::visitor::accept_recursive_mut(
                ty,
                &mut erase_lifetime,
            );
            take_mut::take(ty, |ty| self.normalize_term(ty));
        }

        for constant in &mut generic_arguments.constants {
            pernixc_term::visitor::accept_recursive_mut(
                constant,
                &mut erase_lifetime,
            );
            take_mut::take(constant, |constant| self.normalize_term(constant));
        }
    }

    /// Normalizes the term by removing all the trait member types and packed
    /// tuple elements.
    pub fn normalize_term<T: Term<Model = Model>>(&self, mut term: T) -> T {
        let env = Environment::new(
            Cow::Owned(Premise::<Model> {
                predicates: BTreeSet::default(),
                // NOTE: query site should not influence the simplification
                // process
                query_site: None,
            }),
            self.table(),
            normalizer::NO_OP,
        );

        let mut erase_lifetime = EraseLifetime;
        pernixc_term::visitor::accept_recursive_mut(
            &mut term,
            &mut erase_lifetime,
        );

        env.simplify(term).unwrap().result.clone()
    }

    /// Mono-morphizes the term by instantiating the term, normalizing it, and
    /// erase all the lifetimes.
    pub fn monomorphize_term<T: Term<Model = Model>>(
        &self,
        mut term: T,
        instantiation: &Instantiation<Model>,
    ) -> T {
        instantiation::instantiate(&mut term, instantiation);

        self.normalize_term(term)
    }

    /// Gets the [`LlvmStructSignature`] from the Pernix tuple type.
    #[allow(clippy::missing_errors_doc)]
    pub fn get_struct_type(
        &mut self,
        symbol: Symbol<Model>,
    ) -> Result<Rc<LlvmStructSignature<'ctx>>, Zst<Symbol<Model>>> {
        let generic_params =
            self.table().query::<GenericParameters>(symbol.id).unwrap();

        let instantiation = Instantiation::from_generic_arguments(
            symbol.generic_arguments.clone(),
            symbol.id,
            &generic_params,
        )
        .unwrap();

        // already monomorphized
        if let Some(value) =
            self.type_map_mut().struct_sigantures.get(&symbol).cloned()
        {
            return value.ok_or(Zst(symbol));
        }

        let fields = self.table().query::<Fields>(symbol.id).unwrap();

        let mut llvm_field_types = Vec::new();
        let mut llvm_field_indices_by_field_id = HashMap::new();
        let mut current_order = 0;

        for field_id in fields.field_declaration_order.iter().copied() {
            let field = &fields.fields[field_id];
            let llvm_field_ty = self.get_type(self.monomorphize_term(
                Model::from_default_type(field.r#type.clone()),
                &instantiation,
            ));

            if let Ok(ty) = llvm_field_ty {
                llvm_field_types.push(ty);
                llvm_field_indices_by_field_id.insert(field_id, current_order);
                current_order += 1;
            }
        }

        let ty = if llvm_field_types.is_empty() {
            None
        } else {
            Some(Rc::new(LlvmStructSignature {
                llvm_struct_type: self
                    .context()
                    .struct_type(&llvm_field_types, false),
                llvm_field_types,
                llvm_field_indices_by_field_id,
            }))
        };

        self.type_map_mut()
            .struct_sigantures
            .insert(symbol.clone(), ty.clone());

        ty.ok_or(Zst(symbol))
    }

    /// Gets the [`LlvmTupleSignature`] from the Pernix tuple type.
    #[allow(clippy::missing_errors_doc)]
    pub fn get_tuple_type(
        &mut self,
        tuple: Tuple<Model>,
    ) -> Result<Rc<LlvmTupleSignature<'ctx>>, Zst<Tuple<Model>>> {
        if let Some(value) =
            self.type_map_mut().tuple_signatures.get(&tuple).cloned()
        {
            return value.ok_or(Zst(tuple));
        }

        let mut llvm_indices_by_tuple_index = HashMap::new();
        let mut elements = Vec::new();

        for (index, element) in tuple.elements.iter().enumerate() {
            assert!(
                !element.is_unpacked,
                "unpacked tuple element found {element:?}"
            );

            if let Ok(llvm_ty) = self.get_type(element.term.clone()) {
                llvm_indices_by_tuple_index.insert(index, elements.len());
                elements.push(llvm_ty);
            }
        }

        let ty = if elements.is_empty() {
            None
        } else {
            Some(Rc::new(LlvmTupleSignature {
                llvm_tuple_type: self.context().struct_type(&elements, false),
                llvm_element_types: elements,
                llvm_field_indices_by_tuple_idnex: llvm_indices_by_tuple_index,
            }))
        };

        self.type_map_mut().tuple_signatures.insert(tuple.clone(), ty.clone());

        ty.ok_or(Zst(tuple))
    }

    /// Retrieves the LLVM type from the Pernix type. The Pernix type must be
    /// fully monomorphized, meaning that there shouldn't be generic parameters,
    /// trait members, or non-erased lifetimes.
    #[allow(clippy::too_many_lines, clippy::missing_errors_doc)]
    pub fn get_type(
        &mut self,
        ty: Type<Model>,
    ) -> Result<BasicTypeEnum<'ctx>, Zst> {
        match ty {
            Type::Primitive(primitive) => match primitive {
                Primitive::Int8 | Primitive::Uint8 => {
                    Ok(self.context().i8_type().into())
                }
                Primitive::Int16 | Primitive::Uint16 => {
                    Ok(self.context().i16_type().into())
                }
                Primitive::Int32 | Primitive::Uint32 => {
                    Ok(self.context().i32_type().into())
                }
                Primitive::Int64 | Primitive::Uint64 => {
                    Ok(self.context().i64_type().into())
                }

                Primitive::Float32 => Ok(self.context().f32_type().into()),
                Primitive::Float64 => Ok(self.context().f64_type().into()),
                Primitive::Bool => Ok(self.context().bool_type().into()),

                Primitive::Isize | Primitive::Usize => Ok(self
                    .context()
                    .ptr_sized_int_type(self.target_data(), None)
                    .into()),
            },

            Type::Error(error) => {
                panic!("error type found {error:?}")
            }
            Type::Parameter(member_id) => {
                panic!("non-monomorphized type found {member_id:?}")
            }

            Type::Reference(_) | Type::Pointer(_) => {
                Ok(self.context().ptr_type(AddressSpace::default()).into())
            }

            Type::Inference(infer) => match infer {},

            Type::Array(array_ty) => {
                if *array_ty.length.as_primitive().unwrap().as_usize().unwrap()
                    == 0
                {
                    return Err(Zst(Type::Array(array_ty)));
                }
                let element_ty = self.get_type(*array_ty.r#type)?;

                Ok(element_ty
                    .array_type(
                        (*array_ty
                            .length
                            .as_primitive()
                            .unwrap()
                            .as_usize()
                            .unwrap())
                        .try_into()
                        .unwrap(),
                    )
                    .into())
            }

            Type::Symbol(symbol) => {
                let symbol_kind = *self.table().get::<SymbolKind>(symbol.id);

                match symbol_kind {
                    SymbolKind::Struct => {
                        let struct_signature = self
                            .get_struct_type(symbol)
                            .map_err(|x| Zst(x.0.into()))?;

                        Ok(struct_signature.llvm_struct_type.into())
                    }

                    SymbolKind::Enum => todo!(),

                    _ => panic!("unsupported symbol kind {symbol_kind:?}"),
                }
            }

            Type::Tuple(tuple) => {
                let tuple_signature =
                    self.get_tuple_type(tuple).map_err(|x| Zst(x.0.into()))?;

                Ok(tuple_signature.llvm_tuple_type.into())
            }

            ty @ Type::Phantom(_) => Err(Zst(ty)),

            Type::MemberSymbol(_) | Type::TraitMember(_) => {
                panic!("unsupported type {ty:?}")
            }

            Type::FunctionSignature(function_signature) => {
                panic!("should not be here {function_signature:?}")
            }
        }
    }
}
