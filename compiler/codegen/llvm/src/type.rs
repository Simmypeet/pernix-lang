//! Contains the logic mapping from the Pernix type to LLVM's type.

use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap},
    rc::Rc,
};

use getset::Getters;
use inkwell::{
    AddressSpace,
    types::{BasicType, BasicTypeEnum, IntType, PointerType, StructType},
};
use pernixc_arena::ID;
use pernixc_semantic_element::{
    fields::{Field, get_fields},
    variant::get_variant_associated_type,
};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    member::get_members,
    name::get_name,
    variant_declaration_order::get_variant_declaration_order,
};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    display::Display,
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::get_generic_parameters,
    instantiation::Instantiation,
    lifetime::Lifetime,
    sub_term::TermLocation,
    r#type::{Primitive, Tuple, Type},
    visitor::MutableRecursive,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    normalizer,
    term::Term,
};

use crate::{context::Context, zst::Zst};

/// An extension trait for [`BasicTypeEnum`] to check whether the type is an
/// aggregate type.
pub trait IsAggregateTypeExt {
    /// Checks whether the type is an aggregate type.
    fn is_aggregate(&self) -> bool;
}

impl IsAggregateTypeExt for BasicTypeEnum<'_> {
    fn is_aggregate(&self) -> bool {
        matches!(
            self,
            BasicTypeEnum::ArrayType(_)
                | BasicTypeEnum::StructType(_)
                | BasicTypeEnum::VectorType(_)
        )
    }
}

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

/// A niche optimization on the enum that can be represented as a nullable
/// pointer.
///
/// It's when the enum contains 2 variants where one of the is ZST and  the
/// other is reference type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NullablePointer<'ctx> {
    /// The LLVM pointer type
    pub pointer: PointerType<'ctx>,

    /// The variant index that is ZST.
    pub null_variant_index: u8,
}

/// Enum represented as a tagged union.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TaggedUnion<'ctx> {
    /// The LLVM struct type of the tagged union.
    ///
    /// The representation looks like this:
    /// ``` txt
    /// type { {tag_ty, payload }, [i8 x extra_bytes_needed] }
    /// ```
    /// where the `{tag_ty, payload}` is the variant that has the most
    /// alignment requirement and `[i8 x extra_bytes_needed]` is the padding
    /// to make sure that the size of the tagged union is equal to the size of
    /// the largest variant.
    pub llvm_struct_type: StructType<'ctx>,

    /// The LLVM types of each variant in the tagged union.
    ///
    /// The pointer will be casted from the [`Self::llvm_struct_type`] to the
    /// variant type.
    ///
    /// The representation looks like this:
    /// `type { tag_ty, payload } or type { tag_ty }` depending on whether the
    /// variant's associated type is ZST or not.
    pub llvm_variant_types:
        HashMap<Global<pernixc_symbol::ID>, StructType<'ctx>>,

    /// The layout of the tagged union of the most aligned variant.
    pub most_alignment_type: StructType<'ctx>,

    /// The LLVM type of the tag.
    pub llvm_tag_type: IntType<'ctx>,
}

/// Represents a translation from the Pernix enum to the LLVM enum.
///
/// The Pernix enum can be translated into different flavours of structure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LlvmEnumSignature<'ctx> {
    /// The enum is a zero-sized type. Either it has no variants at all all has
    /// one variant that is ZST.
    Zst,

    /// The enum can be represented as a nullable pointer; there're 2
    /// variants in the enum  where one of them is `ZST` and the other is a
    /// reference.
    NullablePointer(NullablePointer<'ctx>),

    /// The enum is a transparent enum; it has only one variant that is  not
    /// ZST.
    Transparent(BasicTypeEnum<'ctx>),

    /// The enum is a numeric enum; it has multiple variants where all of them
    /// are ZST.
    Numeric(IntType<'ctx>),

    /// The enum is a tagged union; the most general form of the enum.
    TaggedUnion(TaggedUnion<'ctx>),
}

/// Represents the mapping between the Pernix type and the LLVM type.
#[derive(Debug, Default, Getters)]
pub struct Map<'ctx> {
    /// The mapping between the ADT instantiation and the LLVM type.
    ///
    /// If the value is `None`, it means that the struct is `Zero-sized type`.
    struct_sigantures: HashMap<Symbol, Option<Rc<LlvmStructSignature<'ctx>>>>,

    /// The mapping between the tuple and the LLVM type.
    ///
    /// If the value is `None`, it means that the tuple is `Zero-sized type`.
    tuple_signatures: HashMap<Tuple, Option<Rc<LlvmTupleSignature<'ctx>>>>,

    /// The mapping between the enum and the LLVM type.
    enum_signatures: HashMap<Symbol, Rc<LlvmEnumSignature<'ctx>>>,
}

/// A mutable visitor that erases all the lifetimes in the term.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct EraseLifetime;

impl MutableRecursive<Lifetime> for EraseLifetime {
    fn visit(
        &mut self,
        term: &mut Lifetime,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        *term = Lifetime::Erased;

        true
    }
}

impl MutableRecursive<Type> for EraseLifetime {
    fn visit(
        &mut self,
        _: &mut Type,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Constant> for EraseLifetime {
    fn visit(
        &mut self,
        _: &mut Constant,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl<'ctx> Context<'_, 'ctx> {
    /// Performs [`Self::normalize_term`] on all the terms in the generic
    /// arguments.
    pub async fn normalize_instantiation(
        &self,
        instantiation: &mut Instantiation,
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

            *ty = self.normalize_term(std::mem::take(ty)).await;
        }

        for constant in &mut instantiation.constants.values_mut() {
            pernixc_term::visitor::accept_recursive_mut(
                constant,
                &mut erase_lifetime,
            );

            *constant = self.normalize_term(std::mem::take(constant)).await;
        }
    }

    /// Performs [`Self::normalize_term`] on all the terms in the generic
    /// arguments.
    pub async fn normalize_generic_arguments(
        &self,
        generic_arguments: &mut GenericArguments,
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
            *ty = self.normalize_term(std::mem::take(ty)).await;
        }

        for constant in &mut generic_arguments.constants {
            pernixc_term::visitor::accept_recursive_mut(
                constant,
                &mut erase_lifetime,
            );
            *constant = self.normalize_term(std::mem::take(constant)).await;
        }
    }

    /// Normalizes the term by removing all the trait member types and packed
    /// tuple elements.
    pub async fn normalize_term<T: Term>(&self, mut term: T) -> T {
        let env = Environment::new(
            Cow::Owned(Premise {
                predicates: BTreeSet::default(),
                // NOTE: query site should not influence the simplification
                // process
                query_site: None,
            }),
            Cow::Borrowed(self.engine()),
            normalizer::NO_OP,
        );

        let mut erase_lifetime = EraseLifetime;
        pernixc_term::visitor::accept_recursive_mut(
            &mut term,
            &mut erase_lifetime,
        );

        env.simplify(term).await.unwrap().result.clone()
    }

    /// Mono-morphizes the term by instantiating the term, normalizing it, and
    /// erase all the lifetimes.
    pub async fn monomorphize_term<T: Term>(
        &self,
        mut term: T,
        instantiation: &Instantiation,
    ) -> T {
        instantiation.instantiate(&mut term);

        self.normalize_term(term).await
    }

    /// Gets the [`LlvmStructSignature`] from the Pernix tuple type.
    #[allow(clippy::missing_errors_doc)]
    pub async fn get_struct_type(
        &mut self,
        symbol: Symbol,
    ) -> Result<Rc<LlvmStructSignature<'ctx>>, Zst> {
        let generic_params =
            self.engine().get_generic_parameters(symbol.id).await;

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
            return value.ok_or(Zst);
        }

        let fields = self.engine().get_fields(symbol.id).await;

        let mut llvm_field_types = Vec::new();
        let mut llvm_field_indices_by_field_id = HashMap::new();
        let mut current_order = 0;

        for field_id in fields.field_declaration_order.iter().copied() {
            let field = &fields.fields[field_id];
            let llvm_field_ty = Box::pin(
                self.get_type(
                    self.monomorphize_term(
                        field.r#type.clone(),
                        &instantiation,
                    )
                    .await,
                ),
            )
            .await;

            if let Ok(ty) = llvm_field_ty {
                llvm_field_types.push(ty);
                llvm_field_indices_by_field_id.insert(field_id, current_order);
                current_order += 1;
            }
        }

        let ty = if llvm_field_types.is_empty() {
            None
        } else {
            let mut qualified_name = String::new();
            symbol
                .write_async(self.engine(), &mut qualified_name)
                .await
                .unwrap();

            let llvm_struct_type =
                self.context().opaque_struct_type(&qualified_name);

            assert!(llvm_struct_type.set_body(&llvm_field_types, false));

            Some(Rc::new(LlvmStructSignature {
                llvm_struct_type,
                llvm_field_types,
                llvm_field_indices_by_field_id,
            }))
        };

        self.type_map_mut().struct_sigantures.insert(symbol, ty.clone());

        ty.ok_or(Zst)
    }

    /// Gets the [`LlvmTupleSignature`] from the Pernix tuple type.
    #[allow(clippy::missing_errors_doc)]
    pub async fn get_tuple_type(
        &mut self,
        tuple: Tuple,
    ) -> Result<Rc<LlvmTupleSignature<'ctx>>, Zst> {
        if let Some(value) =
            self.type_map_mut().tuple_signatures.get(&tuple).cloned()
        {
            return value.ok_or(Zst);
        }

        let mut llvm_indices_by_tuple_index = HashMap::new();
        let mut elements = Vec::new();

        for (index, element) in tuple.elements.iter().enumerate() {
            assert!(
                !element.is_unpacked,
                "unpacked tuple element found {element:?}"
            );

            if let Ok(llvm_ty) =
                Box::pin(self.get_type(element.term.clone())).await
            {
                llvm_indices_by_tuple_index.insert(index, elements.len());
                elements.push(llvm_ty);
            }
        }

        let ty = if elements.is_empty() {
            None
        } else {
            let mut qualified_name = String::new();
            tuple
                .write_async(self.engine(), &mut qualified_name)
                .await
                .unwrap();

            let llvm_tuple_type =
                self.context().opaque_struct_type(&qualified_name);

            assert!(llvm_tuple_type.set_body(&elements, false));

            Some(Rc::new(LlvmTupleSignature {
                llvm_tuple_type,
                llvm_element_types: elements,
                llvm_field_indices_by_tuple_idnex: llvm_indices_by_tuple_index,
            }))
        };

        self.type_map_mut().tuple_signatures.insert(tuple, ty.clone());

        ty.ok_or(Zst)
    }

    /// Retrieves the [`LlvmEnumSignature`] from the Pernix enum type.
    #[allow(
        clippy::missing_errors_doc,
        clippy::cast_sign_loss,
        clippy::cast_possible_truncation,
        clippy::cast_precision_loss,
        clippy::too_many_lines,
        clippy::cognitive_complexity // omg, refactor this pleaese
    )]
    pub async fn get_enum_type(
        &mut self,
        symbol: Symbol,
    ) -> Rc<LlvmEnumSignature<'ctx>> {
        fn get_bit_count(length: usize) -> usize {
            // calculates the number of bits required to represent the enum
            let bits = (length as f64).log2().ceil() as u32;
            let mut ceiled_bit_count = 1;

            while bits > ceiled_bit_count {
                if ceiled_bit_count == 1 {
                    ceiled_bit_count = 8;
                    continue;
                }

                // i8 -> i16 -> i32 -> i64, etc.
                ceiled_bit_count *= 2;
            }

            ceiled_bit_count as usize
        }

        if let Some(value) =
            self.type_map_mut().enum_signatures.get(&symbol).cloned()
        {
            return value;
        }

        let generic_params =
            self.engine().get_generic_parameters(symbol.id).await;

        let instantiation = Instantiation::from_generic_arguments(
            symbol.generic_arguments.clone(),
            symbol.id,
            &generic_params,
        )
        .unwrap();

        let mut variants = Vec::new();

        for variant in self
            .engine()
            .get_members(symbol.id)
            .await
            .member_ids_by_name
            .values()
            .copied()
            .map(|x| symbol.id.target_id.make_global(x))
        {
            variants.push((
                variant,
                self.engine().get_variant_declaration_order(variant).await,
            ));
        }

        variants.sort_by_key(|x| x.1);

        let mut llvm_variant_types = Vec::new();

        // gets the of each enum variants
        for (variant_id, _) in variants.iter().copied() {
            let variant =
                self.engine().get_variant_associated_type(variant_id).await;

            if let Some(associated_ty) = &variant {
                let mut ty = (**associated_ty).clone();
                ty = self.monomorphize_term(ty, &instantiation).await;

                llvm_variant_types.push(
                    Box::pin(self.get_type(ty.clone()))
                        .await
                        .ok()
                        .map(|x| (x, ty)),
                );
            } else {
                llvm_variant_types.push(None);
            }
        }

        // the enum can be categorized into five kinds
        //
        // 1. Nullable pointer, for example Option[&T] like type
        // 2. ZST, for example enum { A(()) } or enum {}
        // 3. Transparent enum, for example enum { A(T) }
        // 4. Numeric enum, for example enum { A, B, C }
        // 5. Tagged union, for example enum { A(i32), B(f32), C(bool) }

        // must have 2 variants, one is `zst` and the other is `reference`. In
        // pnx, null reference is not allowed therefore, we can safely
        // assume that the `0` pointer value is the `zst` variant.

        // the enum is ZST
        let enum_ty = if llvm_variant_types.is_empty()
            || (llvm_variant_types.len() == 1
                && llvm_variant_types[0].is_none())
        {
            LlvmEnumSignature::Zst
        }
        // the enum is nullable pointer
        else if llvm_variant_types.len() == 2
            && llvm_variant_types.iter().any(Option::is_none)
            && llvm_variant_types
                .iter()
                .any(|x| x.as_ref().is_some_and(|x| x.1.is_reference()))
        {
            let (null_index, pointer_index) = if llvm_variant_types[0].is_none()
            {
                (0u8, 1)
            } else {
                (1u8, 0)
            };
            LlvmEnumSignature::NullablePointer(NullablePointer {
                pointer: llvm_variant_types[pointer_index]
                    .as_ref()
                    .unwrap()
                    .0
                    .into_pointer_type(),
                null_variant_index: null_index,
            })
        }
        // the enum is transparent
        else if llvm_variant_types.len() == 1
            && llvm_variant_types[0].is_some()
        {
            LlvmEnumSignature::Transparent(
                llvm_variant_types[0].as_ref().unwrap().0,
            )
        }
        // the enum is numeric
        else if llvm_variant_types.iter().all(Option::is_none) {
            LlvmEnumSignature::Numeric(self.context().custom_width_int_type(
                get_bit_count(llvm_variant_types.len()).try_into().unwrap(),
            ))
        }
        // normal tagged union
        else {
            let mut enum_qualified_name = String::new();
            symbol
                .write_async(self.engine(), &mut enum_qualified_name)
                .await
                .unwrap();

            let tag_bit_count = get_bit_count(llvm_variant_types.len());
            let tag_ty = self
                .context()
                .custom_width_int_type(tag_bit_count.try_into().unwrap());

            let mut llvm_variant_with_tag_types = HashMap::new();
            for ((variant_id, _), variant) in
                variants.iter().copied().zip(llvm_variant_types)
            {
                let variant_qualified_name = format!(
                    "{}::{}",
                    enum_qualified_name,
                    self.engine().get_name(variant_id).await.as_ref()
                );

                let ty =
                    self.context().opaque_struct_type(&variant_qualified_name);

                if let Some((llvm_ty, _)) = variant {
                    assert!(ty.set_body(&[tag_ty.into(), llvm_ty], false));
                } else {
                    assert!(ty.set_body(&[tag_ty.into()], false));
                }

                llvm_variant_with_tag_types.insert(variant_id, ty);
            }

            // we'll pick the most aligned variant as the representation of the
            // enum and add a storage to make sure that it can hold up to the
            // largest variant.
            //
            // type { most_aligned_variant, [i8 x extra_bytes_needed] }

            let max_abi_aligned_variant = llvm_variant_with_tag_types
                .iter()
                .max_by_key(|(_, x)| self.target_data().get_abi_alignment(&**x))
                .map(|(x, _)| *x)
                .unwrap();
            let max_size_variant = llvm_variant_with_tag_types
                .iter()
                .max_by_key(|(_, x)| self.target_data().get_abi_size(&**x))
                .map(|(x, _)| *x)
                .unwrap();

            let max_abi_aligned_ty = BasicTypeEnum::from(
                llvm_variant_with_tag_types
                    .get(&max_abi_aligned_variant)
                    .copied()
                    .unwrap(),
            );
            let max_size_ty = BasicTypeEnum::from(
                llvm_variant_with_tag_types
                    .get(&max_size_variant)
                    .copied()
                    .unwrap(),
            );

            // the minimum number of bytes needed to make sure that the
            // representation can hold up to the largest variant in the enum
            let extra_bytes_needed =
                self.target_data().get_abi_size(&max_size_ty)
                    - self.target_data().get_abi_size(&max_abi_aligned_ty);

            let repr = self.context().opaque_struct_type(&enum_qualified_name);

            if extra_bytes_needed == 0 {
                assert!(repr.set_body(&[max_abi_aligned_ty], false));
            } else {
                // pedantic check
                assert_eq!(
                    self.target_data()
                        .get_abi_alignment(&self.context().i8_type()),
                    1
                );
                assert_eq!(
                    self.target_data().get_abi_size(&self.context().i8_type()),
                    1
                );

                assert!(
                    repr.set_body(
                        &[
                            max_abi_aligned_ty,
                            self.context()
                                .i8_type()
                                .array_type(
                                    extra_bytes_needed.try_into().unwrap()
                                )
                                .into(),
                        ],
                        false,
                    )
                );
            }

            // make sure it can accommodate the largest variant. the size might
            // not exactly matches the size of the largest variant since the
            // added extra bytes might not align with the alignment of the
            // largest variant in the enum.
            assert!(
                self.target_data().get_abi_size(&repr)
                    >= self.target_data().get_abi_size(&max_size_ty)
            );

            assert_eq!(
                self.target_data().get_abi_alignment(&repr),
                self.target_data().get_abi_alignment(&max_abi_aligned_ty)
            );

            LlvmEnumSignature::TaggedUnion(TaggedUnion {
                llvm_struct_type: repr,
                llvm_variant_types: llvm_variant_with_tag_types,
                most_alignment_type: max_abi_aligned_ty.into_struct_type(),
                llvm_tag_type: tag_ty,
            })
        };

        let ty = Rc::new(enum_ty);

        assert!(
            self.type_map_mut()
                .enum_signatures
                .insert(symbol, ty.clone())
                .is_none()
        );

        ty
    }

    /// Gets the LLVM type from the Pernix type.
    pub fn get_primitive_type(
        &mut self,
        primitive_ty: Primitive,
    ) -> BasicTypeEnum<'ctx> {
        match primitive_ty {
            Primitive::Int8 | Primitive::Uint8 => {
                self.context().i8_type().into()
            }
            Primitive::Int16 | Primitive::Uint16 => {
                self.context().i16_type().into()
            }
            Primitive::Int32 | Primitive::Uint32 => {
                self.context().i32_type().into()
            }
            Primitive::Int64 | Primitive::Uint64 => {
                self.context().i64_type().into()
            }

            Primitive::Bool => self.context().bool_type().into(),

            Primitive::Float32 => self.context().f32_type().into(),
            Primitive::Float64 => self.context().f64_type().into(),

            Primitive::Isize | Primitive::Usize => self
                .context()
                .ptr_sized_int_type(self.target_data(), None)
                .into(),
        }
    }

    /// Retrieves the LLVM type from the Pernix type. The Pernix type must be
    /// fully monomorphized, meaning that there shouldn't be generic parameters,
    /// trait members, or non-erased lifetimes.
    #[allow(clippy::too_many_lines, clippy::missing_errors_doc)]
    pub async fn get_type(
        &mut self,
        ty: Type,
    ) -> Result<BasicTypeEnum<'ctx>, Zst> {
        match ty {
            Type::Primitive(primitive) => {
                Ok(self.get_primitive_type(primitive))
            }

            Type::Error(error) => {
                panic!("error type found {error:?}")
            }
            Type::Parameter(member_id) => {
                panic!("non-monomorphized type found {member_id:?}")
            }

            Type::Reference(_) | Type::Pointer(_) => {
                Ok(self.context().ptr_type(AddressSpace::default()).into())
            }

            Type::Inference(infer) => panic!("inference type found {infer:?}"),

            Type::Array(array_ty) => {
                if *array_ty.length.as_primitive().unwrap().as_usize().unwrap()
                    == 0
                {
                    return Err(Zst);
                }
                let element_ty =
                    Box::pin(self.get_type(*array_ty.r#type)).await?;

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
                let symbol_kind = self.engine().get_kind(symbol.id).await;

                match symbol_kind {
                    Kind::Struct => {
                        let struct_signature = self
                            .get_struct_type(symbol)
                            .await
                            .map_err(|_| Zst)?;

                        Ok(struct_signature.llvm_struct_type.into())
                    }

                    Kind::Enum => {
                        let enum_signature = self.get_enum_type(symbol).await;

                        match &*enum_signature {
                            LlvmEnumSignature::Zst => Err(Zst),
                            LlvmEnumSignature::NullablePointer(
                                nullable_pointer,
                            ) => Ok(nullable_pointer.pointer.into()),
                            LlvmEnumSignature::Transparent(basic_type_enum) => {
                                Ok(*basic_type_enum)
                            }
                            LlvmEnumSignature::Numeric(int_type) => {
                                Ok((*int_type).into())
                            }
                            LlvmEnumSignature::TaggedUnion(tagged_union) => {
                                Ok(tagged_union.llvm_struct_type.into())
                            }
                        }
                    }

                    _ => panic!("unsupported symbol kind {symbol_kind:?}"),
                }
            }

            Type::Tuple(tuple) => {
                let tuple_signature =
                    self.get_tuple_type(tuple).await.map_err(|_| Zst)?;

                Ok(tuple_signature.llvm_tuple_type.into())
            }

            Type::Phantom(_) => Err(Zst),

            Type::MemberSymbol(_) | Type::TraitMember(_) => {
                panic!("unsupported type {ty:?}")
            }

            Type::FunctionSignature(function_signature) => {
                panic!("should not be here {function_signature:?}")
            }
        }
    }
}
