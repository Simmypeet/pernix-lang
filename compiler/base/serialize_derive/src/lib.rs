//! Procedural macros for automatic serialization trait implementations.

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, Data, DataEnum, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, Generics, Ident, Index,  Variant
};

/// Automatically derive the Serialize trait for structs and enums.
///
/// This macro generates an implementation of the `Serialize` trait that
/// serializes all fields of a struct or all variants of an enum.
///
/// # Example
///
/// ```ignore
/// use pernixc_serialize_derive::Serialize;
///
/// #[derive(Serialize)]
/// struct Person {
///     name: String,
///     age: u32,
/// }
///
/// #[derive(Serialize)]
/// struct Point<T> {
///     x: T,
///     y: T,
/// }
/// ```
#[proc_macro_derive(Serialize)]
pub fn derive_serialize(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    
    match expand_serialize(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn expand_serialize(input: &DeriveInput) -> Result<proc_macro2::TokenStream, syn::Error> {
    let name = &input.ident;
    let generics = &input.generics;
    
    match &input.data {
        Data::Struct(data_struct) => {
            Ok(expand_serialize_struct(name, generics, &data_struct.fields))
        }
        Data::Enum(data_enum) => {
            Ok(expand_serialize_enum(name, generics, data_enum))
        }
        Data::Union(_) => {
            Err(syn::Error::new_spanned(
                name,
                "Union serialization is not supported",
            ))
        }
    }
}

fn expand_serialize_struct(
    name: &Ident,
    generics: &Generics,
    fields: &Fields,
) -> proc_macro2::TokenStream {
    let serialize_body = match fields {
        Fields::Named(fields) => serialize_named_fields(name, fields),
        Fields::Unnamed(fields) => serialize_unnamed_fields(name, fields),
        Fields::Unit => serialize_unit_struct(name),
    };

    let (_, ty_generics, _) = generics.split_for_impl();
    let (generic_list, bounds) = generate_serialize_impl_generics(generics);

    quote! {
        impl<
            #(#generic_list),*
        > ::pernixc_serialize::__internal::Serialize<__S> for #name #ty_generics
        where
            #(#bounds),*
        {
            fn serialize(&self, serializer: &mut __S) -> Result<(), __S::Error> {
                #serialize_body
            }
        }
    }
}

fn serialize_named_fields(name: &Ident, fields: &FieldsNamed) -> proc_macro2::TokenStream {
    let struct_name_str = name.to_string();
    let field_count = fields.named.len();
    
    let field_serializations = fields.named.iter().map(|field| {
        let field_name = field.ident.as_ref().unwrap();
        let field_name_str = field_name.to_string();
        
        quote! {
            pernixc_serialize::__internal::Struct::serialize_field(
                &mut struct_serializer,
                #field_name_str,
                &self.#field_name
            )?;
        }
    });

    quote! {
        pernixc_serialize::__internal::Serializer::emit_struct(
            serializer,
            #struct_name_str,
            #field_count,
            |mut struct_serializer| {
                #(#field_serializations)*
                Ok(())
            }
        )
    }
}

fn serialize_unnamed_fields(name: &Ident, fields: &FieldsUnnamed) -> proc_macro2::TokenStream {
    let struct_name_str = name.to_string();
    let field_count = fields.unnamed.len();
    
    let field_serializations = fields.unnamed.iter().enumerate().map(|(i, _field)| {
        let index = Index::from(i);
        
        quote! {
            pernixc_serialize::__internal::TupleStruct::serialize_field(
                &mut tuple_struct_serializer,
                &self.#index
            )?;
        }
    });

    quote! {
        pernixc_serialize::__internal::Serializer::emit_tuple_struct(
            serializer,
            #struct_name_str,
            #field_count,
            |mut tuple_struct_serializer| {
                #(#field_serializations)*
                Ok(())
            }
        )
    }
}

fn serialize_unit_struct(name: &Ident) -> proc_macro2::TokenStream {
    let struct_name_str = name.to_string();
    
    quote! {
        pernixc_serialize::__internal::Serializer::emit_unit_struct(
            serializer,
            #struct_name_str
        )
    }
}

fn expand_serialize_enum(
    name: &Ident,
    generics: &Generics,
    data_enum: &DataEnum,
) -> proc_macro2::TokenStream {
    let serialize_body = serialize_enum_variants(name, &data_enum.variants);

    let (_, ty_generics, _) = generics.split_for_impl();
    let (generic_list, bounds) = generate_serialize_impl_generics(generics);
    
    quote! {
        impl<
            #(#generic_list),*
        > ::pernixc_serialize::__internal::Serialize<__S> for #name #ty_generics
        where
            #(#bounds),*
        {
            fn serialize(&self, serializer: &mut __S) -> Result<(), __S::Error> {
                #serialize_body
            }
        }
    }
}

fn serialize_enum_variants(
    enum_name: &Ident,
    variants: &syn::punctuated::Punctuated<Variant, syn::Token![,]>,
) -> proc_macro2::TokenStream {
    let enum_name_str = enum_name.to_string();
    
    let variant_arms = variants.iter().enumerate().map(|(index, variant)| {
        let variant_name = &variant.ident;
        let variant_name_str = variant_name.to_string();
        let Ok(variant_index) = u32::try_from(index) else {
            return syn::Error::new_spanned(
                variant_name,
                "Enum has too many variants to serialize",
            )
            .into_compile_error();
        };

        
        match &variant.fields {
            Fields::Unit => {
                // Unit variant: MyEnum::Variant
                quote! {
                    #enum_name::#variant_name => {
                        serializer.emit_unit_variant(#enum_name_str, #variant_index)
                    }
                }
            }
            Fields::Unnamed(fields) => {
                // Tuple variant: MyEnum::Variant(T1, T2, ...)
                let field_count = fields.unnamed.len();
                let field_bindings: Vec<_> = (0..field_count)
                    .map(|i| {
                        Ident::new(&format!("__field_{i}"), variant_name.span())
                    })
                    .collect();
                
                let field_serializations = field_bindings.iter().map(|field_name| {
                    quote! {
                        tuple_variant.serialize_field(#field_name)?;
                    }
                });
                
                quote! {
                    #enum_name::#variant_name(#(#field_bindings),*) => {
                        use pernixc_serialize::__internal::TupleVariant;
                        
                        serializer.emit_tuple_variant(
                            #enum_name_str,
                            #variant_name_str,
                            #variant_index,
                            #field_count,
                            |mut tuple_variant| {
                                #(#field_serializations)*
                                Ok(())
                            }
                        )
                    }
                }
            }
            Fields::Named(fields) => {
                // Struct variant: MyEnum::Variant { field1: T1, field2: T2, ... }
                let field_count = fields.named.len();
                let field_bindings = fields.named.iter().map(|field| {
                    let field_name = field.ident.as_ref().unwrap();
                    field_name
                });
                
                let field_serializations = fields.named.iter().map(|field| {
                    let field_name = field.ident.as_ref().unwrap();
                    let field_name_str = field_name.to_string();
                    
                    quote! {
                        struct_variant.serialize_field(#field_name_str, #field_name)?;
                    }
                });
                
                quote! {
                    #enum_name::#variant_name { #(#field_bindings),* } => {
                        use pernixc_serialize::__internal::StructVariant;
                        
                        serializer.emit_struct_variant(
                            #enum_name_str,
                            #variant_name_str,
                            #variant_index,
                            #field_count,
                            |mut struct_variant| {
                                #(#field_serializations)*
                                Ok(())
                            }
                        )
                    }
                }
            }
        }
    });
    
    quote! {
        match self {
            #(#variant_arms)*
        }
    }
}

/// Helper function to generate impl generics and where clause for Serialize implementations.
/// Returns (`generic_list`, `bounds`) where:
/// - `generic_list`: List of generic parameters for the impl block
/// - `bounds`: List of where clause predicates
fn generate_impl_generics(
    generics: &Generics,
    param_name: &str,
    param_bound: proc_macro2::TokenStream,
    type_bound_template: fn(&syn::Ident, &str) -> proc_macro2::TokenStream,
) -> (Vec<proc_macro2::TokenStream>, Vec<proc_macro2::TokenStream>) {
    let mut generic_list = Vec::new();
    let mut bounds = Vec::new();

    let param_ident = syn::Ident::new(param_name, proc_macro2::Span::call_site());

    // Always use consistent order: lifetimes, __D or __S, type parameters, const parameters
    
    // Add lifetimes first
    for lt in generics.lifetimes() {
        generic_list.push(quote! { #lt });
    }
    
    // Add the serializer/deserializer type parameter second
    generic_list.push(quote! { #param_ident });
    bounds.push(param_bound);
    
    // Add type parameters third
    for type_param in generics.type_params() {
        generic_list.push(quote! { #type_param });
        let param_ident_ref = &type_param.ident;
        bounds.push(type_bound_template(param_ident_ref, param_name));
    }

    // Add const parameters last
    for const_param in generics.const_params() {
        generic_list.push(quote! { #const_param });
    }

    // Add existing where clause predicates
    if let Some(where_clause) = &generics.where_clause {
        for predicate in &where_clause.predicates {
            bounds.push(predicate.into_token_stream());
        }
    }

    (generic_list, bounds)
}

fn generate_serialize_impl_generics(
    generics: &Generics,
) -> (Vec<proc_macro2::TokenStream>, Vec<proc_macro2::TokenStream>) {
    generate_impl_generics(
        generics,
        "__S",
        quote! { __S: ::pernixc_serialize::__internal::Serializer },
        |param_ident, param_name| {
            let param_token = syn::Ident::new(param_name, proc_macro2::Span::call_site());
            quote! { #param_ident: ::pernixc_serialize::__internal::Serialize<#param_token> }
        },
    )
}

fn generate_deserialize_impl_generics(
    generics: &Generics,
) -> (Vec<proc_macro2::TokenStream>, Vec<proc_macro2::TokenStream>) {
    generate_impl_generics(
        generics,
        "__D",
        quote! { __D: ::pernixc_serialize::__internal::Deserializer },
        |param_ident, param_name| {
            let param_token = syn::Ident::new(param_name, proc_macro2::Span::call_site());
            quote! { #param_ident: ::pernixc_serialize::__internal::Deserialize<#param_token> }
        },
    )
}

/// Automatically derive the Deserialize trait for structs and enums.
///
/// This macro generates an implementation of the `Deserialize` trait that
/// deserializes all fields of a struct or all variants of an enum.
///
/// # Example
///
/// ```ignore
/// use pernixc_serialize_derive::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Person {
///     name: String,
///     age: u32,
/// }
///
/// #[derive(Deserialize)]
/// struct Point<T> {
///     x: T,
///     y: T,
/// }
/// ```
#[proc_macro_derive(Deserialize)]
pub fn derive_deserialize(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    
    match expand_deserialize(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn expand_deserialize(input: &DeriveInput) -> Result<proc_macro2::TokenStream, syn::Error> {
    let name = &input.ident;
    let generics = &input.generics;
    
    match &input.data {
        Data::Struct(data_struct) => {
            Ok(expand_deserialize_struct(name, generics, &data_struct.fields))
        }
        Data::Enum(data_enum) => {
            Ok(expand_deserialize_enum(name, generics, data_enum))
        }
        Data::Union(_) => {
            Err(syn::Error::new_spanned(
                name,
                "Union deserialization is not supported",
            ))
        }
    }
}

fn expand_deserialize_struct(
    name: &Ident,
    generics: &Generics,
    fields: &Fields,
) -> proc_macro2::TokenStream {
    let deserialize_body = match fields {
        Fields::Named(fields) => deserialize_named_fields(name, fields),
        Fields::Unnamed(fields) => deserialize_unnamed_fields(name, fields),
        Fields::Unit => deserialize_unit_struct(name),
    };

    let (_, ty_generics, _) = generics.split_for_impl();
    let (generic_list, bounds) = generate_deserialize_impl_generics(generics);

    quote! {
        impl<
            #(#generic_list),*
        > ::pernixc_serialize::__internal::Deserialize<__D> for #name #ty_generics
        where
            #(#bounds),*
        {
            fn deserialize(deserializer: &mut __D) -> Result<Self, __D::Error> {
                #deserialize_body
            }
        }
    }
}

fn expand_deserialize_enum(
    name: &Ident,
    generics: &Generics,
    data_enum: &DataEnum,
) -> proc_macro2::TokenStream {
    let deserialize_body = deserialize_enum_variants(name, &data_enum.variants);

    let (_, ty_generics, _) = generics.split_for_impl();
    let (generic_list, bounds) = generate_deserialize_impl_generics(generics);

    quote! {
        impl<
            #(#generic_list),*
        > ::pernixc_serialize::__internal::Deserialize<__D> for #name #ty_generics
        where
            #(#bounds),*
        {
            fn deserialize(deserializer: &mut __D) -> Result<Self, __D::Error> {
                #deserialize_body
            }
        }
    }
}

/// Generates code for deserializing a struct with named fields or an enum struct variant.
/// The `wrapper` closure allows customizing how the field processing logic is wrapped.
#[allow(clippy::too_many_lines)]
fn generate_named_fields_deserialize<F>(
    fields: &FieldsNamed,
    constructor: &proc_macro2::TokenStream,
    access_trait: &str,
    wrapper: F,
) -> proc_macro2::TokenStream
where
    F: FnOnce(
        Vec<proc_macro2::TokenStream>, // field_names
        Vec<proc_macro2::TokenStream>, // field processing logic
    ) -> proc_macro2::TokenStream,
{
    let field_names: Vec<_> = fields.named.iter().map(|field| {
        let field_name_str = field.ident.as_ref().unwrap().to_string();
        quote! { #field_name_str }
    }).collect();
    
    // Generate field variables for each field
    let field_vars: Vec<_> = fields.named.iter().map(|field| {
        let field_name = field.ident.as_ref().unwrap();
        let var_name = Ident::new(&format!("__{field_name}_value"), field_name.span());
        (field_name, var_name)
    }).collect();
    
    // Initialize field variables
    let field_initializations: Vec<_> = field_vars.iter().map(|(_field_name, var_name)| {
        quote! {
            let mut #var_name: Option<_> = None;
        }
    }).collect();
    
    // Generate match arms for known fields
    let field_match_arms: Vec<_> = field_vars.iter().enumerate().map(|(index, (field_name, var_name))| {
        let field_name_str = field_name.to_string();
        let Ok(field_index) = u32::try_from(index) else {
            return syn::Error::new_spanned(
                field_name,
                "Struct has too many fields to deserialize",
            )
            .into_compile_error();
        };

        quote! {
            ::pernixc_serialize::__internal::Identifier::Name(#field_name_str) |
            ::pernixc_serialize::__internal::Identifier::Index(#field_index) => {
                if #var_name.is_some() {
                    Err(::pernixc_serialize::__internal::DeError::duplicated_field(
                        ::pernixc_serialize::__internal::Identifier::Name(#field_name_str)
                    ))
                } else {
                    #var_name = Some(::pernixc_serialize::__internal::FieldAccess::deserialize(field_access)?);
                    Ok(())
                }
            }
        }
    }).collect();
    
    // Check for missing fields and construct the final value using pattern matching
    let field_construction: Vec<_> = field_vars.iter().map(|(field_name, var_name)| {
        let field_name_str = field_name.to_string();
        quote! { 
            #field_name: match #var_name {
                Some(value) => value,
                None => return Err(::pernixc_serialize::__internal::DeError::missing_field(
                    ::pernixc_serialize::__internal::Identifier::Name(#field_name_str)
                )),
            }
        }
    }).collect();

    let access_trait_ident = Ident::new(access_trait, proc_macro2::Span::call_site());
    let field_processing_logic = vec![
        quote! { #(#field_initializations)* },
        quote! {
            // Process all fields from the input
            loop {
                let should_continue = ::pernixc_serialize::__internal::#access_trait_ident::next_field(
                    &mut access,
                    |field_opt| {
                        match field_opt {
                            Some((field_identifier, field_access)) => {
                                let result = match field_identifier {
                                    #(#field_match_arms)*
                                    ::pernixc_serialize::__internal::Identifier::Index(idx) => {
                                        Err(::pernixc_serialize::__internal::DeError::unknown_field(
                                            ::pernixc_serialize::__internal::Identifier::Index(idx)
                                        ))
                                    }
                                    ::pernixc_serialize::__internal::Identifier::Name(name) => {
                                        Err(::pernixc_serialize::__internal::DeError::unknown_field(
                                            ::pernixc_serialize::__internal::Identifier::Name(name)
                                        ))
                                    }
                                };
                                result?;
                                Ok(true) // Continue processing
                            }
                            None => {
                                // No more fields
                                Ok(false)
                            }
                        }
                    }
                )?;
                
                if !should_continue {
                    break;
                }
            }
        },
        quote! {
            // Construct the final value (missing fields are handled in the pattern matching)
            Ok(#constructor {
                #(#field_construction),*
            })
        },
    ];

    wrapper(field_names, field_processing_logic)
}

fn deserialize_named_fields(name: &Ident, fields: &FieldsNamed) -> proc_macro2::TokenStream {
    let struct_name_str = name.to_string();
    let constructor = quote! { #name };
    
    generate_named_fields_deserialize(
        fields,
        &constructor,
        "StructAccess",
        |field_names, field_processing_logic| {
            quote! {
                ::pernixc_serialize::__internal::Deserializer::expect_struct(
                    deserializer,
                    #struct_name_str,
                    &[#(#field_names),*],
                    |mut struct_access| {
                        let mut access = struct_access;
                        #(#field_processing_logic)*
                    }
                )
            }
        }
    )
}

/// Generates code for deserializing a tuple struct or tuple enum variant.
/// The `wrapper` closure allows customizing how the field processing logic is wrapped.
fn generate_unnamed_fields_deserialize<F>(
    fields: &FieldsUnnamed,
    access_trait: &str,
    access_var: &str,
    span: proc_macro2::Span,
    wrapper: F,
) -> proc_macro2::TokenStream
where
    F: FnOnce(
        usize, // field_count
        Vec<proc_macro2::TokenStream>, // field_deserializations
        Vec<proc_macro2::TokenStream>, // field_construction
    ) -> proc_macro2::TokenStream,
{
    let field_count = fields.unnamed.len();
    
    let field_deserializations: Vec<_> = (0..field_count).map(|i| {
        let field_name = Ident::new(&format!("field_{i}"), span);
        let access_trait_ident = Ident::new(access_trait, proc_macro2::Span::call_site());
        let access_var_ident = Ident::new(access_var, proc_macro2::Span::call_site());
        quote! {
            let #field_name = ::pernixc_serialize::__internal::#access_trait_ident::next_field(
                &mut #access_var_ident
            )?;
        }
    }).collect();
    
    let field_construction: Vec<_> = (0..field_count).map(|i| {
        let field_name = Ident::new(&format!("field_{i}"), span);
        quote! { #field_name }
    }).collect();

    wrapper(field_count, field_deserializations, field_construction)
}

fn deserialize_unnamed_fields(name: &Ident, fields: &FieldsUnnamed) -> proc_macro2::TokenStream {
    let struct_name_str = name.to_string();
    
    generate_unnamed_fields_deserialize(
        fields,
        "TupleStructAccess",
        "tuple_struct_access",
        name.span(),
        |field_count, field_deserializations, field_construction| {
            quote! {
                ::pernixc_serialize::__internal::Deserializer::expect_tuple_struct(
                    deserializer,
                    #struct_name_str,
                    #field_count,
                    |mut tuple_struct_access| {
                        #(#field_deserializations)*
                        Ok(#name(#(#field_construction),*))
                    }
                )
            }
        }
    )
}

fn deserialize_unit_struct(name: &Ident) -> proc_macro2::TokenStream {
    let struct_name_str = name.to_string();
    
    quote! {
        ::pernixc_serialize::__internal::Deserializer::expect_unit_struct(
            deserializer,
            #struct_name_str
        ).map(|_| #name)
    }
}

#[allow(clippy::too_many_lines)]
fn deserialize_enum_variants(
    enum_name: &Ident,
    variants: &syn::punctuated::Punctuated<Variant, syn::Token![,]>,
) -> proc_macro2::TokenStream {
    let enum_name_str = enum_name.to_string();
    
    let variant_arms = variants.iter().enumerate().map(|(index, variant)| {
        let variant_name = &variant.ident;
        let variant_name_str = variant_name.to_string();
        let Ok(variant_index) = u32::try_from(index) else {
            return syn::Error::new_spanned(
                variant_name,
                "Enum has too many variants to deserialize",
            )
            .into_compile_error();
        };

        let variant_arm = quote!{
            ::pernixc_serialize::__internal::Identifier::Name(#variant_name_str) |
            ::pernixc_serialize::__internal::Identifier::Index(#variant_index) 
        };
        
        match &variant.fields {
            Fields::Unit => {
                quote! {
                    #variant_arm => Ok(#enum_name::#variant_name),
                }
            }

            Fields::Unnamed(fields) => {
                generate_unnamed_fields_deserialize(
                    fields,
                    "TupleVariantAccess",
                    "tuple_variant_access",
                    variant_name.span(),
                    |field_count, field_deserializations, field_construction| {
                        quote! {
                            #variant_arm => {
                                ::pernixc_serialize::__internal::EnumAccess::tuple_variant(
                                    enum_access,
                                    #field_count,
                                    |mut tuple_variant_access| {
                                        #(#field_deserializations)*
                                        Ok(#enum_name::#variant_name(#(#field_construction),*))
                                    }
                                )
                            }
                        }
                    }
                )
            }

            Fields::Named(fields) => {
                let constructor = quote! { #enum_name::#variant_name };
                
                generate_named_fields_deserialize(
                    fields,
                    &constructor,
                    "StructVariantAccess",
                    |field_names, field_processing_logic| {
                        quote! {
                            #variant_arm => {
                                ::pernixc_serialize::__internal::EnumAccess::struct_variant(
                                    enum_access,
                                    &[#(#field_names),*],
                                    |mut struct_variant_access| {
                                        let mut access = struct_variant_access;
                                        #(#field_processing_logic)*
                                    }
                                )
                            }
                        }
                    }
                )
            }
        }
    });

    let variant_names = variants.iter().map(|v| v.ident.to_string());

    quote! {
        ::pernixc_serialize::__internal::Deserializer::expect_enum(
            deserializer,
            #enum_name_str,
            &[#(#variant_names),*],
            |variant_ident, mut enum_access| {
                match variant_ident {
                    #(#variant_arms)*
                    _ => Err(::pernixc_serialize::__internal::DeError::unknown_enum_variant(
                        variant_ident
                    ))
                }
            }
        )
    }
}

