//! Procedural macros for automatic serialization trait implementations.

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, Data, DataEnum, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, Generics, Ident, Index, Variant
};

/// Automatically derive the Serialize trait for structs and enums.
///
/// This macro generates an implementation of the `Serialize` trait that
/// serializes all fields of a struct or all variants of an enum.
///
/// # Example
///
/// ```rust
/// use pernixc_serialize_derive::Serialize;
/// use pernixc_serialize::ser::{Serialize, Serializer};
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
fn generate_serialize_impl_generics(
    generics: &Generics,
) -> (Vec<proc_macro2::TokenStream>, Vec<proc_macro2::TokenStream>) {
    let mut generic_list = Vec::new();
    let mut bounds = Vec::new();

    // Add lifetimes first
    for lt in generics.lifetimes() {
        generic_list.push(quote! { #lt });
    }

    // Add the serializer type parameter
    generic_list.push(quote! { __S });
    bounds.push(quote! {
        __S: ::pernixc_serialize::__internal::Serializer
    });

    // Add type parameters
    for type_param in generics.type_params() {
        generic_list.push(quote! { #type_param });
        let param_ident = &type_param.ident;
        bounds.push(quote! {
            #param_ident: ::pernixc_serialize::__internal::Serialize<__S>
        });
    }

    // Add const parameters
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

