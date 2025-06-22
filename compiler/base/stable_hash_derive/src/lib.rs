//! Derive macro for the `StableHash` trait.
//! 
//! This crate provides a derive macro for automatically implementing the `StableHash` trait
//! from the `pernixc_stable_hash` crate. The derive macro can be used on structs and enums
//! to generate stable hash implementations.
//! 
//! # Examples
//! 
//! ## Struct
//! 
//! ```rust
//! use pernixc_stable_hash::StableHash;
//! 
//! #[derive(StableHash)]
//! struct Point {
//!     x: i32,
//!     y: i32,
//! }
//! ```
//! 
//! ## Enum
//! 
//! ```rust
//! use pernixc_stable_hash::StableHash;
//! 
//! #[derive(StableHash)]
//! enum Message {
//!     Quit,
//!     Move { x: i32, y: i32 },
//!     Write(String),
//!     ChangeColor(i32, i32, i32),
//! }
//! ```

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DataEnum, DataStruct, DeriveInput, Fields, Index};

/// Derive macro for `StableHash`.
/// 
/// This macro automatically implements the `StableHash` trait for structs and enums.
/// The implementation ensures that:
/// 
/// - For structs: all fields are hashed in declaration order
/// - For enums: the discriminant is hashed first, followed by any variant data
/// - The hash is stable across different program runs and platforms
/// 
/// # Struct Example
/// 
/// ```rust
/// use pernixc_stable_hash_derive::StableHash;
/// 
/// #[derive(StableHash)]
/// struct Person {
///     name: String,
///     age: u32,
/// }
/// ```
/// 
/// # Enum Example
/// 
/// ```rust
/// use pernixc_stable_hash_derive::StableHash;
/// 
/// #[derive(StableHash)]
/// enum Color {
///     Red,
///     Green,
///     Blue,
///     Rgb(u8, u8, u8),
///     Named { name: String },
/// }
/// ```
#[proc_macro_derive(StableHash)]
pub fn derive_stable_hash(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    
    // Build where clause for StableHash bounds
    let mut where_clause = where_clause.cloned().unwrap_or_else(|| syn::parse_quote!(where));
    
    // Add StableHash bounds for all generic type parameters
    for param in &input.generics.params {
        if let syn::GenericParam::Type(type_param) = param {
            let ident = &type_param.ident;
            where_clause.predicates.push(syn::parse_quote!(#ident: ::pernixc_stable_hash::StableHash));
        }
    }
    
    let stable_hash_impl = match &input.data {
        Data::Struct(data_struct) => impl_stable_hash_struct(data_struct),
        Data::Enum(data_enum) => impl_stable_hash_enum(data_enum),
        Data::Union(_) => {
            return syn::Error::new_spanned(
                &input,
                "StableHash cannot be derived for unions due to memory safety concerns"
            ).to_compile_error().into();
        }
    };
    
    let expanded = quote! {
        impl #impl_generics ::pernixc_stable_hash::StableHash for #name #ty_generics #where_clause {
            fn stable_hash<H: ::pernixc_stable_hash::StableHasher + ?Sized>(&self, state: &mut H) {
                #stable_hash_impl
            }
        }
    };
    
    TokenStream::from(expanded)
}

fn impl_stable_hash_struct(data_struct: &DataStruct) -> proc_macro2::TokenStream {
    match &data_struct.fields {
        Fields::Named(fields) => {
            let field_hashes = fields.named.iter().map(|field| {
                let field_name = &field.ident;
                quote! {
                    ::pernixc_stable_hash::StableHash::stable_hash(&self.#field_name, state);
                }
            });
            
            quote! {
                #(#field_hashes)*
            }
        }
        Fields::Unnamed(fields) => {
            let field_hashes = fields.unnamed.iter().enumerate().map(|(i, _)| {
                let index = Index::from(i);
                quote! {
                    ::pernixc_stable_hash::StableHash::stable_hash(&self.#index, state);
                }
            });
            
            quote! {
                #(#field_hashes)*
            }
        }
        Fields::Unit => {
            quote! {
                // Unit struct has no fields to hash
            }
        }
    }
}

fn impl_stable_hash_enum(data_enum: &DataEnum) -> proc_macro2::TokenStream {
    let variant_matches = data_enum.variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        
        match &variant.fields {
            Fields::Named(fields) => {
                let field_names: Vec<_> = fields.named.iter().map(|f| &f.ident).collect();
                let field_hashes = field_names.iter().map(|field_name| {
                    quote! {
                        ::pernixc_stable_hash::StableHash::stable_hash(#field_name, state);
                    }
                });
                
                quote! {
                    Self::#variant_name { #(#field_names),* } => {
                        #(#field_hashes)*
                    }
                }
            }
            Fields::Unnamed(fields) => {
                let field_bindings: Vec<_> = (0..fields.unnamed.len())
                    .map(|i| syn::Ident::new(&format!("field_{i}"), proc_macro2::Span::call_site()))
                    .collect();
                
                let field_hashes = field_bindings.iter().map(|field_name| {
                    quote! {
                        ::pernixc_stable_hash::StableHash::stable_hash(#field_name, state);
                    }
                });
                
                quote! {
                    Self::#variant_name(#(#field_bindings),*) => {
                        #(#field_hashes)*
                    }
                }
            }
            Fields::Unit => {
                quote! {
                    Self::#variant_name => {}
                }
            }
        }
    });
    
    quote! {
        ::pernixc_stable_hash::StableHash::stable_hash(
            &::std::mem::discriminant(self),
            state
        );

        match self {
            #(#variant_matches)*
        }
    }
}

