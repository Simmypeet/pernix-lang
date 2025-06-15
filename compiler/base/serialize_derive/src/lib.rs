//! Procedural macros for automatic serialization trait implementations.
//! 
//! This crate provides derive macros for automatically implementing the `Serialize` 
//! and `Deserialize` traits on structs and enums.
//!
//! # Supported Attributes
//!
//! ## `#[serde(skip)]`
//!
//! The `#[serde(skip)]` attribute can be applied to fields in structs, tuple structs,
//! and enum variants to exclude them from serialization and deserialization.
//!
//! ### Serialization Behavior
//! - Fields marked with `#[serde(skip)]` are completely omitted from the serialized output
//! - No bytes are written for these fields
//! - The serialized data is identical to a structure without the skipped fields
//!
//! ### Deserialization Behavior  
//! - Skipped fields are not read from the input stream
//! - They are initialized with their `Default` value
//! - The containing type must implement `Default` when using skipped fields
//! - No bytes are consumed from the input for skipped fields
//!
//! ### Use Cases
//! - **Runtime-only data**: Caches, computed values, temporary state
//! - **Version compatibility**: Fields added in newer versions that shouldn't break older readers
//! - **Non-serializable types**: Fields containing types that can't or shouldn't be serialized
//! - **Performance optimization**: Avoiding serialization of expensive-to-serialize data
//!
//! ## `#[serde(extension(T + U + 'static))]`
//!
//! The `#[serde(extension(...))]` attribute can be applied to structs and enums to add
//! additional bounds to both the serializer and deserializer's `Extension` associated type.
//!
//! ### Behavior
//! - Adds `__S::Extension: T + U + 'static` to the `where` clause for `Serialize` implementations
//! - Adds `__D::Extension: T + U + 'static` to the `where` clause for `Deserialize` implementations
//! - The bounds can be any valid trait bounds, type bounds, or lifetime bounds
//! - Multiple bounds can be specified using `+` syntax
//!
//! ## `#[serde(ser_extension(T + U + 'static))]`
//!
//! The `#[serde(ser_extension(...))]` attribute can be applied to structs and enums to add
//! additional bounds only to the serializer's `Extension` associated type.
//!
//! ### Behavior
//! - Adds `__S::Extension: T + U + 'static` to the `where` clause for `Serialize` implementations only
//! - Does not affect `Deserialize` implementations
//! - The bounds can be any valid trait bounds, type bounds, or lifetime bounds
//! - Multiple bounds can be specified using `+` syntax
//!
//! ## `#[serde(de_extension(T + U + 'static))]`
//!
//! The `#[serde(de_extension(...))]` attribute can be applied to structs and enums to add
//! additional bounds only to the deserializer's `Extension` associated type.
//!
//! ### Behavior
//! - Adds `__D::Extension: T + U + 'static` to the `where` clause for `Deserialize` implementations only
//! - Does not affect `Serialize` implementations
//! - The bounds can be any valid trait bounds, type bounds, or lifetime bounds
//! - Multiple bounds can be specified using `+` syntax
//!
//! ## `#[serde(ser_bound(T: Clone + Copy + 'static))]`
//!
//! The `#[serde(ser_bound(...))]` attribute can be applied to structs and enums to add
//! additional generic bounds to the `where` clause for `Serialize` implementations only.
//!
//! ### Behavior
//! - Adds the specified bounds directly to the `where` clause for `Serialize` implementations
//! - Does not affect `Deserialize` implementations
//! - The bounds can reference generic type parameters and add any valid trait bounds
//! - Multiple bounds can be specified using `+` syntax
//!
//! ## `#[serde(de_bound(T: Clone + Copy + 'static))]`
//!
//! The `#[serde(de_bound(...))]` attribute can be applied to structs and enums to add
//! additional generic bounds to the `where` clause for `Deserialize` implementations only.
//!
//! ### Behavior
//! - Adds the specified bounds directly to the `where` clause for `Deserialize` implementations
//! - Does not affect `Serialize` implementations
//! - The bounds can reference generic type parameters and add any valid trait bounds
//! - Multiple bounds can be specified using `+` syntax
//!
//! ### Use Cases
//! - **Extension trait requirements**: When serialization/deserialization requires specific capabilities from the extension
//! - **Custom serialization behavior**: Enabling custom logic based on extension capabilities
//! - **Type safety**: Ensuring the extension provides required functionality
//! - **Asymmetric requirements**: When serialization and deserialization have different extension requirements
//!
//! ### Example
//!
//! ```ignore
//! use pernixc_serialize_derive::{Serialize, Deserialize};
//!
//! // Extension must implement Clone + Debug + Send + 'static for both serialization and deserialization
//! #[derive(Serialize, Deserialize)]
//! #[serde(extension(Clone + Debug + Send + 'static))]
//! struct ExtensionUser {
//!     data: String,
//! }
//!
//! // Extension must implement Clone + Debug + Send + 'static only for serialization
//! #[derive(Serialize, Deserialize)]
//! #[serde(ser_extension(Clone + Debug + Send + 'static))]
//! struct SerExtensionUser {
//!     data: String,
//! }
//!
//! // Extension must implement Clone + Debug + Send + 'static only for deserialization
//! #[derive(Serialize, Deserialize)]
//! #[serde(de_extension(Clone + Debug + Send + 'static))] 
//! struct DeExtensionUser {
//!     data: String,
//! }
//!
//! // Generic bounds example - T must be Clone + Copy + 'static for serialization
//! #[derive(Serialize, Deserialize)]
//! #[serde(ser_bound(T: Clone + Copy + 'static))]
//! #[serde(de_bound(T: Clone + Default + 'static))]
//! struct GenericBoundUser<T> {
//!     data: T,
//! }
//! ```
//!
//! # Complete Example
//!
//! ```ignore
//! use pernixc_serialize_derive::{Serialize, Deserialize};
//!
//! #[derive(Serialize, Deserialize, Default, Debug, PartialEq)]
//! #[serde(extension(Clone + Debug))]
//! struct UserSession {
//!     user_id: u64,
//!     username: String,
//!     #[serde(skip)]
//!     auth_token: String,        // Runtime-only, not persisted
//!     #[serde(skip)]
//!     last_activity: u64,        // Computed field, reset on load
//! }
//!
//! let session = UserSession {
//!     user_id: 123,
//!     username: "alice".to_string(),
//!     auth_token: "secret-token".to_string(),
//!     last_activity: 1640995200,
//! };
//!
//! // Serialize - only user_id and username are written
//! // Extension must implement Clone + Debug
//! let bytes = serialize(&session);
//!
//! // Deserialize - auth_token and last_activity get default values
//! let restored: UserSession = deserialize(&bytes);
//! assert_eq!(restored.user_id, 123);
//! assert_eq!(restored.username, "alice");
//! assert_eq!(restored.auth_token, "");  // Default String
//! assert_eq!(restored.last_activity, 0);  // Default u64
//! ```

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, Data, DataEnum, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, Generics, Ident, Index, Variant, 
    Field
};

/// Container for extension bounds parsed from attributes
#[derive(Debug, Default)]
struct ExtensionBounds {
    /// Bounds for both serialization and deserialization (from `#[serde(extension(...))]`)
    both: Option<proc_macro2::TokenStream>,
    /// Bounds only for serialization (from `#[serde(ser_extension(...))]`)
    serialize_only: Option<proc_macro2::TokenStream>,
    /// Bounds only for deserialization (from `#[serde(de_extension(...))]`)
    deserialize_only: Option<proc_macro2::TokenStream>,
    /// Generic bounds for serialization (from `#[serde(ser_bound(...))]`)
    serialize_bound: Option<proc_macro2::TokenStream>,
    /// Generic bounds for deserialization (from `#[serde(de_bound(...))]`)
    deserialize_bound: Option<proc_macro2::TokenStream>,
}

impl ExtensionBounds {
    /// Get the effective bounds for serialization
    fn serialize_bounds(&self) -> Option<proc_macro2::TokenStream> {
        match (&self.both, &self.serialize_only) {
            (Some(both), Some(ser_only)) => {
                Some(quote! { #both + #ser_only })
            }
            (Some(both), None) => Some(both.clone()),
            (None, Some(ser_only)) => Some(ser_only.clone()),
            (None, None) => None,
        }
    }

    /// Get the effective bounds for deserialization
    fn deserialize_bounds(&self) -> Option<proc_macro2::TokenStream> {
        match (&self.both, &self.deserialize_only) {
            (Some(both), Some(de_only)) => {
                Some(quote! { #both + #de_only })
            }
            (Some(both), None) => Some(both.clone()),
            (None, Some(de_only)) => Some(de_only.clone()),
            (None, None) => None,
        }
    }

    /// Get the generic bounds for serialization
    fn serialize_generic_bounds(&self) -> Option<proc_macro2::TokenStream> {
        self.serialize_bound.clone()
    }

    /// Get the generic bounds for deserialization
    fn deserialize_generic_bounds(&self) -> Option<proc_macro2::TokenStream> {
        self.deserialize_bound.clone()
    }
}

/// Automatically derive the Serialize trait for structs and enums.
///
/// This macro generates an implementation of the `Serialize` trait that
/// serializes all fields of a struct or all variants of an enum.
///
/// # Attributes
///
/// ## `#[serde(skip)]`
///
/// Fields marked with `#[serde(skip)]` are completely omitted from serialization.
/// This means:
/// - The field's value is not written to the serialized output
/// - No bytes are contributed to the binary representation
/// - The field can contain any value without affecting the serialized data
///
/// This is useful for:
/// - Runtime-only data that shouldn't be persisted
/// - Computed or cached values that can be regenerated
/// - Fields that would cause serialization issues (e.g., non-serializable types)
///
/// ## `#[serde(extension(T + U + 'static))]`
///
/// Container-level attribute that adds bounds to both the serializer and deserializer's `Extension` associated type.
/// The bounds are added to the `where` clause as `__S::Extension: T + U + 'static` and `__D::Extension: T + U + 'static`.
///
/// ## `#[serde(ser_extension(T + U + 'static))]`
///
/// Container-level attribute that adds bounds only to the serializer's `Extension` associated type.
/// The bounds are added to the `where` clause as `__S::Extension: T + U + 'static`.
///
/// ## `#[serde(de_extension(T + U + 'static))]`
///
/// Container-level attribute that adds bounds only to the deserializer's `Extension` associated type.
/// The bounds are added to the `where` clause as `__D::Extension: T + U + 'static`.
///
/// ## `#[serde(ser_bound(T: Clone + Copy + 'static))]`
///
/// Container-level attribute that adds additional generic bounds to the `where` clause for `Serialize` implementations only.
/// The bounds are added directly to the `where` clause and can reference generic type parameters.
///
/// ## `#[serde(de_bound(T: Clone + Copy + 'static))]`
///
/// Container-level attribute that adds additional generic bounds to the `where` clause for `Deserialize` implementations only.
/// The bounds are added directly to the `where` clause and can reference generic type parameters.
///
/// This is useful for:
/// - Requiring specific capabilities from the serializer/deserializer extension
/// - Enabling custom serialization behavior based on extension traits
/// - Ensuring type safety for extension-dependent serialization logic
/// - Asymmetric requirements when serialization and deserialization have different extension needs
/// - Adding custom bounds for generic type parameters in serialization/deserialization
///
/// # Examples
///
/// ## Basic struct serialization
///
/// ```ignore
/// use pernixc_serialize_derive::Serialize;
///
/// #[derive(Serialize)]
/// struct Person {
///     name: String,
///     age: u32,
/// }
/// ```
///
/// ## Struct with skipped field
///
/// ```ignore
/// use pernixc_serialize_derive::Serialize;
///
/// #[derive(Serialize)]
/// struct Cache {
///     id: u64,
///     data: String,
///     #[serde(skip)]
///     computed_hash: u64,  // Not serialized
/// }
/// ```
///
/// ## Struct with extension bounds
///
/// ```ignore
/// use pernixc_serialize_derive::Serialize;
///
/// #[derive(Serialize)]
/// #[serde(extension(Clone + Debug + Send))]
/// struct ExtensionUser {
///     data: String,
/// }
/// // Generated impl will include: where __S::Extension: Clone + Debug + Send
/// ```
///
/// ## Struct with serialize-only extension bounds
///
/// ```ignore
/// use pernixc_serialize_derive::Serialize;
///
/// #[derive(Serialize)]
/// #[serde(ser_extension(Clone + Debug + Send))]
/// struct SerExtensionUser {
///     data: String,
/// }
/// // Generated impl will include: where __S::Extension: Clone + Debug + Send
/// ```
///
/// ## Struct with generic bounds
///
/// ```ignore
/// use pernixc_serialize_derive::Serialize;
///
/// #[derive(Serialize)]
/// #[serde(ser_bound(T: Clone + Copy + 'static))]
/// struct Container<T> {
///     data: T,
/// }
/// // Generated impl will include: where T: Clone + Copy + 'static
/// ```
///
/// ## Generic struct
///
/// ```ignore
/// use pernixc_serialize_derive::Serialize;
///
/// #[derive(Serialize)]
/// struct Point<T> {
///     x: T,
///     y: T,
///     #[serde(skip)]
///     cached_distance: f64,  // Not serialized
/// }
/// ```
///
/// ## Enum with skipped fields
///
/// ```ignore
/// use pernixc_serialize_derive::Serialize;
///
/// #[derive(Serialize)]
/// #[serde(extension(Debug))]
/// enum Message {
///     Text {
///         content: String,
///         #[serde(skip)]
///         word_count: usize,  // Not serialized
///     },
///     Image(Vec<u8>),
/// }
/// ```
///
/// ## Tuple struct with skipped field
///
/// ```ignore
/// use pernixc_serialize_derive::Serialize;
///
/// #[derive(Serialize)]
/// struct Coordinates(f64, f64, #[serde(skip)] String);  // Third field not serialized
/// ```
#[proc_macro_derive(Serialize, attributes(serde))]
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
    let extension_bounds = extract_serde_extension_bounds(&input.attrs);
    
    match &input.data {
        Data::Struct(data_struct) => {
            Ok(expand_serialize_struct(name, generics, &data_struct.fields, extension_bounds.serialize_bounds(), extension_bounds.serialize_generic_bounds()))
        }
        Data::Enum(data_enum) => {
            Ok(expand_serialize_enum(name, generics, data_enum, extension_bounds.serialize_bounds(), extension_bounds.serialize_generic_bounds()))
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
    extension_bounds: Option<proc_macro2::TokenStream>,
    generic_bounds: Option<proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
    let serialize_body = match fields {
        Fields::Named(fields) => serialize_named_fields(name, fields),
        Fields::Unnamed(fields) => serialize_unnamed_fields(name, fields),
        Fields::Unit => serialize_unit_struct(name),
    };

    let (_, ty_generics, _) = generics.split_for_impl();
    let (generic_list, bounds) = generate_serialize_impl_generics(generics, extension_bounds, generic_bounds);

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
    let field_count = count_non_skipped_fields(fields);
    
    let field_serializations = fields.named.iter().filter_map(|field| {
        if has_serde_skip_attr(field) {
            None // Skip this field
        } else {
            let field_name = field.ident.as_ref().unwrap();
            let field_name_str = field_name.to_string();
            
            Some(quote! {
                pernixc_serialize::__internal::Struct::serialize_field(
                    &mut struct_serializer,
                    #field_name_str,
                    &self.#field_name
                )?;
            })
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
    let field_count = count_non_skipped_unnamed_fields(fields);
    
    let field_serializations = fields.unnamed.iter().enumerate().filter_map(|(i, field)| {
        if has_serde_skip_attr(field) {
            None // Skip this field
        } else {
            let index = Index::from(i);
            
            Some(quote! {
                pernixc_serialize::__internal::TupleStruct::serialize_field(
                    &mut tuple_struct_serializer,
                    &self.#index
                )?;
            })
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
    extension_bounds: Option<proc_macro2::TokenStream>,
    generic_bounds: Option<proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
    let serialize_body = serialize_enum_variants(name, &data_enum.variants);

    let (_, ty_generics, _) = generics.split_for_impl();
    let (generic_list, bounds) = generate_serialize_impl_generics(generics, extension_bounds, generic_bounds);
    
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
                let total_field_count = fields.unnamed.len();
                let non_skipped_fields: Vec<_> = fields.unnamed.iter().enumerate()
                    .filter(|(_, field)| !has_serde_skip_attr(field))
                    .collect();
                let field_count = non_skipped_fields.len();
                
                let all_field_bindings: Vec<_> = (0..total_field_count)
                    .map(|i| {
                        Ident::new(&format!("__field_{i}"), variant_name.span())
                    })
                    .collect();
                
                let field_serializations = non_skipped_fields.iter().map(|(i, _)| {
                    let field_name = &all_field_bindings[*i];
                    quote! {
                        tuple_variant.serialize_field(#field_name)?;
                    }
                });
                
                quote! {
                    #enum_name::#variant_name(#(#all_field_bindings),*) => {
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
                let field_count = count_non_skipped_fields(fields);
                let field_bindings = fields.named.iter().map(|field| {
                    let field_name = field.ident.as_ref().unwrap();
                    field_name
                });
                
                let field_serializations = fields.named.iter().filter_map(|field| {
                    if has_serde_skip_attr(field) {
                        None // Skip this field
                    } else {
                        let field_name = field.ident.as_ref().unwrap();
                        let field_name_str = field_name.to_string();
                        
                        Some(quote! {
                            struct_variant.serialize_field(#field_name_str, #field_name)?;
                        })
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
    extension_bounds: Option<proc_macro2::TokenStream>,
    generic_bounds: Option<proc_macro2::TokenStream>,
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
    
    // Add extension bounds if specified
    if let Some(ext_bounds) = extension_bounds {
        let extension_bound = quote! { #param_ident::Extension: #ext_bounds };
        bounds.push(extension_bound);
    }
    
    // Add type parameters third
    for type_param in generics.type_params() {
        generic_list.push(quote! { #type_param });
        
        // Only generate implicit Serialize/Deserialize bounds if no explicit generic bounds are provided
        if generic_bounds.is_none() {
            let param_ident_ref = &type_param.ident;
            bounds.push(type_bound_template(param_ident_ref, param_name));
        }
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

    // Add generic bounds if specified (these replace the implicit bounds)
    if let Some(gen_bounds) = generic_bounds {
        bounds.push(gen_bounds);
    }

    (generic_list, bounds)
}

fn generate_serialize_impl_generics(
    generics: &Generics,
    extension_bounds: Option<proc_macro2::TokenStream>,
    generic_bounds: Option<proc_macro2::TokenStream>,
) -> (Vec<proc_macro2::TokenStream>, Vec<proc_macro2::TokenStream>) {
    generate_impl_generics(
        generics,
        "__S",
        quote! { __S: ::pernixc_serialize::__internal::Serializer },
        |param_ident, param_name| {
            let param_token = syn::Ident::new(param_name, proc_macro2::Span::call_site());
            quote! { #param_ident: ::pernixc_serialize::__internal::Serialize<#param_token> }
        },
        extension_bounds,
        generic_bounds,
    )
}

fn generate_deserialize_impl_generics(
    generics: &Generics,
    extension_bounds: Option<proc_macro2::TokenStream>,
    generic_bounds: Option<proc_macro2::TokenStream>,
) -> (Vec<proc_macro2::TokenStream>, Vec<proc_macro2::TokenStream>) {
    generate_impl_generics(
        generics,
        "__D",
        quote! { __D: ::pernixc_serialize::__internal::Deserializer },
        |param_ident, param_name| {
            let param_token = syn::Ident::new(param_name, proc_macro2::Span::call_site());
            quote! { #param_ident: ::pernixc_serialize::__internal::Deserialize<#param_token> }
        },
        extension_bounds,
        generic_bounds,
    )
}

/// Automatically derive the Deserialize trait for structs and enums.
///
/// This macro generates an implementation of the `Deserialize` trait that
/// deserializes all fields of a struct or all variants of an enum.
///
/// # Attributes
///
/// ## `#[serde(skip)]`
///
/// Fields marked with `#[serde(skip)]` are not read from the serialized data during 
/// deserialization. Instead, they are filled with their `Default` value. This means:
/// - The field's value is not read from the input stream
/// - The field is initialized using `Default::default()`
/// - The type must implement the `Default` trait
/// - No bytes are consumed from the input for this field
///
/// This is useful for:
/// - Runtime-only data that should be reset to a default state
/// - Computed or cached values that need to be recalculated
/// - Fields that were added later and don't exist in older serialized data
///
/// **Important**: The struct/enum must implement `Default` when using skipped fields.
///
/// ## `#[serde(extension(T + U + 'static))]`
///
/// Container-level attribute that adds bounds to both the serializer and deserializer's `Extension` associated type.
/// The bounds are added to the `where` clause as `__S::Extension: T + U + 'static` and `__D::Extension: T + U + 'static`.
///
/// ## `#[serde(ser_extension(T + U + 'static))]`
///
/// Container-level attribute that adds bounds only to the serializer's `Extension` associated type.
/// The bounds are added to the `where` clause as `__S::Extension: T + U + 'static`.
///
/// ## `#[serde(de_extension(T + U + 'static))]`
///
/// Container-level attribute that adds bounds only to the deserializer's `Extension` associated type.
/// The bounds are added to the `where` clause as `__D::Extension: T + U + 'static`.
///
/// ## `#[serde(ser_bound(T: Clone + Copy + 'static))]`
///
/// Container-level attribute that adds additional generic bounds to the `where` clause for `Serialize` implementations only.
/// The bounds are added directly to the `where` clause and can reference generic type parameters.
///
/// ## `#[serde(de_bound(T: Clone + Copy + 'static))]`
///
/// Container-level attribute that adds additional generic bounds to the `where` clause for `Deserialize` implementations only.
/// The bounds are added directly to the `where` clause and can reference generic type parameters.
///
/// This is useful for:
/// - Requiring specific capabilities from the deserializer extension
/// - Enabling custom deserialization behavior based on extension traits  
/// - Ensuring type safety for extension-dependent deserialization logic
/// - Asymmetric requirements when serialization and deserialization have different extension needs
///
/// # Examples
///
/// ## Basic struct deserialization
///
/// ```ignore
/// use pernixc_serialize_derive::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Person {
///     name: String,
///     age: u32,
/// }
/// ```
///
/// ## Struct with skipped field
///
/// ```ignore
/// use pernixc_serialize_derive::{Serialize, Deserialize};
///
/// #[derive(Serialize, Deserialize, Default)]
/// struct Cache {
///     id: u64,
///     data: String,
///     #[serde(skip)]
///     computed_hash: u64,  // Will be 0 after deserialization
/// }
/// ```
///
/// ## Struct with extension bounds
///
/// ```ignore
/// use pernixc_serialize_derive::Deserialize;
///
/// #[derive(Deserialize)]
/// #[serde(extension(Clone + Debug + Send))]
/// struct ExtensionUser {
///     data: String,  
/// }
/// // Generated impl will include: where __D::Extension: Clone + Debug + Send
/// ```
///
/// ## Struct with deserialize-only extension bounds
///
/// ```ignore
/// use pernixc_serialize_derive::Deserialize;
///
/// #[derive(Deserialize)]
/// #[serde(de_extension(Clone + Debug + Send))]
/// struct DeExtensionUser {
///     data: String,
/// }
/// // Generated impl will include: where __D::Extension: Clone + Debug + Send
/// ```
///
/// ## Generic struct with Default
///
/// ```ignore
/// use pernixc_serialize_derive::{Serialize, Deserialize};
///
/// #[derive(Serialize, Deserialize, Default)]
/// #[serde(extension(Debug))]
/// struct Point<T: Default> {
///     x: T,
///     y: T,
///     #[serde(skip)]
///     cached_distance: f64,  // Will be 0.0 after deserialization
/// }
/// ```
///
/// ## Enum with skipped fields
///
/// ```ignore
/// use pernixc_serialize_derive::{Serialize, Deserialize};
///
/// #[derive(Serialize, Deserialize, Default)]
/// #[serde(extension(Clone))]
/// enum Message {
///     #[default]
///     Empty,
///     Text {
///         content: String,
///         #[serde(skip)]
///         word_count: usize,  // Will be 0 after deserialization
///     },
///     Image(Vec<u8>),
/// }
/// ```
///
/// ## Round-trip compatibility
///
/// ```ignore
/// use pernixc_serialize_derive::{Serialize, Deserialize};
///
/// #[derive(Serialize, Deserialize, Default, PartialEq, Debug)]
/// #[serde(extension(Send + Sync))]
/// struct Data {
///     value: i32,
///     #[serde(skip)]
///     temp: String,  // Skipped in both directions
/// }
///
/// let original = Data { value: 42, temp: "ignored".to_string() };
/// let bytes = serialize(&original);
/// let deserialized: Data = deserialize(&bytes);
/// 
/// // deserialized.value == 42
/// // deserialized.temp == String::default() (empty string)
/// // Extension must implement Send + Sync for both serialization and deserialization
/// ```
#[proc_macro_derive(Deserialize, attributes(serde))]
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
    let extension_bounds = extract_serde_extension_bounds(&input.attrs);
    
    match &input.data {
        Data::Struct(data_struct) => {
            Ok(expand_deserialize_struct(name, generics, &data_struct.fields, extension_bounds.deserialize_bounds(), extension_bounds.deserialize_generic_bounds()))
        }
        Data::Enum(data_enum) => {
            Ok(expand_deserialize_enum(name, generics, data_enum, extension_bounds.deserialize_bounds(), extension_bounds.deserialize_generic_bounds()))
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
    extension_bounds: Option<proc_macro2::TokenStream>,
    generic_bounds: Option<proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
    let deserialize_body = match fields {
        Fields::Named(fields) => deserialize_named_fields(name, fields),
        Fields::Unnamed(fields) => deserialize_unnamed_fields(name, fields),
        Fields::Unit => deserialize_unit_struct(name),
    };

    let (_, ty_generics, _) = generics.split_for_impl();
    let (generic_list, bounds) = generate_deserialize_impl_generics(generics, extension_bounds, generic_bounds);

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
    extension_bounds: Option<proc_macro2::TokenStream>,
    generic_bounds: Option<proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
    let deserialize_body = deserialize_enum_variants(name, &data_enum.variants);

    let (_, ty_generics, _) = generics.split_for_impl();
    let (generic_list, bounds) = generate_deserialize_impl_generics(generics, extension_bounds, generic_bounds);

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
    let field_names: Vec<_> = fields.named.iter()
        .filter(|field| !has_serde_skip_attr(field))
        .map(|field| {
            let field_name_str = field.ident.as_ref().unwrap().to_string();
            quote! { #field_name_str }
        }).collect();
    
    // Generate field variables for each field (including skipped ones for construction)
    let all_field_vars: Vec<_> = fields.named.iter().map(|field| {
        let field_name = field.ident.as_ref().unwrap();
        let var_name = Ident::new(&format!("__{field_name}_value"), field_name.span());
        (field_name, var_name, has_serde_skip_attr(field))
    }).collect();
    
    // Only process non-skipped fields for deserialization
    let non_skipped_field_vars: Vec<_> = all_field_vars.iter()
        .filter(|(_, _, is_skipped)| !*is_skipped)
        .map(|(field_name, var_name, _)| (*field_name, var_name))
        .collect();
    
    // Initialize field variables (only for non-skipped fields)
    let field_initializations: Vec<_> = non_skipped_field_vars.iter().map(|(_field_name, var_name)| {
        quote! {
            let mut #var_name: Option<_> = None;
        }
    }).collect();
    
    // Generate match arms for known fields (only non-skipped)
    let field_match_arms: Vec<_> = non_skipped_field_vars.iter().enumerate().map(|(index, (field_name, var_name))| {
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
    let field_construction: Vec<_> = all_field_vars.iter().map(|(field_name, var_name, is_skipped)| {
        if *is_skipped {
            // For skipped fields, use default value
            quote! { 
                #field_name: std::default::Default::default()
            }
        } else {
            // For non-skipped fields, check if they were provided
            let field_name_str = field_name.to_string();
            quote! { 
                #field_name: match #var_name {
                    Some(value) => value,
                    None => return Err(::pernixc_serialize::__internal::DeError::missing_field(
                        ::pernixc_serialize::__internal::Identifier::Name(#field_name_str)
                    )),
                }
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
        usize, // field_count (only non-skipped fields)
        Vec<proc_macro2::TokenStream>, // field_deserializations
        Vec<proc_macro2::TokenStream>, // field_construction
    ) -> proc_macro2::TokenStream,
{
    let total_field_count = fields.unnamed.len();
    let non_skipped_fields: Vec<_> = fields.unnamed.iter().enumerate()
        .filter(|(_, field)| !has_serde_skip_attr(field))
        .collect();
    let non_skipped_count = non_skipped_fields.len();
    
    let field_deserializations: Vec<_> = non_skipped_fields.iter().map(|(i, _)| {
        let field_name = Ident::new(&format!("field_{i}"), span);
        let access_trait_ident = Ident::new(access_trait, proc_macro2::Span::call_site());
        let access_var_ident = Ident::new(access_var, proc_macro2::Span::call_site());
        quote! {
            let #field_name = ::pernixc_serialize::__internal::#access_trait_ident::next_field(
                &mut #access_var_ident
            )?;
        }
    }).collect();
    
    // Generate construction arguments for all fields (including defaults for skipped ones)
    let field_construction: Vec<_> = (0..total_field_count).map(|i| {
        let field = &fields.unnamed[i];
        if has_serde_skip_attr(field) {
            // Use default value for skipped fields
            quote! { std::default::Default::default() }
        } else {
            // Use deserialized value for non-skipped fields
            let field_name = Ident::new(&format!("field_{i}"), span);
            quote! { #field_name }
        }
    }).collect();

    wrapper(non_skipped_count, field_deserializations, field_construction)
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

/// Helper function to check if a field has the `#[serde(skip)]` attribute.
///
/// This function parses field attributes to determine if a field should be 
/// skipped during serialization and deserialization. It looks for the 
/// `#[serde(skip)]` attribute on struct fields, tuple struct fields, and 
/// enum variant fields.
///
/// # Arguments
///
/// * `field` - The field to check for the skip attribute
///
/// # Returns
///
/// `true` if the field has `#[serde(skip)]`, `false` otherwise
///
/// # Examples
///
/// This is used internally by the derive macros to identify fields like:
/// ```ignore
/// struct Example {
///     normal_field: i32,
///     #[serde(skip)]  // This field will return true
///     skipped_field: String,
/// }
/// ```
fn has_serde_skip_attr(field: &Field) -> bool {
    field.attrs.iter().any(|attr| {
        if attr.path().is_ident("serde") {
            if let Ok(list) = attr.meta.require_list() {
                return list.tokens.to_string().contains("skip");
            }
        }
        false
    })
}

/// Helper function to get field count excluding skipped fields.
///
/// This counts only the fields that will actually be serialized/deserialized,
/// excluding any fields marked with `#[serde(skip)]`.
///
/// # Arguments
///
/// * `fields` - The named fields to count
///
/// # Returns
///
/// The number of fields that are not marked with `#[serde(skip)]`
fn count_non_skipped_fields(fields: &FieldsNamed) -> usize {
    fields.named.iter().filter(|field| !has_serde_skip_attr(field)).count()
}

/// Helper function to get unnamed field count excluding skipped fields.
///
/// This counts only the tuple struct fields that will actually be serialized/deserialized,
/// excluding any fields marked with `#[serde(skip)]`.
///
/// # Arguments
///
/// * `fields` - The unnamed fields to count
///
/// # Returns
///
/// The number of fields that are not marked with `#[serde(skip)]`
fn count_non_skipped_unnamed_fields(fields: &FieldsUnnamed) -> usize {
    fields.unnamed.iter().filter(|field| !has_serde_skip_attr(field)).count()
}

/// Helper function to extract extension bounds from `#[serde(extension(...))]` attribute.
///
/// This function parses container-level attributes to find extension bounds that should be
/// applied to the `__S::Extension` or `__D::Extension` associated type in the where clause.
///
/// # Arguments
///
/// * `attrs` - The attributes to search through (typically from a struct or enum)
///
/// # Returns
///
/// `Some(TokenStream)` containing the bounds if found, `None` otherwise
///
/// # Examples
///
/// For an attribute like `#[serde(extension(Clone + Debug + 'static))]`, this returns
/// the token stream `Clone + Debug + 'static`.
fn extract_serde_extension_bounds(attrs: &[syn::Attribute]) -> ExtensionBounds {
    let mut bounds = ExtensionBounds::default();
    
    for attr in attrs {
        if attr.path().is_ident("serde") {
            let parse_result = attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("extension") {
                    let content;
                    syn::parenthesized!(content in meta.input);
                    bounds.both = Some(content.parse::<proc_macro2::TokenStream>()?);
                    Ok(())
                } else if meta.path.is_ident("ser_extension") {
                    let content;
                    syn::parenthesized!(content in meta.input);
                    bounds.serialize_only = Some(content.parse::<proc_macro2::TokenStream>()?);
                    Ok(())
                } else if meta.path.is_ident("de_extension") {
                    let content;
                    syn::parenthesized!(content in meta.input);
                    bounds.deserialize_only = Some(content.parse::<proc_macro2::TokenStream>()?);
                    Ok(())
                } else if meta.path.is_ident("ser_bound") {
                    let content;
                    syn::parenthesized!(content in meta.input);
                    bounds.serialize_bound = Some(content.parse::<proc_macro2::TokenStream>()?);
                    Ok(())
                } else if meta.path.is_ident("de_bound") {
                    let content;
                    syn::parenthesized!(content in meta.input);
                    bounds.deserialize_bound = Some(content.parse::<proc_macro2::TokenStream>()?);
                    Ok(())
                } else {
                    // Skip other serde attributes like "skip"
                    if meta.input.peek(syn::Token![=]) {
                        meta.input.parse::<syn::Token![=]>()?;
                        meta.input.parse::<syn::Expr>()?;
                    } else if meta.input.peek(syn::token::Paren) {
                        let content;
                        syn::parenthesized!(content in meta.input);
                        let _: proc_macro2::TokenStream = content.parse()?;
                    }
                    Ok(())
                }
            });
            
            // Continue parsing other attributes even if this one fails
            let _ = parse_result;
        }
    }
    
    bounds
}

