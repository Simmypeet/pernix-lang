//! Procedural macros for the `pernixc_stable_type_id` crate.
//!
//! This crate provides the `#[derive(Identifiable)]` macro that automatically
//! implements the [`pernixc_stable_type_id::Identifiable`] trait for types.
//!
//! # Overview
//!
//! The derive macro generates stable, collision-resistant type identifiers
//! that remain consistent across different compiler runs. This is essential
//! for incremental compilation systems and other scenarios where type identity
//! must be preserved between executions.
//!
//! # Usage
//!
//! ```rust
//! use pernixc_stable_type_id::Identifiable;
//!
//! #[derive(Identifiable)]
//! struct MyType {
//!     field: i32,
//! }
//!
//! #[derive(Identifiable)]
//! enum MyEnum {
//!     Variant1,
//!     Variant2(String),
//! }
//!
//! #[derive(Identifiable)]
//! struct GenericType<T: Identifiable, U: Identifiable> {
//!     first: T,
//!     second: U,
//! }
//! ```
//!
//! # ID Generation Strategy
//!
//! The generated stable type ID is computed based on:
//!
//! 1. **Package Information**: Package name and version from `Cargo.toml`
//! 2. **Module Path**: The full module path where the type is defined
//! 3. **Type Name**: The exact name of the type
//! 4. **Generic Parameters**: For generic types, the IDs of concrete type
//!    arguments
//!
//! This ensures that:
//! - Different types have different IDs (collision resistance)
//! - The same type has the same ID across runs (stability)
//! - Generic instantiations are distinguished (e.g., `Vec<i32>` vs
//!   `Vec<String>`)
//!
//! # Restrictions
//!
//! The derive macro has the following limitations:
//!
//! - **No lifetime parameters**: Types with lifetime parameters cannot derive
//!   `Identifiable`
//! - **No const parameters**: Types with const generic parameters are not
//!   supported
//! - **Bounded type parameters**: All type parameters must implement
//!   `Identifiable`
//!
//! # Examples
//!
//! ## Simple Types
//!
//! ```rust
//! # use pernixc_stable_type_id::Identifiable;
//! #[derive(Identifiable)]
//! struct Point {
//!     x: f64,
//!     y: f64,
//! }
//!
//! // The ID is automatically computed and available as a constant
//! let id = Point::STABLE_TYPE_ID;
//! ```
//!
//! ## Generic Types
//!
//! ```rust
//! # use pernixc_stable_type_id::Identifiable;
//! #[derive(Identifiable)]
//! struct Container<T: Identifiable> {
//!     value: T,
//! }
//!
//! // Different instantiations have different IDs
//! type IntContainer = Container<i32>;
//! type StringContainer = Container<String>;
//! // IntContainer::STABLE_TYPE_ID != StringContainer::STABLE_TYPE_ID
//! ```
//!
//! ## Error Cases
//!
//! ```compile_fail
//! # use pernixc_stable_type_id::Identifiable;
//! // This will fail to compile - lifetime parameters not allowed
//! #[derive(Identifiable)]
//! struct WithLifetime<'a> {
//!     data: &'a str,
//! }
//! ```
//!
//! ```compile_fail
//! # use pernixc_stable_type_id::Identifiable;
//! // This will fail to compile - const parameters not allowed
//! #[derive(Identifiable)]
//! struct WithConst<const N: usize> {
//!     data: [i32; N],
//! }
//! ```

use proc_macro::TokenStream;

/// Derives the [`Identifiable`](pernixc_stable_type_id::Identifiable) trait for
/// a type.
///
/// This proc macro automatically implements the `Identifiable` trait, providing
/// a stable, collision-resistant type identifier that remains consistent across
/// different compiler runs.
///
/// # Generated Implementation
///
/// The macro generates an implementation that computes the stable type ID
/// using:
///
/// 1. **Base ID**: Computed from package name, version, module path, and type
///    name
/// 2. **Generic Combination**: For generic types, combines the base ID with the
///    stable IDs of all concrete type parameters
///
/// # Type Requirements
///
/// ## Allowed Type Parameters
/// - **Type parameters**: Must implement `Identifiable` (automatically
///   enforced)
///
/// ## Forbidden Parameters
/// - **Lifetime parameters**: Not supported and will cause a compile error
/// - **Const parameters**: Not supported and will cause a compile error
///
/// # Examples
///
/// ## Simple Struct
///
/// ```rust
/// # use pernixc_stable_type_id::Identifiable;
/// #[derive(Identifiable)]
/// struct User {
///     id: u64,
///     name: String,
/// }
///
/// // Generated implementation equivalent to:
/// // impl Identifiable for User {
/// //     const STABLE_TYPE_ID: StableTypeID = /* computed at compile time */;
/// // }
/// ```
///
/// ## Generic Struct
///
/// ```rust
/// # use pernixc_stable_type_id::Identifiable;
/// #[derive(Identifiable)]
/// struct Pair<T: Identifiable, U: Identifiable> {
///     first: T,
///     second: U,
/// }
///
/// // The generated ID varies based on concrete type parameters:
/// // Pair<i32, String>::STABLE_TYPE_ID != Pair<f64, Vec<u8>>::STABLE_TYPE_ID
/// ```
///
/// ## Enum Types
///
/// ```rust
/// # use pernixc_stable_type_id::Identifiable;
/// #[derive(Identifiable)]
/// enum Result<T: Identifiable, E: Identifiable> {
///     Ok(T),
///     Err(E),
/// }
/// ```
///
/// # Error Cases
///
/// The following will produce compile-time errors:
///
/// ```compile_fail
/// # use pernixc_stable_type_id::Identifiable;
/// #[derive(Identifiable)]
/// struct BadLifetime<'a> {  // ❌ Lifetime parameters not allowed
///     data: &'a str,
/// }
/// ```
///
/// ```compile_fail
/// # use pernixc_stable_type_id::Identifiable;
/// #[derive(Identifiable)]
/// struct BadConst<const N: usize> {  // ❌ Const parameters not allowed
///     buffer: [u8; N],
/// }
/// ```
///
/// # Implementation Details
///
/// For non-generic types, the stable ID is computed at compile time using:
/// ```text
/// StableTypeID::from_unique_type_name(
///     "package_name@version::module::path::TypeName"
/// )
/// ```
///
/// For generic types, the macro additionally combines the base ID with each
/// type parameter's stable ID:
/// ```text
/// base_id.combine(T::STABLE_TYPE_ID).combine(U::STABLE_TYPE_ID)...
/// ```
///
/// This ensures that `Vec<i32>` and `Vec<String>` have different stable IDs
/// while maintaining deterministic generation.
///
/// # Performance
///
/// The stable ID computation happens entirely at compile time and has zero
/// runtime cost. The generated constant can be used directly without any
/// function calls or allocations.
#[proc_macro_derive(Identifiable)]
#[allow(clippy::too_many_lines)]
pub fn derive_key(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = input.ident.clone();
    let mut generics = input.generics;

    // should not have lifetime or constant parameters, only type parameters are
    // allowed
    if let Some(lt_param) = generics.lifetimes().next() {
        return syn::Error::new_spanned(
            lt_param,
            "lifetime parameters are not allowed in key types",
        )
        .to_compile_error()
        .into();
    }
    if let Some(const_param) = generics.const_params().next() {
        return syn::Error::new_spanned(
            const_param,
            "constant parameters are not allowed in key types",
        )
        .to_compile_error()
        .into();
    }

    let stable_type_id_computation = if generics.params.is_empty() {
        quote::quote! {
            {
                let unique_type_name = concat!(
                    env!("CARGO_PKG_NAME"),
                    "@",
                    env!("CARGO_PKG_VERSION"),
                    "::",
                    module_path!(),
                    "::",
                    stringify!(#name)
                );
                pernixc_stable_type_id::StableTypeID::from_unique_type_name(
                    unique_type_name
                )
            }
        }
    } else {
        for ty_param in generics.type_params_mut() {
            ty_param
                .bounds
                .push(syn::parse_quote!(pernixc_stable_type_id::Identifiable));
        }

        let type_params = generics.type_params().map(|x| &x.ident);

        quote::quote! {
            {
                let unique_type_name = concat!(
                    env!("CARGO_PKG_NAME"),
                    "@",
                    env!("CARGO_PKG_VERSION"),
                    "::",
                    module_path!(),
                    "::",
                    stringify!(#name),
                );
                let mut hash = pernixc_stable_type_id::StableTypeID::from_unique_type_name(
                    unique_type_name
                );

                #(
                    hash = <#type_params as pernixc_stable_type_id::Identifiable>::STABLE_TYPE_ID
                        .combine(hash);
                )*

                hash
            }
        }
    };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote::quote! {
        impl #impl_generics pernixc_stable_type_id::Identifiable for #name #ty_generics #where_clause {
            const STABLE_TYPE_ID: pernixc_stable_type_id::StableTypeID
                = #stable_type_id_computation;
        }
    }.into()
}
