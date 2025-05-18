//! Contains the procedural macros for the `pernixc_query` crate.

use proc_macro::TokenStream;

/// This is a procedural macro that derives the `Key` trait for a struct or
/// enum. It requires the `#[value(Type)]` attribute to be present on the type
/// of the key.
#[proc_macro_derive(Key, attributes(value, pernixc_query))]
pub fn derive_key(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = input.ident.clone();
    let generics = input.generics;

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let Some(value_attr) =
        input.attrs.iter().find(|attr| attr.path().is_ident("value"))
    else {
        return syn::Error::new_spanned(
            name,
            "missing `#[value(Type)]` attribute on key type",
        )
        .to_compile_error()
        .into();
    };

    let Ok(value_type) = value_attr.parse_args::<syn::Type>() else {
        return syn::Error::new_spanned(
            value_attr,
            "invalid `#[value(Type)]` attribute on key type",
        )
        .to_compile_error()
        .into();
    };

    let pernixc_query_crate: syn::Path = match input
            .attrs
            .iter()
            .find(|attr| attr.path().is_ident("pernixc_query")) {
        Some(attr) => {
            let Ok(value) = attr.parse_args::<syn::Path>() else {
                return syn::Error::new_spanned(
                    attr,
                    "invalid `#[pernixc_query]` attribute on key type",
                )
                .to_compile_error()
                .into();
            };

            value
        },
        None => {
            syn::parse_quote!(::pernixc_query)
        },
    };
    
    quote::quote! {
        impl #impl_generics #pernixc_query_crate::key::Key for #name #ty_generics #where_clause {
            type Value = #value_type;

            fn unique_type_name() -> &'static str {
                concat!(
                    env!("CARGO_PKG_NAME"),
                    "@", 
                    env!("CARGO_PKG_VERSION"), 
                    "::", 
                    std::module_path!(), 
                    "::",
                    stringify!(#name)
                )
            }
        }
    }.into()
}
