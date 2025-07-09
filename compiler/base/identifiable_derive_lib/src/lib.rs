//! Crate primarily used to derive the `Identifiable` trait for types, which
//! allows them to be uniquely identified by a stable type ID.

use syn::Generics;

/// Derives the `Identifiable` trait for a type, allowing it to be uniquely
/// identified by a stable type ID.
#[must_use]
pub fn implements_identifiable(
    name: &syn::Ident,
    mut generics: Generics,
    identifiable_trait: Option<&syn::Path>,
    stable_type_id: Option<&syn::Path>,
) -> proc_macro2::TokenStream {
    // should not have lifetime or constant parameters, only type parameters are
    // allowed
    if let Some(lt_param) = generics.lifetimes().next() {
        return syn::Error::new_spanned(
            lt_param,
            "lifetime parameters are not allowed in key types",
        )
        .to_compile_error();
    }
    if let Some(const_param) = generics.const_params().next() {
        return syn::Error::new_spanned(
            const_param,
            "constant parameters are not allowed in key types",
        )
        .to_compile_error();
    }

    let default_trait_path: syn::Path =
        syn::parse_quote!(::pernixc_stable_type_id::Identifiable);

    let default_stable_type_id_path: syn::Path =
        syn::parse_quote!(::pernixc_stable_type_id::StableTypeID);

    let identifiable_trait = identifiable_trait.unwrap_or(&default_trait_path);

    let stable_type_id = stable_type_id.unwrap_or(&default_stable_type_id_path);

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
                #stable_type_id::from_unique_type_name(
                    unique_type_name
                )
            }
        }
    } else {
        for ty_param in generics.type_params_mut() {
            ty_param.bounds.push(syn::parse_quote!(
                ::pernixc_stable_type_id::Identifiable
            ));
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
                let mut hash = #stable_type_id::from_unique_type_name(
                    unique_type_name
                );

                #(
                    hash = <#type_params as #identifiable_trait>::STABLE_TYPE_ID
                        .combine(hash);
                )*

                hash
            }
        }
    };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote::quote! {
        #[allow(clippy::trait_duplication_in_bounds)]
        impl #impl_generics
            #identifiable_trait for #name #ty_generics #where_clause
        {
            const STABLE_TYPE_ID: #stable_type_id
                = #stable_type_id_computation;
        }
    }
}
