use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{DeriveInput, Result};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);

    extend(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn extend(input: DeriveInput) -> Result<TokenStream> {
    let name = &input.ident;

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        named
    } else {
        unimplemented!()
    };

    let db_struct_fields = fields.iter().map(|f| {
        let name = &f.ident;

        quote_spanned! {f.span()=>
            field(stringify!(#name), &self.#name)
        }
    });

    Ok(quote! {
        impl std::fmt::Debug for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#name))
                    #( .#db_struct_fields )*
                    .finish()
            }
        }
    })
}
