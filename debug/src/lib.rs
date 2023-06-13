#![allow(unused)]
use std::fmt::{Debug, Write};

use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{DeriveInput, Result};

#[proc_macro_derive(CustomDebug, attributes(debug))]
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

    let generics = add_trait_bounds(input.generics, fields);
    let generics = add_associated_ty_where_clause(generics, fields);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let db_struct_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let fmt_str = format_str(f).unwrap_or("{:?}".into());

        quote_spanned! {f.span()=>
            field(stringify!(#name), &format_args!(#fmt_str, &self.#name))
        }
    });

    Ok(quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#name))
                    #( .#db_struct_fields )*
                    .finish()
            }
        }
    })
}

fn format_str(f: &syn::Field) -> Option<String> {
    for attr in &f.attrs {
        if let syn::Meta::NameValue(ref nv) = attr.meta {
            if nv.path.is_ident("debug") {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(ref ls),
                    ..
                }) = nv.value
                {
                    return Some(ls.value());
                }
            }
        }
    }
    None
}

fn has_t_inside(t: &syn::TypeParam, path: &syn::Path) -> bool {
    let mut sgmt = path.segments.last().unwrap();
    while let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
        ref args,
        ..
    }) = sgmt.arguments
    {
        for arg in args {
            if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath { ref path, .. })) = arg
            {
                if path.is_ident(&t.ident) {
                    return true;
                } else {
                    sgmt = path.segments.last().unwrap()
                }
            }
        }
    }
    false
}

fn is_only_in_phantomdata(
    ty_param: &syn::TypeParam,
    fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> bool {
    for field in fields {
        if let syn::Type::Path(syn::TypePath { ref path, .. }) = field.ty {
            let segment = path.segments.last().unwrap();
            if segment.ident == ty_param.ident {
                return false;
            }

            if segment.ident == "PhantomData" {
                continue;
            }

            if has_t_inside(ty_param, path) {
                return false;
            }
        }
    }
    true
}

fn add_associated_ty_where_clause(
    mut generics: syn::Generics,
    fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> syn::Generics {
    for param in generics.params.clone() {
        if let syn::GenericParam::Type(ref type_param) = param {
            for field in fields {
                let mut ty = &field.ty;
                if let syn::Type::Path(syn::TypePath { ref path, .. }) = ty {

                    let mut sgmt = path.segments.last().unwrap();
                    while let syn::PathArguments::AngleBracketed(
                        syn::AngleBracketedGenericArguments { ref args, .. },
                    ) = sgmt.arguments
                    {
                        for arg in args {
                            if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                                ref path,
                                ..
                            })) = arg
                            {
                                if path.segments.len() > 1
                                    && path.segments[0].ident == type_param.ident
                                {
                                    generics
                                        .make_where_clause()
                                        .predicates
                                        .push(syn::parse_quote!(#path: std::fmt::Debug));
                                    return generics;
                                } else {
                                    sgmt = path.segments.last().unwrap();
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    generics
}

fn add_trait_bounds(
    mut generics: syn::Generics,
    fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> syn::Generics {
    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = param {
            if is_only_in_phantomdata(type_param, fields) {
                continue;
            }
            type_param.bounds.push(syn::parse_quote!(std::fmt::Debug));
        }
    }

    generics
}
