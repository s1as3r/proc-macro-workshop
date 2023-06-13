#![allow(unused, dead_code)]
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, Data, DeriveInput, Error, Expr, Field, Fields,
    FieldsNamed, GenericArgument, Ident, Lit, Meta, MetaNameValue, PathArguments, Result, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn expand(input: DeriveInput) -> Result<TokenStream> {
    let name = input.ident;

    let builder_name = format_ident!("{}Builder", name);
    let fields = fields_with_ty(&input.data)?;
    let init = init_fields(&input.data)?;
    let funcs = impl_funcs(&name, &input.data)?;

    Ok(quote! {
        pub struct #builder_name {
            #fields
        }

        impl #builder_name {
            #funcs
        }

        impl #name {
            fn builder() -> #builder_name {
                #builder_name {
                    #init
                }
            }
        }
    })
}

fn for_struct_data<F>(data: &Data, action: F) -> Result<TokenStream>
where
    F: Fn(&FieldsNamed) -> Result<TokenStream>,
{
    if let Data::Struct(syn::DataStruct {
        fields: Fields::Named(ref fields),
        ..
    }) = data
    {
        action(fields)
    } else {
        unimplemented!()
    }
}

fn fields_with_ty(data: &Data) -> Result<TokenStream> {
    for_struct_data(data, |fields| {
        let fields_with_ty = fields.named.iter().map(|f| {
            let name = &f.ident;
            let ty = &f.ty;

            if has_each(f)?.is_some() {
                return Ok(quote_spanned! {f.span()=>
                    #name: #ty,
                });
            }

            Ok(quote_spanned! {f.span()=>
                #name: std::option::Option<#ty>,
            })
        });
        let vec_f: Vec<TokenStream> = fields_with_ty.collect::<Result<Vec<_>>>()?;
        Ok(quote! { #( #vec_f )* })
    })
}

fn is_optional(ty: &Type) -> bool {
    if let Type::Path(path) = ty {
        path.path.segments[0].ident == "Option"
    } else {
        false
    }
}

fn inner_ty(ty: &Type) -> Option<Type> {
    if let Type::Path(ty) = ty {
        if let PathArguments::AngleBracketed(args) = &ty.path.segments[0].arguments {
            if let GenericArgument::Type(ty) = &args.args[0] {
                return Some(ty.clone());
            }
        }
    }
    None
}

fn has_each(field: &Field) -> Result<Option<Ident>> {
    for attr in &field.attrs {
        if let Meta::List(ref ml) = attr.meta {
            if ml.path.is_ident("builder") {
                let args: MetaNameValue = attr.parse_args().unwrap();
                if args.path.is_ident("each") {
                    if let Expr::Lit(l) = args.value {
                        if let Lit::Str(ls) = l.lit {
                            return Ok(Some(format_ident!("{}", ls.value())));
                        }
                    }
                } else {
                    return Err(Error::new_spanned(
                        ml,
                        r#"expected `builder(each = "...")`"#,
                    ));
                }
            }
        }
    }
    Ok(None)
}

fn init_fields(data: &Data) -> Result<TokenStream> {
    for_struct_data(data, |fields| {
        let fields_init = fields.named.iter().map(|f| {
            let name = &f.ident;
            let ty = &f.ty;

            if is_optional(ty) {
                return Ok(quote_spanned! {f.span() =>
                    #name: std::option::Option::Some(None)
                });
            }

            if has_each(f)?.is_some() {
                return Ok(quote_spanned! {f.span() =>
                    #name: std::vec![],
                });
            }

            Ok(quote_spanned! {f.span()=>
                #name: std::option::Option::None,
            })
        });
        let vec_f: Vec<TokenStream> = fields_init.collect::<Result<_>>()?;
        Ok(quote! { #( #vec_f )* })
    })
}

fn impl_funcs(name: &Ident, data: &Data) -> Result<TokenStream> {
    for_struct_data(data, |fields| {
        let funcs = fields.named.iter().map(|f| {
            let name = &f.ident;
            let ty = &f.ty;

            if is_optional(ty) {
                let ty = inner_ty(ty).unwrap();
                return Ok(quote_spanned! {f.span()=>
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = std::option::Option::Some(std::option::Option::Some(#name));
                        self
                    }
                });
            }

            if let Some(fnname) = has_each(f)? {
                let ity = inner_ty(&f.ty).unwrap();
                let set_one = quote_spanned! {f.span()=>
                    fn #fnname(&mut self, #fnname: #ity) -> &mut Self {
                        self.#name.push(#fnname);
                        self
                    }
                };

                if fnname == name.clone().unwrap() {
                    return Ok(set_one);
                } else {
                    return Ok(quote_spanned! {f.span()=>
                        #set_one

                        fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = #name;
                            self
                        }
                    });
                }
            }

            Ok(quote_spanned! {f.span()=>
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                }
            })
        });

        let checks = fields.named.iter().map(|f| {
            let name = &f.ident;

            if has_each(f)?.is_some() {
                return Ok(quote!())
            }
            Ok(quote_spanned! {f.span() =>
                if self.#name.is_none() {
                    return std::result::Result::Err(<std::boxed::Box<dyn std::error::Error>>::from(format!("{} is None", stringify!(#name))));
                }
            })
        });

        let inits = fields.named.iter().map(|f| {
            let name = &f.ident;

            if has_each(f)?.is_some() {
                return Ok(quote_spanned! {f.span()=>
                    #name: self.#name.clone(),
                });
            }
            Ok(quote_spanned! {f.span() =>
                #name: self.#name.clone().unwrap(),
            })
        });

        let funcs: Vec<_> = funcs.collect::<Result<_>>()?;
        let checks: Vec<_> = checks.collect::<Result<_>>()?;
        let inits: Vec<_> = inits.collect::<Result<_>>()?;
        Ok(quote! {
            #( #funcs )*

            fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                #( #checks )*

                std::result::Result::Ok(#name {
                    #( #inits )*
                })
            }
        })
    })
}
