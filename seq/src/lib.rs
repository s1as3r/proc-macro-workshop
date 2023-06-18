use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use syn::parse_macro_input;

struct Seq {
    ident: syn::Ident,
    from: syn::LitInt,
    to: syn::LitInt,
    body: TokenStream,
}

impl syn::parse::Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        input.parse::<syn::Token![in]>()?;
        let from = input.parse()?;
        input.parse::<syn::Token![..]>()?;
        let to = input.parse()?;
        let content;
        syn::braced!(content in input);
        let body = content.parse()?;

        Ok(Self {
            ident,
            from,
            to,
            body,
        })
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    parse_seq(parse_macro_input!(input as Seq))
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn parse_seq(seq: Seq) -> syn::Result<TokenStream> {
    let Seq {
        ident,
        from,
        to,
        body,
    } = seq;
    let from = from.base10_parse::<usize>()?;
    let to = to.base10_parse::<usize>()?;

    let sequenced: Vec<_> = (from..to)
        .map(|n| replace_ident(body.clone(), ident.clone(), n))
        .map(concat_ident)
        .collect();

    Ok(quote!(#(#sequenced)*))
}

fn replace_ident(body: TokenStream, ident: syn::Ident, with: usize) -> TokenStream {
    let mut new_ts = TokenStream::new();

    new_ts.extend(body.into_iter().map(|tt| match tt {
        TokenTree::Group(g) => {
            let delim = g.delimiter();
            let mut new_g =
                proc_macro2::Group::new(delim, replace_ident(g.stream(), ident.clone(), with));
            new_g.set_span(g.span());
            TokenTree::Group(new_g)
        }
        TokenTree::Ident(id) => {
            if id == ident {
                let mut lit = proc_macro2::Literal::usize_unsuffixed(with);
                lit.set_span(id.span());
                TokenTree::Literal(lit)
            } else {
                TokenTree::Ident(id)
            }
        }
        p @ TokenTree::Punct(_) => p,
        l @ TokenTree::Literal(_) => l,
    }));
    new_ts
}

fn concat_ident(ts: TokenStream) -> TokenStream {
    let tts: Vec<TokenTree> = ts.into_iter().collect();
    let mut indices = vec![];

    for (i, tt) in tts.iter().enumerate() {
        if let TokenTree::Punct(p) = tt {
            if p.as_char() == '~' {
                indices.push(i);
            }
        }
    }

    let mut concated = vec![];
    for (i, tt) in tts.iter().enumerate() {
        if indices.contains(&i) || indices.contains(&(i.saturating_sub(1))) {
            continue;
        }

        if indices.contains(&(i.saturating_add(1))) {
            if let TokenTree::Ident(id) = tt.clone() {
                if let TokenTree::Literal(l) = tts[i + 2].clone() {
                    let new_id_s = format!("{}{}", id, l);
                    let new_id = proc_macro2::Ident::new(&new_id_s, id.span());
                    concated.push(TokenTree::Ident(new_id));
                    continue;
                }
            }
        }
        concated.push(tt.clone())
    }

    let mut new_ts = TokenStream::new();
    new_ts.extend(concated);
    new_ts
}
