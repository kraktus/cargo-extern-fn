use std::collections::HashSet;
use std::fmt::Display;

use proc_macro2::{Ident, TokenStream};
use quote::format_ident;
use quote::quote;
use quote::ToTokens;
use syn::parse::Parse;
use syn::Pat;
use syn::PatIdent;
use syn::PatType;
use syn::Signature;
use syn::{
    punctuated::{Pair, Punctuated},
    Attribute, FnArg, GenericParam, Generics, Item, ItemConst, ItemEnum, ItemExternCrate, ItemFn,
    ItemForeignMod, ItemImpl, ItemMacro, ItemMacro2, ItemMod, ItemStatic, ItemStruct, ItemTrait,
    ItemTraitAlias, ItemType, ItemUnion, ItemUse, Lit, Meta, MetaNameValue, Token, Type,
    WhereClause, WherePredicate,
};

pub fn attrs(item: &Item) -> Option<&Vec<Attribute>> {
    match item {
        Item::ExternCrate(ItemExternCrate { attrs, .. })
        | Item::Use(ItemUse { attrs, .. })
        | Item::Static(ItemStatic { attrs, .. })
        | Item::Const(ItemConst { attrs, .. })
        | Item::Fn(ItemFn { attrs, .. })
        | Item::Mod(ItemMod { attrs, .. })
        | Item::ForeignMod(ItemForeignMod { attrs, .. })
        | Item::Type(ItemType { attrs, .. })
        | Item::Struct(ItemStruct { attrs, .. })
        | Item::Enum(ItemEnum { attrs, .. })
        | Item::Union(ItemUnion { attrs, .. })
        | Item::Trait(ItemTrait { attrs, .. })
        | Item::TraitAlias(ItemTraitAlias { attrs, .. })
        | Item::Impl(ItemImpl { attrs, .. })
        | Item::Macro(ItemMacro { attrs, .. })
        | Item::Macro2(ItemMacro2 { attrs, .. }) => Some(attrs),
        Item::Verbatim(_) => None,
        _ => unreachable!(),
    }
}

// return true if there is a doc comment of the type [doc = "extern_fn:skip"]`
pub fn meta_is_extern_fn_skip(meta: syn::Result<Meta>) -> bool {
    if let Ok(Meta::NameValue(MetaNameValue {
        path,
        lit: Lit::Str(lit_str),
        ..
    })) = meta
    {
        path.is_ident("doc") && lit_str.value().trim() == "extern_fn:skip"
    } else {
        false
    }
}

// return the ident of the type if available
// if the type contains generics such as `Foo<T>`, scrap them,
// so in our example it would be converted to `foo_`
pub fn get_ident(ty: &Type) -> Option<Ident> {
    if let Type::Path(path_ty) = ty {
        let mut segs_without_generics = vec![];
        for p in path_ty.path.segments.iter() {
            segs_without_generics.push(p.ident.clone().to_string());
            if !p.arguments.is_none() {
                break;
            }
        }
        let seg_string = segs_without_generics.join("_");
        Some(syn::parse_str(&seg_string).unwrap())
    } else {
        None
    }
}

// return the lower-cased version of the ident of a type, with a trailing `_`
// the trailing underscore ensure it will not result in a keyword
pub fn get_ident_as_function(ty: &Type) -> Option<Ident> {
    get_ident(ty).map(|ident| {
        let ident_str = ident.to_string().to_ascii_lowercase();
        format_ident!("{}_", ident_str)
    })
}

/// Convert `&self`, `self`, `&mut self` to
/// `self<SUFFIX>: &<TYPE>`, `self<SUFFIX>: <TYPE>`, `self<SUFFIX>: &mut <TYPE>`
pub fn normalise_receiver_arg(
    arg: &FnArg,
    ty: &Option<Type>,
    suffix: Option<&str>,
) -> Option<FnArg> {
    if let FnArg::Receiver(rec) = arg {
        let name = ty.as_ref().expect("Method not in an struct/enum impl");
        let normalised_self_ident = format_ident!("self{}", suffix.unwrap_or_default());
        Some(
            syn::parse2(match (rec.reference.clone(), rec.mutability) {
                (None, None) => quote!(#normalised_self_ident: #name),
                (None, Some(_)) => quote!(#normalised_self_ident: mut #name),
                (Some((_, lifetime_opt)), None) => {
                    let lifetime = lifetime_opt.map(|lt| quote!(#lt)).unwrap_or_default();
                    quote!(#normalised_self_ident: &#lifetime #name)
                }
                (Some((_, lifetime_opt)), Some(_)) => {
                    let lifetime = lifetime_opt.map(|lt| quote!(#lt)).unwrap_or_default();
                    quote!(#normalised_self_ident: &#lifetime mut #name)
                }
            })
            .unwrap(),
        )
    } else {
        None
    }
}

// from: https://github.com/bcpeinhardt/formulaY/blob/525f34fa15e0add50e402360f06a36fd0fb3711c/src/util.rs#L6
// Return whether a type matches a given &str
// /!\ Always return `false` if it has a len() > 1 (eg. std::fmt::..)
pub fn is_type(type_as_str: &str, ty: &syn::Type) -> bool {
    if let syn::Type::Path(ref p) = ty {
        return p.path.segments.len() == 1 && p.path.segments[0].ident == type_as_str;
    } else {
        false
    }
}

pub fn is_method(sig: &Signature) -> bool {
    sig.inputs
        .iter()
        .any(|arg| matches!(arg, FnArg::Receiver(_)))
}

pub enum SelfType {
    Value,
    ValueMut,
    Ref,
    RefMut,
}

pub fn method_self_type(arg: &FnArg) -> Option<SelfType> {
    if let FnArg::Receiver(rec) = arg {
        Some(match (rec.reference.clone(), rec.mutability) {
            (None, None) => SelfType::Value,
            (None, Some(_)) => SelfType::ValueMut,
            (Some(_), None) => SelfType::Ref,
            (Some(_), Some(_)) => SelfType::RefMut,
        })
    } else {
        None
    }
}

// given a function signature `fn foo(u: usize, bar: &str) -> bool`
// return an expression calling that function:
// `foo(u, bar)`
//
// TODO would it be better if trying to build it as a `syn::Call` type?
pub fn call_function_from_sig(
    ty: Option<&Type>,
    sig: &Signature,
    self_expr: TokenStream,
) -> TokenStream {
    let fn_ident = format!(
        "{}{}",
        ty.map(|ty| quote!(<#ty>::).to_string()).unwrap_or_default(),
        sig.ident
    );
    let mut args_buf = TokenStream::new();
    let mut iter_peek = sig.inputs.iter().peekable();
    // we scrap the types of the signature to effectively use their idents as arguments
    while let Some(arg) = iter_peek.next() {
        match arg {
            FnArg::Receiver(_) => self_expr.to_tokens(&mut args_buf),
            FnArg::Typed(PatType { pat, .. }) => {
                if let Pat::Ident(PatIdent { ident, .. }) = (**pat).clone() {
                    quote!(#ident).to_tokens(&mut args_buf)
                }
            }
        }
        if iter_peek.peek().is_some() {
            quote!(,).to_tokens(&mut args_buf)
        }
    }

    format!("{fn_ident}({args_buf})").parse().unwrap()
}

// return the union of both generics constraints
// it's the union in term of quantity but it's effectively
// the intersection in terms of predicates
//
// TODO how to make it shorter/better/faster/stronger?
pub fn union(g1: Generics, g2: Generics) -> Generics {
    let g1_param_sets: HashSet<GenericParam> = HashSet::from_iter(g1.params);
    let g2_param_sets: HashSet<GenericParam> = HashSet::from_iter(g2.params);
    let union_params = <Punctuated<GenericParam, Token![,]>>::from_iter(
        g1_param_sets
            .union(&g2_param_sets)
            .cloned()
            .into_iter()
            .map(|p| Pair::new(p, Some(<Token![,]>::default()))),
    );

    let g1_where_clause: Option<HashSet<WherePredicate>> =
        g1.where_clause.map(|w| HashSet::from_iter(w.predicates));
    let g2_where_clause: Option<HashSet<WherePredicate>> =
        g2.where_clause.map(|w| HashSet::from_iter(w.predicates));
    let union_where = g1_where_clause
        .or(g2_where_clause.clone())
        .map(|g| WhereClause {
            where_token: <Token![where]>::default(),
            // if g1_where_clause is some, correct behavior
            // if only g2_where_clause is some, then union is idempotent
            predicates: <Punctuated<WherePredicate, Token![,]>>::from_iter(
                g2_where_clause
                    .clone()
                    .map_or(g.clone(), |g2| g.union(&g2).cloned().collect())
                    .into_iter()
                    .map(|p| Pair::new(p, Some(<Token![,]>::default()))),
            ),
        });

    Generics {
        lt_token: g1.lt_token.or(g2.lt_token),
        params: union_params,
        gt_token: g1.gt_token.or(g2.gt_token),
        where_clause: union_where,
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use proc_macro2::Span;
    use syn::parse::{Parse, ParseStream};

    use super::*;

    struct TypeTest(Type);

    impl Parse for TypeTest {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(Self(Type::without_plus(input)?))
        }
    }
    impl Deref for TypeTest {
        type Target = Type;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    #[test]
    fn test_call_function_from_sig() {
        let sig: Signature = syn::parse_str("fn foo(f: Foo, x: u64) -> bool").unwrap();
        assert_eq!(
            "foo (f , x)",
            format!("{}", call_function_from_sig(None, &sig, quote!(self_)))
        )
    }

    #[test]
    fn test_call_method_from_sig() {
        let sig: Signature = syn::parse_str("fn foo(&self) -> u8").unwrap();
        let ty: TypeTest = syn::parse_str("bar::Bar").unwrap();
        assert_eq!(
            "< bar :: Bar > :: foo (self_)",
            format!("{}", call_function_from_sig(Some(&ty), &sig, quote!(self_)))
        )
    }

    #[test]
    fn test_ident() {
        let ty: Type = syn::parse_str("Gen<T>").unwrap();
        assert_eq!(
            Some(Ident::new("gen_", Span::call_site())),
            get_ident_as_function(&ty)
        )
    }

    #[test]
    fn test_ident2() {
        let ty: Type = syn::parse_str("foo::Gen<T>").unwrap();
        assert_eq!(
            Some(Ident::new("foo_gen_", Span::call_site())),
            get_ident_as_function(&ty)
        )
    }
}
