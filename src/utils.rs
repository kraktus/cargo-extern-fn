use indexmap::IndexSet;
use proc_macro2::{Ident, TokenStream};
use quote::format_ident;
use quote::quote;
use quote::ToTokens;

use syn::parse::Parse;
use syn::visit;
use syn::visit::Visit;
use syn::visit_mut;
use syn::visit_mut::VisitMut;
use syn::GenericArgument;
use syn::PathArguments;
use syn::ReturnType;

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
// if the type contains generics such as `Foo<T>`, scrap the brackets
// so it becomes `Foo_T`
pub fn get_ident(ty: &Type) -> Option<Ident> {
    if let Type::Path(path_ty) = ty {
        let mut segs = vec![];
        for p in path_ty.path.segments.iter() {
            segs.push(p.ident.to_string());
            if let PathArguments::AngleBracketed(ref in_brackets) = p.arguments {
                for generic_arg in in_brackets.args.iter() {
                    if let GenericArgument::Type(ty) = generic_arg {
                        if let Some(ident) = get_ident(ty) {
                            segs.push(ident.to_string())
                        }
                    }
                }
            }
        }
        let seg_string = segs.join("_");
        Some(syn::parse_str(&seg_string).unwrap())
    } else {
        None
    }
}

pub fn get_ident_camel_case(ty: &Type) -> Option<Ident> {
    get_ident(ty).map(|ident| {
        let camel = ident.to_string().replace('_', "");
        format_ident!("{camel}")
    })
}

pub fn add_suffix_last_segment(ty: &Type, suffix: &str) -> Type {
    let mut ty_suffixed = ty.clone();
    let mut v = AddSuffixLastSegment::new(suffix);
    v.visit_type_mut(&mut ty_suffixed);
    ty_suffixed
}

pub fn add_suffix(ty: &Type, suffix: &str, idents: &IndexSet<Ident>) -> Type {
    let mut ty_suffixed = ty.clone();
    let mut v = AddSuffix::new(suffix, idents);
    v.visit_type_mut(&mut ty_suffixed);
    ty_suffixed
}

pub struct AddSuffixLastSegment<'a> {
    suffix: &'a str,
}

impl<'a> AddSuffixLastSegment<'a> {
    pub fn new(suffix: &'a str) -> Self {
        Self { suffix }
    }
}

impl VisitMut for AddSuffixLastSegment<'_> {
    fn visit_type_path_mut(&mut self, ty_path: &mut syn::TypePath) {
        if let Some(last_seg) = ty_path.path.segments.last_mut() {
            last_seg.ident = format_ident!("{}{}", last_seg.ident, self.suffix);
        }
        visit_mut::visit_type_path_mut(self, ty_path)
    }
}

pub struct AddSuffix<'a> {
    suffix: &'a str,
    idents_to_add: &'a IndexSet<Ident>,
}

impl<'a> AddSuffix<'a> {
    pub fn new(suffix: &'a str, idents_to_add: &'a IndexSet<Ident>) -> Self {
        Self {
            suffix,
            idents_to_add,
        }
    }
}

impl VisitMut for AddSuffix<'_> {
    fn visit_ident_mut(&mut self, i: &mut Ident) {
        if self.idents_to_add.contains(i) {
            *i = format_ident!("{i}{}", self.suffix);
        }
        visit_mut::visit_ident_mut(self, i);
    }
}

pub fn return_contains_ref(return_ty: &ReturnType) -> bool {
    let mut contains_ref = ContainsRef::default();
    contains_ref.visit_return_type(return_ty);
    contains_ref.0
}

#[derive(Default)]
struct ContainsRef(bool);

impl<'ast> visit::Visit<'ast> for ContainsRef {
    fn visit_type_reference(&mut self, _: &'ast syn::TypeReference) {
        self.0 = true;
    }
}

pub fn contains_tuple(sig: &Signature) -> bool {
    let mut c = ContainsTuple::default();
    c.visit_signature(sig);
    c.0
}

#[derive(Default)]
struct ContainsTuple(bool);

impl<'ast> visit::Visit<'ast> for ContainsTuple {
    fn visit_type_tuple(&mut self, _: &'ast syn::TypeTuple) {
        self.0 = true;
    }
}

// This ASSUME that `FFi`-ation was done before
pub struct CamelCaseOption;

impl VisitMut for CamelCaseOption {
    fn visit_type_mut(&mut self, ty: &mut Type) {
        if is_type("Option", ty) {
            let camel = get_ident_camel_case(ty)
                .expect("Option to have ident")
                .to_string();
            *ty = syn::parse_str(&camel).expect("parsing ident as option type");
        }
        visit_mut::visit_type_mut(self, ty)
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

pub fn get_inner_option_type_sig(sig: &Signature) -> IndexSet<Type> {
    let mut inner_opt = OptionsInnerType::default();
    inner_opt.visit_signature(sig);
    inner_opt.0
}

pub fn get_inner_option_type_enum(enum_: &ItemEnum) -> IndexSet<Type> {
    let mut inner_opt = OptionsInnerType::default();
    inner_opt.visit_item_enum(enum_);
    inner_opt.0
}

pub fn get_inner_option_type_struct(struct_: &ItemStruct) -> IndexSet<Type> {
    let mut inner_opt = OptionsInnerType::default();
    inner_opt.visit_item_struct(struct_);
    inner_opt.0
}

#[derive(Default)]
struct OptionsInnerType(pub IndexSet<Type>);

impl<'a> Visit<'a> for OptionsInnerType {
    fn visit_type_path(&mut self, ty_path: &'a syn::TypePath) {
        for segment in ty_path.path.segments.iter() {
            if segment.ident == format_ident!("Option") {
                if let PathArguments::AngleBracketed(ref in_brackets) = segment.arguments {
                    if let Some(GenericArgument::Type(ty)) = in_brackets.args.first() {
                        self.0.insert(ty.clone());
                    }
                }
            }
        }
    }
}

/// Convert `&self`, `self`, `&mut self` to
/// `self<SUFFIX>: &<TYPE>`, `self<SUFFIX>: <TYPE>`, `self<SUFFIX>: &mut <TYPE>`
pub fn normalise_receiver_arg(
    arg: &FnArg,
    ty: Option<Type>,
    suffix: Option<&str>,
) -> Option<FnArg> {
    if let FnArg::Receiver(rec) = arg {
        let name = ty.as_ref().expect("Method not in an struct/enum impl");
        let normalised_self_ident = format_ident!("self{}", suffix.unwrap_or_default());
        Some(
            syn::parse2(match (rec.reference.clone(), rec.mutability) {
                (None, None) => quote!(#normalised_self_ident: #name),
                (None, Some(_)) => quote!(mut #normalised_self_ident: #name),
                (Some((_, lifetime_opt)), None) => {
                    let lifetime = lifetime_opt.map(|lt| quote!(#lt)).unwrap_or_default();
                    quote!(#normalised_self_ident: &#lifetime #name)
                }
                (Some((_, lifetime_opt)), Some(_)) => {
                    let lifetime = lifetime_opt.map(|lt| quote!(#lt)).unwrap_or_default();
                    quote!(#normalised_self_ident: &#lifetime mut #name)
                }
            })
            .expect("parsing of normalised arg"),
        )
    } else {
        None
    }
}

// from: https://github.com/bcpeinhardt/formulaY/blob/525f34fa15e0add50e402360f06a36fd0fb3711c/src/util.rs#L6
// Return whether a type matches a given &str
// /!\ Always return `false` if it has a len() > 1 (eg. std::fmt::..)
pub fn is_type(type_as_str: &str, ty: &Type) -> bool {
    if let syn::Type::Path(ref p) = ty {
        p.path.segments.len() == 1 && p.path.segments[0].ident == type_as_str
    } else {
        false
    }
}

pub fn is_method(sig: &Signature) -> bool {
    sig.inputs
        .iter()
        .any(|arg| matches!(arg, FnArg::Receiver(_)))
}

// given `Result<A, B>`, returns `Result<A>`. If not a result, return itself
pub fn result_without_error(ty: Type) -> Type {
    if is_type("Result", &ty) {
        if let syn::Type::Path(ref p) = ty {
            if let PathArguments::AngleBracketed(ref args) = p.path.segments[0].arguments {
                let first_arg = &args.args[0];
                syn::parse2(quote!(Result<#first_arg>))
                    .expect("`result_without_error` result parse")
            } else {
                panic!("Result args not bracked")
            }
        } else {
            panic!("Result ty not of type path, impossible")
        }
    } else {
        ty
    }
}

pub enum SelfType {
    Value,
    ValueMut,
    Ref,
    RefMut,
}

impl SelfType {
    pub fn is_ref_kind(&self) -> bool {
        matches!(self, Self::Ref | Self::RefMut)
    }

    pub fn is_by_value_kind(&self) -> bool {
        matches!(self, Self::Value | Self::ValueMut)
    }
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
        ty.map(|ty| quote!(#ty::).to_string()).unwrap_or_default(),
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
                    quote!(#ident.into()).to_tokens(&mut args_buf)
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
    let g1_param_sets: IndexSet<GenericParam> = IndexSet::from_iter(g1.params);
    let g2_param_sets: IndexSet<GenericParam> = IndexSet::from_iter(g2.params);
    let union_params = <Punctuated<GenericParam, Token![,]>>::from_iter(
        g1_param_sets
            .union(&g2_param_sets)
            .cloned()
            .into_iter()
            .map(|p| Pair::new(p, Some(<Token![,]>::default()))),
    );

    let g1_where_clause: Option<IndexSet<WherePredicate>> =
        g1.where_clause.map(|w| IndexSet::from_iter(w.predicates));
    let g2_where_clause: Option<IndexSet<WherePredicate>> =
        g2.where_clause.map(|w| IndexSet::from_iter(w.predicates));
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
use std::ops::Deref;
#[cfg(test)]
use syn::parse::ParseStream;

#[cfg(test)]
pub struct TypeTest(pub Type);

#[cfg(test)]
impl Parse for TypeTest {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(Type::without_plus(input)?))
    }
}
#[cfg(test)]
impl Deref for TypeTest {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod tests {

    use proc_macro2::Span;

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_call_function_from_sig() {
        let sig: Signature = syn::parse_str("fn foo(f: Foo, x: u64) -> bool").unwrap();
        assert_eq!(
            "foo (f . into () , x . into ())",
            format!("{}", call_function_from_sig(None, &sig, quote!(self_)))
        )
    }

    #[test]
    fn test_call_method_from_sig() {
        let sig: Signature = syn::parse_str("fn foo(&self) -> u8").unwrap();
        let ty: TypeTest = syn::parse_str("bar::Bar").unwrap();
        assert_eq!(
            "bar :: Bar :: foo (self_)",
            format!("{}", call_function_from_sig(Some(&ty), &sig, quote!(self_)))
        )
    }

    #[test]
    fn test_ident() {
        let ty: Type = syn::parse_str("Gen<T>").unwrap();
        assert_eq!(
            Some(Ident::new("gen_t_", Span::call_site())),
            get_ident_as_function(&ty)
        )
    }

    #[test]
    fn test_ident2() {
        let ty: Type = syn::parse_str("foo::Gen<T>").unwrap();
        assert_eq!(
            Some(Ident::new("foo_gen_t_", Span::call_site())),
            get_ident_as_function(&ty)
        )
    }

    #[test]
    fn test_result_without_error() {
        for (input, output) in [
            ("Result<T, E>", "Result<T>"),
            ("foo::Foo", "foo::Foo"),
            ("Result<foo::Foo, e::E>", "Result<foo::Foo>"),
        ] {
            println!("input {input}, expected output {output}");
            let ty: Type = syn::parse_str(input).unwrap();
            assert_eq!(result_without_error(ty), syn::parse_str(output).unwrap())
        }
    }

    #[test]
    fn test_add_suffix_ty() {
        let mut ty_visitor: Type = syn::parse_str("foo::Gen<Bar>").unwrap();
        let idents = [format_ident!("Bar")].into();
        let mut ffi_visitor = AddSuffix::new("Ffi", &idents);
        ffi_visitor.visit_type_mut(&mut ty_visitor);
        let ty_ffi: TypeTest = syn::parse_str("foo::Gen<BarFfi>").unwrap();
        assert_eq!(ty_visitor, ty_ffi.0);
    }

    #[test]
    fn test_add_suffix_ty_slice() {
        let mut ty_visitor: Type = syn::parse_str("&[Bar]").unwrap();
        let idents = [format_ident!("Bar")].into();
        let mut ffi_visitor = AddSuffix::new("Ffi", &idents);
        ffi_visitor.visit_type_mut(&mut ty_visitor);
        let ty_ffi: TypeTest = syn::parse_str("&[BarFfi]").unwrap();
        assert_eq!(ty_visitor, ty_ffi.0);
    }
}
