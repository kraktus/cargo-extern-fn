use std::collections::HashMap;

use std::fs::{DirEntry, File, OpenOptions};
use std::io::{Read, Write};
use std::path::Path;

use log::trace;
use proc_macro2::{Span, TokenStream};

use indexmap::IndexSet;
use syn::punctuated::Punctuated;
use syn::visit::{self, Visit};
use syn::visit_mut::VisitMut;
use syn::{
    parse_quote, Fields, FieldsNamed, Ident, ImplItemMethod, Index, Item, ItemFn, ItemImpl,
    ReturnType, Signature, Token, Type,
};
use syn::{ItemEnum, ItemStruct, Visibility};

use quote::{format_ident, quote, ToTokens};

use crate::utils::{
    attrs, contains_tuple, get_ident, get_ident_as_function, is_method, meta_is_extern_fn_skip,
    method_self_type, normalise_receiver_arg, result_without_error, return_contains_ref, AddSuffix,
    SelfType, add_suffix,
};
use crate::utils::{call_function_from_sig, is_type};

#[derive(Default, Debug)]
pub struct Cxx {
    all_cxx_fn: HashMap<Option<Type>, Vec<CxxFn>>,
    all_cxx_struct_or_enum: IndexSet<StructOrEnum>, // TODO maybe switch to derive
    all_cxx_idents: IndexSet<Ident>,
}

fn as_idents(s_e: &IndexSet<StructOrEnum>) -> IndexSet<Ident> {
    s_e.iter().map(StructOrEnum::ident).cloned().collect()
}

fn find_struct_enum<'a>(
    hash_set: &'a IndexSet<StructOrEnum>,
    ty: &Type,
) -> Option<&'a StructOrEnum> {
    let ident = get_ident(ty)?;
    hash_set.iter().find(|se| se.ident() == &ident)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Inner {
    S(ItemStruct),
    E(ItemEnum),
}

impl Inner {
    fn visit_mut<V: VisitMut>(&mut self, visitor: &mut V) {
        match self {
            Self::S(ref mut x) => syn::visit_mut::visit_item_struct_mut(visitor, x),
            Self::E(ref mut x) => syn::visit_mut::visit_item_enum_mut(visitor, x),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct StructOrEnum {
    inner: Inner,
    module: Option<Ident>,
}

impl ToTokens for StructOrEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match &self.inner {
            Inner::S(x) => x.to_tokens(tokens),
            Inner::E(x) => x.to_tokens(tokens),
        }
    }
}

impl StructOrEnum {
    pub fn new_struct(s: ItemStruct, module: Option<Ident>) -> Self {
        Self {
            inner: Inner::S(s),
            module,
        }
    }

    pub fn new_enum(e: ItemEnum, module: Option<Ident>) -> Self {
        Self {
            inner: Inner::E(e),
            module,
        }
    }

    fn ident(&self) -> &Ident {
        match &self.inner {
            Inner::S(x) => &x.ident,
            Inner::E(x) => &x.ident,
        }
    }

    fn ident_mut(&mut self) -> &mut Ident {
        match self.inner {
            Inner::S(ref mut x) => &mut x.ident,
            Inner::E(ref mut x) => &mut x.ident,
        }
    }

    fn is_enum(&self) -> bool {
        matches!(self.inner, Inner::E(_))
    }

    fn visit_mut<V: VisitMut>(&mut self, visitor: &mut V) {
        match self.inner {
            Inner::S(ref mut x) => syn::visit_mut::visit_item_struct_mut(visitor, x),
            Inner::E(ref mut x) => syn::visit_mut::visit_item_enum_mut(visitor, x),
        }
    }

    fn as_ffi(&self, idents_to_add: &IndexSet<Ident>) -> Self {
        let mut inner_ffi = match &self.inner {
            Inner::S(_x) => Inner::S(self.as_x_struct("Ffi").expect("We know it's a struct")),
            Inner::E(x) => {
                let mut enum_ffi = x.clone();
                enum_ffi.ident = format_ident!("{}Ffi", enum_ffi.ident);
                Inner::E(enum_ffi)
            }
        };
        let mut visitor = AddSuffix::new("Ffi", idents_to_add);
        inner_ffi.visit_mut(&mut visitor);
        Self {
            inner: inner_ffi,
            module: None // for now use glob import all the ffi module at first, otherwise Some(format_ident!("ffi"))
        }
    }

    fn as_x_struct(&self, suffix: &str) -> Option<ItemStruct> {
        match &self.inner {
            Inner::S(struct_) => {
                trace!("Creating ffi struct of {}", struct_.ident);
                let mut raw_struct = struct_.clone();
                raw_struct.ident = format_ident!("{}{suffix}", struct_.ident);
                for field_mut in raw_struct.fields.iter_mut() {
                    field_mut.vis = struct_.vis.clone() // make it public, not sure if reusing the span will cause issue
                }
                // tuple struct are not supported by cxx
                // need to be converted to named-struct:
                // S(A,B,C) -> S {n0: A, n1: B, n2: C}
                if matches!(raw_struct.fields, Fields::Unnamed(_)) {
                    let mut named_fields = Punctuated::<_, Token![,]>::new();
                    if let Fields::Unnamed(fields) = raw_struct.fields {
                        for (i, field) in fields.unnamed.iter().enumerate() {
                            let mut field_converted_to_named = field.clone();
                            field_converted_to_named.ident = Some(format_ident!("n{}", i));
                            named_fields.push(field_converted_to_named);
                        }
                    }
                    raw_struct.fields = Fields::Named(FieldsNamed {
                        brace_token: syn::token::Brace {
                            span: Span::call_site(),
                        },
                        named: named_fields,
                    })
                }
                Some(raw_struct)
            }
            Inner::E(_) => None,
        }
    }

    fn original_and_ffi(&self, idents_to_add: &IndexSet<Ident>) -> (Self, Self) {
        let mut without_mod = self.clone();
        without_mod.module = None;
        (without_mod, self.as_ffi(idents_to_add))
    }
}

#[derive(Debug, Clone, Default)]
struct GatherDataStructures {
    pub_struct_or_enum: IndexSet<StructOrEnum>,
    idents: IndexSet<Ident>, // idents of struct/enums. TODO union?
    module: Option<Ident>,
}

impl GatherDataStructures {
    pub fn new(module: Option<Ident>) -> Self {
        Self {
            module,
            ..Default::default()
        }
    }

    fn results(self) -> (IndexSet<StructOrEnum>, IndexSet<Ident>) {
        let Self {
            pub_struct_or_enum,
            idents,
            ..
        } = self;
        (pub_struct_or_enum, idents)
    }
}

impl<'ast> Visit<'ast> for GatherDataStructures {
    fn visit_item_struct(&mut self, struct_: &'ast ItemStruct) {
        if matches!(struct_.vis, Visibility::Public(_))
            && struct_.generics.params.is_empty()
            && !struct_.fields.is_empty()
        // unit structs not supported in cxx bridge
        // generics not handled by cxx
        {
            self.pub_struct_or_enum.insert(StructOrEnum::new_struct(
                struct_.clone(),
                self.module.clone(),
            ));
            self.idents.insert(struct_.ident.clone());
        }
        visit::visit_item_struct(self, struct_);
    }

    fn visit_item_enum(&mut self, enum_: &'ast ItemEnum) {
        if matches!(enum_.vis, Visibility::Public(_))
            && enum_.generics.params.is_empty() // generics not handled by cxx
            && !enum_.variants.is_empty() // unit enum not supported in cxx bridge
            && enum_
                .variants
                .iter()
                .all(|v| matches!(v.fields, Fields::Unit))
        {
            self.idents.insert(enum_.ident.clone());
            self.pub_struct_or_enum
                .insert(StructOrEnum::new_enum(enum_.clone(), self.module.clone()));
        }
        visit::visit_item_enum(self, enum_);
    }

    fn visit_item(&mut self, i: &'ast Item) {
        if attrs(i).map_or(true, |attrs| {
            attrs
                .iter()
                .all(|a| !meta_is_extern_fn_skip(a.parse_meta()))
        }) {
            visit::visit_item(self, i)
        }
    }
}

// for each file, add at the end of it its externalised fn
// regular `pub fn foo(arg1: X, arg2: &Y) -> bool`
// are converted to `#[no_mangle] pub extern "C" fn ffi_foo(arg1: X, arg2: &Y) -> bool`
// method `pub fn foo_method(&self,arg1: X, arg2: &Y) -> bool`
// are converted to `#[no_mangle] pub extern "C" fn ffi_foo_method(self_: &Foo,arg1: X, arg2: &Y) -> bool`
#[derive(Debug, Clone, Default)]
struct GatherSignatures {
    // only set if not a trait method
    current_impl_ty: Option<syn::Type>,
    allowed_ds: IndexSet<StructOrEnum>,
    cxx_fn_buf: HashMap<Option<Type>, Vec<CxxFn>>,
    module: Option<Ident>, // name of the module (only looking at the file for now). `None` if declared in `lib.rs`
}

#[derive(Debug, Clone)]
struct CxxFn {
    pub item_fn: ItemFn,
    ty: Option<Type>, // set on methods and associated functions
    is_associated: bool,
    is_from_enum: bool,      // either a method or associated function from an enum
    pub return_is_opt: bool, // cxx does not handle `Option`... convert to `Result`
    module: Option<Ident>, // name of the module (only looking at the file for now). `None` if declared in `lib.rs`
}

impl CxxFn {
    fn new(item_fn: ItemFn, ty: Option<Type>, module: Option<Ident>, is_from_enum: bool) -> Self {
        let return_is_opt = if let ReturnType::Type(_, ty) = item_fn.sig.output.clone() {
            is_type("Option", &ty)
        } else {
            false
        };
        Self {
            is_associated: ty.is_some() && !is_method(&item_fn.sig),
            item_fn,
            module,
            return_is_opt,
            is_from_enum,
            ty,
        }
    }

    fn is_unsafe(&self) -> bool {
        self.item_fn.sig.unsafety.is_some()
    }

    // if a method, return `true` if `self` or `mut self`
    fn takes_by_value(&self) -> bool {
        self.self_type()
            .as_ref()
            .map(SelfType::is_by_value_kind)
            .unwrap_or_default()
    }

    // if the function is a method, return the type of
    fn self_type(&self) -> Option<SelfType> {
        self.item_fn.sig.inputs.iter().find_map(method_self_type)
    }

    fn should_be_turned_in_free_fn(&self) -> bool {
        self.is_associated || self.takes_by_value() || self.is_from_enum
    }

    fn return_result(&self) -> bool {
        if let ReturnType::Type(_, ref ty) = self.item_fn.sig.output {
            is_type("Result", ty)
        } else {
            false
        }
    }

    fn cxx_ident(&self) -> Ident {
        let mut cxx_ident = self.item_fn.sig.ident.clone();
        if self.is_unsafe() {
            cxx_ident = format_ident!("unsafe_{}", cxx_ident);
        }
        if self.should_be_turned_in_free_fn() {
            cxx_ident = format_ident!(
                "{}{}",
                self.ty
                    .as_ref()
                    .and_then(get_ident_as_function)
                    .expect("type with ident"),
                cxx_ident
            )
        } else if self.ty.is_none() {
            // add a suffix to disambiguate free functions
            // TODO find better way if possible
            cxx_ident = format_ident!("{}_ffi", cxx_ident)
        }
        cxx_ident
    }

    fn as_cxx_sig(&self, to_be_ffied: &IndexSet<StructOrEnum>) -> Signature {
        trace!(
            "Generating cxx sig of {}::{}",
            self.ty
                .as_ref()
                .and_then(get_ident)
                .as_ref()
                .map(Ident::to_string)
                .unwrap_or_default(),
            self.item_fn.sig.ident
        );
        let mut cxx_sig = self.item_fn.sig.clone();
        // const and unsafe functions not supported on the bridge
        cxx_sig.unsafety = None;
        cxx_sig.constness = None;
        cxx_sig.ident = self.cxx_ident();
        // Options are not supported by cxx, convert them to result
        if self.return_is_opt {
            if let ReturnType::Type(_, ref mut ty) = cxx_sig.output {
                if let Type::Path(ref mut p) = **ty {
                    p.path.segments[0].ident = syn::parse_str("Result").unwrap();
                }
            }
        }
        // We need to remove the error side of Result<T, E> to be accepted on the bridge
        if let ReturnType::Type(_, ref mut ty) = cxx_sig.output {
            let unboxed_ty = *ty.clone();
            **ty = result_without_error(unboxed_ty);
        }
        // enums need to be transformed into free functions
        if self.is_from_enum || self.takes_by_value() {
            for arg in cxx_sig.inputs.iter_mut() {
                if let Some(normalised_arg) =
                    normalise_receiver_arg(arg, self.ty.clone(), Some("_"))
                {
                    *arg = normalised_arg;
                }
            }
        }
        let idents = as_idents(to_be_ffied);
        let mut visitor = AddSuffix::new("Ffi", &idents);
        visitor.visit_signature_mut(&mut cxx_sig);

        cxx_sig
    }

    // same as cxx_sig but with the additional `&self` -> `self: &Foo` trick
    fn as_cxx_bridge_sig(&self, to_be_ffied: &IndexSet<StructOrEnum>) -> TokenStream {
        let mut cxx_sig = self.as_cxx_sig(to_be_ffied);
        for arg in cxx_sig.inputs.iter_mut() {
            let ffied_ty = self
                .ty
                .as_ref()
                .map(|ty| add_suffix(ty, "Ffi"));
            if let Some(normalised_arg) = normalise_receiver_arg(arg, ffied_ty, None) {
                *arg = normalised_arg;
            }
        }
        quote!(#cxx_sig;)
    }

    // TokenStream of an ItemFn
    fn as_cxx_impl(&self, idents_to_be_ffied: &IndexSet<StructOrEnum>) -> TokenStream {
        let mut cxx_item = self.item_fn.clone();
        cxx_item.sig = self.as_cxx_sig(idents_to_be_ffied);

        // if the function returns a reference, bail out and keep the original function body
        // which is usually just reading a field
        if return_contains_ref(&cxx_item.sig.output) {
            return quote!(#cxx_item);
        }

        let self_type_opt = self.item_fn.sig.inputs.iter().find_map(method_self_type);
        // if the function takes by value mut or ref mut, we need to create a mutable clone before calling
        // the method on the original struct/enum
        let before_call_fn = if let Some(self_type) = self_type_opt.as_ref() {
            let ty_ = self.ty.as_ref().expect("No type defined in method");
            match self_type {
                SelfType::ValueMut => quote!(let mut x = <#ty_>::from(self);),
                SelfType::RefMut => quote!(let mut x = <#ty_>::from(self.clone());),
                _ => TokenStream::new(),
            }
        } else {
            TokenStream::new()
        };

        let mut call_fn = if let Some(self_type) = self_type_opt.as_ref() {
            let ty_ = self.ty.as_ref().expect("No type defined in method");
            let self_ident = if self.is_from_enum || self.takes_by_value() {
                format_ident!("self_")
            } else {
                format_ident!("self")
            };
            match self_type {
                SelfType::Value => call_function_from_sig(
                    self.ty.as_ref(),
                    &self.item_fn.sig,
                    quote!(#self_ident.into()),
                ),
                SelfType::ValueMut => {
                    call_function_from_sig(self.ty.as_ref(), &self.item_fn.sig, quote!(x))
                }
                SelfType::Ref => call_function_from_sig(
                    self.ty.as_ref(),
                    &self.item_fn.sig,
                    quote!(&<#ty_>::from(#self_ident.clone())),
                ),
                SelfType::RefMut => {
                    call_function_from_sig(self.ty.as_ref(), &self.item_fn.sig, quote!(&mut x))
                }
            }
        } else {
            call_function_from_sig(self.ty.as_ref(), &self.item_fn.sig, quote!())
        };
        // if the function is not accessible in the current namespace
        if let Some(module) = self.module.as_ref() {
            call_fn = quote!(#module :: #call_fn);
        }
        if self.is_unsafe() {
            call_fn = quote!(unsafe { #call_fn });
        }

        if self.return_is_opt {
            call_fn = quote!(#call_fn.map(::std::convert::Into::into).ok_or("option none"));
        } else if self.return_result() {
            call_fn = quote!(#call_fn.map_or(Err("result error"), |r| Ok(r.into())))
        };

        // if the function takes by value mut or ref mut, we need to apply the differences of state from the original
        // to the Ffi datastructure
        let after_call_fn = if let Some(SelfType::RefMut) = self_type_opt.as_ref() {
            quote!(*self = Self::from(x);)
        } else if let Some(SelfType::ValueMut) = self_type_opt.as_ref() {
            // Only valueMut that return `Self` are supported for the moment
            quote!(let res = Self::from(x);)
        } else {
            TokenStream::new()
        };
        cxx_item.block = parse_quote!({#before_call_fn
            let res = #call_fn;
            #after_call_fn
            res.into()});
        quote!(#cxx_item)
    }
}

impl GatherSignatures {
    fn new(allowed_ds: IndexSet<StructOrEnum>, module: Option<Ident>) -> Self {
        Self {
            current_impl_ty: None,
            allowed_ds,
            cxx_fn_buf: HashMap::new(),
            module,
        }
    }

    fn handle_item_fn(
        &mut self,
        item_fn: &ItemFn,
    ) {
        if item_fn.sig.asyncness.is_none()
            && item_fn.sig.abi.is_none()
            && matches!(item_fn.vis, Visibility::Public(_))
            // do not handle function with `cfg` attributes for the moment
            && item_fn.attrs.iter().all(|a| !a.path.is_ident("cfg") && !meta_is_extern_fn_skip(a.parse_meta()))
            && item_fn.sig.generics.params.is_empty()
            && !contains_tuple(&item_fn.sig)
        // no supported by the bridge
        {
            trace!("handling fn {:?}", item_fn.sig.ident);
            let is_from_enum = self
                .current_impl_ty
                .as_ref()
                .and_then(|ty| find_struct_enum(&self.allowed_ds, ty))
                .map(StructOrEnum::is_enum)
                .unwrap_or_default();
            let cxx_fn = CxxFn::new(
                    item_fn.clone(),
                    self.current_impl_ty.clone(),
                    self.module.clone(),
                    is_from_enum,
                );

            self.cxx_fn_buf
                .entry(if cxx_fn.should_be_turned_in_free_fn() {
                    None // associated functions are converted into free functions
                         // same for functions from enum, because cxx does not support those
                } else {
                    self.current_impl_ty.clone()
                })
                .or_default()
                .push(cxx_fn);
        }
    }
}

impl<'ast> Visit<'ast> for GatherSignatures {
    fn visit_item_impl(&mut self, item_impl: &'ast ItemImpl) {
        if item_impl.trait_.is_none()
            && get_ident(&item_impl.self_ty)
                .map_or(false, |ident| as_idents(&self.allowed_ds).contains(&ident))
        {
            let outer_impl_ty = self.current_impl_ty.clone();
            self.current_impl_ty = Some(*item_impl.self_ty.clone());
            trace!("Looking at impl of {:?}", get_ident(&item_impl.self_ty));
            visit::visit_item_impl(self, item_impl);
            self.current_impl_ty = outer_impl_ty;
        }
    }

    fn visit_item(&mut self, i: &'ast Item) {
        if attrs(i).map_or(true, |attrs| {
            attrs
                .iter()
                .all(|a| !a.path.is_ident("cfg") && !meta_is_extern_fn_skip(a.parse_meta()))
        }) {
            visit::visit_item(self, i)
        }
    }

    fn visit_item_fn(&mut self, item_fn: &'ast ItemFn) {
        self.handle_item_fn(item_fn);
        visit::visit_item_fn(self, item_fn);
    }

    fn visit_impl_item_method(&mut self, item_method: &'ast ImplItemMethod) {
        let item_fn: ItemFn =
            syn::parse2(item_method.to_token_stream()).expect("from method to bare fn failed");
        self.handle_item_fn(&item_fn);

        visit::visit_impl_item_method(self, item_method);
    }
}

fn path(ident: &Ident, ident_mod: &Option<Ident>) -> TokenStream {
    if let Some(ref module) = ident_mod {
        quote!(crate::#module::#ident)
    } else {
        quote!(#ident)
    }
}

fn impl_from_x_to_y(
    (x, x_mod): (&ItemStruct, &Option<Ident>),
    (y, y_mod): (&ItemStruct, &Option<Ident>),
) -> TokenStream {
    let x_path = path(&x.ident, x_mod);
    let is_x_unnamed = matches!(&x.fields, Fields::Unnamed(_));
    let y_path = path(&y.ident, y_mod);
    let body = match &y.fields {
        Fields::Named(nameds) => {
            let named_token = nameds.named.iter().enumerate().map(|(i, f)| {
                let ident = f.ident.as_ref().expect("named field");
                let unnamed_ident = Index::from(i);
                if is_x_unnamed {
                    quote!(#ident: x.#unnamed_ident.into())
                } else {
                    quote!(#ident: x.#ident.into())
                }
            });
            quote!({#(#named_token),*})
        }
        Fields::Unnamed(unnameds) => {
            let unnamed_token = (0..unnameds.unnamed.len())
                .map(Index::from)
                .map(|i| // hardcode the fact that named fields are of the form nX, for named-struct -> unnamed struct conversions
                    if is_x_unnamed {
                     quote!(x.#i.into())
                } else {
                    let named_ident = format_ident!("n{}", i);
                     quote!(x.#named_ident.into())
                });
            quote!((#(#unnamed_token),*))
        }
        Fields::Unit => TokenStream::new(),
    };
    quote!(impl From<#x_path> for #y_path {
        fn from(x: #x_path) -> Self {
            Self #body.into()
        }
    })
}

fn impl_from_x_to_y_enum(
    (x, x_mod): (&ItemEnum, &Option<Ident>),
    (y, y_mod): (&ItemEnum, &Option<Ident>),
) -> TokenStream {
    let x_path = path(&x.ident, x_mod);
    let y_path = path(&y.ident, y_mod);
    // We assume the enum has only unit fields
    let body = x.variants.iter().map(|v| {
        let variant_ident = &v.ident;
        quote!(#x_path::#variant_ident => Self::#variant_ident)
    });

    quote!(impl From<#x_path> for #y_path {
        fn from(x: #x_path) -> Self {
                match x {
                    #(#body),*,
                    _ => unreachable!("No variant left")
                }
        }
    })
}

impl Cxx {
    fn generate_cxx_types(&self) -> TokenStream {
        trace!("Generating FFI struct declarations");
        let all_ffi = self
            .all_cxx_struct_or_enum
            .iter()
            .map(|enum_or_struct| enum_or_struct.as_ffi(&self.all_cxx_idents));
        trace!("Finished generating FFI struct declarations");
        quote!(#(#all_ffi)*)
    }

    fn generate_cxx_signatures(&self) -> TokenStream {
        let cxx_sig: Vec<TokenStream> = self
            .all_cxx_fn
            .iter()
            .flat_map(|(_, vec_cxx_fn)| {
                vec_cxx_fn
                    .iter()
                    .map(|cxx_fn| cxx_fn.as_cxx_bridge_sig(&self.all_cxx_struct_or_enum))
            })
            .collect();
        quote!(#(#cxx_sig)*)
    }

    fn generate_ffi_conversions(
        &self,
        struct_or_enum: IndexSet<StructOrEnum>,
        dry: bool,
    ) -> TokenStream {
        let mut buf = TokenStream::new();
        for (original, ffi) in struct_or_enum
            .iter()
            .map(|x| x.original_and_ffi(&self.all_cxx_idents))
        {
            trace!("Generating conversion impl of {}", ffi.ident());
            let original_mod = &original.module;
            let ffi_mod = &ffi.module;
            match (&original.inner, &ffi.inner) {
                (Inner::S(a), Inner::S(b)) => {
                    let back = impl_from_x_to_y((a, original_mod), (b, ffi_mod));
                    let forth = impl_from_x_to_y((b, ffi_mod), (a, original_mod));
                    quote!(#back #forth)
                }
                (Inner::E(a), Inner::E(b)) => {
                    let back = impl_from_x_to_y_enum((a, original_mod), (b, ffi_mod));
                    let forth = impl_from_x_to_y_enum((b, ffi_mod), (a, original_mod));
                    quote!(#back #forth)
                }
                _ => unreachable!("Impossible to have a struct turned into enum or vice-versa"),
            }
            .to_tokens(&mut buf);
            trace!("Finished conversion impl of {}", ffi.ident());
        }
        if !buf.is_empty() && !dry {
            quote!(use crate::ffi::*; #buf)
        } else {
            buf
        }
    }

    fn generate_ffi_impl(&self) -> TokenStream {
        let mut buf = TokenStream::new();
        for (ty_opt, vec_fn) in self.all_cxx_fn.iter() {
            let fn_impls = vec_fn
                .iter()
                .map(|f| f.as_cxx_impl(&self.all_cxx_struct_or_enum));

            if let Some(ty) = ty_opt {
                let ffi_ident = format_ident!("{}Ffi", get_ident(ty).unwrap());
                quote!(impl #ffi_ident {
                    #(#fn_impls)*
                })
            } else {
                quote!(#(#fn_impls)*)
            }
            .to_tokens(&mut buf)
        }
        buf
    }

    // Scan a source-code file to get Idents of data_struct (enum/struct) and function signatures
    // (method or free functions)
    // to be added to the bridge and turned into Ffi data_struct
    pub fn gather_data_struct_and_sign(
        &mut self,
        parsed_file: &syn::File,
        module: Ident,
        dry: bool,
    ) {
        let module_opt = (!dry && module != format_ident!("lib")).then(|| module);
        trace!("Starting GatherDataStructures pass");
        let mut gather_ds = GatherDataStructures::new(module_opt.clone());
        gather_ds.visit_file(parsed_file);
        trace!("Finished GatherDataStructures pass");
        let (pub_struct_or_enum, idents) = gather_ds.results();
        trace!("Starting GatherSignatures pass");
        let mut gather_sig = GatherSignatures::new(pub_struct_or_enum.clone(), module_opt);
        gather_sig.visit_file(parsed_file);
        trace!("Finished GatherSignatures pass");
        self.all_cxx_fn.extend(gather_sig.cxx_fn_buf);
        self.all_cxx_idents.extend(idents);
        self.all_cxx_struct_or_enum.extend(pub_struct_or_enum);
    }

    // once all files have been parsed once, add the conversion between
    // the ffi data_struct and original struct
    pub fn ffi_conversion(&self, parsed_file: &syn::File, dry: bool, module: Ident) -> TokenStream {
        let module_opt = (!dry && module != format_ident!("lib")).then(|| module);
        // need to get the idents declared in this file
        let mut gather_ds = GatherDataStructures::new(module_opt);
        gather_ds.visit_file(parsed_file);
        let (struct_or_enum, _) = gather_ds.results();
        self.generate_ffi_conversions(struct_or_enum, dry)
    }

    pub fn generate_ffi_bridge_and_impl(&self, code_dir: &Path, dry: bool) {
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .read(true)
            .open(code_dir.join("ffi.rs"))
            .expect("reading ffi.rs in src_dir failed");
        let mut src_file = String::new();
        file.read_to_string(&mut src_file)
            .expect("Unable to read file");
        let cxx_struct_declarations = self.generate_cxx_types();
        let cxx_sig = self.generate_cxx_signatures();
        let ffi_impl = self.generate_ffi_impl();
        // only need import when not in the same file
        let import_crate = if !dry {
            quote!(
                use crate::*;
            )
        } else {
            quote!()
        };
        let parsed_file_formated = prettyplease::unparse(&parse_quote!(
            /// Auto-generated code with `cargo-extern-fn`
            #[cxx::bridge]
            pub mod cxx_bridge {
                #cxx_struct_declarations

                    extern "Rust" {
                        #cxx_sig
                    }
            }
            pub use cxx_bridge::*;
            #import_crate
            type Result<T> = ::std::result::Result<T, &'static str>;

            #ffi_impl
        ));
        if dry {
            println!("\n{parsed_file_formated}")
        } else {
            file.write_all(parsed_file_formated.as_bytes())
                .expect("final writing of cxx::bridge failed");
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::utils::TypeTest;
    use pretty_assertions::assert_eq;

    use super::*;

    fn gen_struct(name: &str) -> StructOrEnum {
        let ident = format_ident!("{name}");
        StructOrEnum::new_struct(
            syn::parse2(quote!(pub struct #ident {x: usize})).expect("gen struct"),
            None,
        )
    }

    fn gen_enum(name: &str) -> StructOrEnum {
        let ident = format_ident!("{name}");
        StructOrEnum::new_enum(
            syn::parse2(quote!(pub enum #ident {A, B})).expect("gen enum"),
            None,
        )
    }

    fn gen_ty(name: &str) -> Type {
        syn::parse_str(name).unwrap()
    }

    #[test]
    fn test_avoiding_unit_struct_enum() {
        let file: syn::File = syn::parse_str(r#"pub struct S; pub enum E{}"#).unwrap();
        let mut gather_ds = GatherDataStructures::default();
        gather_ds.visit_file(&file);
        let (struct_or_enum, _) = gather_ds.results();
        assert!(struct_or_enum.is_empty())
    }

    #[test]
    fn test_ffi_struct_conversions() {
        let file: syn::File = syn::parse_str(r#"pub struct Foo(usize, u64, u8);"#).unwrap();
        let cxx = Cxx::default();
        let conv = cxx.ffi_conversion(&file, false, format_ident!("demo"));
        assert_eq!(
            prettyplease::unparse(&parse_quote!(#conv)),
            "use crate::ffi::*;
impl From<Foo> for FooFfi {
    fn from(x: Foo) -> Self {
        Self {
            n0: x.0.into(),
            n1: x.1.into(),
            n2: x.2.into(),
        }
            .into()
    }
}
impl From<FooFfi> for Foo {
    fn from(x: FooFfi) -> Self {
        Self(x.n0.into(), x.n1.into(), x.n2.into()).into()
    }
}
"
        )
    }

    #[test]
    fn test_ffi_enum_conversion() {
        let file: syn::File = syn::parse_str(r#"pub enum Citizen { Adult, Minor}"#).unwrap();
        let cxx = Cxx::default();
        let conv = cxx.ffi_conversion(&file, false, format_ident!("demo"));
        assert_eq!(
            prettyplease::unparse(&parse_quote!(#conv)),
            r#"use crate::ffi::*;
impl From<Citizen> for CitizenFfi {
    fn from(x: Citizen) -> Self {
        match x {
            Citizen::Adult => Self::Adult,
            Citizen::Minor => Self::Minor,
            _ => unreachable!("No variant left"),
        }
    }
}
impl From<CitizenFfi> for Citizen {
    fn from(x: CitizenFfi) -> Self {
        match x {
            CitizenFfi::Adult => Self::Adult,
            CitizenFfi::Minor => Self::Minor,
            _ => unreachable!("No variant left"),
        }
    }
}
"#
        )
    }

    #[test]
    fn test_ffi_enum_conversion_with_discriminant() {
        let file: syn::File = syn::parse_str(
            r#"pub enum Citizen {
                /// Nice doc comment 
                Adult = 12, 
                Minor = 123}"#,
        )
        .unwrap();
        let cxx = Cxx::default();
        let conv = cxx.ffi_conversion(&file, false, format_ident!("demo"));
        assert_eq!(
            prettyplease::unparse(&parse_quote!(#conv)),
            r#"use crate::ffi::*;
impl From<Citizen> for CitizenFfi {
    fn from(x: Citizen) -> Self {
        match x {
            Citizen::Adult => Self::Adult,
            Citizen::Minor => Self::Minor,
            _ => unreachable!("No variant left"),
        }
    }
}
impl From<CitizenFfi> for Citizen {
    fn from(x: CitizenFfi) -> Self {
        match x {
            CitizenFfi::Adult => Self::Adult,
            CitizenFfi::Minor => Self::Minor,
            _ => unreachable!("No variant left"),
        }
    }
}
"#
        )
    }

    #[test]
    fn test_ffi_conversions_with_2structs() {
        let file: syn::File =
            syn::parse_str(r#"pub struct Foo(usize, u64, u8); pub struct Bar {x: usize, y: u8}"#)
                .unwrap();
        let cxx = Cxx::default();
        let conv = cxx.ffi_conversion(&file, false, format_ident!("demo"));
        assert_eq!(
            prettyplease::unparse(&parse_quote!(#conv)),
            "use crate::ffi::*;
impl From<Foo> for FooFfi {
    fn from(x: Foo) -> Self {
        Self {
            n0: x.0.into(),
            n1: x.1.into(),
            n2: x.2.into(),
        }
            .into()
    }
}
impl From<FooFfi> for Foo {
    fn from(x: FooFfi) -> Self {
        Self(x.n0.into(), x.n1.into(), x.n2.into()).into()
    }
}
impl From<Bar> for BarFfi {
    fn from(x: Bar) -> Self {
        Self {
            x: x.x.into(),
            y: x.y.into(),
        }
            .into()
    }
}
impl From<BarFfi> for Bar {
    fn from(x: BarFfi) -> Self {
        Self {
            x: x.x.into(),
            y: x.y.into(),
        }
            .into()
    }
}
"
        )
    }

    #[test]
    fn test_cxx_sig() {
        let item_fn = syn::parse_str(
            r#"pub fn get_ident_as_function(ty: &Type) -> Ident{
    todo!()
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None, None, false);
        let cxx_sig = cxx_fn.as_cxx_sig(&IndexSet::new()).to_token_stream();

        assert_eq!(
            format!("{cxx_sig}"),
            "fn get_ident_as_function_ffi (ty : & Type) -> Ident"
        )
    }

    #[test]
    fn test_cxx_sig_result() {
        let item_fn = syn::parse_str(
            r#"pub fn get_ident_as_function(ty: &Type) -> Result<usize, ()> {
    todo!()
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None, None, false);
        let cxx_sig = cxx_fn.as_cxx_sig(&IndexSet::new()).to_token_stream();

        assert_eq!(
            format!("{cxx_sig}"),
            "fn get_ident_as_function_ffi (ty : & Type) -> Result < usize >"
        )
    }

    #[test]
    fn test_cxx_sig_const() {
        let item_fn = syn::parse_str(
            r#"pub const fn foo(ty: u8) -> usize {
    todo!()
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None, None, false);
        let cxx_sig = cxx_fn.as_cxx_sig(&IndexSet::new()).to_token_stream();

        assert_eq!(format!("{cxx_sig}"), "fn foo_ffi (ty : u8) -> usize")
    }

    #[test]
    fn test_cxx_sig_const_unsafe() {
        let item_fn = syn::parse_str(
            r#"pub const unsafe fn foo(ty: u8) -> usize {
    todo!()
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None, None, false);
        let cxx_sig = cxx_fn.as_cxx_sig(&IndexSet::new()).to_token_stream();

        assert_eq!(format!("{cxx_sig}"), "fn unsafe_foo_ffi (ty : u8) -> usize")
    }

    #[test]
    fn test_cxx_sig_with_ffi() {
        let item_fn = syn::parse_str(
            r#"pub fn add_ffi_to_types(foo: &Foo) -> Vec<Bar> {
    todo!()
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None, None, false);
        let cxx_sig = cxx_fn
            .as_cxx_sig(&[gen_struct("Foo"), gen_struct("Bar")].into())
            .to_token_stream();

        assert_eq!(
            format!("{cxx_sig}"),
            "fn add_ffi_to_types_ffi (foo : & FooFfi) -> Vec < BarFfi >"
        )
    }

    #[test]
    fn test_cxx_sig_opt() {
        let item_fn = syn::parse_str(
            r#"pub fn get_ident_as_function(ty: &Type) -> Option<Ident> {
    todo!()
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None, None, false);
        let cxx_sig = cxx_fn.as_cxx_sig(&IndexSet::new()).to_token_stream();

        assert_eq!(
            format!("{cxx_sig}"),
            "fn get_ident_as_function_ffi (ty : & Type) -> Result < Ident >"
        )
    }

    #[test]
    fn test_cxx_sig_enum() {
        let item_fn = syn::parse_str(
            r#"pub fn foo(&self, x: usize) -> usize {
    self.bar + x
    }"#,
        )
        .unwrap();
        let enum_name = "Person";
        let enum_ = gen_enum(enum_name);
        let cxx_fn = CxxFn::new(item_fn, Some(gen_ty(enum_name)), None, true);
        let cxx_sig = cxx_fn.as_cxx_sig(&[enum_].into()).to_token_stream();

        assert_eq!(
            format!("{cxx_sig}"),
            "fn person_foo (self_ : & PersonFfi , x : usize) -> usize"
        )
    }

    #[test]
    fn test_cxx_impl() {
        let item_fn = syn::parse_str(
            r#"pub fn foo(u: usize) -> usize {
    u+1
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None, None, false);
        let cxx_impl = cxx_fn.as_cxx_impl(&IndexSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn foo_ffi(u: usize) -> usize {
    let res = foo(u.into());
    res.into()
}
"
        )
    }

    #[test]
    fn test_cxx_impl_unsafe() {
        let item_fn = syn::parse_str(
            r#"pub unsafe fn foo(u: usize) -> usize {
    u+1
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None, None, false);
        let cxx_impl = cxx_fn.as_cxx_impl(&IndexSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn unsafe_foo_ffi(u: usize) -> usize {
    let res = unsafe { foo(u.into()) };
    res.into()
}
"
        )
    }

    #[test]
    fn test_cxx_impl_with_module() {
        let item_fn = syn::parse_str(
            r#"pub fn foo(u: usize) -> usize {
    u+1
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None, Some(format_ident!("foo")), false);
        let cxx_impl = cxx_fn.as_cxx_impl(&IndexSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn foo_ffi(u: usize) -> usize {
    let res = foo::foo(u.into());
    res.into()
}
"
        )
    }

    #[test]
    fn test_cxx_impl_return_ref() {
        let item_fn = syn::parse_str(
            r#"pub fn foo(p: Person) -> &str {
    p.name
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None, None, false);
        let cxx_impl = cxx_fn.as_cxx_impl(&[gen_struct("Person")].into());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn foo_ffi(p: PersonFfi) -> &str {
    p.name
}
"
        )
    }

    #[test]
    fn test_cxx_impl_ref_self_method() {
        let item_fn = syn::parse_str(
            r#"pub fn is_adult(&self) -> bool {
        self.age >= 18
    }"#,
        )
        .unwrap();
        let ty: TypeTest = syn::parse_str("bar::Bar").unwrap();
        let cxx_fn = CxxFn::new(item_fn, Some(ty.0), None, false);
        let cxx_impl = cxx_fn.as_cxx_impl(&IndexSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn is_adult(&self) -> bool {
    let res = bar::Bar::is_adult(&<bar::Bar>::from(self.clone()));
    res.into()
}
"
        )
    }

    #[test]
    fn test_cxx_impl_enum_ref_self_method() {
        let item_fn = syn::parse_str(
            r#"pub fn is_adult(&self) -> bool {
        self.age >= 18
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, Some(gen_ty("bar::Bar")), None, true);
        let cxx_impl = cxx_fn.as_cxx_impl(&IndexSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn bar_bar_is_adult(self_: &bar::Bar) -> bool {
    let res = bar::Bar::is_adult(&<bar::Bar>::from(self_.clone()));
    res.into()
}
"
        )
    }

    #[test]
    fn test_gather_associated_functions() {
        let parsed_file = syn::parse_str(
            r#"impl Foo {
                pub fn new(x: usize) -> Foo {
                    Self {x}
                }
            }
    "#,
        )
        .unwrap();
        let mut gather_sig = GatherSignatures::new([gen_struct("Foo")].into(), None);
        gather_sig.visit_file(&parsed_file);
        let mut cxx_sig = TokenStream::new();
        for (ty_opt, vec_fn) in gather_sig.cxx_fn_buf.iter() {
            assert!(ty_opt.is_none());
            assert_eq!(vec_fn.len(), 1);
            vec_fn[0]
                .as_cxx_sig(&IndexSet::new())
                .to_tokens(&mut cxx_sig);
        }

        assert_eq!(format!("{cxx_sig}"), "fn foo_new (x : usize) -> Foo")
    }

    #[test]
    fn test_gather_no_fn_with_tupless() {
        let parsed_file = syn::parse_str(
            r#"impl Foo {
                    pub fn name_and_age(self) -> (String, Age) {
                        (self.name, self.age)
                    }
                    pub fn new_from_tuple((age, name): (u8, String)) -> Person {
                        Self {
                            age: Age(age),
                            name,
                        }
                    }
            }"#,
        )
        .unwrap();
        let mut gather_sig = GatherSignatures::new([gen_struct("Foo")].into(), None);
        gather_sig.visit_file(&parsed_file);
        assert!(gather_sig.cxx_fn_buf.is_empty())
    }

    #[test]
    fn test_gather_in_pub_crate() {
        let parsed_file = syn::parse_str(
            r#"pub(crate) struct Foo { x: usize }
            impl Foo {
                pub fn new(x: usize) -> Foo {
                    Self {x}
                }
            }
    "#,
        )
        .unwrap();
        // no ident since the struct is not public
        let mut gather_sig = GatherSignatures::new(IndexSet::new(), None);
        gather_sig.visit_file(&parsed_file);
        let mut cxx_sig = TokenStream::new();
        for (ty_opt, vec_fn) in gather_sig.cxx_fn_buf.iter() {
            assert!(ty_opt.is_none());
            assert_eq!(vec_fn.len(), 1);
            vec_fn[0]
                .as_cxx_sig(&IndexSet::new())
                .to_tokens(&mut cxx_sig);
        }

        assert_eq!(format!("{cxx_sig}"), "")
    }

    #[test]
    fn test_not_gather_generic_fn() {
        let parsed_file = syn::parse_str(
            r#"impl Foo {
                pub fn new<T: Into<usize>>(x: T) -> Foo {
                    Self {x: x.into()}
                }
            }
    "#,
        )
        .unwrap();
        let mut gather_sig = GatherSignatures::new([gen_struct("Foo")].into(), None);
        gather_sig.visit_file(&parsed_file);
        let mut cxx_sig = TokenStream::new();
        for (ty_opt, vec_fn) in gather_sig.cxx_fn_buf.iter() {
            assert!(ty_opt.is_none());
            assert_eq!(vec_fn.len(), 1);
            vec_fn[0]
                .as_cxx_sig(&IndexSet::new())
                .to_tokens(&mut cxx_sig);
        }

        assert_eq!(format!("{cxx_sig}"), "")
    }

    #[test]
    fn test_cxx_impl_value_self_method() {
        let item_fn = syn::parse_str(
            r#"pub fn name(self) -> [char; 5] {
        self.name
    }"#,
        )
        .unwrap();
        let ty: TypeTest = syn::parse_str("bar::Bar").unwrap();
        let cxx_fn = CxxFn::new(item_fn, Some(ty.0), None, false);
        let cxx_impl = cxx_fn.as_cxx_impl(&IndexSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn bar_bar_name(self_: bar::Bar) -> [char; 5] {
    let res = bar::Bar::name(self_.into());
    res.into()
}
"
        )
    }

    #[test]
    fn test_cxx_impl_valuemut_self_method() {
        let item_fn = syn::parse_str(
            r#"pub fn bday_value(mut self) -> Self {
        self.age += 1;
        self
    }"#,
        )
        .unwrap();
        let ty: TypeTest = syn::parse_str("bar::Bar").unwrap();
        let cxx_fn = CxxFn::new(item_fn, Some(ty.0), None, false);
        let cxx_impl = cxx_fn.as_cxx_impl(&IndexSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn bar_bar_bday_value(mut self_: bar::Bar) -> Self {
    let mut x = <bar::Bar>::from(self);
    let res = bar::Bar::bday_value(x);
    let res = Self::from(x);
    res.into()
}
"
        )
    }

    #[test]
    fn test_cxx_impl_refmut_self_method() {
        let item_fn = syn::parse_str(
            r#"pub fn bday(&mut self) {
        self.age += 1
    }"#,
        )
        .unwrap();
        let ty: TypeTest = syn::parse_str("bar::Bar").unwrap();
        let cxx_fn = CxxFn::new(item_fn, Some(ty.0), None, false);
        let cxx_impl = cxx_fn.as_cxx_impl(&IndexSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn bday(&mut self) {
    let mut x = <bar::Bar>::from(self.clone());
    let res = bar::Bar::bday(&mut x);
    *self = Self::from(x);
    res.into()
}
"
        )
    }
}
