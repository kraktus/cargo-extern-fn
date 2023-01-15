use std::collections::{HashMap, HashSet};

use std::fs::{DirEntry, File, OpenOptions};
use std::io::{Read, Write};
use std::path::Path;

use log::trace;
use proc_macro2::{Span, TokenStream};

use syn::punctuated::Punctuated;
use syn::visit::{self, Visit};
use syn::visit_mut::VisitMut;
use syn::{
    parse_quote, Fields, FieldsNamed, Ident, ImplItemMethod, Index, Item, ItemFn, ItemImpl,
    ReturnType, Token, Type,
};
use syn::{ItemEnum, ItemStruct, Visibility};

use quote::{format_ident, quote, ToTokens};

use crate::utils::{
    attrs, get_ident, get_ident_as_function, is_method, meta_is_extern_fn_skip, method_self_type,
    normalise_receiver_arg, return_contains_ref, AddSuffix, SelfType,
};
use crate::utils::{call_function_from_sig, is_type};

#[derive(Default, Debug)]
pub struct Cxx {
    all_cxx_fn: HashMap<Option<Type>, Vec<CxxFn>>,
    all_cxx_struct_or_enum: Vec<StructOrEnum>, // TODO maybe switch to derive
    all_cxx_idents: HashSet<Ident>,
}

#[derive(Debug, Clone)]
enum StructOrEnum {
    S(ItemStruct),
    E(ItemEnum),
}

impl ToTokens for StructOrEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match &self {
            StructOrEnum::S(x) => x.to_tokens(tokens),
            StructOrEnum::E(x) => x.to_tokens(tokens),
        }
    }
}

impl StructOrEnum {
    fn ident(&self) -> &Ident {
        match &self {
            StructOrEnum::S(x) => &x.ident,
            StructOrEnum::E(x) => &x.ident,
        }
    }

    fn ident_mut(&mut self) -> &mut Ident {
        match self {
            StructOrEnum::S(x) => &mut x.ident,
            StructOrEnum::E(x) => &mut x.ident,
        }
    }

    fn visit_mut<V: VisitMut>(&mut self, visitor: &mut V) {
        match self {
            StructOrEnum::S(x) => syn::visit_mut::visit_item_struct_mut(visitor, x),
            StructOrEnum::E(x) => syn::visit_mut::visit_item_enum_mut(visitor, x),
        }
    }

    fn as_ffi(&self, idents_to_add: &HashSet<Ident>) -> StructOrEnum {
        let mut ffi = match &self {
            StructOrEnum::S(_x) => Self::S(self.as_x_struct("Ffi").expect("We know it's a struct")),
            StructOrEnum::E(x) => {
                let mut enum_ffi = x.clone();
                enum_ffi.ident = format_ident!("{}Ffi", enum_ffi.ident);
                Self::E(enum_ffi)
            }
        };
        let mut visitor = AddSuffix::new("Ffi", idents_to_add);
        ffi.visit_mut(&mut visitor);
        ffi
    }

    fn as_x_struct(&self, suffix: &str) -> Option<ItemStruct> {
        match self {
            StructOrEnum::S(struct_) => {
                trace!("Creating raw struct of {}", struct_.ident);
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
            StructOrEnum::E(_) => None,
        }
    }

    fn original_and_ffi(&self, idents_to_add: &HashSet<Ident>) -> (Self, Self) {
        (self.clone(), self.as_ffi(idents_to_add))
    }
}

#[derive(Debug, Clone, Default)]
struct GatherDataStructures {
    pub_struct_or_enum: Vec<StructOrEnum>,
    idents: HashSet<Ident>, // idents of struct/enums. TODO union?
}

impl GatherDataStructures {
    fn results(self) -> (Vec<StructOrEnum>, HashSet<Ident>) {
        let Self {
            pub_struct_or_enum,
            idents,
        } = self;
        (pub_struct_or_enum, idents)
    }
}

impl<'ast> Visit<'ast> for GatherDataStructures {
    fn visit_item_struct(&mut self, struct_: &'ast ItemStruct) {
        if matches!(struct_.vis, Visibility::Public(_)) && struct_.generics.params.is_empty()
        // generics not handled by cxx
        {
            self.pub_struct_or_enum
                .push(StructOrEnum::S(struct_.clone()));
            self.idents.insert(struct_.ident.clone());
        }
        visit::visit_item_struct(self, struct_);
    }

    fn visit_item_enum(&mut self, enum_: &'ast ItemEnum) {
        if matches!(enum_.vis, Visibility::Public(_))
            && enum_.generics.params.is_empty()
            && enum_
                .variants
                .iter()
                .all(|v| matches!(v.fields, Fields::Unit))
        // generics not handled by cxx
        {
            self.idents.insert(enum_.ident.clone());
            self.pub_struct_or_enum.push(StructOrEnum::E(enum_.clone()))
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
    allowed_idents: HashSet<Ident>,
    cxx_fn_buf: HashMap<Option<Type>, Vec<CxxFn>>,
}

#[derive(Debug, Clone)]
struct CxxFn {
    pub item_fn: ItemFn,
    ty: Option<Type>, // set on methods and associated functions
    is_associated: bool,
    pub return_is_opt: bool, // cxx does not handle `Option`... convert to `Result`
}

impl CxxFn {
    fn new(item_fn: ItemFn, ty: Option<Type>) -> Self {
        let return_is_opt = if let ReturnType::Type(_, ty) = item_fn.sig.output.clone() {
            is_type("Option", &ty)
        } else {
            false
        };
        Self {
            is_associated: ty.is_some() && !is_method(&item_fn.sig),
            item_fn,
            return_is_opt,
            ty,
        }
    }

    fn as_cxx_sig(&self, idents_to_add: &HashSet<Ident>) -> TokenStream {
        let mut cxx_sig = self.item_fn.sig.clone();
        if self.is_associated {
            cxx_sig.ident = format_ident!(
                "{}{}",
                self.ty
                    .as_ref()
                    .and_then(get_ident_as_function)
                    .expect("type with ident"),
                cxx_sig.ident
            );
        }
        for arg in cxx_sig.inputs.iter_mut() {
            let ffi_ident = self
                .ty
                .as_ref()
                .and_then(get_ident)
                .map(|ty_ident| format_ident!("{ty_ident}Ffi"));
            if let Some(normalised_arg) = normalise_receiver_arg(arg, ffi_ident, None) {
                *arg = normalised_arg;
            }
        }
        if self.return_is_opt {
            if let ReturnType::Type(_, ref mut ty) = cxx_sig.output {
                if let Type::Path(ref mut p) = **ty {
                    p.path.segments[0].ident = syn::parse_str("Result").unwrap();
                }
            }
        }
        let mut visitor = AddSuffix::new("Ffi", idents_to_add);
        visitor.visit_signature_mut(&mut cxx_sig);
        quote!(#cxx_sig;)
    }

    // TokenStream of an ItemFn
    fn as_cxx_impl(&self, idents_to_be_ffied: &HashSet<Ident>) -> TokenStream {
        let mut cxx_item = self.item_fn.clone();
        if self.is_associated {
            cxx_item.sig.ident = format_ident!(
                "{}{}",
                self.ty
                    .as_ref()
                    .and_then(get_ident_as_function)
                    .expect("type with ident"),
                cxx_item.sig.ident
            );
        }
        // convert all arguments to their Ffi version if needed
        let mut visitor = AddSuffix::new("Ffi", idents_to_be_ffied);
        visitor.visit_signature_mut(&mut cxx_item.sig);

        if self.return_is_opt {
            if let ReturnType::Type(_, ref mut ty) = cxx_item.sig.output {
                if let Type::Path(ref mut p) = **ty {
                    p.path.segments[0].ident = format_ident!("ResultFfi");
                }
            }
        }

        // if the function returns a reference, bail out and keep the original function body
        // which is usually just reading a field
        if return_contains_ref(&cxx_item.sig.output) {
            return quote!(#cxx_item);
        }

        let self_type_opt = cxx_item.sig.inputs.iter().find_map(method_self_type);
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
            match self_type {
                SelfType::Value => {
                    call_function_from_sig(self.ty.as_ref(), &self.item_fn.sig, quote!(self.into()))
                }
                SelfType::ValueMut => {
                    call_function_from_sig(self.ty.as_ref(), &self.item_fn.sig, quote!(x))
                }
                SelfType::Ref => call_function_from_sig(
                    self.ty.as_ref(),
                    &cxx_item.sig,
                    quote!(&<#ty_>::from(self.clone())),
                ),
                SelfType::RefMut => {
                    call_function_from_sig(self.ty.as_ref(), &self.item_fn.sig, quote!(&mut x))
                }
            }
        } else {
            call_function_from_sig(self.ty.as_ref(), &self.item_fn.sig, quote!())
        };
        if self.return_is_opt {
            call_fn = quote!(#call_fn.map(::std::convert::Into::into).ok_or(()));
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
    fn new(allowed_idents: HashSet<Ident>) -> Self {
        Self {
            current_impl_ty: None,
            allowed_idents,
            cxx_fn_buf: HashMap::new(),
        }
    }

    fn handle_item_fn(
        &mut self,
        item_fn: &ItemFn,
        is_associated: bool, // only set when handling associated functions
    ) {
        if item_fn.sig.asyncness.is_none()
            && item_fn.sig.abi.is_none()
            && matches!(item_fn.vis, Visibility::Public(_))
            // do not handle function with `cfg` attributes for the moment
            && item_fn.attrs.iter().all(|a| !a.path.is_ident("cfg") && !meta_is_extern_fn_skip(a.parse_meta()))
        {
            trace!("handling fn {:?}", item_fn.sig.ident);
            self.cxx_fn_buf
                .entry(if is_associated {
                    None // associated functions are converted into free functions
                } else {
                    self.current_impl_ty.clone()
                })
                .or_default()
                .push(CxxFn::new(item_fn.clone(), self.current_impl_ty.clone()));
        }
    }
}

impl<'ast> Visit<'ast> for GatherSignatures {
    fn visit_item_impl(&mut self, item_impl: &'ast ItemImpl) {
        if item_impl.trait_.is_none()
            && get_ident(&item_impl.self_ty)
                .map_or(false, |ident| self.allowed_idents.contains(&ident))
        {
            self.current_impl_ty = Some(*item_impl.self_ty.clone());
            trace!("Looking at impl of {:?}", get_ident(&item_impl.self_ty))
        } else {
            self.current_impl_ty = None;
        }
        visit::visit_item_impl(self, item_impl);
        self.current_impl_ty = None;
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

    fn visit_item_fn(&mut self, item_fn: &'ast ItemFn) {
        self.handle_item_fn(item_fn, false);
        visit::visit_item_fn(self, item_fn);
    }

    fn visit_impl_item_method(&mut self, item_method: &'ast ImplItemMethod) {
        let item_fn: ItemFn =
            syn::parse2(item_method.to_token_stream()).expect("from method to bare fn failed");
        let is_associated = !is_method(&item_fn.sig);
        self.handle_item_fn(&item_fn, is_associated);

        visit::visit_impl_item_method(self, item_method);
    }
}

fn impl_from_x_to_y(x: &ItemStruct, y: &ItemStruct) -> TokenStream {
    let x_ident = &x.ident;
    let is_x_unnamed = matches!(&x.fields, Fields::Unnamed(_));
    let y_ident = &y.ident;
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
        Fields::Unit => unimplemented!("TBD"),
    };
    quote!(impl From<#x_ident> for #y_ident {
        fn from(x: #x_ident) -> Self {
            Self #body.into()
        }
    })
}

fn impl_from_x_to_y_enum(x: &ItemEnum, y: &ItemEnum) -> TokenStream {
    let x_ident = &x.ident;
    let y_ident = &y.ident;
    // We assume the enum has only unit fields
    let body = x
        .variants
        .iter()
        .map(|v| quote!(<#x_ident>::#v => Self::#v));

    quote!(impl From<#x_ident> for #y_ident {
        fn from(x: #x_ident) -> Self {
                match x {
                    #(#body),*
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
                    .map(|cxx_fn| cxx_fn.as_cxx_sig(&self.all_cxx_idents))
            })
            .collect();
        quote!(#(#cxx_sig)*)
    }

    fn generate_ffi_conversions(&self, struct_or_enum: Vec<StructOrEnum>) -> TokenStream {
        let mut buf = TokenStream::new();
        for (original, ffi) in struct_or_enum
            .iter()
            .map(|x| x.original_and_ffi(&self.all_cxx_idents))
        {
            trace!("Generating conversion impl of {}", ffi.ident());
            match (&original, &ffi) {
                (StructOrEnum::S(a), StructOrEnum::S(b)) => {
                    let back = impl_from_x_to_y(a, b);
                    let forth = impl_from_x_to_y(b, a);
                    quote!(#back #forth)
                }
                (StructOrEnum::E(a), StructOrEnum::E(b)) => {
                    let back = impl_from_x_to_y_enum(a, b);
                    let forth = impl_from_x_to_y_enum(b, a);
                    quote!(#back #forth)
                }
                _ => unreachable!("Impossible to have a struct turned into enum or vice-versa"),
            }
            .to_tokens(&mut buf);
            trace!("Finished conversion impl of {}", ffi.ident());
        }
        quote!(
            /// Auto-generated code with `cargo-extern-fn`
            mod ffi_conversion {
            use crate::ffi::*;
            #buf
        })
    }

    fn generate_ffi_impl(&self) -> TokenStream {
        let mut buf = TokenStream::new();
        for (ty_opt, vec_fn) in self.all_cxx_fn.iter() {
            let fn_impls = vec_fn.iter().map(|f| f.as_cxx_impl(&self.all_cxx_idents));

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
    pub fn gather_data_struct_and_sign(&mut self, parsed_file: &syn::File) {
        trace!("Starting GatherDataStructures pass");
        let mut gather_ds = GatherDataStructures::default();
        gather_ds.visit_file(parsed_file);
        trace!("Finished GatherDataStructures pass");
        let (pub_struct_or_enum, idents) = gather_ds.results();
        trace!("Starting GatherSignatures pass");
        let mut gather_sig = GatherSignatures::new(idents);
        gather_sig.visit_file(parsed_file);
        trace!("Finished GatherSignatures pass");
        self.all_cxx_fn.extend(gather_sig.cxx_fn_buf);
        self.all_cxx_idents.extend(gather_sig.allowed_idents);
        self.all_cxx_struct_or_enum.extend(pub_struct_or_enum);
    }

    // once all files have been parsed once, add the conversion between
    // the ffi data_struct and original struct
    pub fn ffi_conversion(&self, parsed_file: &syn::File) -> TokenStream {
        // need to get the idents declared in this file
        let mut gather_ds = GatherDataStructures::default();
        gather_ds.visit_file(parsed_file);
        let (struct_or_enum, _) = gather_ds.results();
        self.generate_ffi_conversions(struct_or_enum)
    }

    pub fn generate_ffi_bridge_and_impl(
        &self,
        code_dir: &Path,
        dry: bool,
        _entries: impl Iterator<Item = DirEntry>,
    ) {
        let mut file = File::open(code_dir.join("lib.rs"))
            .or_else(|_| {
                OpenOptions::new()
                    .create(true)
                    .append(true)
                    .read(true)
                    .open(code_dir.join("main.rs"))
            })
            .expect("reading lib.rs and main.rs in src_dir failed");
        let mut src_file = String::new();
        file.read_to_string(&mut src_file)
            .expect("Unable to read file");
        let cxx_struct_declarations = self.generate_cxx_types();
        let cxx_sig = self.generate_cxx_signatures();
        let ffi_impl = self.generate_ffi_impl();
        let parsed_file_formated = prettyplease::unparse(&parse_quote!(
            /// Auto-generated code with `cargo-extern-fn`
            #[cxx::bridge]
            pub mod ffi {
                #cxx_struct_declarations

                    extern "Rust" {
                        #cxx_sig
                    }
            }
            use ffi::*;
            type ResultFfi<T> = Result<T, ()>;

            #ffi_impl
        ));
        if dry {
            println!("\n{parsed_file_formated}")
        } else {
            file.write_all(parsed_file_formated.as_bytes())
                .expect("final writing of cxx::bridge failed")
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::utils::TypeTest;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_ffi_struct_conversions_with_struc() {
        let file: syn::File = syn::parse_str(r#"pub struct Foo(usize, u64, u8);"#).unwrap();
        let cxx = Cxx::default();
        let conv = cxx.ffi_conversion(&file);
        assert_eq!(
            prettyplease::unparse(&parse_quote!(#conv)),
            "/// Auto-generated code with `cargo-extern-fn`
mod ffi_conversion {
    use crate::ffi::*;
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
}
"
        )
    }


    #[test]
    fn test_ffi_struct_conversions_with_2struc() {
        let file: syn::File = syn::parse_str(r#"pub struct Foo(usize, u64, u8); pub struct Bar {x: usize, y: u8}"#).unwrap();
        let cxx = Cxx::default();
        let conv = cxx.ffi_conversion(&file);
        assert_eq!(
            prettyplease::unparse(&parse_quote!(#conv)),
            "/// Auto-generated code with `cargo-extern-fn`
mod ffi_conversion {
    use crate::ffi::*;
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
        let cxx_fn = CxxFn::new(item_fn, None);
        let cxx_sig = cxx_fn.as_cxx_sig(&HashSet::new());

        assert_eq!(
            format!("{cxx_sig}"),
            "fn get_ident_as_function (ty : & Type) -> Ident ;"
        )
    }

    #[test]
    fn test_cxx_sig_with_ffi() {
        let item_fn = syn::parse_str(
            r#"pub fn add_ffi_to_types(foo: &Foo) -> Vec<Bar> {
    todo!()
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None);
        let cxx_sig = cxx_fn.as_cxx_sig(&[format_ident!("Foo"), format_ident!("Bar")].into());

        assert_eq!(
            format!("{cxx_sig}"),
            "fn add_ffi_to_types (foo : & FooFfi) -> Vec < BarFfi > ;"
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
        let cxx_fn = CxxFn::new(item_fn, None);
        let cxx_sig = cxx_fn.as_cxx_sig(&HashSet::new());

        assert_eq!(
            format!("{cxx_sig}"),
            "fn get_ident_as_function (ty : & Type) -> Result < Ident > ;"
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
        let cxx_fn = CxxFn::new(item_fn, None);
        let cxx_impl = cxx_fn.as_cxx_impl(&HashSet::new());

        assert_eq!(
            format!("{cxx_impl}"),
            "pub fn foo (u : usize) -> usize { let res = foo (u) ; res . into () }"
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
        let cxx_fn = CxxFn::new(item_fn, None);
        let cxx_impl = cxx_fn.as_cxx_impl(&[format_ident!("Person")].into());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn foo(p: PersonFfi) -> &str {
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
        let cxx_fn = CxxFn::new(item_fn, Some(ty.0));
        let cxx_impl = cxx_fn.as_cxx_impl(&HashSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn is_adult(&self) -> bool {
    let res = <bar::Bar>::is_adult(&<bar::Bar>::from(self.clone()));
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
        let mut gather_sig = GatherSignatures::new([format_ident!("Foo")].into());
        gather_sig.visit_file(&parsed_file);
        let mut cxx_sig = TokenStream::new();
        for (ty_opt, vec_fn) in gather_sig.cxx_fn_buf.iter() {
            assert!(ty_opt.is_none());
            assert_eq!(vec_fn.len(), 1);
            vec_fn[0]
                .as_cxx_sig(&HashSet::new())
                .to_tokens(&mut cxx_sig);
        }

        assert_eq!(format!("{cxx_sig}"), "fn foo_new (x : usize) -> Foo ;")
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
        let cxx_fn = CxxFn::new(item_fn, Some(ty.0));
        let cxx_impl = cxx_fn.as_cxx_impl(&HashSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn name(self) -> [char; 5] {
    let res = <bar::Bar>::name(self.into());
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
        let cxx_fn = CxxFn::new(item_fn, Some(ty.0));
        let cxx_impl = cxx_fn.as_cxx_impl(&HashSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn bday_value(mut self) -> Self {
    let mut x = <bar::Bar>::from(self);
    let res = <bar::Bar>::bday_value(x);
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
        let cxx_fn = CxxFn::new(item_fn, Some(ty.0));
        let cxx_impl = cxx_fn.as_cxx_impl(&HashSet::new());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn bday(&mut self) {
    let mut x = <bar::Bar>::from(self.clone());
    let res = <bar::Bar>::bday(&mut x);
    *self = Self::from(x);
    res.into()
}
"
        )
    }
}
