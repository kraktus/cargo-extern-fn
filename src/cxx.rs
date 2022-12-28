use std::collections::{HashMap, HashSet};

use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::path::Path;

use log::trace;
use proc_macro2::{Span, TokenStream};

use syn::punctuated::Punctuated;
use syn::visit::{self, Visit};
use syn::{
    parse_quote, Fields, FieldsNamed, Ident, ImplItemMethod, Index, Item, ItemFn, ItemImpl,
    ReturnType, Token, Type,
};
use syn::{ItemEnum, ItemStruct, Visibility};

use quote::{format_ident, quote, ToTokens};

use crate::utils::{
    attrs, get_ident, get_ident_as_function, meta_is_extern_fn_skip, method_self_type,
    normalise_receiver_arg, SelfType,
};
use crate::utils::{call_function_from_sig, is_type};

#[derive(Default, Debug)]
pub struct Cxx {
    all_cxx_fn: HashMap<Option<Type>, Vec<CxxFn>>,
    all_cxx_struct_or_enum: Vec<StructOrEnum>, // TODO maybe switch to derive
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

    fn as_raw_struct(&self) -> Option<ItemStruct> {
        match self {
            StructOrEnum::S(struct_) => {
                trace!("Creating raw struct of {}", struct_.ident);
                let mut raw_struct = struct_.clone();
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
}

#[derive(Debug, Clone, Default)]
struct CreateRawStruct {
    pub_struct_or_enum: Vec<StructOrEnum>,
    idents: HashSet<Ident>, // idents of struct/enums. TODO union?
}

impl CreateRawStruct {
    fn results(self) -> (Vec<StructOrEnum>, HashSet<Ident>) {
        let Self {
            pub_struct_or_enum,
            idents,
        } = self;
        (pub_struct_or_enum, idents)
    }
}

impl<'ast> Visit<'ast> for CreateRawStruct {
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
    ty: Option<Type>,
    pub return_is_opt: bool, // cxx does not handle `Option`... convert to `Result`
}

impl CxxFn {
    fn new(item_fn: ItemFn, ty: Option<Type>) -> Self {
        let return_is_opt = if let ReturnType::Type(_, ty) = item_fn.sig.output.clone() {
            is_type("Option", &*ty)
        } else {
            false
        };
        Self {
            item_fn,
            return_is_opt,
            ty,
        }
    }

    fn as_cxx_sig(&self) -> TokenStream {
        let mut cxx_sig = self.item_fn.sig.clone();
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
        quote!(#cxx_sig;)
    }

    // TokenStream of an ItemFn
    fn as_cxx_impl(&self) -> TokenStream {
        let mut cxx_item = self.item_fn.clone();
        if self.return_is_opt {
            if let ReturnType::Type(_, ref mut ty) = cxx_item.sig.output {
                if let Type::Path(ref mut p) = **ty {
                    p.path.segments[0].ident = syn::parse_str("ResultFfi").unwrap();
                }
            }
        }
        let self_type_opt = cxx_item.sig.inputs.iter().find_map(method_self_type);
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
                    call_function_from_sig(self.ty.as_ref(), &cxx_item.sig, quote!(self.into()))
                }
                SelfType::ValueMut => {
                    call_function_from_sig(self.ty.as_ref(), &cxx_item.sig, quote!(x))
                }
                SelfType::Ref => call_function_from_sig(
                    self.ty.as_ref(),
                    &cxx_item.sig,
                    quote!(<#ty_>::from(self.clone())),
                ),
                SelfType::RefMut => {
                    call_function_from_sig(self.ty.as_ref(), &cxx_item.sig, quote!(&mut x))
                }
            }
        } else {
            call_function_from_sig(self.ty.as_ref(), &cxx_item.sig, quote!())
        };
        if self.return_is_opt {
            call_fn = quote!(#call_fn.map(::std::convert::Into::into).ok_or(()));
        };

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
            res});
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
        prefix_opt: Option<Ident>, // only set when handling associated functions
    ) {
        if item_fn.sig.asyncness.is_none()
            && item_fn.sig.abi.is_none()
            && matches!(item_fn.vis, Visibility::Public(_))
            // do not handle function with `cfg` attributes for the moment
            && item_fn.attrs.iter().all(|a| !a.path.is_ident("cfg") && !meta_is_extern_fn_skip(a.parse_meta()))
        {
            let mut extern_fn = item_fn.clone();
            trace!("handling fn {:?}", extern_fn.sig.ident);
            if let Some(prefix) = prefix_opt.as_ref() {
                extern_fn.sig.ident = format_ident!("{}{}", prefix, extern_fn.sig.ident);
            }

            self.cxx_fn_buf
                .entry(if prefix_opt.is_some() {
                    None // static method are converted into free functions
                } else {
                    self.current_impl_ty.clone()
                })
                .or_default()
                .push(CxxFn::new(extern_fn, self.current_impl_ty.clone()));
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
            trace!("Looking at impl of {:?}", get_ident(&*item_impl.self_ty))
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
        self.handle_item_fn(
            item_fn,
            self.current_impl_ty
                .clone()
                .and_then(|ty| get_ident_as_function(&ty)),
        );
        visit::visit_item_fn(self, item_fn);
    }

    fn visit_impl_item_method(&mut self, item_method: &'ast ImplItemMethod) {
        let item_fn: ItemFn =
            syn::parse2(item_method.to_token_stream()).expect("from method to bare fn failed");
        self.handle_item_fn(&item_fn, None);
        visit::visit_impl_item_method(self, item_method);
    }
}

fn impl_from_x_to_y(x: &ItemStruct, y: &ItemStruct) -> TokenStream {
    let x_ident = &x.ident;
    let is_x_unnamed = matches!(&x.fields, Fields::Unnamed(_));

    let y_ident = &y.ident;
    let is_y_named = matches!(&y.fields, Fields::Named(_));
    let body = match &y.fields {
        Fields::Named(nameds) => {
            let named_token = nameds.named.iter().enumerate().map(|(i, f)| {
                let ident = f.ident.as_ref().expect("named field");
                let unnamed_ident = Index::from(i);
                if is_x_unnamed {
                    quote!(#ident: x.#unnamed_ident)
                } else {
                    quote!(#ident: x.#ident)
                }
            });
            quote!({#(#named_token),*})
        }
        Fields::Unnamed(unnameds) => {
            let unnamed_token = (0..unnameds.unnamed.len())
                .map(Index::from)
                .map(|i| // hardcode the fact that named fields are of the form nX, for named-struct -> unnamed struct conversions
                    if is_y_named {
                    quote!(x.n #i)
                } else {
                    quote!(x.#i)
                });
            quote!((#(#unnamed_token),*))
        }
        Fields::Unit => unimplemented!("TBD"),
    };
    quote!(impl From<#x_ident> for #y_ident {
        fn from(x: #x_ident) -> Self {
            Self #body
        }
    })
}

impl Cxx {
    fn generate_cxx_types(&self) -> TokenStream {
        trace!("Generating FFI struct declarations");
        let mut buf = TokenStream::new();
        for struct_or_enum in self.all_cxx_struct_or_enum.iter() {
            let mut ffi_struct = struct_or_enum.clone();
            let ident_ffi: Ident = format_ident!("{}Ffi", struct_or_enum.ident());
            *ffi_struct.ident_mut() = ident_ffi.clone();
            ffi_struct.to_tokens(&mut buf);
        }
        trace!("Finished generating FFI struct declarations");
        buf
    }

    fn generate_cxx_signatures(&self) -> TokenStream {
        let cxx_sig: Vec<TokenStream> = self
            .all_cxx_fn
            .iter()
            .flat_map(|(_, vec_cxx_fn)| vec_cxx_fn.iter().map(|cxx_fn| cxx_fn.as_cxx_sig()))
            .collect();
        quote!(#(#cxx_sig)*)
    }

    fn generate_conversions(pub_struct_or_enum: &[StructOrEnum], buf: &mut TokenStream) {
        for struct_or_enum in pub_struct_or_enum.iter() {
            if let StructOrEnum::S(pub_struct) = struct_or_enum {
                let mut raw_struct = pub_struct.clone();
                let ident_raw: Ident = format_ident!("{}Raw", pub_struct.ident);
                raw_struct.ident = ident_raw.clone();
                raw_struct.to_tokens(buf);
                trace!("Generating conversion impl of {ident_raw}");
                impl_from_x_to_y(&pub_struct, &raw_struct).to_tokens(buf);
                impl_from_x_to_y(&raw_struct, &pub_struct).to_tokens(buf);
                trace!("Finished conversion impl of {ident_raw}");
            }
        }
    }

    pub fn handle_file(&mut self, parsed_file: &syn::File) -> TokenStream {
        trace!("Starting CreateRawStruct pass");
        let mut create_raw_struct = CreateRawStruct::default();
        create_raw_struct.visit_file(parsed_file);
        trace!("Finished CreateRawStruct pass");
        let (pub_struct_or_enum, idents) = create_raw_struct.results();
        trace!("Starting GatherSignatures pass");
        let mut gather_sig = GatherSignatures::new(idents);
        gather_sig.visit_file(parsed_file);
        trace!("Finished GatherSignatures pass");
        self.all_cxx_fn = gather_sig.cxx_fn_buf;
        let mut parsed_file_tokens = quote!(#parsed_file);
        Self::generate_conversions(&pub_struct_or_enum, &mut parsed_file_tokens);
        self.all_cxx_struct_or_enum.extend(pub_struct_or_enum);
        parsed_file_tokens
    }

    pub fn generate_ffi_bridge_and_impl(self, code_dir: &Path) {
        let mut file = File::open(code_dir.join("lib.rs"))
            .or_else(|_| {
                OpenOptions::new()
                    .create(true)
                    .write(true)
                    .read(true)
                    .open(code_dir.join("main.rs"))
            })
            .expect("reading lib.rs and main.rs in src_dir failed");
        let mut src_file = String::new();
        file.read_to_string(&mut src_file)
            .expect("Unable to read file");
        let parsed_file = syn::parse_file(&src_file).expect("Unable to parse file");
        let cxx_struct_declarations = self.generate_cxx_types();
        let cxx_sig = self.generate_cxx_signatures();
        let parsed_file_formated = prettyplease::unparse(&parse_quote!(
            #parsed_file

            #[cxx::bridge]
            pub mod ffi {
                #cxx_struct_declarations

                    extern "Rust" {
                        #cxx_sig
                    }
            }
        ));
        file.write_all(parsed_file_formated.as_bytes())
            .expect("final writing of cxx::bridge failed")
    }
}

#[cfg(test)]
mod tests {

    use crate::utils::TypeTest;

    use super::*;

    #[test]
    fn test_cxx_sig() {
        let item_fn = syn::parse_str(
            r#"pub fn get_ident_as_function(ty: &Type) -> Ident{
    todo!()
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None);
        let cxx_sig = cxx_fn.as_cxx_sig();

        assert_eq!(
            format!("{cxx_sig}"),
            "fn get_ident_as_function (ty : & Type) -> Ident ;"
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
        let cxx_sig = cxx_fn.as_cxx_sig();

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
        let cxx_impl = cxx_fn.as_cxx_impl();

        assert_eq!(
            format!("{cxx_impl}"),
            "pub fn foo (u : usize) -> usize { let res = foo (u) ; res }"
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
        let cxx_impl = cxx_fn.as_cxx_impl();

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn is_adult(&self) -> bool {
    let res = <bar::Bar>::is_adult(<bar::Bar>::from(self.clone()));
    res
}
"
        )
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
        let cxx_impl = cxx_fn.as_cxx_impl();

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn name(self) -> [char; 5] {
    let res = <bar::Bar>::name(self.into());
    res
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
        let cxx_impl = cxx_fn.as_cxx_impl();

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn bday_value(mut self) -> Self {
    let mut x = <bar::Bar>::from(self);
    let res = <bar::Bar>::bday_value(x);
    let res = Self::from(x);
    res
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
        let cxx_impl = cxx_fn.as_cxx_impl();

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#cxx_impl)),
            "pub fn bday(&mut self) {
    let mut x = <bar::Bar>::from(self.clone());
    let res = <bar::Bar>::bday(&mut x);
    *self = Self::from(x);
    res
}
"
        )
    }

    #[test]
    fn test_raw_struct_converting_un_named_tuples() {
        let item_struct: ItemStruct = syn::parse_str(r#"pub struct Foo(usize, u64, u8);"#).unwrap();
        let mut create_raw_struct = CreateRawStruct::default();
        create_raw_struct.visit_item_struct(&item_struct);
        let (vec, idents) = create_raw_struct.results();
        let raw_structs = vec.into_iter().flat_map(|x| x.as_raw_struct());
        assert_eq!(idents, [format_ident!("Foo")].into());

        assert_eq!(
            prettyplease::unparse(&parse_quote!(#(#raw_structs)*)),
            "pub struct Foo {
    pub n0: usize,
    pub n1: u64,
    pub n2: u8,
}
"
        )
    }

    #[test]
    fn test_raw_struct_conversions_with_struc() {
        let item_struct: ItemStruct = syn::parse_str(r#"pub struct Foo(usize, u64, u8);"#).unwrap();
        let mut create_raw_struct = CreateRawStruct::default();
        create_raw_struct.visit_item_struct(&item_struct);
        let (raw_vec, _) = create_raw_struct.results();
        let mut buf = quote!(#item_struct);
        Cxx::generate_conversions(&raw_vec, &mut buf);
        assert_eq!(
            prettyplease::unparse(&parse_quote!(#buf)),
            "pub struct Foo(usize, u64, u8);
pub struct FooRaw(usize, u64, u8);
impl From<Foo> for FooRaw {
    fn from(x: Foo) -> Self {
        Self(x.0, x.1, x.2)
    }
}
impl From<FooRaw> for Foo {
    fn from(x: FooRaw) -> Self {
        Self(x.0, x.1, x.2)
    }
}
"
        )
    }
}
