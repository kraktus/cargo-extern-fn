use std::collections::HashSet;
use std::default;

use clap::{Args, Parser};

use log::trace;
use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::{Pair, Punctuated};
use syn::visit::{self, Visit};
use syn::{
    visit_mut::{self, VisitMut},
    Attribute, ItemEnum, ItemStruct, Type, Visibility,
};
use syn::{
    DeriveInput, Fields, FnArg, GenericParam, Generics, Ident, ImplItemMethod, Item, ItemConst,
    ItemExternCrate, ItemFn, ItemForeignMod, ItemImpl, ItemMacro, ItemMacro2, ItemMod, ItemStatic,
    ItemTrait, ItemTraitAlias, ItemType, ItemUnion, ItemUse, Lit, Meta, MetaNameValue, Pat,
    PatIdent, PatType, Signature, Token, WhereClause, WherePredicate,
};

use quote::{format_ident, quote, ToTokens};

use crate::cbindgen::{attrs, meta_is_extern_fn_skip};

struct Cxx;

#[derive(Debug, Clone)]
struct CreateRawStruct {
    raw_structs: Vec<ItemStruct>, // public enums always have their variants public, so no need to construct a sibling
    idents: HashSet<Ident>,       // idents of struct/enums. TODO union?
}

impl Default for CreateRawStruct {
    fn default() -> Self {
        Self {
            raw_structs: vec![],
            idents: HashSet::new(),
        }
    }
}

impl<'ast> Visit<'ast> for CreateRawStruct {
    fn visit_item_struct(&mut self, struct_: &'ast ItemStruct) {
        if matches!(struct_.vis, Visibility::Public(_)) && struct_.generics.params.is_empty()
        // generics not handled by cxx
        {
            let mut raw_struct = struct_.clone();
            raw_struct.ident = format_ident!("{}Raw", struct_.ident);
            for field_mut in raw_struct.fields.iter_mut() {
                field_mut.vis = struct_.vis // make it public, not sure if reusing the span will cause issue
            }
            self.raw_structs.push(raw_struct);
            self.idents.insert(struct_.ident);
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
            self.idents.insert(enum_.ident);
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

impl Cxx {
    pub fn handle_file(parsed_file: &mut syn::File) -> TokenStream {
        trace!("Starting CreateRawStruct pass");
        let mut create_raw_struct = CreateRawStruct::default();
        create_raw_struct.visit_file(parsed_file);
        trace!("Finished CreateRawStruct pass");
        let mut parsed_file_tokens = quote!(#parsed_file);
        externalised_fn.to_tokens(&mut parsed_file_tokens);
        parsed_file_tokens
    }
}
