#![crate_name = "dispatch_trait"]
#![crate_type = "dylib"]
#![license = "MIT/ASL2"]

#![feature(plugin_registrar)]
#![feature(managed_boxes)]

extern crate syntax;
extern crate rustc;

use rustc::plugin::registry::Registry;
use std::gc::{Gc, GC};
use std::str::StrVector;
use syntax::ast::{Item, MetaItem, ItemTrait, Public, Inherited, Ident, Required, Provided, MetaList, MetaWord};
use syntax::codemap::Span;
use syntax::ext::base::{ExtCtxt, ItemModifier, ItemDecorator};
use syntax::ext::build::AstBuilder;
use syntax::parse::token;
use syntax::parse::token::InternedString;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    // rustc gives us the choice between ItemModifier that modifies item in-place and ItemDecorator that
    // generates new items. Also, syntax extensions are applied in batches. First ItemModifier and then ItemDecorator.
    // We go around this by using two traits. First we run ItemModifier and modify its name to #Trait#__#Visibility#__Original
    // and mark wioth attribute #[dispatch_trait__postprocess]. This triggers second attribute, which triggers ItemDecorator
    // that generates trait with name #Trait# back and all the implementation scaffolding.
    reg.register_syntax_extension(token::intern("dispatch_trait"), ItemModifier(expand_normalize_trait));
    reg.register_syntax_extension(token::intern("dispatch_trait__postprocess"), ItemDecorator(expand_generate_traits));
}

fn expand_normalize_trait(cx: &mut ExtCtxt,
                          sp: Span,
                          meta: Gc<MetaItem>,
                          item: Gc<Item>) -> Gc<Item> {
    // Make sure that we were called correctly
    let impl_struct = match validate_meta_attr("dispatch_trait", cx, sp, &meta) {
        Some(struct_name) => struct_name,
        None => { return item; }
    };
    match item.node {
        ItemTrait(_,_,_, ref methods) => {
            // Make sure that all methods have body
            for method in methods.iter() {
                match method {
                    &Required(ref method_decl) => { cx.span_err(method_decl.span, "all methods in a trait marked with `dispatch_trait` require default implementations") },
                    &Provided(_) => {}
                }
            }
            // Copy Item struct and change visibility
            let mut item : Item = (*item).clone();
            let vis = match item.vis {
                Public => "Public",
                Inherited => "Inherited"
            };
            item.vis = Inherited;
            // Rename to #Trait#__#Visibility#__Original
            item.ident = Ident::new(token::intern(vec!(item.ident.as_str(), "__", vis, "__Original" ).concat().as_slice()));
            // Push the dispatch_trait__postprocess trait on the item
            item.attrs.push(cx.attribute(sp, cx.meta_list(sp, token::intern_and_get_ident("dispatch_trait__postprocess"), vec!(cx.meta_word(sp, impl_struct.clone())))));
            return box(GC) item;
        },
        _ => {
            cx.span_err(sp, "attribute `dispatch_trait` can be only applied to traits");
            return item;
        }
    }
}

fn emit_invalid_meta_attr_error(cx: &mut ExtCtxt, sp: Span, trait_name: &str) {
    cx.span_err(sp, format!("malformed attribute `{:}`, correct form is dispatch_trait(StructName)", trait_name).as_slice());
}

fn validate_meta_attr<'a>(trait_name: &str,
                          cx: &mut ExtCtxt,
                          sp: Span,
                          meta: &'a Gc<MetaItem>) -> Option<&'a InternedString> {
    match meta.node {
        MetaList(_, ref inner_items) => {
            if inner_items.len() != 1 {
                emit_invalid_meta_attr_error(cx, sp, trait_name);
                return None;
            }
            match inner_items[0].node {
                MetaWord(ref interned) => { return Some(interned); }
                _ => {
                    emit_invalid_meta_attr_error(cx, sp, trait_name);
                    return None;
                }
            }
        }
        _ => {
            emit_invalid_meta_attr_error(cx, sp, trait_name);
            return None;
        }
    };
}

fn expand_generate_traits(cx: &mut ExtCtxt,
                          span: Span,
                          meta: Gc<MetaItem>,
                          item: Gc<Item>,
                          push: |Gc<Item>|) {
}