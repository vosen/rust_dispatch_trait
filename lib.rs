#![crate_name = "dispatch_trait"]
#![crate_type = "dylib"]
#![license = "MIT/ASL2"]

#![feature(plugin_registrar)]
#![feature(managed_boxes)]

extern crate syntax;
extern crate rustc;

use std::gc::{Gc, GC};
use syntax::ast::{Item, MetaItem};
use syntax::ext::base::{ExtCtxt, ItemModifier, ItemDecorator};
use syntax::ext::build::AstBuilder;
use syntax::codemap::Span;
use syntax::parse::token;
use rustc::plugin::registry::Registry;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    // rustc gives us the choice between ItemModifier that modifies item in-place and ItemDecorator that
    // generates new items. We go around this by using two traits, first running  ItemModifier and adding
    // #[dispatch_trait_generated] attribute, which triggers ItemDecorator.
    reg.register_syntax_extension(token::intern("dispatch_trait"), ItemModifier(expand_modify_trait));
    reg.register_syntax_extension(token::intern("dispatch_trait_generated"), ItemDecorator(expand_generate_traits));
}

fn expand_generate_traits(cx: &mut ExtCtxt,
                     span: Span,
                     meta: Gc<MetaItem>,
                     item: Gc<Item>,
                     push: |Gc<Item>|) {
}

fn expand_modify_trait(cx: &mut ExtCtxt,
                       span: Span,
                       meta: Gc<MetaItem>,
                       item: Gc<Item>) -> Gc<Item> {
    let mut item : Item = (*item).clone();
    // Push the dispatch_trait_generate trait on the item 
    item.attrs.push(cx.attribute(span, cx.meta_word(span, token::intern_and_get_ident("dispatch_trait_generated"))));
    box (GC) item
}