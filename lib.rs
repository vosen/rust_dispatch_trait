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
use syntax::abi::Rust;
use syntax::ast;
use syntax::ast::{Generics, Item, MetaItem, ItemTrait, Public, Inherited, TraitRef, TraitTyParamBound, ItemStruct};
use syntax::ast::{Ident, Required, Provided, MetaList, MetaWord, TraitMethod, TypeMethod,TyParam, Method, MethDecl};
use syntax::ast::{SelfRegion, NormalFn, MutImmutable, Block, CompilerGenerated, UnsafeBlock, ItemImpl, StructDef};
use syntax::codemap::{Span, Spanned};
use syntax::ext::base::{ExtCtxt, ItemModifier, ItemDecorator};
use syntax::ext::build::AstBuilder;
use syntax::owned_slice::OwnedSlice;
use syntax::parse::token;
use syntax::parse::token::InternedString;
use syntax::ast_util::PostExpansionMethod;

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
            // Hide trait from the docs
            item.attrs.push(cx.attribute(sp, cx.meta_list(sp, token::intern_and_get_ident("doc"), vec!(cx.meta_word(sp, InternedString::new("hidden"))))));
            item.attrs.push(cx.attribute(sp, cx.meta_list(sp, token::intern_and_get_ident("allow"), vec!(cx.meta_word(sp, InternedString::new("non_camel_case_types"))))));
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
                          sp: Span,
                          meta: Gc<MetaItem>,
                          item: Gc<Item>,
                          push: |Gc<Item>|) {
    // Check if we were called correctly
    let impl_struct = match validate_meta_attr("dispatch_trait__postprocess", cx, sp, &meta) {
        Some(struct_name) => struct_name,
        None => { return; }
    };
    // Unpickle methods for the passed ItemTrait
    let trait_methods = match item.node {
        ItemTrait(_,_,_, ref methods) => { methods },
        _ => {
            cx.span_err(sp, "attribute `dispatch_trait__postprocess` can be only applied to traits");
            return;
        }
    };
    // demangle the name
    let mangled_name = item.ident.as_str();
    // cut off "__Original"
    let visible_name = mangled_name.slice_to(mangled_name.len() - 10);
    // pare visibility suffix
    let (base_name, visibility) = if visible_name.ends_with("Public") {
        (visible_name.slice_to(visible_name.len() - 8), Public)
    }
    else {
        (visible_name.slice_to(visible_name.len() - 11), Inherited)
    };
    // Generate #Trait#__Base
    let base_trait_ident = cx.ident_of((base_name.to_string() + "__Base").as_slice());
    let base_trait = Item {
        ident: base_trait_ident,
        attrs: vec!(
            cx.attribute(sp, cx.meta_list(sp, token::intern_and_get_ident("doc"), vec!(cx.meta_word(sp, InternedString::new("hidden"))))),
            cx.attribute(sp, cx.meta_list(sp, token::intern_and_get_ident("allow"), vec!(cx.meta_word(sp, InternedString::new("non_camel_case_types")))))),
        id: ast::DUMMY_NODE_ID,
        vis: Inherited,
        span: sp,
        node: ItemTrait(
            Generics {
                lifetimes: vec!(),
                ty_params: OwnedSlice::empty()
            },
            None,
            vec!(),
            trait_methods.iter().map(|m| method_to_unimplemented(m, cx, sp)).collect()
        )
    };
    push(box (GC) base_trait);
    // Generate #Trait#<T:#Trait#__Base>
    let impl_trait_ident = cx.ident_of(base_name);
    let impl_trait = Item {
        ident: impl_trait_ident,
        attrs: vec!(), // just calling item.attrs.clone() leads to a crash and I can't 
                       // imagine why would you want other attrs on this trait
        id: ast::DUMMY_NODE_ID,
        vis: visibility,
        span: sp,
        node: ItemTrait(
            Generics {
                lifetimes: vec!(),
                ty_params: OwnedSlice::from_vec(vec!( 
                    TyParam {
                        ident: cx.ident_of("T"),
                        id: ast::DUMMY_NODE_ID,
                        bounds: OwnedSlice::from_vec(vec!(TraitTyParamBound(
                            TraitRef {
                                path: cx.path_ident(sp, base_trait_ident),
                                ref_id: ast::DUMMY_NODE_ID
                            }
                        ))),
                        unbound: None,
                        default: None,
                        span: sp
                    }
                ))
            },
            None,
            vec!(),
            trait_methods.iter().map(|m| method_to_base_call(m, cx, sp)).collect::<Vec<_>>().append_one(base_method(cx, sp))
        )
    };
    push(box (GC) impl_trait);
    // Generare impl<B:#Trait#__Base, T:#Trait#<B>> #Trait#__Base for Extends<T>
    let impl_for_extends = Item {
        ident: base_trait_ident,
        attrs: vec!(),
        id: ast::DUMMY_NODE_ID,
        vis: Inherited,
        span: sp,
        node: ItemImpl(
            Generics {
                lifetimes: vec!(),
                ty_params: OwnedSlice::from_vec(vec!( 
                    TyParam {
                        ident: cx.ident_of("B"),
                        id: ast::DUMMY_NODE_ID,
                        bounds: OwnedSlice::from_vec(vec!(TraitTyParamBound(
                            TraitRef {
                                path: cx.path_ident(sp, base_trait_ident),
                                ref_id: ast::DUMMY_NODE_ID
                            }
                        ))),
                        unbound: None,
                        default: None,
                        span: sp
                    },
                    TyParam {
                        ident: cx.ident_of("T"),
                        id: ast::DUMMY_NODE_ID,
                        bounds: OwnedSlice::from_vec(vec!(TraitTyParamBound(
                            cx.trait_ref(
                                cx.path_all(
                                    sp,
                                    false,
                                    vec!(impl_trait_ident),
                                    vec!(),
                                    vec!(cx.ty_ident(sp, cx.ident_of("B")))
                                )
                            )
                        ))),
                        unbound: None,
                        default: None,
                        span: sp
                    }
                ))
            },
            Some(cx.trait_ref(cx.path_ident(sp, base_trait_ident))),
            cx.ty_path(cx.path_all(sp, false, vec!(cx.ident_of("Extends")), vec!(), vec!(cx.ty_ident(sp, cx.ident_of("T")))), None),
            trait_methods.iter().map(|m| method_to_impl_call(m, cx, sp)).collect()
        )
    };
    push(box (GC) impl_for_extends);
    // generate impl struct
    let dispatch_struct = Item {
        ident: cx.ident_of(impl_struct.get()),
        attrs: vec!(cx.attribute(sp, cx.meta_list(sp, token::intern_and_get_ident("allow"), vec!(cx.meta_word(sp, InternedString::new("dead_code")))))),
        id: ast::DUMMY_NODE_ID,
        vis: Inherited,
        span: sp,
        node: ItemStruct(
            box (GC) StructDef {
                fields: vec!(),
                ctor_id: Some(ast::DUMMY_NODE_ID),
                super_struct: None,
                is_virtual: false
            },
            Generics {
                lifetimes: vec!(),
                ty_params: OwnedSlice::empty()
            }
        )
    };
    push(box (GC) dispatch_struct);
    // Copy #Trait#__#Visibility#__Original to the struct impl
    let impl_with_code = Item {
        ident: base_trait_ident,
        attrs: vec!(),
        id: ast::DUMMY_NODE_ID,
        vis: Inherited,
        span: sp,
        node: ItemImpl(
            Generics {
                lifetimes: vec!(),
                ty_params: OwnedSlice::empty()
            },
            Some(cx.trait_ref(cx.path_ident(sp, base_trait_ident))),
            cx.ty_ident(sp, cx.ident_of(impl_struct.get())),
            trait_methods.iter().map(|m| { trait_method_to_impl_method(m, cx, sp) }).collect()
        )
    };
    push(box (GC) impl_with_code);
    // generate default impl of #Trait#__#Visibility#__Original for the struct
    let impl_with_code = Item {
        ident: cx.ident_of(impl_struct.get()),
        attrs: vec!(),
        id: ast::DUMMY_NODE_ID,
        vis: Inherited,
        span: sp,
        node: ItemImpl(
            Generics {
                lifetimes: vec!(),
                ty_params: OwnedSlice::empty()
            },
            Some(cx.trait_ref(
                cx.path_all(
                    sp,
                    false,
                    vec!(impl_trait_ident),
                    vec!(),
                    vec!(cx.ty_ident(sp, cx.ident_of(impl_struct.get())))
                )
            )),
            cx.ty_ident(sp, cx.ident_of(impl_struct.get())),
            vec!()
        )
    };
    push(box (GC) impl_with_code);

}

fn method_to_unimplemented(src: &TraitMethod, cx: &mut ExtCtxt, sp: Span) -> TraitMethod {
    match src {
        &Required(_) => fail!(),
        &Provided(ref prov_method) => {
            Required(
                TypeMethod {
                    ident: cx.ident_of(("__".to_string() + prov_method.pe_ident().as_str()).as_slice()).clone(),
                    attrs: prov_method.attrs.clone().clone(),
                    fn_style: prov_method.pe_fn_style().clone(),
                    abi: prov_method.pe_abi().clone(),
                    decl: prov_method.pe_fn_decl().clone(),
                    generics: prov_method.pe_generics().clone(),
                    explicit_self: prov_method.pe_explicit_self().clone(),
                    id: ast::DUMMY_NODE_ID,
                    span: sp,
                    vis: prov_method.pe_vis().clone()
                }
            )
        }
    }
}

fn base_method(cx: &mut ExtCtxt, sp: Span) -> TraitMethod {
    Provided(box (GC) Method {
        attrs: vec!(),
        id: ast::DUMMY_NODE_ID,
        span: sp,
        node: MethDecl(
            cx.ident_of("base"),
            Generics {
                lifetimes: vec!(),
                ty_params: OwnedSlice::empty()
            },
            Rust,
            Spanned {
                node: SelfRegion(None, MutImmutable, cx.ident_of("self")),
                span: sp
            },
            NormalFn,
            cx.fn_decl (
                vec!(cx.arg(sp, cx.ident_of("self"), cx.ty_infer(sp))),
                cx.ty_ident(sp, cx.ident_of("T"))
            ),
            cx.block_expr(cx.expr_block(box (GC) Block {
                view_items: vec!(),
                stmts: vec!(),
                expr: Some(cx.expr_call_global(sp, vec!(cx.ident_of("std"), cx.ident_of("mem"), cx.ident_of("zeroed")), vec!())),
                id: ast::DUMMY_NODE_ID,
                rules: UnsafeBlock(CompilerGenerated),
                span: sp
            })),
            Inherited
        )

    })
}

fn method_to_base_call(src: &TraitMethod, cx: &mut ExtCtxt, sp: Span) -> TraitMethod {
    match src {        
        &Required(_) => fail!(),
        &Provided(ref prov_method) => {
            Provided(box (GC) Method {
                attrs: prov_method.attrs.clone().clone(),
                id: ast::DUMMY_NODE_ID,
                span: sp,
                node: MethDecl(
                    prov_method.pe_ident(),
                    prov_method.pe_generics().clone(),
                    prov_method.pe_abi().clone(),
                    prov_method.pe_explicit_self().clone(),
                    prov_method.pe_fn_style().clone(),
                    prov_method.pe_fn_decl().clone(),
                    cx.block_expr(cx.expr_method_call(
                        sp,
                        cx.expr_method_call(
                            sp,
                            cx.expr_self(sp),
                            cx.ident_of("base"),
                            vec!()
                        ),
                        cx.ident_of(("__".to_string() + prov_method.pe_ident().as_str()).as_slice()),
                        vec!()
                    )),
                    prov_method.pe_vis().clone()
                )
            })
        }
    }
}


fn method_to_impl_call(src: &TraitMethod, cx: &mut ExtCtxt, sp: Span) -> Gc<Method> {
    match src {        
        &Required(_) => fail!(),
        &Provided(ref prov_method) => {
            box (GC) Method {
                attrs: prov_method.attrs.clone().clone(),
                id: ast::DUMMY_NODE_ID,
                span: sp,
                node: MethDecl(
                    cx.ident_of(("__".to_string() + prov_method.pe_ident().as_str()).as_slice()),
                    prov_method.pe_generics().clone(),
                    prov_method.pe_abi().clone(),
                    prov_method.pe_explicit_self().clone(),
                    prov_method.pe_fn_style().clone(),
                    prov_method.pe_fn_decl().clone(),
                    cx.block_expr(
                        cx.expr_method_call(
                            sp,
                            cx.expr_block(box (GC) Block {
                                view_items: vec!(),
                                stmts: vec!(),
                                expr: Some(cx.expr_call(
                                    sp,
                                    cx.expr_path(cx.path_all(
                                        sp,
                                        true,
                                        vec!(cx.ident_of("std"), cx.ident_of("mem"), cx.ident_of("zeroed")),
                                        vec!(),
                                        vec!(cx.ty_ident(sp, cx.ident_of("T")))
                                    )),
                                    vec!()
                                )),
                                id: ast::DUMMY_NODE_ID,
                                rules: UnsafeBlock(CompilerGenerated),
                                span: sp
                            }),
                            prov_method.pe_ident(),
                            vec!()
                        )
                    ),
                    prov_method.pe_vis().clone()
                )
            }
        }
    }
}

fn trait_method_to_impl_method(src: &TraitMethod, cx: &mut ExtCtxt, sp: Span) -> Gc<Method> {
    match src {
        &Required(_) => fail!(),
        &Provided(ref prov_method) => {
            box (GC) Method {
                attrs: prov_method.attrs.clone().clone(),
                id: ast::DUMMY_NODE_ID,
                span: sp,
                node: MethDecl(
                    cx.ident_of(("__".to_string() + prov_method.pe_ident().as_str()).as_slice()),
                    prov_method.pe_generics().clone(),
                    prov_method.pe_abi().clone(),
                    prov_method.pe_explicit_self().clone(),
                    prov_method.pe_fn_style().clone(),
                    prov_method.pe_fn_decl().clone(),
                    prov_method.pe_body().clone(),
                    prov_method.pe_vis().clone()
                )
            }
        }
    }
}