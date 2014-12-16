#![crate_name = "dispatch_trait"]
#![crate_type = "dylib"]

#![feature(plugin_registrar)]

extern crate syntax;
extern crate rustc;

use rustc::plugin::registry::Registry;
use std::str::StrVector;
use syntax::abi::Rust;
use syntax::ast;
use syntax::ast::{Generics, Item, MetaItem, ItemTrait, Public, Inherited, TraitRef, TraitTyParamBound, ItemStruct};
use syntax::ast::{Ident, MetaList, MetaWord, TypeMethod, TyParam, Method, MethDecl, TraitItem, RequiredMethod, ProvidedMethod};
use syntax::ast::{SelfRegion, Unsafety, MutImmutable, Block, CompilerGenerated, UnsafeBlock, ItemImpl, StructDef};
use syntax::ast::{Arg, Expr, PatIdent, WhereClause, ImplItem, MethodImplItem, TypeTraitItem, PolyTraitRef};
use syntax::codemap::{Span, Spanned};
use syntax::ext::base::{ExtCtxt, Modifier, Decorator};
use syntax::ext::build::AstBuilder;
use syntax::owned_slice::OwnedSlice;
use syntax::parse::token;
use syntax::parse::token::InternedString;
use syntax::ast_util::PostExpansionMethod;
use syntax::ptr::P;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    // rustc gives us the choice between ItemModifier that modifies item in-place and ItemDecorator that
    // generates new items. Also, syntax extensions are applied in batches. First ItemModifier and then ItemDecorator.
    // We go around this by using two traits. First we run ItemModifier and modify its name to #Trait#__#Visibility#__Original
    // and mark wioth attribute #[dispatch_trait__postprocess]. This triggers second attribute, which triggers ItemDecorator
    // that generates trait with name #Trait# back and all the implementation scaffolding.
    reg.register_syntax_extension(token::intern("dispatch_trait"), Modifier(box expand_normalize_trait));
    reg.register_syntax_extension(token::intern("dispatch_trait__postprocess"), Decorator(box expand_generate_traits));
}

fn expand_normalize_trait(cx: &mut ExtCtxt,
                          sp: Span,
                          meta: &MetaItem,
                          item: P<Item>) -> P<Item> {
    // Make sure that we were called correctly
    let impl_struct = match validate_meta_attr("dispatch_trait", cx, sp, meta) {
        Some(struct_name) => struct_name,
        None => { return item; }
    };
    match item.node.clone() {
        ItemTrait(_,_,_,_, ref methods) => {
            // Make sure that all methods have body
            for method in methods.iter() {
                match method {
                    &RequiredMethod(ref method_decl) => { cx.span_err(method_decl.span, "all methods in a trait marked with `dispatch_trait` require default implementations") },
                    &ProvidedMethod(_) => {}
                    &TypeTraitItem(_) => {}
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
            return P(item);
        },
        _ => {}
    };
    cx.span_err(sp, "attribute `dispatch_trait` can be only applied to traits");
    return item;
}

fn emit_invalid_meta_attr_error(cx: &mut ExtCtxt, sp: Span, trait_name: &str) {
    cx.span_err(sp, format!("malformed attribute `{:}`, correct form is dispatch_trait(StructName)", trait_name).as_slice());
}

fn validate_meta_attr<'a>(trait_name: &str,
                          cx: &mut ExtCtxt,
                          sp: Span,
                          meta: &'a MetaItem) -> Option<&'a InternedString> {
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
                          meta: &MetaItem,
                          item: &Item,
                          push: |P<Item>|) {
    // Check if we were called correctly
    let impl_struct = match validate_meta_attr("dispatch_trait__postprocess", cx, sp, meta) {
        Some(struct_name) => struct_name,
        None => { return; }
    };
    // Unpickle methods for the passed ItemTrait
    let trait_methods = match item.node {
        ItemTrait(_,_,_,_, ref methods) => { methods },
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
    // Load generics from otriginal defintion
    let generics_original = match item.node {
        ItemTrait(_, ref generics, _, _, _) => generics,
        _ => panic!()
    };
    // generate empty Extends<T> struct
    let extends_struct = Item {
        ident: cx.ident_of("Extends"),
        attrs: vec!(cx.attribute(sp, cx.meta_list(sp, token::intern_and_get_ident("allow"), vec!(cx.meta_word(sp, InternedString::new("dead_code")))))),
        id: ast::DUMMY_NODE_ID,
        vis: visibility,
        span: sp,
        node: ItemStruct(
            P(StructDef {
                fields: vec!(),
                ctor_id: Some(ast::DUMMY_NODE_ID)
            }),
            Generics {                
                lifetimes: vec!(),
                ty_params: OwnedSlice::from_vec(vec!(
                    TyParam {
                        ident: cx.ident_of("T"),
                        id: ast::DUMMY_NODE_ID,
                        bounds: OwnedSlice::empty(),
                        unbound: None,
                        default: None,
                        span: sp
                    }
                )),
                where_clause: WhereClause { id: ast::DUMMY_NODE_ID, predicates: vec!() }
            }
        )
    };
    push(P(extends_struct));
    // Generate #Trait#_Base
    let base_trait_ident = cx.ident_of((base_name.to_string() + "_Base").as_slice());
    let base_trait = Item {
        ident: base_trait_ident,
        attrs: vec!(
            cx.attribute(sp, cx.meta_list(sp, token::intern_and_get_ident("doc"), vec!(cx.meta_word(sp, InternedString::new("hidden"))))),
            cx.attribute(sp, cx.meta_list(sp, token::intern_and_get_ident("allow"), vec!(cx.meta_word(sp, InternedString::new("non_camel_case_types")))))),
        id: ast::DUMMY_NODE_ID,
        vis: visibility,
        span: sp,
        node: ItemTrait(
            Unsafety::Normal,
            generics_original.clone(),
            None,
            OwnedSlice::empty(),
            trait_methods.iter().map(|m| method_to_unimplemented(m, cx, sp)).collect(),
        )
    };
    push(P(base_trait));
    // Generate #Trait#<T:#Trait#_Base>
    let impl_trait_ident = cx.ident_of(base_name);
    let impl_trait = Item {
        ident: impl_trait_ident,
        attrs: vec!(), // just calling item.attrs.clone() leads to a crash and I can't 
                       // imagine why would you want other attrs on this trait
        id: ast::DUMMY_NODE_ID,
        vis: visibility,
        span: sp,
        node: ItemTrait(
            Unsafety::Normal,
            {
                let mut generics = generics_original.clone();
                generics.ty_params = OwnedSlice::from_vec(vec!( 
                    TyParam {
                        ident: cx.ident_of("__T"),
                        id: ast::DUMMY_NODE_ID,
                        bounds: OwnedSlice::from_vec(vec!(TraitTyParamBound(
                            PolyTraitRef { trait_ref: base_trait_ref(generics_original, base_trait_ident, cx, sp), bound_lifetimes: vec!() }
                        ))),
                        unbound: None,
                        default: None,
                        span: sp
                    }
                ).into_iter().chain(generics.ty_params.iter().map(|x| x.clone())).collect::<Vec<_>>());
                generics
            },
            None,
            OwnedSlice::empty(),
            append_one(trait_methods.iter().map(|m| method_to_base_call(m, cx, sp)).collect::<Vec<_>>(), base_method(cx, sp))
        )
    };
    push(P(impl_trait));
    // Generare impl<B:#Trait#_Base, T:#Trait#<B>> #Trait#_Base for Extends<T>
    let impl_for_extends = Item {
        ident: base_trait_ident,
        attrs: vec!(),
        id: ast::DUMMY_NODE_ID,
        vis: Inherited,
        span: sp,
        node: ItemImpl(
            Unsafety::Normal,
            {
                let mut generics = generics_original.clone();
                generics.ty_params = OwnedSlice::from_vec(vec!( 
                    TyParam {
                        ident: cx.ident_of("__B"),
                        id: ast::DUMMY_NODE_ID,
                        bounds: OwnedSlice::from_vec(vec!(TraitTyParamBound(
                            PolyTraitRef { trait_ref: base_trait_ref(generics_original, base_trait_ident, cx, sp), bound_lifetimes: vec!() }
                        ))),
                        unbound: None,
                        default: None,
                        span: sp
                    },
                    TyParam {
                        ident: cx.ident_of("__T"),
                        id: ast::DUMMY_NODE_ID,
                        bounds: OwnedSlice::from_vec(vec!(TraitTyParamBound(
                            cx.poly_trait_ref(
                                cx.path_all(
                                    sp,
                                    false,
                                    vec!(impl_trait_ident),
                                    generics_original.lifetimes.iter().map(|x| x.lifetime).collect::<Vec<_>>().clone(),
                                    vec!(cx.ty_ident(sp, cx.ident_of("__B"))).into_iter().chain(generics_original.ty_params.iter().map(|par| cx.ty_ident(sp, par.ident))).collect(),
                                    vec!()
                                )
                            )
                        ))),
                        unbound: None,
                        default: None,
                        span: sp
                    }
                ).into_iter().chain(generics.ty_params.iter().map(|x| x.clone())).collect::<Vec<_>>());
                generics
            },
            Some(base_trait_ref(generics_original, base_trait_ident, cx, sp)),
            cx.ty_path(cx.path_all(sp, false, vec!(cx.ident_of("Extends")), vec!(), vec!(cx.ty_ident(sp, cx.ident_of("__T"))), vec!())),
            trait_methods.iter().map(|m| method_to_impl_call(m, cx, sp)).collect()
        )
    };
    push(P(impl_for_extends));
    // generate base struct
    let dispatch_struct = Item {
        ident: cx.ident_of(impl_struct.get()),
        attrs: vec!(cx.attribute(sp, cx.meta_list(sp, token::intern_and_get_ident("allow"), vec!(cx.meta_word(sp, InternedString::new("dead_code")))))),
        id: ast::DUMMY_NODE_ID,
        vis: visibility,
        span: sp,
        node: ItemStruct(
            P(StructDef {
                fields: vec!(),
                ctor_id: Some(ast::DUMMY_NODE_ID)
            }),
            {
                let mut new_gen = generics_original.clone();
                strip_trait_bounds(&mut new_gen);
                new_gen
            }
        )
    };
    push(P(dispatch_struct));
    // Copy #Trait#__#Visibility#__Original to the impl of #Trait#_Base for the base struct
    let impl_with_code = Item {
        ident: base_trait_ident,
        attrs: vec!(),
        id: ast::DUMMY_NODE_ID,
        vis: Inherited,
        span: sp,
        node: ItemImpl(
            Unsafety::Normal,
            generics_original.clone(),
            Some(base_trait_ref(generics_original, base_trait_ident, cx, sp)),
            cx.ty_path(cx.path_all(
                sp,
                false, 
                vec!(cx.ident_of(impl_struct.get())),
                generics_original.lifetimes.iter().map(|x| x.lifetime).collect::<Vec<_>>().clone(),
                generics_original.ty_params.iter().map(|par| cx.ty_ident(sp, par.ident)).collect(),
                vec!()
            )),
            trait_methods.iter().map(|m| { trait_method_to_impl_method(m, cx, sp) }).collect()
        )
    };
    push(P(impl_with_code));
    // generate empty impl of #Trait# for the struct
    let impl_with_code = Item {
        ident: impl_trait_ident,
        attrs: vec!(),
        id: ast::DUMMY_NODE_ID,
        vis: Inherited,
        span: sp,
        node: ItemImpl(
            Unsafety::Normal,
            generics_original.clone(),
            Some(cx.trait_ref(
                cx.path_all(
                    sp,
                    false,
                    vec!(impl_trait_ident),
                    generics_original.lifetimes.iter().map(|x| x.lifetime).collect::<Vec<_>>().clone(),
                    vec!(
                        cx.ty_path(cx.path_all(
                            sp,
                            false,
                            vec!(cx.ident_of(impl_struct.get())),                            
                            generics_original.lifetimes.iter().map(|x| x.lifetime).collect::<Vec<_>>().clone(),
                            generics_original.ty_params.iter().map(|par| cx.ty_ident(sp, par.ident)).collect(),
                            vec!()
                        ))
                    ).into_iter().chain(generics_original.ty_params.iter().map(|par| cx.ty_ident(sp,par.ident))).collect(),
                    vec!()
                )
            )),
            cx.ty_path(cx.path_all(
                sp,
                false, 
                vec!(cx.ident_of(impl_struct.get())),
                generics_original.lifetimes.iter().map(|x| x.lifetime).collect::<Vec<_>>().clone(),
                generics_original.ty_params.iter().map(|par| cx.ty_ident(sp, par.ident)).collect(),
                vec!()
            )),
            vec!()
        )
    };
    push(P(impl_with_code));
}

fn strip_trait_bounds(gen: &mut Generics) {
    let new_params = gen.ty_params.iter().map(|p| {
        let mut new_p = p.clone();
        new_p.bounds = OwnedSlice::empty();
        new_p
    }).collect();
    gen.ty_params = OwnedSlice::from_vec(new_params);
}

fn base_trait_ref(generics_original: &Generics, base_trait_ident: Ident, cx: &mut ExtCtxt, sp: Span) -> TraitRef {
    cx.trait_ref(
        cx.path_all(
            sp,
            false,
            vec!(base_trait_ident),
            generics_original.lifetimes.iter().map(|x| x.lifetime).collect::<Vec<_>>().clone(),
            generics_original.ty_params.iter().map(|par| cx.ty_ident(sp, par.ident)).collect(),
            vec!()
        )
    )
}

fn method_to_unimplemented(src: &TraitItem, cx: &mut ExtCtxt, sp: Span) -> TraitItem {
    match src {
        &TypeTraitItem(_) => panic!(),
        &RequiredMethod(_) => panic!(),
        &ProvidedMethod(ref prov_method) => {
            RequiredMethod(
                TypeMethod {
                    ident: cx.ident_of(("_".to_string() + prov_method.pe_ident().as_str()).as_slice()).clone(),
                    attrs: prov_method.attrs.clone().clone(),
                    unsafety: prov_method.pe_unsafety(),
                    abi: prov_method.pe_abi().clone(),
                    decl: P(prov_method.pe_fn_decl().clone()),
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

fn base_method(cx: &mut ExtCtxt, sp: Span) -> TraitItem {
    ProvidedMethod(P(Method {
        attrs: vec!(),
        id: ast::DUMMY_NODE_ID,
        span: sp,
        node: MethDecl(
            cx.ident_of("base"),
            Generics {
                lifetimes: vec!(),
                ty_params: OwnedSlice::empty(),
                where_clause: WhereClause { id: ast::DUMMY_NODE_ID, predicates: vec!() }
            },
            Rust,
            Spanned {
                node: SelfRegion(None, MutImmutable, cx.ident_of("self")),
                span: sp
            },
            Unsafety::Normal,
            cx.fn_decl (
                vec!(cx.arg(sp, cx.ident_of("self"), cx.ty_infer(sp))),
                cx.ty_ident(sp, cx.ident_of("__T"))
            ),
            cx.block_expr(cx.expr_block(P(Block {
                view_items: vec!(),
                stmts: vec!(),
                expr: Some(cx.expr_call_global(sp, vec!(cx.ident_of("std"), cx.ident_of("mem"), cx.ident_of("zeroed")), vec!())),
                id: ast::DUMMY_NODE_ID,
                rules: UnsafeBlock(CompilerGenerated),
                span: sp
            }))),
            Inherited
        )

    }))
}

fn method_to_base_call(src: &TraitItem, cx: &mut ExtCtxt, sp: Span) -> TraitItem {
    match src {
        &TypeTraitItem(_) => panic!(),
        &RequiredMethod(_) => panic!(),
        &ProvidedMethod(ref prov_method) => {
            ProvidedMethod(P(Method {
                attrs: prov_method.attrs.clone().clone(),
                id: ast::DUMMY_NODE_ID,
                span: sp,
                node: MethDecl(
                    prov_method.pe_ident(),
                    prov_method.pe_generics().clone(),
                    prov_method.pe_abi().clone(),
                    prov_method.pe_explicit_self().clone(),
                    prov_method.pe_unsafety().clone(),
                    P(prov_method.pe_fn_decl().clone()),
                    cx.block_expr(cx.expr_method_call(
                        sp,
                        cx.expr_method_call(
                            sp,
                            cx.expr_self(sp),
                            cx.ident_of("base"),
                            vec!()
                        ),
                        cx.ident_of(("_".to_string() + prov_method.pe_ident().as_str()).as_slice()),
                        prov_method.pe_fn_decl().inputs.iter().skip(1).map(|m| { method_inputs_to_args(m, cx, sp) } ).collect()
                    )),
                    prov_method.pe_vis().clone()
                )
            }))
        }
    }
}

fn method_inputs_to_args(arg: &Arg, cx: &ExtCtxt, sp: Span) -> P<Expr> {
    match arg.pat.node {
        PatIdent(_, ident, _) => { cx.expr_ident(sp, ident.node) }
        _ => panic!()
    }
}


fn method_to_impl_call(src: &TraitItem, cx: &mut ExtCtxt, sp: Span) -> ImplItem {
    match src {
        &TypeTraitItem(_) => panic!(),
        &RequiredMethod(_) => panic!(),
        &ProvidedMethod(ref prov_method) => {
            MethodImplItem(P(Method {
                attrs: prov_method.attrs.clone().clone(),
                id: ast::DUMMY_NODE_ID,
                span: sp,
                node: MethDecl(
                    cx.ident_of(("_".to_string() + prov_method.pe_ident().as_str()).as_slice()),
                    prov_method.pe_generics().clone(),
                    prov_method.pe_abi().clone(),
                    prov_method.pe_explicit_self().clone(),
                    prov_method.pe_unsafety().clone(),
                    P(prov_method.pe_fn_decl().clone()),
                    cx.block_expr(
                        cx.expr_method_call(
                            sp,
                            cx.expr_block(P(Block {
                                view_items: vec!(),
                                stmts: vec!(),
                                expr: Some(cx.expr_call(
                                    sp,
                                    cx.expr_path(cx.path_all(
                                        sp,
                                        true,
                                        vec!(cx.ident_of("std"), cx.ident_of("mem"), cx.ident_of("zeroed")),
                                        vec!(),
                                        vec!(cx.ty_ident(sp, cx.ident_of("__T"))),
                                        vec!()
                                    )),
                                    vec!()
                                )),
                                id: ast::DUMMY_NODE_ID,
                                rules: UnsafeBlock(CompilerGenerated),
                                span: sp
                            })),
                            prov_method.pe_ident(),
                            prov_method.pe_fn_decl().inputs.iter().skip(1).map(|m| { method_inputs_to_args(m, cx, sp) } ).collect()
                        )
                    ),
                    prov_method.pe_vis().clone()
                )
            }))
        }
    }
}

fn trait_method_to_impl_method(src: &TraitItem, cx: &mut ExtCtxt, sp: Span) -> ImplItem {
    let meth = match src {
        &TypeTraitItem(_) => panic!(),
        &RequiredMethod(_) => panic!(),
        &ProvidedMethod(ref prov_method) => {
            P(Method {
                attrs: prov_method.attrs.clone().clone(),
                id: ast::DUMMY_NODE_ID,
                span: sp,
                node: MethDecl(
                    cx.ident_of(("_".to_string() + prov_method.pe_ident().as_str()).as_slice()),
                    prov_method.pe_generics().clone(),
                    prov_method.pe_abi().clone(),
                    prov_method.pe_explicit_self().clone(),
                    prov_method.pe_unsafety().clone(),
                    P(prov_method.pe_fn_decl().clone()),
                    P(prov_method.pe_body().clone()),
                    prov_method.pe_vis().clone()
                )
            })
        }
    };
    MethodImplItem(meth)
}

fn append_one<T>(v: Vec<T>, x: T) -> Vec<T> {
    let mut v = v;
    v.push(x);
    v
}