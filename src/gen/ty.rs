use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{
	Context,
	Ids,
	ty
};
use super::Generate;

// impl Generate<pseudo::Type> for Rust {
// 	fn generate(&self, context: &pseudo::Context, ty: &pseudo::Type) -> TokenStream {
// 		use pseudo::ty::Desc;
// 		let id = self.generate(context, ty.id());
// 		let params = if ty.parameters().is_empty() {
// 			None
// 		} else {
// 			let params = ty
// 				.parameters()
// 				.iter()
// 				.map(|p| quote::format_ident!("{}", p.to_caml_case()));
// 			Some(quote! { <#(#params),*> })
// 		};
// 		match ty.desc() {
// 			Desc::Opaque => quote! { },
// 			Desc::Enum(enm) => {
// 				let variants = enm.variants().iter().map(|v| {
// 					use pseudo::ty::Variant;
// 					match v {
// 						Variant::BuiltIn(v) => {
// 							let id = self.generate(context, v);
// 							let param = v.parameter().map(|p| {
// 								let p = self.generate(context, p);
// 								quote! { (#p) }
// 							});
// 							quote! { #id #param }
// 						}
// 						Variant::Defined(id, desc) => {
// 							use pseudo::ty::VariantDesc;
// 							let id = self.defined_variant_id(id);
// 							let desc = match desc {
// 								_ if desc.is_empty() => None,
// 								VariantDesc::Tuple(args) => {
// 									let args = args.iter().map(|a| self.generate(context, a));
// 									Some(quote! { (#(#args),*) })
// 								}
// 								VariantDesc::Struct(strct) => {
// 									let fields = strct.fields().iter().map(|f| {
// 										let id = self.field_id(&f.id);
// 										let ty = self.generate(context, &f.ty);
// 										quote! { #id: #ty }
// 									});
// 									Some(quote! { { #(#fields),* } })
// 								}
// 							};
// 							quote! { #id #desc }
// 						}
// 					}
// 				});
// 				quote! { pub enum #id #params { #(#variants),* } }
// 			}
// 			Desc::Struct(strct) => {
// 				let fields = strct.fields().iter().map(|f| {
// 					let id = self.field_id(&f.id);
// 					let ty = self.generate(context, &f.ty);
// 					quote! { #id: #ty }
// 				});
// 				quote! { pub struct #id #params { #(#fields),* } }
// 			}
// 			Desc::TupleStruct(args) => {
// 				let args = args.iter().map(|a| self.generate(context, a));
// 				quote! { pub struct #id #params (#(pub #args),*); }
// 			}
// 		}
// 	}
// }

// impl Generate<pseudo::ty::Id> for Rust {
// 	fn generate(&self, context: &pseudo::Context, id: &pseudo::ty::Id) -> TokenStream {
// 		use pseudo::ty::Id;
// 		match id {
// 			Id::BuiltIn(ty) => self.generate(context, ty),
// 			Id::Defined(id) => {
// 				let id = quote::format_ident!("{}", id.to_caml_case());
// 				quote! { #id }
// 			}
// 		}
// 	}
// }

// impl<T: Ids> Generate<T> for ty::Variant<T> {
// 	fn generate(&self, context: &Context<T>) -> TokenStream {
// 		match self {
// 			Self::Native(v) => self.generate(context, v),
// 			Self::Defined(id, _) => self.defined_variant_id(id),
// 		}
// 	}
// }

impl<T: Ids> Generate<T> for ty::Ref {
	fn generate(&self, context: &Context<T>) -> TokenStream {
		super::path(context, context.type_path(*self).unwrap())
	}
}

// impl Generate<Box<pseudo::ty::Expr>> for Rust {
// 	fn generate(&self, context: &pseudo::Context, e: &Box<pseudo::ty::Expr>) -> TokenStream {
// 		self.generate(context, e.as_ref())
// 	}
// }

// impl Generate<pseudo::ty::Expr> for Rust {
// 	fn generate(&self, context: &pseudo::Context, e: &pseudo::ty::Expr) -> TokenStream {
// 		use pseudo::ty::Expr;
// 		match e {
// 			Expr::Unit => quote! { () },
// 			Expr::Var(i) => {
// 				let id = quote::format_ident!("T{}", i);
// 				quote! { #id }
// 			}
// 			Expr::BuiltIn(ty) => self.generate(context, ty),
// 			Expr::Defined(i, args) => {
// 				let ty = context.defined_type(*i).unwrap();
// 				let module_path = self.module_path(context.module_path(ty.module()).unwrap());
// 				let id = self.type_id(ty.id());
// 				if args.is_empty() {
// 					quote! { #module_path::#id }
// 				} else {
// 					let args = args.iter().map(|a| self.generate(context, a));
// 					quote! { #module_path::#id <#(#args),*> }
// 				}
// 			}
// 			Expr::Heap(e) => {
// 				let e = self.generate(context, e);
// 				quote! { Box<#e> }
// 			}
// 			Expr::List(e) => {
// 				let e = self.generate(context, e);
// 				quote! { Vec<#e> }
// 			}
// 			Expr::Option(e) => {
// 				let e = self.generate(context, e);
// 				quote! { Option<#e> }
// 			}
// 			Expr::Loc(e) => {
// 				let e = self.generate(context, e);
// 				quote! { ::source_span::Loc<#e> }
// 			}
// 		}
// 	}
// }

// impl Generate<pseudo::built_in::Type> for Rust {
// 	fn generate(&self, context: &pseudo::Context, ty: &pseudo::built_in::Type) -> TokenStream {
// 		use pseudo::built_in::Type;
// 		let lexer_module_path = self.module_path(context.lexer_module_path());
// 		let parser_module_path = self.module_path(context.parser_module_path());
// 		match ty {
// 			Type::Token => quote! { #lexer_module_path::Token },
// 			Type::Keyword => quote! { #lexer_module_path::Keyword },
// 			Type::Operator => quote! { #lexer_module_path::Operator },
// 			Type::Delimiter => quote! { #lexer_module_path::Delimiter },
// 			Type::Punct => quote! { #lexer_module_path::Punct },
// 			Type::Node => quote! { #parser_module_path::Node },
// 			Type::Item => quote! { #parser_module_path::Item },
// 		}
// 	}
// }