use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{Namespace, Context};

mod scope;
mod constant;
mod expr;
mod ty;
mod pattern;
mod function;
mod module;

pub use scope::Scope;

/// Generate code inside a scope.
pub trait Generate<T: Namespace> {
	fn generate(&self, context: &Context<T>) -> TokenStream;
}

/// Generate code inside a scope.
pub trait GenerateIn<T: Namespace> {
	fn generate_in(&self, context: &Context<T>, scope: Scope<T>) -> TokenStream;
}

fn path<T: Namespace>(context: &Context<T>, path: chom_ir::Path<'_, T>) -> TokenStream {
	let mut tokens = TokenStream::new();

	for segment in path {
		use chom_ir::path::Segment;

		if !tokens.is_empty() {
			tokens.extend(quote! { :: })
		}

		let id = match segment {
			Segment::Module(id) => module_id(context, id),
			Segment::Type(id) => type_id(context, id),
			Segment::Function(sig) => function_id(context, sig)
		};

		tokens.extend(id)
	}

	tokens
}

fn module_id<T: Namespace>(context: &Context<T>, id: &chom_ir::module::Id<T>) -> TokenStream {
	use chom_ir::module::Id;
	match id {
		Id::Root => quote! { crate },
		Id::Named(id) => {
			let ident = context.id().module_ident(*id);
			let id = quote::format_ident!("{}", ident.to_snake_case());
			quote! { #id }
		}
	}
}

fn type_id<T: Namespace>(context: &Context<T>, id: chom_ir::ty::Id<T>) -> TokenStream {
	match id {
		chom_ir::ty::Id::Native(n) => {
			use chom_ir::ty::Native;
			match n {
				Native::Unit => quote! { () },
				Native::Option => quote! { Option },
				Native::Result => quote! { Result },
				Native::List => quote! { Vec },
				Native::Stack => quote! { Vec },
				Native::Heap => quote! { Box },
				Native::Position => quote! { ::source_span::Position },
				Native::Span => quote! { ::source_span::Span },
				Native::Loc => quote! { ::source_span::Loc }
			}
		},
		chom_ir::ty::Id::Defined(id) => {
			let ident = context.id().type_ident(id);
			let id = quote::format_ident!("{}", ident.to_caml_case());
			quote! { #id }
		}
	}
}

fn is_ubiquitous(r: chom_ir::ty::Ref) -> bool {
	use chom_ir::ty::Ref;
	match r {
		Ref::Native(chom_ir::ty::Native::Option) => true,
		Ref::Native(chom_ir::ty::Native::Result) => true,
		_ => false
	}
}

fn param_id<T: Namespace>(context: &Context<T>, id: T::Param) -> TokenStream {
	let ident = context.id().param_ident(id);
	let id = quote::format_ident!("{}", ident.to_caml_case());
	quote! { #id }
}

fn field_id<T: Namespace>(context: &Context<T>, id: T::Field) -> TokenStream {
	let ident = context.id().field_ident(id);
	let id = quote::format_ident!("{}", ident.to_snake_case());
	quote! { #id }
}

fn variant_id<T: Namespace>(context: &Context<T>, v: &chom_ir::ty::Variant<T>) -> TokenStream {
	use chom_ir::ty::Variant;
	match v {
		Variant::Native(n) => {
			use chom_ir::ty::NativeVariant;
			match n {
				NativeVariant::None => quote! { None },
				NativeVariant::Some => quote! { Some },
				NativeVariant::Ok => quote! { Ok },
				NativeVariant::Err => quote! { Err }
			}
		},
		Variant::Defined(id, _) => {
			let ident = context.id().variant_ident(*id);
			let id = quote::format_ident!("{}", ident.to_caml_case());
			quote! { #id }
		}
	}
}

fn function_id<T: Namespace>(context: &Context<T>, sig: &chom_ir::function::Signature<T>) -> TokenStream {
	use chom_ir::function::Signature;
	match sig {
		Signature::ExternParser(_, ident) => {
			let id = quote::format_ident!("{}", ident.to_caml_case());
			quote! { #id }
		},
		Signature::UndefinedChar(_, _) => {
			quote! { undefined }
		},
		Signature::Lexer(_, _) => {
			quote! { next_token }
		},
		Signature::Parser(_, _, return_ty) => {
			let ident = type_expr_ident(context, return_ty);
			let id = quote::format_ident!("parse_{}", ident.to_snake_case());
			quote! { #id }
		}
	}
}

fn type_expr_ident<T: Namespace>(context: &Context<T>, e: &chom_ir::ty::Expr<T>) -> chom_ir::Ident {
	use chom_ir::ty::Expr;
	match e {
		Expr::Var(_) => panic!("type parameter cannot occur here"),
		Expr::Instance(ty_ref, args) => {
			let ty = context.ty(*ty_ref).unwrap();

			let mut id = match ty.id() {
				chom_ir::ty::Id::Defined(id) => context.id().type_ident(id),
				chom_ir::ty::Id::Native(_) => panic!("native type cannot occur here")
			};

			for a in args {
				let a_id = type_expr_ident(context, a);
				id.push_ident(&a_id);
			}

			id
		}
	}
}

fn var_id<T: Namespace>(context: &Context<T>, id: T::Var) -> TokenStream {
	let ident = context.id().var_ident(id);
	let id = quote::format_ident!("{}", ident.to_snake_case());
	quote! { #id }
}

fn label_id<T: Namespace>(context: &Context<T>, id: T::Label) -> TokenStream {
	let ident = context.id().label_ident(id);
	let lft = syn::Lifetime::new(&format!("'{}", ident.to_snake_case()), proc_macro2::Span::call_site());
	quote! { #lft }
}