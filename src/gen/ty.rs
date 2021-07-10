use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{
	Context,
	Namespace,
	Type,
	ty
};
use super::{
	Generate,
	GenerateIn,
	Scope
};

impl<T: Namespace> Generate<T> for ty::Param<T> {
	fn generate(&self, context: &Context<T>) -> TokenStream {
		match self {
			Self::Defined(id) => super::param_id(context, *id),
			Self::Native(n) => {
				use ty::NativeParam;
				match n {
					NativeParam::Value => quote! { T },
					NativeParam::Error => quote! { E }
				}
			}
		}
	}
}

impl<T: Namespace> GenerateIn<T> for Type<T> {
	fn generate_in(&self, context: &Context<T>, scope: Scope<T>) -> TokenStream {
		use ty::Desc;
		let id = super::type_id(context, self.id());
		let params = if self.parameters().is_empty() {
			None
		} else {
			let params = self
				.parameters()
				.iter()
				.map(|p| p.generate(context));
			Some(quote! { <#(#params),*> })
		};
		let definition = match self.desc() {
			Desc::Opaque => quote! { },
			Desc::Enum(enm) => {
				let variants = enm.variants().iter().map(|v| v.generate(context));
				quote! { pub enum #id #params { #(#variants),* } }
			}
			Desc::Struct(strct) => {
				let fields = strct.fields().iter().map(|f| {
					let id = super::field_id(context, f.id);
					let ty = f.ty.generate(context);
					quote! { #id: #ty }
				});
				quote! { pub struct #id #params { #(#fields),* } }
			}
			Desc::TupleStruct(args) => {
				let args = args.iter().map(|a| a.generate(context));
				quote! { pub struct #id #params (#(pub #args),*); }
			},
			Desc::Lexer => {
				quote! {
					/// Lexer.
					pub struct #id<I: Iterator, M> {
						/// Source character stream.
						source: ::std::iter::Peekable<I>,

						/// Token buffer.
						buffer: String,

						/// Character metrics.
						metrics: M,

						/// Token span.
						span: ::source_span::Span,
					}
				}
			}
		};

		if self.methods().is_empty() {
			definition
		} else {
			let impls = self.methods().iter().map(|i| {
				context.function(*i).unwrap().generate_in(context, scope)
			});

			quote! {
				#definition
				#(#impls)*
			}
		}
	}
}

impl<T: Namespace> Generate<T> for ty::Variant<T> {
	fn generate(&self, context: &Context<T>) -> TokenStream {
		let id = super::variant_id(context, self);
		match self {
			Self::Native(v) => {
				let params = if v.parameters::<T>().is_empty() {
					None
				} else {
					let params = v.parameters().iter().map(|p| p.generate(context));
					Some(quote! { ( #(#params),* ) })
				};
				quote! { #id #params }
			}
			Self::Defined(_, desc) => {
				use ty::VariantDesc;
				let desc = match desc {
					_ if desc.is_empty() => None,
					VariantDesc::Tuple(args) => {
						let args = args.iter().map(|a| a.generate(context));
						Some(quote! { (#(#args),*) })
					}
					VariantDesc::Struct(strct) => {
						let fields = strct.fields().iter().map(|f| {
							let id = super::field_id(context, f.id);
							let ty = f.ty.generate(context);
							quote! { #id: #ty }
						});
						Some(quote! { { #(#fields),* } })
					}
				};
				quote! { #id #desc }
			}
		}
	}
}

impl<T: Namespace> Generate<T> for ty::Ref {
	fn generate(&self, context: &Context<T>) -> TokenStream {
		super::path(context, context.type_path(*self).unwrap())
	}
}

impl<T: Namespace> Generate<T> for Box<ty::Expr<T>> {
	fn generate(&self, context: &Context<T>) -> TokenStream {
		self.as_ref().generate(context)
	}
}

impl<T: Namespace> Generate<T> for ty::Expr<T> {
	fn generate(&self, context: &Context<T>) -> TokenStream {
		match self {
			Self::Var(p) => p.generate(context),
			Self::Instance(ty_ref, args) => {
				let ty_path = super::path(context, context.type_path(*ty_ref).unwrap());
				if args.is_empty() {
					quote! { #ty_path }
				} else {
					let args = args.iter().map(|a| a.generate(context));
					quote! { #ty_path <#(#args),*> }
				}
			}
		}
	}
}