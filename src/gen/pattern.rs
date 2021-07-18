use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{
	Context,
	Namespace,
	Pattern
};
use super::Generate;

impl<T: Namespace> Generate<T> for Box<Pattern<T>> {
	fn generate(&self, context: &Context<T>) -> TokenStream {
		self.as_ref().generate(context)
	}
}

impl<T: Namespace> Generate<T> for Pattern<T> {
	fn generate(&self, context: &Context<T>) -> TokenStream {
		match self {
			Self::Any => quote! { _ },
			Self::Bind(id) => super::var_id(context, None, *id),
			Self::Literal(c) => c.generate(context),
			Self::Cons(ty_ref, v, args) => {
				let ty = context.ty(*ty_ref).unwrap();
				let enm = ty.as_enum().unwrap();
				let variant = enm.variant(*v).unwrap();
				let variant_id = super::variant_id(context, variant);
				let args = if args.is_empty() {
					None
				} else {
					Some(match variant {
						chom_ir::ty::Variant::Native(_) => {
							let args = args.iter().map(|a| a.generate(context));
							quote! { ( #(#args),* ) }
						}
						chom_ir::ty::Variant::Defined(_, desc) => {
							match desc {
								chom_ir::ty::VariantDesc::Tuple(_) => {
									let args = args.iter().map(|a| a.generate(context));
									quote! { ( #(#args),* ) }
								}
								chom_ir::ty::VariantDesc::Struct(strct) => {
									assert_eq!(strct.len(), args.len() as u32);
									let bindings = strct.fields().iter().enumerate().map(|(i, b)| {
										let id = super::field_id(context, b.id);
										let pattern = args[i].generate(context);
										quote! { #id as #pattern }
									});
									quote! { { #(#bindings),* } }
								}
							}
							
						}
					})
				};

				if super::is_ubiquitous(*ty_ref) {
					quote! { #variant_id #args }
				} else {
					let ty_path = ty_ref.generate(context);
					quote! { #ty_path :: #variant_id #args }
				}
			}
			Self::Or(patterns) => {
				let patterns = patterns.iter().map(|p| p.generate(context));
				quote! { #(#patterns)|* }
			}
		}
	}
}