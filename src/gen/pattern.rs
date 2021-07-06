use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{
	Context,
	Ids,
	pattern,
	Pattern
};
use super::Generate;

impl<T: Ids> Generate<T> for Box<Pattern<T>> {
	fn generate(&self, context: &Context<T>) -> TokenStream {
		self.as_ref().generate(context)
	}
}

impl<T: Ids> Generate<T> for Pattern<T> {
	fn generate(&self, context: &Context<T>) -> TokenStream {
		match self {
			Self::Any => quote! { _ },
			Self::Bind(id) => super::var_id(context, *id),
			Self::Literal(c) => c.generate(context),
			Self::Cons(ty_ref, v, args) => {
				let ty = context.ty(*ty_ref).unwrap();
				let variant = ty.as_enum().unwrap().variant(*v).unwrap();
				let variant_id = super::variant_id(context, variant);
				let ty_path = ty_ref.generate(context);
				let args = if args.is_empty() {
					None
				} else {
					use pattern::ConsArgs;
					Some(match args {
						ConsArgs::Tuple(args) => {
							let args = args.iter().map(|a| a.generate(context));
							quote! { ( #(#args),* ) }
						}
						ConsArgs::Struct(bindings) => {
							let bindings = bindings.iter().map(|b| {
								let id = super::field_id(context, b.id);
								let pattern = b.pattern.generate(context);
								quote! { #id as #pattern }
							});
							quote! { { #(#bindings),* } }
						}
					})
				};
				quote! { #ty_path :: #variant_id #args }
			}
			Self::Or(patterns) => {
				let patterns = patterns.iter().map(|p| p.generate(context));
				quote! { #(#patterns)|* }
			}
		}
	}
}