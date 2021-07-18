use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{
	Context,
	Namespace,
	Module
};
use super::{
	Generate,
	GenerateIn,
	Scope
};

impl<T: Namespace> Generate<T> for Module<T> {
	fn generate(&self, context: &Context<T>) -> TokenStream {
		let scope = Scope::new(self.index().unwrap());

		let types = self.types().map(|t| {
			let ty = context.ty(t).unwrap();
			ty.generate_in(context, scope)
		});

		let functions = self.functions().map(|index| {
			let f = context.function(index).unwrap();
			f.generate_in(context, scope)
		});

		let submodules = self.modules().map(|index| {
			let m = context.module(index).unwrap();
			let id =  super::module_id(context, m.id());
			if m.is_extern() {
				quote! { pub mod #id; }
			} else {
				let inner = m.generate(context);
				quote! {
					pub mod #id {
						#inner
					}
				}
			}
		});

		quote! {
			#(#types)*
			#(#functions)*
			#(#submodules)*
		}
	}
}