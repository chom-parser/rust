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

impl<T: Namespace + ?Sized> Generate<T> for Module<T> {
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
			if !m.is_extern() {
				let id =  super::module_id(context, m.id());
				let inner = m.generate(context);
				Some(quote! {
					mod #id {
						#inner
					}
				})
			} else {
				None
			}
		});

		quote! {
			#(#types)*
			#(#functions)*
			#(#submodules)*
		}
	}
}