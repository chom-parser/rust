use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{
	Namespace,
	Constant,
	Context
};
use super::{
	Generate
};

impl<T: Namespace> Generate<T> for Constant {
	fn generate(&self, _: &Context<T>) -> TokenStream {
		match self {
			Constant::Unit => quote! { () },
			Constant::Bool(b) => quote! { #b },
			Constant::String(s) => quote! { #s },
			Constant::Int(n) => quote! { #n },
			Constant::Char(c) => quote! { #c },
			Constant::CharRange(a, b) => quote! { #a..=#b },
		}
	}
}