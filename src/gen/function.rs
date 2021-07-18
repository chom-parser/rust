use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{
	Context,
	Namespace,
	Function,
	function
};
use super::{
	Generate,
	GenerateIn,
	Scope
};

mod lexer;

impl<T: Namespace> GenerateIn<T> for Function<T> {
	fn generate_in(&self, context: &Context<T>, mut scope: Scope<T>) -> TokenStream {
		scope.set_marker(self.signature().marker());
		scope.set_this(self.this());
		
		let body = match self.body() {
			Some(expr) => expr.generate_in(context, scope),
			None => quote! { unimplemented!() }
		};
		let id = super::function_id(context, self.id(), self.signature());
		match self.signature().marker() {
			None => {
				let args = self.signature().arguments().iter().map(|a| {
					let id = super::var_id(context, Some(scope), a.id());
					let ty = a.ty().generate(context);
					quote! { #id: #ty }
				});

				let return_ty = self.signature().return_types()[0].generate(context);

				quote! {
					fn #id(#(#args),*) -> #return_ty {
						#body
					}
				}
			}
			Some(function::Marker::ExternParser) => {
				TokenStream::new() // TODO generate a dummy function.
			}
			Some(function::Marker::UnexpectedChar) => {
				let return_ty = self.signature().return_types()[0].generate(context);
				quote! { pub fn #id (c: Option<char>) -> #return_ty { #body } }
			}
			Some(function::Marker::Lexer) => {
				debug_assert!(scope.this().is_some());
				lexer::generate(context, self, body)
			}
			Some(function::Marker::Parser) => {
				let lexer = super::var_id(context, Some(scope), self.signature().arguments()[0].id());
				let lexer_ty = self.signature().arguments()[0].ty();
				let lexer_item_ty = lexer_ty.stream_item().unwrap();
				let (token_ty, lexer_error_ty) = lexer_item_ty.as_result_type().unwrap();
				let return_ty = &self.signature().return_types()[0];

				let lexer_item_ty = lexer_item_ty.generate(context);
				let token_ty = token_ty.generate(context);
				let return_ty = return_ty.generate(context);

				quote! {
					pub fn #id<
						L: ::std::iter::Iterator<
							Item = #lexer_item_ty,
						>,
					>(
						#lexer: &mut L,
					) -> #return_ty {
						#body
					}
				}
			}
			Some(function::Marker::DebugFormat) => {
				let ty = self.signature().arguments()[0].ty().referenced_type().unwrap();
				let ty_path = ty.generate(context);
				let output = super::var_id(context, Some(scope), self.signature().arguments()[1].id());

				quote! {
					impl std::fmt::Debug for #ty_path {
						fn fmt(&self, #output: &mut std::fmt::Formatter) -> std::fmt::Result {
							#body
						}
					}
				}
			}
		}
	}
}