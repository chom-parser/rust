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

impl<T: Namespace> GenerateIn<T> for Function<T> {
	fn generate_in(&self, context: &Context<T>, scope: Scope<T>) -> TokenStream {
		let body = match self.body() {
			Some(expr) => expr.generate_in(context, scope),
			None => quote! { unimplemented!() }
		};
		let id = super::function_id(context, self.id(), self.signature());
		match self.signature().marker() {
			None => {
				TokenStream::new() // TODO generate function.
			}
			Some(function::Marker::ExternParser) => {
				TokenStream::new() // TODO generate a dummy function.
			}
			Some(function::Marker::UndefinedChar) => {
				let return_ty = self.signature().return_types()[0].generate(context);
				quote! { pub fn #id (c: Option<char>) -> #return_ty { #body } }
			}
			Some(function::Marker::Lexer) => {
				let return_ty = &self.signature().return_types()[1];
				let result_ty = return_ty.some_type().unwrap();
				let (_token_ty, error_ty) = result_ty.as_result_type().unwrap();

				let error_ty = error_ty.generate(context);

				quote! {
					impl<
						E: Into<#error_ty>,
						I: Iterator<Item = Result<char, E>>,
						M: ::source_span::Metrics,
					> Lexer<I, M> {
						/// Peeks the next character without consuming it.
						fn peek_char(
							&mut self
						) -> Result<
							Option<char>,
							::source_span::Loc<#error_ty>,
						> {
							match self.source.peek() {
								Some(Ok(c)) => Ok(Some(*c)),
								Some(Err(_)) => Err(self.consume_char().unwrap_err()),
								None => Ok(None),
							}
						}

						/// Consumes the next character.
						fn consume_char(
							&mut self,
						) -> Result<(), ::source_span::Loc<#error_ty>> {
							match self.source.next() {
								Some(Ok(c)) => {
									self.buffer.push(c);
									self.span.push(c, &self.metrics);
									Ok(())
								}
								Some(Err(e)) => Err(::source_span::Loc::new(e.into(), self.span.end().into())),
								None => Ok(()),
							}
						}

						/// Clears the lexer buffer.
						fn clear(&mut self) {
							self.buffer.clear();
							self.span.clear()
						}

						/// Parses the next token.
						fn #id(
							&mut self,
						) -> Result<
							Option<::source_span::Loc<Token>>,
							::source_span::Loc<#error_ty>,
						> {
							#body
						}
					}

					impl<
						E: Into<#error_ty>,
						I: Iterator<Item = Result<char, E>>,
						M: ::source_span::Metrics,
					> Iterator for Lexer<I, M> {
						type Item = Result<
							::source_span::Loc<Token>,
							::source_span::Loc<#error_ty>,
						>;

						fn next(&mut self) -> Option<Self::Item> {
							self.#id().transpose()
						}
					}
				}
			}
			Some(function::Marker::Parser) => {
				let lexer = super::var_id(context, self.signature().arguments()[0].id());
				let lexer_ty = self.signature().arguments()[0].ty();
				let lexer_result_ty = lexer_ty.stream_item().unwrap();
				let (token_ty, _error_ty) = lexer_result_ty.as_result_type().unwrap();
				let return_ty = &self.signature().return_types()[0];

				let token_type_path = token_ty.generate(context);
				let result_type_path = return_ty.generate(context);

				quote! {
					pub fn #id<
						L: ::std::iter::Iterator<
							Item = ::std::result::Result<
								::source_span::Loc<#token_type_path>,
								::source_span::Loc<Error>,
							>,
						>,
					>(
						#lexer: &mut L,
					) -> ::std::result::Result<::source_span::Loc<#result_type_path>, ::source_span::Loc<Error>> {
						#body
					}
				}
			}
			Some(function::Marker::DebugFormat) => {
				panic!("TODO debug format")
			}
		}
	}
}