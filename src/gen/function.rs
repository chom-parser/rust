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

impl<T: Namespace + ?Sized> GenerateIn<T> for Function<T> {
	fn generate_in(&self, context: &Context<T>, scope: Scope<T>) -> TokenStream {
		let body = match self.body() {
			Some(expr) => expr.generate_in(context, scope),
			None => quote! { unimplemented!() }
		};
		use function::Signature;
		let id = super::function_id(context, self.signature());
		match self.signature() {
			Signature::ExternParser(_, _) => TokenStream::new(), // TODO generate a dummy function.
			Signature::UndefinedChar(_, error_type) => {
				let error_type = error_type.generate(context);
				quote! { pub fn #id (c: Option<char>) -> #error_type { #body } }
			}
			Signature::Lexer(_, error_type) => {
				let error_type = error_type.generate(context);
				// let extern_module_path = super::path(context, context.extern_module_path());
				quote! {
					impl<
						E: Into<#error_type>,
						I: Iterator<Item = Result<char, E>>,
						M: ::source_span::Metrics,
					> Lexer<I, M> {
						/// Peeks the next character without consuming it.
						fn peek_char(
							&mut self
						) -> Result<
							Option<char>,
							::source_span::Loc<#error_type>,
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
						) -> Result<(), ::source_span::Loc<#error_type>> {
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
							::source_span::Loc<#error_type>,
						> {
							#body
						}
					}

					impl<
						E: Into<#error_type>,
						I: Iterator<Item = Result<char, E>>,
						M: ::source_span::Metrics,
					> Iterator for Lexer<I, M> {
						type Item = Result<
							::source_span::Loc<Token>,
							::source_span::Loc<#error_type>,
						>;

						fn next(&mut self) -> Option<Self::Item> {
							self.#id().transpose()
						}
					}
				}
			},
			Signature::Parser(lexer, token_type, result_type) => {
				let lexer = super::var_id(context, *lexer);
				let token_type_path = token_type.generate(context);
				let result_type_path = result_type.generate(context);

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
		}
	}
}