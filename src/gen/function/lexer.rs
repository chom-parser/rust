use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{
	Namespace,
	Context,
	Function
};
use crate::gen::Generate;

pub fn generate<T: Namespace>(context: &Context<T>, f: &Function<T>, body: TokenStream) -> TokenStream {
	let return_ty = &f.signature().return_types()[1];
	let (token_opt_ty, error_ty) = return_ty.as_result_type().unwrap();
	let token_ty = token_opt_ty.some_type().unwrap();

	let locate = error_ty.is_loc();

	let raw_error_ty = if locate {
		error_ty.located_type().unwrap()
	} else {
		&error_ty
	};

	let return_ty = return_ty.generate(context);
	let token_ty = token_ty.generate(context);
	let error_ty = error_ty.generate(context);
	let raw_error_ty = raw_error_ty.generate(context);

	// Lexer type parameters.
	let (ty_params_def, ty_params) = if locate {
		(
			quote! {
				E: Into<#raw_error_ty>,
				I: Iterator<Item = Result<char, E>>,
				M: ::source_span::Metrics,
			},
			quote! { I, M }
		)
	} else {
		(
			quote! {
				E: Into<#error_ty>,
				I: Iterator<Item = Result<char, E>>
			},
			quote! { I }
		)
	};

	// Consume char body.
	let consume_body = if locate {
		quote! {
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
	} else {
		quote! {
			match self.source.next() {
				Some(Ok(c)) => {
					self.buffer.push(c);
					Ok(())
				}
				Some(Err(e)) => Err(e.into()),
				None => Ok(()),
			}
		}
	};

	let clear_body = if locate {
		quote! {
			self.buffer.clear();
			self.span.clear()
		}
	} else {
		quote! {
			self.buffer.clear()
		}
	};

	// Full definition.
	quote! {
		impl<#ty_params_def> Lexer<#ty_params> {
			/// Checks if the buffer is empty.
			fn is_empty(&self) -> bool {
				self.buffer.is_empty()
			}

			/// Peeks the next character without consuming it.
			fn peek_char(
				&mut self
			) -> Result<
				Option<char>,
				#error_ty,
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
			) -> Result<(), #error_ty> {
				#consume_body
			}

			/// Clears the lexer buffer.
			fn clear(&mut self) {
				#clear_body
			}

			/// Parses the next token.
			fn next_token(
				&mut self,
			) -> #return_ty {
				#body
			}
		}

		impl<#ty_params_def> Iterator for Lexer<#ty_params> {
			type Item = Result<#token_ty, #error_ty>;

			fn next(&mut self) -> Option<Self::Item> {
				self.next_token().transpose()
			}
		}
	}
}