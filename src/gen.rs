use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{Ids, Context};

mod scope;
mod constant;
mod expr;
mod ty;
mod pattern;

pub use scope::Scope;

/// Generate code inside a scope.
pub trait Generate<T: Ids> {
	fn generate(&self, context: &Context<T>) -> TokenStream;
}

/// Generate code inside a scope.
pub trait GenerateIn<T: Ids> {
	fn generate_in(&self, context: &Context<T>, scope: Scope<T>) -> TokenStream;
}

fn path<T: Ids>(path: chom_ir::module::Path<'_, T>) -> TokenStream {
	panic!("TODO")
}

fn type_id<T: Ids>(context: &Context<T>, id: chom_ir::ty::Id<T>) -> TokenStream {
	match id {
		chom_ir::ty::Id::Native(n) => {
			use chom_ir::ty::Native;
			match n {
				Native::Unit => quote! { () },
				Native::Option => quote! { Option },
				Native::List => quote! { Vec },
				Native::Stack => quote! { Vec },
				Native::Heap => quote! { Box },
				Native::Loc => quote! { ::source_span::Loc },
				Native::Lexer => quote! { L }
			}
		},
		chom_ir::ty::Id::Defined(id) => {
			let ident = context.id().type_ident(id);
			let id = quote::format_ident!("{}", ident.to_caml_case());
			quote! { #id }
		}
	}
}

fn field_id<T: Ids>(context: &Context<T>, id: T::Field) -> TokenStream {
	let ident = context.id().field_ident(id);
	let id = quote::format_ident!("{}", ident.to_snake_case());
	quote! { #id }
}

fn variant_id<T: Ids>(context: &Context<T>, v: &chom_ir::ty::Variant<T>) -> TokenStream {
	use chom_ir::ty::Variant;
	match v {
		Variant::Native(n) => {
			use chom_ir::ty::NativeVariant;
			match n {
				NativeVariant::None => quote! { None },
				NativeVariant::Some => quote! { Some },
				NativeVariant::Ok => quote! { Ok },
				NativeVariant::Err => quote! { Err }
			}
		},
		Variant::Defined(id, _) => {
			let ident = context.id().variant_ident(*id);
			let id = quote::format_ident!("{}", ident.to_caml_case());
			quote! { #id }
		}
	}
}

fn var_id<T: Ids>(context: &Context<T>, id: T::Var) -> TokenStream {
	let ident = context.id().var_ident(id);
	let id = quote::format_ident!("{}", ident.to_snake_case());
	quote! { #id }
}

fn label_id<T: Ids>(context: &Context<T>, id: T::Label) -> TokenStream {
	let ident = context.id().label_ident(id);
	let lft = syn::Lifetime::new(&format!("'{}", ident.to_snake_case()), proc_macro2::Span::call_site());
	quote! { #lft }
}

// impl Rust {
	// fn module_id(&self, id: &pseudo::module::Id) -> TokenStream {
	// 	match id {
	// 		pseudo::module::Id::Root => quote! { crate },
	// 		pseudo::module::Id::Named(name) => {
	// 			let id = quote::format_ident!("{}", crate::util::to_snake_case(&name));
	// 			quote! { #id }
	// 		}
	// 	}
	// }

	// fn type_id(&self, id: &pseudo::ty::Id) -> TokenStream {
	// 	use pseudo::ty::Id;
	// 	match id {
	// 		Id::BuiltIn(ty) => {
	// 			use pseudo::built_in::Type;
	// 			match ty {
	// 				Type::Token => quote! { Token },
	// 				Type::Keyword => quote! { Keyword },
	// 				Type::Operator => quote! { Operator },
	// 				Type::Delimiter => quote! { Delimiter },
	// 				Type::Punct => quote! { Punct },
	// 				Type::Node => quote! { Node },
	// 				Type::Item => quote! { Item },
	// 			}
	// 		}
	// 		Id::Defined(ident) => {
	// 			let id = quote::format_ident!("{}", ident.to_caml_case());
	// 			quote! { #id }
	// 		}
	// 	}
	// }

	// fn monomorphic_type_id(&self, context: &Context, index: mono::Index) -> TokenStream {
	// 	let name = context
	// 		.grammar()
	// 		.ty(index)
	// 		.unwrap()
	// 		.composed_id(context.grammar());
	// 	let id = quote::format_ident!("{}", name.to_caml_case());
	// 	quote! { #id }
	// }

	// fn defined_variant_id(&self, id: &Ident) -> TokenStream {
	// 	let id = quote::format_ident!("{}", id.to_caml_case());
	// 	quote! { #id }
	// }

	// fn field_id(&self, name: &Ident) -> TokenStream {
	// 	let id = quote::format_ident!("{}", name.to_snake_case());
	// 	quote! { #id }
	// }

	// fn is_member(&self, id: pseudo::Id) -> bool {
	// 	use pseudo::{id, Id};
	// 	match id {
	// 		Id::Lexer(id::Lexer::Buffer) | Id::Lexer(id::Lexer::Span) => true,
	// 		_ => false,
	// 	}
	// }

	// fn is_mutable(&self, id: pseudo::Id) -> bool {
	// 	use pseudo::{id, Id};
	// 	match id {
	// 		Id::Lexer(id::Lexer::State)
	// 		| Id::Parser(id::Parser::Position)
	// 		| Id::Parser(id::Parser::Stack)
	// 		| Id::Parser(id::Parser::AnyNodeOpt)
	// 		| Id::Parser(id::Parser::State) => true,
	// 		_ => false,
	// 	}
	// }

	// fn module_path(&self, path: pseudo::module::Path) -> TokenStream {
	// 	let mut tokens = TokenStream::new();

	// 	for id in path {
	// 		if !tokens.is_empty() {
	// 			tokens.extend(quote! { :: })
	// 		}
	// 		tokens.extend(self.module_id(id))
	// 	}

	// 	tokens
	// }

	// fn generate_lexer_definition(&self) -> TokenStream {
	// 	quote! {
	// 		/// Lexer.
	// 		pub struct Lexer<I: Iterator, M> {
	// 			/// Source character stream.
	// 			source: ::std::iter::Peekable<I>,

	// 			/// Token buffer.
	// 			buffer: String,

	// 			/// Character metrics.
	// 			metrics: M,

	// 			/// Token span.
	// 			span: ::source_span::Span
	// 		}
	// 	}
	// }

	// fn generate_parser_definition(&self, context: &Context) -> TokenStream {
	// 	let extern_module_path = self.module_path(context.extern_module_path());
	// 	let lexer_module_path = self.module_path(context.lexer_module_path());
	// 	quote! {
	// 		/// Parsing errors.
	// 		pub enum Error {
	// 			/// Error comming from the lexer.
	// 			Lexer(#extern_module_path::Error),

	// 			/// Unexpected lexer token.
	// 			UnexpectedToken(Option<#lexer_module_path::Token>),

	// 			/// Unexpected AST node.
	// 			UnexpectedNode(Node)
	// 		}

	// 		impl From<#extern_module_path::Error> for Error {
	// 			fn from(e: #extern_module_path::Error) -> Self {
	// 				Self::Lexer(e)
	// 			}
	// 		}
	// 	}
	// }

	// fn generate_lexer_impl(
	// 	&self,
	// 	context: &Context,
	// 	scope: Scope,
	// 	expr: &pseudo::Expr,
	// ) -> TokenStream {
	// 	let extern_module_path = self.module_path(context.extern_module_path());
	// 	let body = self.generate_in(context, scope, expr);

	// 	quote! {
	// 		impl<
	// 			E: Into<#extern_module_path::Error>,
	// 			I: Iterator<Item = Result<char, E>>,
	// 			M: ::source_span::Metrics,
	// 		> Lexer<I, M> {
	// 			fn peek_char(
	// 				&mut self
	// 			) -> Result<
	// 				Option<char>,
	// 				::source_span::Loc<#extern_module_path::Error>,
	// 			> {
	// 				match self.source.peek() {
	// 					Some(Ok(c)) => Ok(Some(*c)),
	// 					Some(Err(_)) => Err(self.consume_char().unwrap_err()),
	// 					None => Ok(None),
	// 				}
	// 			}

	// 			fn consume_char(
	// 				&mut self,
	// 			) -> Result<(), ::source_span::Loc<#extern_module_path::Error>> {
	// 				match self.source.next() {
	// 					Some(Ok(c)) => {
	// 						self.buffer.push(c);
	// 						self.span.push(c, &self.metrics);
	// 						Ok(())
	// 					}
	// 					Some(Err(e)) => Err(::source_span::Loc::new(e.into(), self.span.end().into())),
	// 					None => Ok(()),
	// 				}
	// 			}

	// 			fn next_token(
	// 				&mut self,
	// 			) -> Result<
	// 				Option<::source_span::Loc<Token>>,
	// 				::source_span::Loc<#extern_module_path::Error>,
	// 			> {
	// 				#body
	// 			}
	// 		}

	// 		impl<
	// 			E: Into<#extern_module_path::Error>,
	// 			I: Iterator<Item = Result<char, E>>,
	// 			M: ::source_span::Metrics,
	// 		> Iterator for Lexer<I, M> {
	// 			type Item = Result<
	// 				::source_span::Loc<Token>,
	// 				::source_span::Loc<#extern_module_path::Error>,
	// 			>;

	// 			fn next(&mut self) -> Option<Self::Item> {
	// 				self.next_token().transpose()
	// 			}
	// 		}
	// 	}
	// }

	// fn generate_parser(
	// 	&self,
	// 	context: &Context,
	// 	scope: Scope,
	// 	index: mono::Index,
	// 	parser: &Parser,
	// ) -> TokenStream {
	// 	let ty = context.grammar().ty(index).unwrap();
	// 	let id = quote::format_ident!(
	// 		"parse_{}",
	// 		ty.composed_id(context.grammar()).to_snake_case()
	// 	);

	// 	let token_type_path = self.generate(
	// 		context,
	// 		&pseudo::ty::Ref::BuiltIn(pseudo::built_in::Type::Token),
	// 	);
	// 	let result_type_path = self.generate(context, &context.type_expr(index));

	// 	match parser {
	// 		Parser::LR0(expr) => {
	// 			let body = self.generate_in(context, scope, expr);
	// 			quote! {
	// 				pub fn #id<
	// 					L: ::std::iter::Iterator<
	// 						Item = ::std::result::Result<
	// 							::source_span::Loc<#token_type_path>,
	// 							::source_span::Loc<Error>,
	// 						>,
	// 					>,
	// 				>(
	// 					lexer: &mut L,
	// 				) -> ::std::result::Result<::source_span::Loc<#result_type_path>, ::source_span::Loc<Error>> {
	// 					#body
	// 				}
	// 			}
	// 		}
	// 		Parser::LALR1(expr) => {
	// 			unimplemented!()
	// 		}
	// 	}
	// }

	// fn generate_debug_formatter(
	// 	&self,
	// 	context: &Context,
	// 	scope: Scope,
	// 	ty_ref: pseudo::ty::Ref,
	// 	expr: &pseudo::Expr,
	// ) -> TokenStream {
	// 	let ty = context.ty(ty_ref).unwrap();
	// 	let id = self.generate(context, ty.id());

	// 	let mut args_def = None;
	// 	let mut args = None;

	// 	if !ty.parameters().is_empty() {
	// 		let mut defs = Vec::new();
	// 		let mut uses = Vec::new();
	// 		for p in ty.parameters() {
	// 			let id = quote::format_ident!("{}", p.to_caml_case());
	// 			defs.push(quote! { #id: std::fmt::Debug });
	// 			uses.push(quote! { #id });
	// 		}

	// 		args_def = Some(quote! { <#(#defs),*> });
	// 		args = Some(quote! { <#(#uses),*> });
	// 	}

	// 	let body = self.generate_in(context, scope, expr);

	// 	quote! {
	// 		impl #args_def std::fmt::Debug for #id #args {
	// 			fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
	// 				#body
	// 			}
	// 		}
	// 	}
	// }
// }

