use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{
	Ids,
	Expr,
	expr,
	Context
};
use super::{
	Generate,
	GenerateIn,
	Scope
};

impl<T: Ids> GenerateIn<T> for expr::BuildArgs<T> {
	fn generate_in(
		&self,
		context: &Context<T>,
		scope: Scope<T>
	) -> TokenStream {
		match self {
			Self::Tuple(args) => {
				if args.is_empty() {
					TokenStream::new()
				} else {
					let args = args.iter().map(|a| a.generate_in(context, scope));
					quote! { ( #(#args),* ) }
				}
			}
			Self::Struct(bindings) => {
				let bindings = bindings.iter().map(|b| {
					let field = super::field_id(context, b.id);
					let expr = b.expr.generate_in(context, scope);
					quote! { #field: #expr }
				});
				quote! { { #(#bindings),* } }
			}
		}
	}
}

impl<T: Ids> GenerateIn<T> for Box<Expr<T>> {
	fn generate_in(
		&self,
		context: &Context<T>,
		scope: Scope<T>
	) -> TokenStream {
		self.as_ref().generate_in(context, scope)
	}
}

impl<T: Ids> GenerateIn<T> for Expr<T> {
	fn generate_in(
		&self,
		context: &Context<T>,
		scope: Scope<T>,
	) -> TokenStream {
		let expr = match &self {
			Self::Literal(c) => c.generate(context),
			Self::Get(x) => x.generate(context),
			Self::GetField(value, ty_ref, i) => {
				let value = value.generate_in(context, scope.pure());
				let ty = context.ty(*ty_ref).unwrap();
				use chom_ir::ty::Desc;
				match ty.desc() {
					Desc::Opaque => panic!("cannot get field from opaque type"),
					Desc::Enum(_) => panic!("cannot get field from enum type"),
					Desc::Struct(strct) => {
						let f = strct.fields().get(*i as usize).expect("no such field");
						let id = context.id().field_ident(f.id);
						let field = quote::format_ident!("{}", id.to_snake_case());
						quote! { #value . #field }
					},
					Desc::TupleStruct(_) => {
						let n = proc_macro2::Literal::u32_unsuffixed(*i);
						quote! { #value . #n }
					}
				}
			}
			Self::Let(v, is_mutable, value, next) => {
				let id = super::var_id(context, *v);
				let value = value.generate_in(context, scope.pure());
				let next = next.generate_in(context, scope);

				let mutable = if *is_mutable {
					Some(quote! { mut })
				} else {
					None
				};

				quote! {
					let #mutable #id = #value;
					#next
				}
			}
			Self::New(ty, args) => {
				let ty = ty.generate(context);
				let args = args.generate_in(context, scope.pure());
				quote! { #ty #args }
			}
			Self::Cons(ty_ref, v, args) => {
				let ty = context.ty(*ty_ref).unwrap();
				let variant = ty.as_enum().unwrap().variant(*v).unwrap();
				let variant_id = super::variant_id(context, variant);
				let ty_path = ty_ref.generate(context);
				let args = args.generate_in(context, scope.pure());
				quote! { #ty_path :: #variant_id #args }
			}
			Self::Error(err) => err.generate_in(context, scope.pure()),
			Self::Heap(expr) => {
				let expr = expr.generate_in(context, scope.pure());
				quote! { Box::new(#expr) }
			}
			Self::Match { expr, cases } => {
				let expr = expr.generate_in(context, scope.pure());
				let cases = cases.iter().map(|c| c.generate_in(context, scope));
				quote! {
					match #expr {
						#(#cases)*
					}
				}
			}
			Self::LetMatch(pattern, value, next) => {
				let pattern = pattern.generate(context);
				let value = value.generate_in(context, scope.pure());
				let next = next.generate_in(context, scope);
				quote! {
					if let Some(#pattern) = #value {
						#next
					} else {
						unreachable!()
					}
 				}
			}
			Self::TailRecursion { body, label, .. } => {
				let body = body.generate_in(context, scope.impure(*label));
				let label = super::label_id(context, *label);
				quote! {
					#label: loop {
						#body
					}
				}
			}
			Self::Recurse(label, _) => {
				if scope.is_in_loop(*label) {
					quote! {}
				} else {
					let label = super::label_id(context, *label);
					quote! { continue #label }
				}
			}
			Self::Lexer(lexer, expr) => {
				let lexer = lexer.generate(context);
				use expr::LexerExpr as Expr;
				match expr {
					Expr::Peek => {
						quote! { #lexer.peek_char()? }
					},
					Expr::Parse(f_index) => {
						let f_path = super::path(context.function_path(*f_index).unwrap());
						quote! { #f_path ( #lexer.buffer.as_str() ) }
					},
					Expr::Chars => {
						quote! { #lexer.buffer.chars() }
					},
					Expr::Clear(next) => {
						let next = next.generate_in(context, scope);
						quote! { self.clear(); #next }
					},
					Expr::Consume(next) => {
						let next = next.generate_in(context, scope);
						quote! { self.consume()?; #next }
					}
				}
			},
			Self::Stream(stream, expr) => {
				let stream = stream.generate(context);
				use expr::StreamExpr as Expr;
				match expr {
					Expr::Pull(dst, next) => {
						let dst = super::var_id(context, *dst);
						let next = next.generate_in(context, scope);
						quote! { let #dst = #stream.next(); #next }
					}
				}
			}
			Self::Stack(stack, expr) => {
				let stack = stack.generate(context);
				use expr::StackExpr as Expr;
				match expr {
					Expr::Push(v, q, next) => {
						let v = v.generate_in(context, scope.pure());
						let q = q.generate_in(context, scope.pure());
						let next = next.generate_in(context, scope);
						quote! { #stack.push((#v, #q)); #next }
					},
					Expr::Pop(v, q, next) => {
						let next = next.generate_in(context, scope);
						if v.is_none() && q.is_none() {
							quote! { #stack.pop(); #next }
						} else {
							let v = match v {
								Some(v) => super::var_id(context, *v),
								None => quote! { _ },
							};

							let q = match q {
								Some(q) => super::var_id(context, *q),
								None => quote! { _ },
							};

							quote! {
								let (#v, #q) = #stack.pop().unwrap();
								#next
							}
						}
					}
				}
			}
			Self::Span(expr) => {
				use expr::SpanExpr as Expr;
				match expr {
					Expr::Locate(expr, loc) => {
						let expr = expr.generate_in(context, scope.pure());
						let loc = loc.generate_in(context, scope.pure());
						quote! { ::source_span::Loc::new(#expr, #loc) }
					}
					Expr::FromPosition(expr) => {
						let expr = expr.generate_in(context, scope.pure());
						quote! { #expr . into() }
					}
					Expr::After(expr) => {
						let expr = expr.generate_in(context, scope.pure());
						quote! { #expr . end() }
					}
					Expr::Transpose(expr, default_span) => {
						let expr = expr.generate_in(context, scope.pure());
						let default_span = default_span.generate_in(context, scope.pure());
						quote! {
							::source_span::Loc::transposed(#expr, #default_span)
						}
					}
					Expr::Unwrap(target_value, target_span, value, next) => {
						let target_value = match target_value {
							Some(id) => super::var_id(context, *id),
							None => quote! { _ },
						};
		
						let target_span = match target_span {
							Some(id) => super::var_id(context, *id),
							None => quote! { _ },
						};
		
						let value = value.generate_in(context, scope.pure());
						let next = next.generate_in(context, scope);
						quote! {
							let (#target_value, #target_span) = #value.into_raw_parts();
							#next
						}
					}
					Expr::Merge(a, b) => {
						let a = a.generate_in(context, scope.pure());
						let b = b.generate_in(context, scope.pure());
						quote! {
							#a.union(#b)
						}
					}
				}
			}
			Self::Write(name, args) => {
				let args_exprs = args.iter().map(|a| a.generate_in(context, scope.pure()));
				let mut format_string = name.clone();
				if !args.is_empty() {
					format_string.push('(');
					for i in 0..args.len() {
						if i > 0 {
							format_string.push_str(", ");
						}
						format_string.push_str("{:?}")
					}
					format_string.push(')');
				}
				quote! { write!(f, #format_string, #(#args_exprs),*) }
			}
			Self::Unreachable => quote! { unreachable!() }
		};

		if scope.is_pure() || self.is_continued() {
			expr
		} else {
			quote! { break #expr }
		}
	}
}

impl<T: Ids> GenerateIn<T> for expr::MatchCase<T> {
	fn generate_in(
		&self,
		context: &Context<T>,
		scope: Scope<T>
	) -> TokenStream {
		let pattern = self.pattern.generate(context);
		let expr = self.expr.generate_in(context, scope);
		quote! { #pattern => { #expr } }
	}
}

impl<T: Ids> GenerateIn<T> for expr::Error<T> {
	fn generate_in(
		&self,
		context: &Context<T>,
		scope: Scope<T>
	) -> TokenStream {
		match self {
			Self::UnexpectedChar(expr) => {
				let expr = expr.generate_in(context, scope.pure());
				let extern_module_path = super::path(context.extern_module_path());
				quote! { #extern_module_path::unexpected(#expr) }
			}
			Self::UnexpectedToken(expr) => {
				let expr = expr.generate_in(context, scope.pure());
				quote! { Error::UnexpectedToken(#expr) }
			}
			Self::UnexpectedNode(expr) => {
				let expr = expr.generate_in(context, scope.pure());
				quote! { Error::UnexpectedNode(#expr) }
			}
		}
	}
}

impl<T: Ids> Generate<T> for expr::Var<T> {
	fn generate(
		&self,
		context: &Context<T>
	) -> TokenStream {
		match self {
			Self::This => quote! { self },
			Self::Defined(id) => super::var_id(context, *id)
		}
	}
}