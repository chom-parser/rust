use proc_macro2::TokenStream;
use quote::quote;
use chom_ir::{
	Namespace,
	Expr,
	expr,
	Context
};
use super::{
	Generate,
	GenerateIn,
	Scope
};

impl<T: Namespace> GenerateIn<T> for Box<Expr<T>> {
	fn generate_in(
		&self,
		context: &Context<T>,
		scope: Scope<T>
	) -> TokenStream {
		self.as_ref().generate_in(context, scope)
	}
}

impl<T: Namespace> GenerateIn<T> for Expr<T> {
	fn generate_in(
		&self,
		context: &Context<T>,
		scope: Scope<T>,
	) -> TokenStream {
		match &self {
			Self::Literal(c) => c.generate(context),
			Self::Get(x) => super::var_id(context, Some(scope), *x),
			Self::GetField(x, ty_ref, i) => {
				// let value = value.generate_in(context, scope);
				let x = super::var_id(context, Some(scope), *x);
				let ty = context.ty(*ty_ref).unwrap();
				use chom_ir::ty::Desc;
				match ty.desc() {
					Desc::Opaque => panic!("cannot get field from opaque type"),
					Desc::Enum(_) => panic!("cannot get field from enum type"),
					Desc::Struct(strct) => {
						let f = strct.fields().get(*i as usize).expect("no such field");
						let id = context.id().field_ident(f.id);
						let field = quote::format_ident!("{}", id.to_snake_case());
						quote! { #x . #field }
					},
					Desc::TupleStruct(_) => {
						let n = proc_macro2::Literal::u32_unsuffixed(*i);
						quote! { #x . #n }
					},
					Desc::Lexer => panic!("cannot get field from lexer type")
				}
			}
			Self::Ref(x) => {
				let id = super::var_id(context, Some(scope), *x);
				quote! { &#id }
			}
			Self::RefField(x, ty_ref, i) => {
				let x = super::var_id(context, Some(scope), *x);
				let ty = context.ty(*ty_ref).unwrap();
				use chom_ir::ty::Desc;
				match ty.desc() {
					Desc::Opaque => panic!("cannot get field from opaque type"),
					Desc::Enum(_) => panic!("cannot get field from enum type"),
					Desc::Struct(strct) => {
						let f = strct.fields().get(*i as usize).expect("no such field");
						let id = context.id().field_ident(f.id);
						let field = quote::format_ident!("{}", id.to_snake_case());
						quote! { &#x . #field }
					},
					Desc::TupleStruct(_) => {
						let n = proc_macro2::Literal::u32_unsuffixed(*i);
						quote! { &#x . #n }
					},
					Desc::Lexer => panic!("cannot get field from lexer type")
				}
			}
			Self::Let(v, is_mutable, value, next) => {
				let id = super::var_id(context, Some(scope), *v);
				let value = value.generate_in(context, scope);
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
			Self::Update(v, value, next) => {
				let id = super::var_id(context, Some(scope), *v);
				let value = value.generate_in(context, scope);
				let next = next.generate_in(context, scope);

				quote! {
					#id = #value;
					#next
				}
			}
			Self::Check(x, value, next) => {
				let id = super::var_id(context, Some(scope), *x);
				let value = value.generate_in(context, scope);
				let next = next.generate_in(context, scope);

				quote! {
					let #id = #value?;
					#next
				}
			}
			Self::New(ty_ref, args) => {
				let args = if args.is_empty() {
					None
				} else {
					let args = args.iter().map(|a| a.generate_in(context, scope));
					Some(quote! { (#(#args),*) })
				};

				match ty_ref {
					chom_ir::ty::Ref::Native(n) => {
						use chom_ir::ty::Native;
						match n {
							Native::Unit => quote! { () },
							Native::String => quote! { String::new() },
							Native::Stack => quote! { Vec::new() },
							Native::Position => quote! { ::source_span::Position::default() },
							_ => panic!("cannot instanciate given native type")
						}
					},
					_ => {
						let ty = ty_ref.generate(context);
						quote! { #ty #args }
					}
				}
			}
			Self::Cons(ty_ref, v, args) => {
				let ty = context.ty(*ty_ref).unwrap();
				let variant = ty.as_enum().unwrap().variant(*v).unwrap();
				let variant_id = super::variant_id(context, variant);

				let mut tokens = if super::is_ubiquitous(*ty_ref) {
					quote! { #variant_id }
				} else {
					let ty_path = ty_ref.generate(context);
					quote! { #ty_path :: #variant_id }
				};
				
				if !args.is_empty() {
					let args = args.iter().map(|a| a.generate_in(context, scope));
					tokens.extend(quote! { (#(#args),*) })
				}

				tokens
			}
			Self::Error(err) => err.generate_in(context, scope),
			Self::Heap(expr) => {
				let expr = expr.generate_in(context, scope);
				quote! { Box::new(#expr) }
			}
			Self::If(condition, then_branch, else_branch) => {
				let condition = condition.generate_in(context, scope);
				let then_branch = then_branch.generate_in(context, scope);
				let else_branch = else_branch.generate_in(context, scope);
				quote! {
					if #condition { #then_branch } else { #else_branch }
				}
			}
			Self::Match { expr, cases } => {
				let expr = expr.generate_in(context, scope);
				let cases = cases.iter().map(|c| c.generate_in(context, scope));
				quote! {
					match #expr {
						#(#cases)*
					}
				}
			}
			Self::MatchRef { expr, cases } => {
				let expr = expr.generate_in(context, scope);
				let cases = cases.iter().map(|c| c.generate_in(context, scope));
				quote! {
					match #expr {
						#(#cases)*
					}
				}
			}
			Self::LetMatch(pattern, value, next) => {
				let pattern = pattern.generate(context);
				let value = value.generate_in(context, scope);
				let next = next.generate_in(context, scope);
				quote! {
					if let Some(#pattern) = #value {
						#next
					} else {
						unreachable!()
					}
 				}
			}
			Self::Call(index, args) => {
				let args = args.iter().map(|a| a.generate_in(context, scope));

				// match object {
				// 	Some((object, _)) => {
				// 		let f = context.function(*index).unwrap();
				// 		let id = super::function_id(context, f.signature());
				// 		let object = super::var_id(context, *object);
				// 		quote! { #object . #id ( #(#args),* ) }
				// 	},
				// 	None => {
				// 		let path = super::path(context, context.function_path(*index).unwrap());
				// 		quote! { #path ( #(#args),* ) }
				// 	}
				// }
				
				let path = super::path(context, context.function_path(*index).unwrap());
				quote! { #path ( #(#args),* ) }
			}
			Self::Return(args) => {
				match scope.marker() {
					Some(chom_ir::function::Marker::DebugFormat) => {
						quote! { Ok(()) }
					},
					_ => {
						let expr = args.last().unwrap().generate_in(context, scope);
						if scope.is_in_any_loop() {
							quote! { break #expr }
						} else {
							expr
						}
					}
				}
			}
			Self::TailRecursion { body, label, .. } => {
				let body = body.generate_in(context, scope.begin_loop(*label));
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
				let lexer = super::var_id(context, Some(scope), *lexer);
				use expr::LexerExpr as Expr;
				match expr {
					Expr::IsEmpty => {
						quote! { #lexer.is_empty() }
					}
					Expr::Peek => {
						quote! { #lexer.peek_char() }
					},
					Expr::Buffer => {
						quote! { #lexer.buffer.as_str() }
					}
					Expr::Chars => {
						quote! { #lexer.buffer.chars() }
					},
					Expr::Span => {
						quote! { #lexer.span }
					}
					Expr::Clear(next) => {
						let next = next.generate_in(context, scope);
						quote! { self.clear(); #next }
					},
					Expr::Consume(next) => {
						let next = next.generate_in(context, scope);
						quote! { self.consume_char()?; #next }
					}
				}
			},
			Self::Stream(stream, expr) => {
				let stream = super::var_id(context, Some(scope), *stream);
				use expr::StreamExpr as Expr;
				match expr {
					Expr::Pull(dst, next) => {
						let dst = super::var_id(context, Some(scope), *dst);
						let next = next.generate_in(context, scope);
						quote! { let #dst = #stream.next(); #next }
					}
				}
			}
			Self::Stack(stack, expr) => {
				let stack = super::var_id(context, Some(scope), *stack);
				use expr::StackExpr as Expr;
				match expr {
					Expr::Push(v, q, next) => {
						let v = v.generate_in(context, scope);
						let q = q.generate_in(context, scope);
						let next = next.generate_in(context, scope);
						quote! { #stack.push((#v, #q)); #next }
					},
					Expr::Pop(v, q, next) => {
						let next = next.generate_in(context, scope);
						if v.is_none() && q.is_none() {
							quote! { #stack.pop(); #next }
						} else {
							let v = match v {
								Some(v) => super::var_id(context, Some(scope), *v),
								None => quote! { _ },
							};

							let q = match q {
								Some(q) => super::var_id(context, Some(scope), *q),
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
						let expr = expr.generate_in(context, scope);
						let loc = loc.generate_in(context, scope);
						quote! { ::source_span::Loc::new(#expr, #loc) }
					}
					Expr::FromPosition(expr) => {
						let expr = expr.generate_in(context, scope);
						quote! { #expr . into() }
					}
					Expr::After(expr) => {
						let expr = expr.generate_in(context, scope);
						quote! { #expr . end() }
					}
					Expr::Transpose(expr, default_span) => {
						let expr = expr.generate_in(context, scope);
						let default_span = default_span.generate_in(context, scope);
						quote! {
							::source_span::Loc::transposed(#expr, #default_span)
						}
					}
					Expr::Unwrap(target_value, target_span, value, next) => {
						let target_value = match target_value {
							Some(id) => super::var_id(context, Some(scope), *id),
							None => quote! { _ },
						};
		
						let target_span = match target_span {
							Some(id) => super::var_id(context, Some(scope), *id),
							None => quote! { _ },
						};
		
						let value = value.generate_in(context, scope);
						let next = next.generate_in(context, scope);
						quote! {
							let (#target_value, #target_span) = #value.into_raw_parts();
							#next
						}
					}
					Expr::Merge(a, b) => {
						let a = a.generate_in(context, scope);
						let b = b.generate_in(context, scope);
						quote! {
							#a.union(#b)
						}
					}
				}
			}
			Self::Write(output, string, next) => {
				let output = super::var_id(context, Some(scope), *output);
				let next = next.generate_in(context, scope);
				quote! { write!(#output, #string)?; #next }
			}
			Self::Print(string, next) => {
				let next = next.generate_in(context, scope);
				quote! { eprintln!(#string); #next }
			}
			Self::DebugFormat(output, e, next) => {
				let output = super::var_id(context, Some(scope), *output);
				let value = e.generate_in(context, scope);
				let next = next.generate_in(context, scope);
				quote! { write!(#output, "{:?}", #value)?; #next }
			}
			Self::Unreachable => quote! { unreachable!() }
		}
	}
}

impl<T: Namespace> GenerateIn<T> for expr::MatchCase<T> {
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

impl<T: Namespace> GenerateIn<T> for expr::Error<T> {
	fn generate_in(
		&self,
		context: &Context<T>,
		scope: Scope<T>
	) -> TokenStream {
		match self {
			Self::UnexpectedToken(expr) => {
				let expr = expr.generate_in(context, scope);
				quote! { Error::UnexpectedToken(#expr) }
			}
			Self::UnexpectedNode(expr) => {
				let expr = expr.generate_in(context, scope);
				quote! { Error::UnexpectedNode(#expr) }
			}
		}
	}
}