impl Generate<pseudo::module::Id> for Rust {
	fn generate(&self, _: &pseudo::Context, id: &pseudo::module::Id) -> TokenStream {
		self.module_id(id)
	}
}

impl Generate<pseudo::Module> for Rust {
	fn generate(&self, context: &pseudo::Context, module: &pseudo::Module) -> TokenStream {
		use pseudo::module::Role;
		use pseudo::Routine;

		let scope = Scope::new(module.index());

		let roles = module.roles().map(|r| match r {
			Role::Lexer => self.generate_lexer_definition(),
			Role::ParserRoot => self.generate_parser_definition(context),
			_ => TokenStream::new(),
		});

		let types = module.types().map(|t| {
			let ty = context.ty(t).unwrap();
			self.generate(context, ty)
		});

		let routines = module.routines().map(|r| match r {
			Routine::Lexer(expr) => self.generate_lexer_impl(context, scope, expr),
			Routine::Parser(index, parser) => self.generate_parser(context, scope, *index, parser),
			Routine::Format(r, expr) => self.generate_debug_formatter(context, scope, *r, expr),
		});

		let submodules = module.modules().map(|index| {
			let m = context.module(index).unwrap();
			if !m.is_extern() {
				let id = self.generate(context, m.id());
				let inner = self.generate(context, m);
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
			#(#roles)*
			#(#types)*
			#(#routines)*
			#(#submodules)*
		}
	}
}