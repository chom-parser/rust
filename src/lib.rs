use std::path::{
	Path,
	PathBuf
};

pub mod gen;

pub fn module_filename<P: AsRef<Path>, T: chom_ir::Ids>(namepsace: &T, root: P, module_path: chom_ir::Path<T>) -> PathBuf {
	let mut file_path = root.as_ref().to_path_buf();

	for segment in module_path {
		match segment {
			chom_ir::path::Segment::Module(id) => {
				if let chom_ir::module::Id::Named(id) = id {
					let ident = namepsace.module_ident(*id);
					file_path.push(ident.to_snake_case());
				}
			},
			_ => break
		}
	}

	file_path.set_extension("rs");
	file_path
}

pub fn generate_module<T: chom_ir::Ids>(context: &chom_ir::Context<T>, module: &chom_ir::Module<T>) -> proc_macro2::TokenStream {
	use gen::Generate;
	module.generate(context)
}