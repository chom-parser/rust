pub mod gen;

// impl Target for Rust {
// 	type Output = TokenStream;

// 	fn module_filename<P: AsRef<Path>>(
// 		&self,
// 		root: P,
// 		module_path: pseudo::module::Path,
// 	) -> PathBuf {
// 		let mut file_path = root.as_ref().to_path_buf();

// 		for id in module_path {
// 			if let pseudo::module::Id::Named(name) = id {
// 				file_path.push(crate::util::to_snake_case(&name));
// 			}
// 		}

// 		file_path.set_extension("rs");
// 		file_path
// 	}
// }