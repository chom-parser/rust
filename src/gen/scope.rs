use chom_ir::Namespace;

pub struct Scope<T: Namespace + ?Sized> {
	/// Current module.
	module: u32,

	/// Loop label.
	label: Option<T::Label>,

	/// If false, a `return`/`break` statement is introduced
	/// when generating an expression without successor.
	pure: bool,
}

impl<T: Namespace + ?Sized> Clone for Scope<T> {
	fn clone(&self) -> Self {
		Self {
			module: self.module,
			label: self.label,
			pure: self.pure
		}
	} 
}

impl<T: Namespace + ?Sized> Copy for Scope<T> {}

impl<T: Namespace + ?Sized> Scope<T> {
	pub fn new(module: u32) -> Self {
		Self {
			module,
			label: None,
			pure: true,
		}
	}

	pub fn is_pure(&self) -> bool {
		self.pure
	}

	// pub fn label(&self) -> Option<pseudo::expr::Label> {
	// 	self.label
	// }

	pub fn is_in_loop(&self, label: T::Label) -> bool {
		self.label.map(|l| l == label).unwrap_or(false)
	}

	pub fn pure(self) -> Self {
		Self { pure: true, ..self }
	}

	pub fn impure(self, label: T::Label) -> Self {
		Self {
			label: Some(label),
			pure: false,
			..self
		}
	}
}