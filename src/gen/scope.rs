use chom_ir::Namespace;

pub struct Scope<T: Namespace + ?Sized> {
	/// Current module.
	module: u32,

	/// Loop label.
	label: Option<T::Label>,

	/// `self` variable.
	this: Option<T::Var>,

	/// Function marker.
	function_marker: Option<chom_ir::function::Marker>
}

impl<T: Namespace + ?Sized> Clone for Scope<T> {
	fn clone(&self) -> Self {
		Self {
			module: self.module,
			label: self.label,
			this: self.this,
			function_marker: self.function_marker
		}
	} 
}

impl<T: Namespace + ?Sized> Copy for Scope<T> {}

impl<T: Namespace + ?Sized> Scope<T> {
	pub fn new(module: u32) -> Self {
		Self {
			module,
			label: None,
			this: None,
			function_marker: None
		}
	}

	// pub fn label(&self) -> Option<pseudo::expr::Label> {
	// 	self.label
	// }

	pub fn set_this(&mut self, this: Option<T::Var>) {
		self.this = this
	}

	pub fn with_this(self, this: T::Var) -> Self {
		Self {
			this: Some(this),
			..self
		}
	}

	pub fn set_marker(&mut self, marker: Option<chom_ir::function::Marker>) {
		self.function_marker = marker
	}

	pub fn with_marker(self, marker: chom_ir::function::Marker) -> Self {
		Self {
			function_marker: Some(marker),
			..self
		}
	}

	pub fn this(&self) -> Option<T::Var> {
		self.this
	}

	pub fn marker(&self) -> Option<chom_ir::function::Marker> {
		self.function_marker
	}

	pub fn is_in_loop(&self, label: T::Label) -> bool {
		self.label.map(|l| l == label).unwrap_or(false)
	}

	pub fn is_in_any_loop(&self) -> bool {
		self.label.is_some()
	}

	pub fn begin_loop(self, label: T::Label) -> Self {
		Self {
			label: Some(label),
			..self
		}
	}
}