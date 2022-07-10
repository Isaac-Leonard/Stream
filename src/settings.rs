#[derive(Clone, Debug, PartialEq)]
pub struct Settings {
	pub print_llvm: bool,
	pub skip_optimizations: bool,
	pub call_linker: bool,
	pub input_name: String,
	pub object_name: String,
}
