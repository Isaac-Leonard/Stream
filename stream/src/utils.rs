use crate::errors::CompError;

pub struct WithErrors<T: Sized> {
	pub data: T,
	pub errors: Vec<CompError>,
}
impl<T: Sized> WithErrors<T> {
	pub fn new(data: T, errors: Vec<CompError>) -> Self {
		Self { data, errors }
	}
	pub fn collect_errors_into(mut self, errors: &mut Vec<CompError>) -> T {
		errors.append(&mut self.errors);
		self.data
	}

	/// # Panics with the given message if called when there are errors
	pub fn expect(self, message: &str) -> T {
		if !self.errors.is_empty() {
			for error in self.errors {
				eprintln!("{}", error.get_msg_without_lines())
			}
			panic!("{}", message)
		}
		self.data
	}
}
