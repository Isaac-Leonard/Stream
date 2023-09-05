use crate::errors::CompError;

pub struct WithErrors<T: Sized> {
	pub data: T,
	pub errors: Vec<CompError>,
}
impl<T: Sized> WithErrors<T> {
	pub fn collect_errors_into(mut self, errors: &mut Vec<CompError>) -> T {
		errors.append(&mut self.errors);
		self.data
	}
}
