#[macro_export]
macro_rules! map_vec {
    ($vector:ident, $map:expr) => {
        $vector.iter().map($map).collect::<Vec<_>>()
    };
}
