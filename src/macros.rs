#[macro_export]
macro_rules! map_vec {
    ($vector:expr, $map:expr) => {
        $vector.iter().map($map).collect::<Vec<_>>()
    };
}

#[macro_export]
macro_rules! extract {
    ($exp:expr, $variant:path) => {
        match $exp {
            $variant(value) => Some(value),
            _ => None,
        }
    };
}

#[macro_export]
macro_rules! extract_or {
    ($exp:expr, $variant:path, $or:expr) => {
        match $exp {
            $variant(value) => Ok(value),
            _ => Err($or),
        }
    };
}

#[macro_export]
macro_rules! extract_into {
    ($exp:expr, $variant:path, $map:path) => {
        match $exp {
            $variant(value) => Some($map(value)),
            _ => None,
        }
    };
}

#[macro_export]
macro_rules! extract_into_or {
    ($exp:expr, $variant:path, $map:path, $or:expr) => {
        match $exp {
            $variant(value) => $map(value),
            _ => $or,
        }
    };
}
