#[macro_export]
macro_rules! bug {
    ($object:expr, $message:literal) => {
        panic!(
            "Bug in compiler: {}\n\
            Unexpected Object: \n{:#?}",
            $message, $object
        );
    };
}
