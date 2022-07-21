#[macro_export]
macro_rules! bug {
    ($object:expr, $message:literal) => {
        panic!(
            "Unexpected object: {:?}\n\n\
            Bug in compiler: {}\n\
            We'd appreciate a bug report at: https://github.com/mdgaziur/riglang/issues\n\n\
            Please include this whole message along with the program(s) you've tried to \n\
            compile(if you are trying to compile program).",
            $object, $message
        );
    };
}
