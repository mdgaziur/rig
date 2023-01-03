# The RIG Programming Language

RIG is a statically typed general purpose programming language that
focuses on usability and performance.

## Target

- Designing the syntax
  - As much context free as possible
  - Less verbose
- RIG compiler
  - LLVM IR
  - RIG Bytecode
- RIG VM
  - JIT(LLVM)
  - No more than 256 instructions
  - Possible to use as a library(wrapping the VM)
- RIG standard library
- Concurrency primitives
- Embedding RIG in Rust
- Embedding RIG in languages other than Rust through FFI
- Possibly self-host the compiler
- Built In Macros(for now)

## Syntax

- Everything is an expression(but doesn't necessarily return a non-empty value)
- Blocks return the evaluated value of trailing expression
  if it has no semicolon

```rust
// Use statements can be used to import types, traits and static variables
// from other modules. 
use std::traits {Debug, Display};
use std::alloc::VecDeque;

// Mod statements can be used to declare modules. If there isn't a body,
// rigc will search for rig file with same name in the same directory.
mod test {
    pub testfunc: fn() {}
};
mod test2;

// FFI
// Specify the target language and shared object file. If the symbols
// are declared by a wrapper for the VM, then don't provide the name for
// shared object file.
extern "Rust" "libtest.so" {
    testfunc: fn();
};

// Documentations can be written inside /? ?/ blocks. IDEs
// should count that as the documentation for the static variable or
// variable declared after it. IDEs should also treat the content
// inside the block as markdown. If there are no static variable or
// variable declared after this, IDEs should treat the
// comment as a generic multiline comment. Also, they should display
// a warning about that.
//
// Multiline comments can be made using /* */
//
// Module documentations use /! !/ instead and are located in the
// top.

/*
 * 
 * This is a static variable. The syntax for defining a static variable is:
 * `ident ":" expr ";"`
 * 
 * static variables can be declared in module top level and block
 * statements.
 * 
 * static variables and structs can be made available to importers
 * by adding the 'pub' keyword before the name.
 */
SOME_STATIC_VARIABLE: 1;

/*
 * static variables are also used to declare functions.
 *
 */
main: fn() -> i32 {
    // Variables are declared using the `let` keyword. They can
    // only be declared in block statements. Mutable variables
    // can be declared using the `mut` keyword
    let n = 1;
    
    // Function calls require explicit argument names.
    // If the passed variable has the same name as the argument,
    // it is allowed to implicitly use the argument name.
    // Following is not allowed:
    // add(a: b, b: a)
    // Here, a variable called 'b' is passed to 'add' as
    // argument 'a' while 'a' is passed as 'b'. A good
    // solution is to rename the local variables.
    // Forward declarations are supported in this langauge.
    let res = add(a: 100, b: 200);
    
    // Format strings are supported. They have 'f' prefix.
    println(value: f"{res}");
    
    for x: 1..=10 {
        println(value: f"{x}");
    };
    
    // Infinite loop(can be controlled using break and continue)
    loop {
        
    };
    
    // While loop
    while false {
    
    };
    
    if true {
        // do something
    } else if false {
        // do something
    } else {
        // do something
    };
    
    let x = 2;
    
    // Match statements are similar to Rust's match statement.
    // These must be exhaustive.
    match x {
        2 => {
            println(value: "Value is 2 as expected");
        }
        _ => {
            panic(message: "Something is very wrong and I can sense it.");
        }
    };
    
    // Scoped threads are similar to Rust's scoped threads.
    // Every statement in this block must be a function call;
    let threads = scoped {
        thread1();
        thread2();
        thread3();
    };
    threads.join();
    
    // It is also possible to use Tasks. They start executing immediately
    // when called. You can poll them to know their progress.
    let t1 = task1();
    
    /* do stuff while the task does it's job */
    
    let res = await t1();
    
    // Here 'r' will have the '!' type as the typechecker can prove
    // that the program will never exit the loop unless terminated.
    let r = loop {};
};

/*
 * Contrary to how functions are declared,
 * struct types are declared using the struct keyword.
 * This is because fn() returns a value. But struct types
 * are, well, types. Types can't be static variables, values can be.
 * That is why there is `struct` keyword.
 *
 * Structs can implement traits to implement shared behaviour.
 * This can be done by using `impl Trait for Struct {}`. Then you'll
 * have to implement necessary functions. Traits can derive methods from
 * other traits.
 */
struct Square {
    height: i32,
    width: i32,
};

/*
 * Just like rust, `impl` can be used to declare
 * associated functions and methods for structs.
 * Methods take the `self` argument. Associated functions
 * don't.
 */
impl Square {
    pub new: fn(i32 height, i32 width) -> Self {
        Self {
            height: 10,
            width: 20,
        }
    };
};

impl Rectangle for Square {
    area: fn(self) -> f32 {
        (self.height * self.width) as f32
    }
    
    debug_rectangle: fn(self) {
        println(value: f"Rectangle: Height: {self.height}, \
                  Width: {self.width}");
    };
};

trait Rectangle: Debug {
    area: fn(self) -> f32;
    debug_rectangle: fn(self);
    
    debug: fn(self) {
        debug_rectangle();
    };
};

/* 
 * Function areguments can me mutable and immutable.
 * Immutable bindings don't require any annotations.
 * But mutable bindings require the keyword 'mut' before
 * type name.
 */
add: fn(i32 a, i32 b) -> i32 {
    // Expressions at the end of a block statement
    // without trailing semicolon are treated as implicit
    // returns 
    a + b
};

/*
 * Generics can be used on functions and structs.
 * The traits can be specified like the following.
 */
factorial: fn<T: Subtract + From<i32> + Multiply>(T n) -> T {
    // Here, turbofish is used to pass type.
    // This can be done implicitly. This is used here for
    // demonstration.
    factorial::<T>(n: n - T::from(1)) * n
};

// Struct that takes 2 types that can be added together
struct Add2Numbers<T: Add> {
    a: T,
    b: T,
}

impl<T: Add> Add2Numbers<T> {
    pub new: fn(T a, T b) -> Self {
        Self {
            a,
            b,
        }
    }
    
    // Associated types in traits and structs can be declared
    // like how it's done in rust. Here we used the AdditionResult
    // associated type from the 'Add' trait implementation for
    // given types.
    pub add: fn(self) -> T::AdditionResult {
        self.a + self.b
    }
}
```
