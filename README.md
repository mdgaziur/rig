# The RIG Programming Language

RIG is a statically typed programming language that focuses on usability and performance.

## Target

- Designing the syntax
  - As much context free as possible
  - Less verbose
  - Low learning curve
- RIG compiler
  - RGVM bytecode output
  - WASM output
- RGVM (RIG Virtual Machine)
  - Concurrency
- RIG standard library
- Concurrency primitives
- Embedding RIG in Rust
- Embedding RIG in languages other than Rust through FFI
- Possibly self-host the compiler
- Metaprogramming(Macros)

## Syntax

- Everything is an expression
- Blocks return the evaluated value of trailing expression
  if it has no semicolon

### Function
```
func main i32 {
    const name = io::input::<str>("Your name please: ");
    println("Hi {}, welcome to RIG programming language!", name);
    
    0
}
```

```
func add(a i32, b i32) i32 {
    a + b
}
```

### Structs
```
struct Square {
    height: i32,
    width: i32,
}

impl Square {
    type Output = i32;
    
    pub static new(height: i32, width: i32) Self {
        Self {
            height,
            width,
        }
    }
    
    pub func area Output {
        ?height * ?width
    }
}
```

### Conditionals
```
func main i32 {
    const n = io::input::<i32>("Enter a number: ");
    if n % 2 == 0 {
        println("It's even");
    } else {
      println("It's odd");
    }
    
    0
}
```

### Loops
```
func main i32 {
    const arr = [1, 2, 3, 4, 5, 6];
    
    mut iter = arr.iter();
    loop {
        if iter.end() {
            break;
        }
        
        println(iter.next());
    }
    
    for val in arr {
        println(val);
    }
    
    mut i = 0;
    while i < arr.len() {
        println(arr[i]);
    }
}
```

### Generics
```
func print_anything<T: Display>(value: T) {
    println(value);
}

struct PointlessVecWrapper<T> {
    value: Vec<T>,
}

impl<T> PointlessVecWrapper<T> {
    pub static new<T>(value: Vec<T>) Self {
        Self {
            value,
        }
    }
}

func main i32 {
    print_anything("Hello!");
    debug(PointlessVecWrapper::<T>::new([1, 2, 3]));
}
```

### FFI/External Definitions
```
// NOTE: functions and structs defined by application
//       encapsulating the virtual machine can also be
//       accessed through this
extern {
    struct SomeStruct;
    
    impl SomeStruct {
        pub static func new Self;
    }
    
    func is_prime(n i32) bool;
}
```

```
// NOTE: this is meant to be an implementation detail
//       for standard library
__builtin_definition {
    func __builtin_std_io_println<T: Display>(value T*);
    func __builtin_std_io_debug<T>(value T);
    
    struct Vec<T>;
    
    impl<T> Vec<T> {
        pub static new Self;
        pub func extend_with_array(array T[]);
        pub func extend_with_vec(vec Vec<T>);
        pub func push(value T);
        pub func pop T;
    }
    
    impl<T> Index<usize> for Vec<T> {
        type Output = T;
        
        pub func __builtin_std_vec_index(index usize) T;
    }
}
```

### Imports
```
// this gets desugared into following
import std::math::{factorial, fibonacci};
/*
extern module std;
use std::math::factorial;
use std::math::fibonacci;
*/

func main i32 {
    println(factorial(5));
    println(fibonacci(5));
}
```

### Modules

main.rg
```
// this gets desugared into following
use some_module::some_function;
/*
mod some_module {
    pub func some_function {
        // do something
    }
}
use some_module::some_function;
*/
```

some_module.rg
```
pub func some_function {
    // do something
}
```
