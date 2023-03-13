# The RIG Programming Language

RIG is a type safe and developer friendly language for the web.

## Target

- Type safe design

- Mutability

- Better sum types

- Exhaustive pattern matching

- No user facing dependency on JS

- Safe wrappers for JS in standard library

- No exception in safe RIG

- `unsafe` interop with JS(No packages are supported)

- New packaging system

- Concurrency(?)

- Extensible and customizable compiler(?)



## Syntax

Functions:

```rust
divide_elements_by_2: fn(Vec<Number> v) -> Vec<Number> {
    v.iter().map(|v| v / 2).collect()
} 

main: fn() {
    let p1 = "Hello";
    let p2 = "World!";
    
    println!("{p1} {p2}");
    println!("{}", divide_elements_by_2(vec![12, 36, 40]));
};
```

`main` is a constant value that contains the function. `fn` keyword is an expression that returns a callable type(aka a function). 



Constants:

```rust
DEADBEEF: 0x3735928559;

add_2_numbers: fn(Number a, Number b) -> Number {
    a + b    
}
```

Constants are defined like this. The value must be something that can be evaluated in compile time. Here are some values that can be used as a value for constants:

- Number

- String

- Function

- Arithmetic operations(cannot contain function calls)



Variables:

```rust
let var1 = 1234;
let vec1: Vec<Number> = vec![12, 3, 5];
let c = vec1.iter().map(|v| v / 2).collect::<Number>();
```

Variables can be defined like this. Types can be specified by putting a colon and then the type after that. In most cases, RIG compiler can infer the type of the variable by just looking at the value on the right hand side.



Types:

```rust
type VecOfNumbers: Vec<Number>;
type AdderOfValues<T: Add<T>>: struct {
    a: T,
    b: T,
}


bind<T: Add<T>> to AdderOfValues<T> {
    type Output = Add<T>::Output;

    new: fn(T a, T b) -> Self {
        Self {
            a,
            b
        }
    }
    
    add: #[consumes]fn() -> Self::Output {
        @a + @b
    }
}
```

Types can be declared using the `type` keyword. This can be used to alias other types or create a `struct` or an object `object`. The `bind` keyword can be used to bind functions to a type. Adding `#[consumes]` attribute to a function causes the compiler to no longer allow the user to reuse instance of type after the function is called. This can be useful when handling stuff like sockets. Types can also be binded with other types.



Generics:

```rust
type Vector<T> = Vec<T>;
type StringHashMap<V> = HashMap<String, V>;


add: fn<T>(T a, T b)
    ensure T: Add<T> {
    a + b
}


// or


add: fn<T: Add<T>>(T a, T b) {
    a + b
}
```

Generics are similar to Rust generics. They can be used to define types or function that take arbitrary types(with conditions if needed).



Traits:

```rust
trait Chordate {
    fn notochord() -> String;
}

trait Species: Chordate {
    fn species_name() -> String;
}
```

Traits can be used to define shared behaviours and they can have associated types inside them. Traits can also be used to simulate inheritence.



Block statements:

Block statements are enclosed with curly braces. They return the value of trailing expression. If the trailing expression contains a semicolon at the end, the block statement returns an empty value `()`. 



Nulls:

NO NULLS! Only empty type `()`  (a concrete type that cannot be used as a placeholder for other types) and `Option<T>`.



Interop:

```rust
unsafe js {
    "console.log(\"unsafeeeeeeeeeeeeeeeee\");"
    "alert(\"We're playing with fire!\");"
}
```

That's it.



Enums:

```rust
type Option<T>: enum {
    Some(T),
    None,
}

// Must be exhaustive
match Option::Some(123) {
    Some(v) => println!("it's 123"),
    None => println!("What?"),
}
```



Comments:

Single line comments: `//`

Multi line comments: `/**/`

Doc comment: `/!!/`



Mutability:

```rust
let mut x = 1;
x += 1;


let y = 2;
y += 2; // compiler complains and whines about it

```



Branches:

```rust
if X {
    // do something
} else if Y {
    // do something
} else {
    // do something
}


while A {
    // do something
    continue;
    println!("you will never see me!");
}

loop {
    // infinite loop
    break; // not anymore
}
```
