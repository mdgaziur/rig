# The RIG Programming Language

RIG is a programming language for the web that aims to be type safe and
less error-prone.

## Key Features

Features that are going to be added in the first release:
- Interop with JS(functions only)
- Strict type checking
- Enums that can hold values
- Exhaustive pattern matching
- Simpler borrow checker for fixing issues caused by mutability
- Rust like syntax for fellow Rustaceans out there :)
- Compiled to WASM
- Structs
- Simplified version of Rust's traits
- Builtin macros

Features that may get added later:
- Metaprogramming using macros

## Why?

Fun and learning.

## Syntax

The syntax is mostly similar with Rust syntax and it has some extra stuff
to make things more convenient. It's just a bit simplified.

```rust
fn println<T: Display>(anon fmt: String, vararg vars: T) {
    dom::output::alert(format(fmt, vars));
}

fn add<T: Add<T>>(a: T, b: T) -> T::Add<T>::Result {
    a + b
}

struct Vector3 {
    i_comp: i32,
    j_comp: i32,
    k_comp: i32,
}

fn main() {
    let input = dom::input::alert_input("Enter your name: ");
    println!("Hello world, {}!", input);
    println!("1 + 2 = {}", add(a: 1, b: 2));
    
    let str_input = dom::input::alert_input("Enter a string: ");
    if str_input == str_input.rev() {
        println!("palindrome");
    }
    
    let mut i = 0;
    loop {
        if i > 10 {
            break;
        }
        
        println!("{}", i);
        i += 1;
    }
    
    for x in 0..10 {
        println!("{}", x);
    }
}
```

### Expressions

```rust
1 + 2
1 + 2 * (3 / 2)
3 + (1 - 2)
(1 - 2) * 5
function() * 123
.123 * function()
function::<i32>(123) ** 2 / PI
1 as f32 / 2.54
```

### Variables

```rust
// immutble variable
let x = 1;

// mutable variable
let y = 2;

// eval'd at compile time
const cx = 1 + 3 * 3.1416 - 6;

// mutable global variable
static is_cool = true;
```

### Functions

```rust
fn function1() {
    // body
}

fn function_with_anon_arg(anon arg: i32) {
    // body
}

fn function_with_generic<T>(arg: T) {
    // body
}

fn function_with_generic_and_trait<T: As<i32>>(a: T) {
    // body
}

fn function_with_generic_and_trait2<T>(a: T)
    where T: As<i32> {
    // body
}
```

### Conditionals

```rust
if X {
    println()
} else if Y {
    
} else {

}
```

### Match

```rust
match true {
    true => {},
    false => {},
}

match 123 {
    x if x > 12 => {},
    _ => {},
}

match Y {
    S { f1, .. } => {},
    S { f2: 23, .. } => {},
    _ => {}
}

match Z {
    E(123, 456, 789) => {},
    E(i, j, ..) => {}
}
```

### Loops

```rust
loop {
    // body
}

while CONDITION {
    if CONDITION2 {
        continue;
    }
    break;
}

for x in ITERABLE {
    // body
}
```

### Structs

```rust
struct BasicStruct {
    a: i32,
    b: String,
}

impl BasicStruct {
    fn new(a: i32, b: String) -> Self {
        Self {
            a,
            b,
        }
    }
}

struct StructWithGeneric<T> {
    a: T
}

impl<T: As<i32>> StructWithGeneric<T> {
    pub fn new(a: T) -> Self {
        Self {
            a,
        }
    }
}
```

### Traits

```rust
trait Human {
    fn learn(self, what_to_learn: String) -> Status;
    fn say(self, what_to_say: String);
}

trait Asian < Human {
    fn study_a_lot(self) -> !;
}

trait American < Human {
    fn measure_in_american_way<T: AmericanMeasurable>(self, what_to_measure: T);
}

trait AmericanMeasurable {
    fn as_school_bus(self) -> i32 {}
}

impl AmericanMeasurable for i32 {
    fn as_school_bus(self) -> i32 {
        // in meters
        self / 13.7
    }
}
```

### Ownership

```rust
/// Moving a thing means caller is no longer allowed
/// to do anything on given argument after calling this
/// function.
fn move_thing(s: *String) {}

/// Basically sends a reference to whatever being passed.
/// Referenced thing is dropped when no one holds any
/// reference to passed value.
fn ref_thing(s: String) {}
```

### Module

```rust
/// Compiler tries to find file name with
/// `m1.rig` or directory named `m1` with a
/// `mod.rig` inside it.
mod m1;

mod m2 {
    pub fn m2_fn1() {
        m2_fn1_priv();
    }
    fn m2_fn1_priv() {}
}

use m2::m2_fn1;

fn main() {
    m2_fn1();
}
```
