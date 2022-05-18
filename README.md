## BruhScript

## Intentions

- scripting language
- interpreted
- rust vm
- embedded language, released as a crate
- extensible language
- api tbd
- author wants an embedded language but takes one look at lua and be like… lemme write my own
- inspired by lua
- indexes start at 0
- has proper dynamic array (list) and map (record) type
- statically typed, great type inference, i hope one day
- variable declaration should seldom require type annotation
- tagged union aka enum. no ts-style: `bool | str`
- enum can have value attached
- runtime type introspection
- inspired by Wren but want integer support
- good integer support: `i128 i64 i32 i8 u128 u64 u32 u8 f64 f32 bigint`
- destructuring and pattern matching
- emulates some derive macros
  - `#[derive(Debug, PartialEq, Copy, Clone, Hash, Default)]`
- GCed
- parametric polymorphism
- syntax influenced by c, rust, js, ts, swift
  - function call = function name + parentheses
  - list items separated by commas
  - lexical scope with braces
  - implicit return
  - variable shadowing like rust
  - semicolon: require to separate expressions
  - basically everything is expression
    - declaration, assignment and mutation expressions return unit value `()`
  - newline treated as whitespace
  - whitespace completely irrelevant
  - fat arrow anonymous function
  - struct + trait + impl
  - strict typing, `struct Duck(str)` can’t be used in place for `struct Dack(str)`
  - symbol qualification like rust
    - `instance.associated_function(argument)`
    - `Constructor::associated_function(argument)`
    - `path::to::module::ClassConstructor::associated_function(argument)`
  - functions always require type signature annotations
  - block comments can nest
  - unit-testing included - also like rust
  - standard library implements functional programming primitives (functor applicative monad etc.)
  - no function overloading, no variadic
- `#[test]`
- `#[should_panic]`

## Type

- Basically stealing rust typing wholesale
- Function decls require full type signature, unless return type is ()
- `!` and `?` actual types integrated with type system
- No naked union, only enum, but enum variants first class type
- runtime type introspection reconciles with static analysis to narrow down types at compile time, much like TS.
  - this means enum variants can be narrowed down both at run and compile time
- support for literal type 

## Name

obviously, the name is not final.
maybe we can all it BrushScript, StudentUserScript, or Lub?

## Currently

- basically implementing the language with the [wren source code](*https://github.com/wren-lang/wren/blob/main/src/vm/wren_compiler.c*) (3c43de7) opened to the side of the IDE
- start with tree-walk, think about bytecode and stack/reg-vm later
- not decided on a concurrency model yet, looking at how fiber works

## Wishful thinking

- maybe target llvm-ir / c / js / ts / wasm also
- Interpret a subset of rust code....
  - take a rust source, strip away `&`, `ref`, lifetime specifier
  - add supportive library functions
  - replace unsupported macros with whatever
  - maybe runnable in some cases :/
  - is certainly not the design intention

## Snippets

```*rust*
// function declaration
fn add(x: i64, y: i64) -> i64 { x + y }
pub fn subs(x: i64, y: f64) -> f64 { x - y }

// module/namespace
mod module {
  fn private_fn() { print("private fn") }
  pub fn pubic_fn() { print("pub fn") }
  mod test {
    #[test]
    fn test_1() {
      private_fn();
      public_fn();
    }
  }
}

```

## Type annotation and inference

Most expressions in bruh has definite types.

- all constructors return the corresponding class
- all functions’ return types are required to be annotated so no ambiguity
- most literals have a clear and unambiguous type

some cases where type inference is not trivial:

```
let i = 0  // defaults to i64 if not annotated
fn takes_u32(x: u32) {}
takes_u32(i)  // compiles since i has no bounds. at compile time, i is bound to i32.
```

## Numerical literal syntax

![Image](NumericalLiteralSyntaxDesign.png)

## Similarities and Differences 

```
// [Type Ststem]
// rust = (static + strong + strict + inference) + struct (impl + trait + enum)
// bruh = (static + strong + strict + inference) + struct (impl + trait + enum) + enum variant as type + introspection informs static analysis

// [Misc]
// rust = privacy (mod + crate + pub) + let + let mut + & + &mut
// bruh = privacy (mod + crate + pub) + let + let mut

// [Concurrency]
// rust = async + await + Send + Sync + thread
// bruh = async + await + "Fiber" or whatever

// [Runtime]
// rust = compiled    + machine code + lifetime + borrow checker
// bruh = interpreted + bytecode vm  + gc

// [String]
// rust = Owned, borrowed, slice
// bruh = Behaves as if is primitive
```
