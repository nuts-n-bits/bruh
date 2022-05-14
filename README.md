# BruhScript

## Intentions 

- scripting language
- interpreted
- rust vm
- embedded language, released as a crate
- extensible language
- api tbd
- author wants an embedded language but takes one look at lua and be like... lemme write my own
- inspired by lua
- indexes start at 0
- has proper dynamic array (list) and map (record) type
- statically typed, great type inference 
- variable declaration should seldom require type annotation
- naked union type and sum type like ts
- runtime type introspection
- inspired by Wren but want integer support
- good integer support: i128 i64 i32 i8 u128 u64 u32 u8 f64 f32 bigint
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
  - strict typing, `struct Duck(str)` can't be used in place for `struct Dack(str)`
  - symbol qualification like rust
    - `instance.associated_function(argument)`
    - `Constructor::associated_function(argument)`
    - `path::to::module::ClassConstructor::associated_function(argument)`
  - functions always require type signature annotations
  - block comments can nest
  - privacy model stolen wholesale from rust
    - private = mod-accessible, including class fields and assoc fns
    - public = pub keyword = exposed
  - module system stolen wholesale from rust
  - unit-testing included - also like rust
  - standard library implements functional programming primitives (functor applicative monad etc.)
  - no function overloading, no variadic
- `#[test]`
- `#[should_panic]`

obviously, the name is not final.
maybe we can all it BrushScript, StudentUserScript, or Lub?

## Currently

- basically implementing the language with the [wren source code](https://github.com/wren-lang/wren/blob/main/src/vm/wren_compiler.c) (3c43de7) opened to the side of the IDE
- start with tree-walk, think about bytecode and stack/reg-vm later
- not decided on a concurrency model yet, looking at how fiber works

## Wishful thinking

- maybe target llvm-ir / c / js / ts / wasm also 

## Snippets

```rust
// function declaration
fn add(x: i64, y: i64) -> i64 { x + y }
pub fn subs(x: i64, y: f64) -> f64 { x - y }

// module/namespace
mod module {
  fn private_fn() {}
  pub fn pubic_fn() {}
  mod test {
    fn test_1() -> () {
      private_fn();
      public_fn();
    }
  }
}

```

## Type annotation and inference

Most expressions in bruh has definite types.

- all constructors return the corresponding class
- all functions' return types are required to be annotated so no ambiguity
- most literals have a clear and unambiguous type

some cases where type inference is not trivial:

```
let i = 0  // defaults to i64 if not annotated
fn takes_u32(x: u32) {}
takes_u32(i)  // compiles since i has no bounds. at compile time, i is bound to i32.
```

## Numerical literal syntax

![Image](NumericalLiteralSyntaxDesign.png)

## Feature comparison with Rust

#### Typing
**Rust**               | **Bruh**
:-                     | :-
Static                 | Static
Strong                 | Strong
Strict                 | Strict
Inferred               | Inferred
Non-naked union        | Non-naked union
==No introspection==   | ==Runtime type introspection==
struct + trait + enum  | struct + trait + enum
impl block             | impl block

#### Privacy
**Rust**               | **Bruh**
:-                     | :-
mod + crate + pub      | mod + crate + pub

#### Mutation and Ref
**Rust**               | **Bruh**
:-                     | :-
let + let mut          | let + let mut
==& + &mut==           |

#### Runtime
**Rust**               | **Bruh**
:-                     | :-
==Compiled==           | ==Interpreted==
==Machine Code==       | ==bruh.vm==
==lifetime + borrow checker== | ==GCed==


```
// rust = typing (static + strong + strict + inference) + struct (impl + trait + enum)
// bruh = typing (static + strong + strict + inference) + struct (impl + trait + enum)

// rust = privacy (mod + crate + pub) + let + let mut + & + &mut
// bruh = privacy (mod + crate + pub) + let + let mut

// rust = async + await + Send + Sync + thread
// bruh = async + await + "Fiber" or whatever

// rust = compiled    + machine code + lifetime + borrow checker
// bruh = interpreted + bruh.vm.rs   + gc

```