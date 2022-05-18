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

// [Ownership and reference]
// rust = let + let mut + & + &mut
// bruh = let + let mut

// [Concurrency]
// rust = async + await + Send + Sync + thread
// bruh = async + await + "Fiber" or whatever

// [Runtime]
// rust = compiled    + machine code + lifetime + borrow checker
// bruh = interpreted + bytecode vm  + gc

// [String]
// rust = Owned, borrowed, slice
// bruh = Behaves as if is primitive

// [Module and Visibility
// rust = mod + use + pub(self/crate/super/in...)
// bruh = namespace (`ns`) + import (`use`) + export (`pub`) closely mirroring JS/TS 
```

## Module and Visibility

This aspect of the language is profoundly important for structured projects, 
so it deserves careful thoughts and detailed elaboration.

### Considering Rust's mod/pub approach

At first rust's privacy model looked like its simple yet powerful. Upon closer inspection, 
powerful it is indeed, but there is actually a ton of subtle behaviours and rules. [This](
https://www.sheshbabu.com/posts/rust-module-system/) is a good read.

Though complicated, it is almost uniquely powerful in that it allows for a certain region of implementation detail to be 
completely hidden from other parts of the language, while still allowing those impls to split across files. 
This can't be done in, say, TS. In TS, if you split code across files, some of them must be made public with 
`export function public_interface()` to be useful. But once you do that, another file from across the street can import it,
meaning it is public to the whole project. So you can't for example have a folder of files that is only visible to the parent.
(Not a blow against TS, other languages like python, cpp, ruby etc has even worse encapsulation than TS)

With rust, there is `pub(super)` and `pub(in path::to::parent)`, exposing the impl detail only to the scope where it's needed.
However, I decided against adopting this powerful solution, and instead went with something similar to TS.  
And I document my reasoning below.

1. To adopt rust's mod system, a hierarchy must be established amongst source files. A root (crate) must exist,
and each module resides on a node on the module tree down from the root. Only then is it meaningful to refer to 
`super` and `parent` and `submodule`. Bruh being a scripting language does not want this. 
Bruh wants all source files to simply be blobs of text that encode some AST. Exactly where the AST happens to locate on the
filesystem should be transparent to both the {caller, importer, user} and the {callee, exporter, lib}.
Because of this flatness, 'parent' and 'descendant' don't exist so `pub(super)` wouldn't make sense.

2. Being able to encapsulate across multiple files and expose them to the exact scope needed, like rust allows you to, 
is an advanced feature. Small amounts of people will need this feature, and don't get me wrong, it's really, 
really nice to have this feature! I would hate it if my language couldn't do that. 
But I think it's a reasonable thing to say, let's make everyday module interactions stupidly simple like they have in TS,
So nothing surprising and very easy to learn without having to look at documentation. When you do need the feature, you 
look at the docs, and you learn that the feature exists, in a logical, syntactically consistent way,
and it won't impact you unless you decide to use it. (or maintain code that uses it).

3. So I've devised a way that let you do just that, a cross-file `pub(super)` feature. I'll explain it in a later section.

## Some expectations to be met, being a "scripting language"

> Files should be plug-and-play. If you replace a file with another, and their exports look the same, 
> and the imports aren't broken, it should work. (This is emphatically not true with rust!)

I want this expectation to be met for the most part, the one exception is when the cross-file `pub(super)` feature is 
involved.

> Arbitrary entry-point
> 
> So this just means you can `node src/path/to/deep/folder/random.js` and the interpreter won't complain about the random
file not being an entry point. In most cases, the random.js is not expecting itself to be called by the interpreter directly,
and the call either does nothing, or it does something bad if the code is not careful.

I do not agree with this. I would never want files executed where they were never meant to be directly invoked. 
The python community figured it out a solution: with the pervasive unsightliness of `if __name__ == "__main__": main()`.
That being said, I do appreciate flexible entry points in a scripting language.
To reconcile this, here are 2 easy rules:
- If a file has a main fn at its top level, then this file can be used as an entry point, interpreting it will 
invoke the main fn
- If otherwise, the file is a library and cannot be invoked directly. 

With this rule, you get arbitrary entry points, but only when you intend to. I believe it's a sensible, practical and
clean approach.

> Module is Singleton

I think this is the right approach. Implementing the singleton right is not trivial, though, see 
[what they've done for JS](https://medium.com/@lazlojuly/are-node-js-modules-singletons-764ae97519af) and avoid that 
at all costs.

////////////////////////////////////////////////////////////////////////////////