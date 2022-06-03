## Byte array "literal"

Wouldn't this be cool:

```
let bytes: [u8] = b!(
    "GIF89a" 01 00 01 00 80 01 00 FF FF FF 00 00 00 "!"
    F9 04 01 "\n" 00 01 00 "," 00 00 00 00 01 00 01 00  
    00 02 02 "L" 01 00 ";"
);

```

Within the `b!( )`, you can pile on string literals (with interpolation) and special number literals.
String literals will be encoded with UTF-8 and bytes will be extracted;
Number literals are treated as unsigned 8-bit hex numbers, but they don't require the 0x prefix.
And the result is a byte array. Perfect for embedding small pieces of binary data.
The hex numbers must always be double-digit, can't have a standalone 0, 1, F, etc.
However, can combine multiple hex literals like this:

```
let bytes: [u8] = b!(
    "GIF89a" 01000100 800100FFFF FF000000 "!"  // <-- multiple bytes can be clumped together. 
    F9 04 01 "\n" 00 01 00 "," 00 00 00 00 01 00 01 00  
    00 02 02 "L" 01 00 ";"
);
```

but clumping like this is probably not a good idea, so this is disallowed:

```
let bytes: [u8] = b!(
    "GIF89a""GIF98a""GIF89a"
    00"Gif89a"00"GIF89a"FF
);
```

Q: What about if I want something dynamic?

```
let (parameter, bitflag): (u8, u8) = (0x85, 1);
let input: [u8] = get_input();
let bytes: [u8] = bytes!(
    "GIF89a" (parameter) 00 00 00 (input) 01 00 01 00 (bitflag << 3 | bitflag)
);
```

A: To avoid collision with "naked" hex literals, you must put whatever expression in parentheses.
Types of allowed expressions are: T, \[T], where T is one of u8, u16, u32, u64 and u128.

## Optimization

If a bytes literal:
1. Does not include string interpolation
2. Does not have parenthesized expressions
Then it is known at compile time, and it will be a vec point to a prepared chunk at the text section.

If a bytes literal:
1. Does not include string interpolation
2. Does have parenthesized expressions, but only scalar type
Then its size is known at compile time, and its semantics will be equivalent to calling `Vec::with_capacity(length)`
and dynamically filling the vec.

Otherwise, the byte literal is not known at compile time, and it will be constructed like `Vec::new()` and filling 
dynamically. Some compilation heuristics might be applied to establish a lower bound on vec length. This is an impl 
detail and not specified.