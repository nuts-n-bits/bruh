## Byte array literal

Wouldn't this be cool:

```
let bytes: Vec<u8> = b!(
    "GIF89a" 01 00 01 00 80 01 00 FF FF FF 00 00 00 "!"
    F9 04 01 "\n" 00 01 00 "," 00 00 00 00 01 00 01 00  
    00 02 02 "L" 01 00 ";"
);

```

Within the `b!( )`, you can pile on string literals (no interpolation) and special number literals.
String literals will be encoded with UTF-8 and bytes will be extracted;
Number literals are treated as unsigned 8-bit hex numbers, but they don't require the 0x prefix.
And the result is a known-at-compile-time byte array. Perfect for embedding small pieces of binary data.
The hex numbers must always be double-digit, can't have a standalone 0, 1, F, etc.
However, can combine multiple hex literals like this:

```
let bytes: Vec<u8> = b!(
    "GIF89a" 01000100 800100FFFF FF000000 "!"  // <-- multiple bytes can be clumped together. 
    F9 04 01 "\n" 00 01 00 "," 00 00 00 00 01 00 01 00  
    00 02 02 "L" 01 00 ";"
);
```

but clumping like this is probably not a good idea, so this is disallowed:

```
let bytes: Vec<u8> = b!(
    "GIF89a""GIF98a""GIF89a"
    00"Gif89a"00"GIF89a"FF
);
```

## about the `b!`

In rust, this would be calling a macro called `b!`

But we are a scripting language, we don't have macro or else we could be compiling for too long (I think?);
So, we steal this `ident!` syntax to mean compiler special cases. It's like keywords, 