# f0

MiniML implementation which compiles to C0

## Usage
```
% stack build
% stack run -- <file.sml> # generates file.c1
% cc0 file.c1 
```

## Limitations
Currently a very small subset is implemented.
The only operations are on integers. However
polymorphic functions are supported.

Recursion is implemented using the callstack,
so it's possible to get a stack overflow
