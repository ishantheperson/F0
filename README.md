# f0

MiniML implementation which compiles to C0

## Usage
```
% stack build
% stack run -- -h
Usage: f0-exe [--print-ast] [--print-types] [--print-transformed] <input file>

Available options:
  --print-ast              print out the AST after parsing
  --print-types            print out the types of the top level decls
  --print-transformed      print out the transformed program
  -h,--help                Show this help text
% stack run -- <file.sml> # generates file.c1
% cc0 file.c1 
```

## Example

```
-- Line comments are supported 
-- Currently the only primitive types are int, string, and bool
-- All functions will have their types inferred to the most
-- general type possible

fun eq_if a b c d = if a == b then c true else d true (* Using true to represent unit *)
fun ifz a c d = eq_if a 0 c d

fun fact n = ifz n (fn b => 1) (fn b => n * fact (n-1))

-- All programs must have a value
-- called main of type int 
val main = fact 10
```

## Limitations
Currently a very small subset is implemented.
The only operations are on integers. However
polymorphic functions are supported.

Recursion is implemented using the callstack,
so it's possible to get a stack overflow
