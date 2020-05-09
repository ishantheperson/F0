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

Supported features:
 - `print`, `printint`, `println`
 - Closures/polymorphism/higher order functions
 - Type inference
 - `let` expressions
 - Line comments with `--` 
 - Block comments with `(*` 
 - See `testcases/` or `test/Spec.hs` for examples

## Example

```
fun fib a b i stop =
  if i == stop then ()
  else print "fib(";
       printint i;
       print ") = ";
       printint (a + b);
       print "\n";
       fib b (a + b) (i + 1) stop

-- main always needs to return int
-- it is returned from main in the C1 program
val main = fib 0 1 2 20 ; 0
```

## Limitations
Currently a very small subset is implemented.
The only operations are on integers. However
polymorphic functions are supported.

Recursion is implemented using the callstack,
so it's possible to get a stack overflow
