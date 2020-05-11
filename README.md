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
 - `let` expressions with local `datatype` declarations
 - Line comments with `--` 
 - Block comments with `(*` 
 - See `test/testcases/` or `test/Spec.hs` for examples
 - Tuples and tuple patterns (no recursive patterns)
 - Sum types

## Example

```sml
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

Using sum and product types:
```sml
fun sum l =
  case l of
    Empty () => 0
  | Cons l2 =>
      let val (hd, tl) = l2 in
      hd + (sum tl)
      end

fun tabulate f n =
  if n == 0
    then Empty ()
    else Cons (f n, tabulate f (n - 1))

fun compose f g = fn x => f (g x)

fun tabulateIdx f n = tabulate (fn x => (x, f x)) n

fun id x = x

val main = sum (tabulate id 53)
```

## Limitations
"Pattern matching" is limited as you can really only
do one layer at a time (inspect a sum or product type).

Recursion is implemented using the callstack,
so it's possible to get a stack overflow
