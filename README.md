# F0 Programming Language

MiniML implementation which compiles to C1(VM)

## Usage
The `f0` compiler is already installed on Andrew Linux at `~ibhargav/public/f0`. It can be compiled from source using:
```
% stack build
% stack install 
```

Running:
```
% f0 -h # or stack run -- -h if you don't want to install
Usage: f0 [--print-ast] [--print-types] [--print-transformed] [-O]
          [-x|--execute] [-t|--only-typecheck] [-s|--save-files] [-c <arg>]
          <input file>

Available options:
  --print-ast              print out the AST at various points during
                           compilation
  --print-types            print out the types of the top level decls
  --print-transformed      print out the transformed program
  -O                       optimize by passing -O2 to the C compiler
  -x,--execute             execute the program if it compiles
  -t,--only-typecheck      stop after typechecking. implies --print-types
  -s,--save-files          save the generated C1 code
  -c <arg>                 pass an option to CC0
  -h,--help                Show this help text

% f0 <file.sml> # generates executable "file"
% f0 -x <file.sml> # run "file.sml"
% f0 -s <file.sml> # create "file.c1" for inspection of generated code
```

Supported features:
 - `print`, `printint`, `println`
 - Closures/polymorphism/higher order functions
 - Type inference
 - `let` expressions with local `datatype` declarations
 - Line comments with `--` 
 - Block comments with `(*` 
 - **Dynamically checked contracts** using `(*@requires ... @*)` or `(*@ensures ... @*)` 
 - See `test/testcases/` or `test/Spec.hs` for examples
 - Tuples and tuple patterns (no recursive patterns)
 - Sum types
 - Interface with C0/1 code or C0 libraries by declaring functions in `LibraryBindings.hs`

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
val main = 
  fib 0 1 2 20 ; 
  0
```

Using sum and product types:
```sml
datatype 'a list = Empty of () | Cons of 'a * 'a list 

fun sum l =
  case l of
    Empty () => 0
  | Cons (x, xs) => x + sum xs

fun tabulate f n =
  if n == 0
    then Empty ()
    else Cons (f n, tabulate f (n - 1))

fun tabulateIdx f n = tabulate (fn x => (x, f x)) n

val main = sum (tabulate id 53)
```

Contracts:
```sml
(*@requires 0 <= n && n < 17 @*)
(*@ensures result > 0 @*)
fun fact n = 
  if n == 0 
    then 1 
    else n * fact (n - 1)

(*@requires 0 <= n && n < 17 @*)
(*@ensures result > 0 @*)
fun bad_fact n = -1

val main = 
  printint $ fact 14;
  print "\n";

  -- Crashes at runtime
  -- printint $ fact 20;
  -- print "\n";

  -- Also crashes at runtime
  -- printint $ bad_fact 6;
  -- print "\n";

  0
```

Polymorphism:
```sml
datatype ('a, 'b) either = Left of 'a | Right of 'b 

fun bind x f = 
  case x of 
    Left e => Left e  
  | Right x => f x

fun return x = Right x 

fun validate_int x = 
  if x <= 0
    then Left "Numbers must be positive"
    else 
      print "Validated ";
      printint x;
      print "\n";
      Right x 

fun try_add x y =
  bind (validate_int x) $ fn a => 
  bind (validate_int y) $ fn b => 
  return $ string_fromint $ a + b 

val main = 
  case try_add (4) (-3) of 
    Left e => println e ; 1
  | Right a => print a ; println "" ; 0
```

## Built-in functions
```sml
print, println, printint : string -> unit 
string_join : string * string -> string
string_fromint : int -> string
string_length : string -> int

error : string -> unit
assert : bool -> unit
```

## Limitations
"Pattern matching" is limited as you can really only
do one layer at a time (inspect a sum or product type).

There are no type aliases or type annotations (Type annotations will parse but won't do anything).

Recursion is implemented using the callstack,
so it's possible to get a stack overflow. This can be avoided by
compiling and running using C1VM instead (use `-s` or `--save-files` to
keep the C1 source)
