{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-|
  Steps to producing a C0 file:

  1. Write the general information like the definitions
     of the structs and function pointer typedefs

  2. Write the runtime functions for boxing and unboxing
  3. Write the function header for every function in the function pool
     The function name should be computable from just knowing its index
     in the function pool. 

  4. Generate the code for the actual expression 
-}
module Codegen.PrintC0 where 

import Parser.AST 
import Parser.ASTUtil 
import Codegen.Closure 
import Codegen.Symbolize 
import LibraryBindings

import Text.Printf 
import Control.Monad.State.Strict

import Data.Maybe (mapMaybe)

generalDecls :: String 
generalDecls = unlines
  [
    "#use <conio>",
    "",
    "// The type of all F0 functions",
    "typedef void* f0_function(struct f0_closure* f0_closure, void* arg);",
    "",
    "// Type of tuples",
    "typedef void*[] f0_tuple;",
    "",
    "// Contains the captured variables as well as the actual function to call",
    "struct f0_closure {",
    "  f0_function* f;",
    "  void*[] captured;",
    "};"
  ]

boxingHelpers :: String
boxingHelpers = unlines $ mkBoxingHelpers =<< [F0IntType, F0StringType, F0BoolType]
  where mkBoxingHelpers :: F0PrimitiveType -> [String] 
        mkBoxingHelpers (printPrimitiveTypeC0 -> t) = 
          [ printf "void* f0_box_%s(%s x) { %s* p = alloc(%s); *p = x; return (void*)p; }" t t t t 
          , printf "%s f0_unbox_%s(void* p) { return *(%s*)p; }\n" t t t]

mkLibraryWrapper :: C0LibraryBinding -> String 
mkLibraryWrapper (C0LibraryBinding name (F0PrimitiveType t `F0Function` f0UnitT)) = 
  unlines [
    printf "void* %s(struct f0_closure* closure, void* arg) {" (wrappedNativeName name),
    printf "    %s(f0_unbox_%s(arg));" name (printPrimitiveTypeC0 t),
    printf "    return f0_box_int(0);",
    printf "}"
  ]
mkLibraryWrapper _ = error "Unsupported library function type"

-- | Gets the name of the F0 wrapper function for the given native function 
wrappedNativeName :: String -> String 
wrappedNativeName s = printf "f0_wrap_%s" s 

-- | Gets the canonical name for this function given its index
functionName :: Int -> String 
functionName i = printf "f0_lambda%d" i

varName :: Symbol -> String 
varName (Symbol (i, n)) = printf "f0_var_%s%d" n i 

functionHeaders :: [C0Function] -> String 
functionHeaders = unlines . zipWith functionHeader [0..] 
  where functionHeader :: Int -> C0Function -> String 
        -- Right now all function information is discarded
        -- But it could be used to add some hints as to what is what in the generated code 
        functionHeader i _ = printf "void* %s(struct f0_closure* closure, void* arg);" (functionName i)

data PrintC0State = PrintC0State
  {
    uniqueCount :: Int,
    indentLevel :: Int,
    writtenCode :: [String] -- ^ This is reversed! 
  } 
  deriving Show

initialPrintC0State :: PrintC0State
initialPrintC0State = PrintC0State 
  {
    uniqueCount = 0,
    indentLevel = 0,
    writtenCode = []
  }

type PrintC0 = MonadState PrintC0State

runPrintC0 :: State PrintC0State () -> String 
runPrintC0 p = 
  let finalState = execState p initialPrintC0State 
  in unlines . reverse $ writtenCode finalState 

type C0VarName = String 

freshName :: PrintC0 m => m C0VarName 
freshName = do 
  state <- get 
  let i = uniqueCount state 
      name = "f0_tmp" ++ show i
  put $ state { uniqueCount = i + 1 }
  return name 

indent, unindent :: PrintC0 m => m () 
indent = do 
  state <- get 
  put $ state { indentLevel = indentLevel state + 1 }

unindent = do 
  state <- get 
  put $ state { indentLevel = indentLevel state - 1 }

outputLine :: PrintC0 m => String -> m () 
outputLine s = do 
  state <- get 
  let indentAmount = spacesPerIndent * indentLevel state 
      line = replicate indentAmount ' ' ++ s 
  put (state { writtenCode = line : writtenCode state })

-- | Returns a variable name with the result of this expression 
outputExpr :: PrintC0 m => C0Expression -> m C0VarName 
outputExpr = \case 
  C0Box t e -> do 
    unboxed <- outputExpr e 
    result <- freshName
    outputLine $ printf "void* %s = f0_box_%s(%s);" result (printPrimitiveTypeC0 t) unboxed
    return result 

  C0Unbox t e -> do 
    boxed <- outputExpr e 
    result <- freshName
    outputLine $ printf "%s %s = f0_unbox_%s(%s);" (printPrimitiveTypeC0 t) result (printPrimitiveTypeC0 t) boxed
    return result 

  C0Op Not [e1] -> do 
    a <- outputExpr e1 
    result <- freshName

    outputLine $ printf "bool %s = !(%s);" result a 
    return result 

  C0Op op [e1, e2] -> do 
    a <- outputExpr e1 
    b <- outputExpr e2 

    result <- freshName 

    let t = operatorOutput op 
    outputLine $ printf "%s %s = %s %s %s;" (printPrimitiveTypeC0 t) result a (printOp op) b 
    return result 

  C0CreateTuple es -> do 
    let tupleSize = length es 

    tuplePtrName <- freshName
    outputLine $ printf "f0_tuple* %s = alloc(f0_tuple);" tuplePtrName 

    tupleName <- freshName 
    outputLine $ printf "f0_tuple %s = alloc_array(void*, %d);" tupleName tupleSize 
    outputLine $ printf "*%s = %s;" tuplePtrName tupleName

    forM_ (zip [0..] es) $ \(i :: Int, e) -> do 
      element <- outputExpr e 
      outputLine $ printf "%s[%d] = %s;" tupleName i element 

    result <- freshName 
    outputLine $ printf "void* %s = (void*)%s;" result tuplePtrName 
    return result  

  C0AccessTuple i e -> do 
    tuple <- outputExpr e 
    result <- freshName

    outputLine $ printf "void* %s = (*(f0_tuple*)%s)[%d];" result tuple i 
    return result 

  C0Identifier ref -> do 
    result <- freshName 
    let x = resolveRefIdent ref 
    outputLine $ printf "void* %s = %s;" result x
    return result 

  C0Literal l -> do 
    result <- freshName 
    let (t, x) = case l of 
                   C0IntLiteral i -> (F0IntType, show i)
                   C0StringLiteral s -> (F0StringType, show s)
                   C0BoolLiteral True -> (F0BoolType, "true")
                   C0BoolLiteral False -> (F0BoolType, "false")

    outputLine $ printf "%s %s = %s;" (printPrimitiveTypeC0 t) result x 
    return result 

  C0If e1 e2 e3 -> do 
    result <- freshName 
    outputLine $ printf "void* %s;" result 
    test <- outputExpr e1 
    
    outputLine $ printf "if (%s) {" test 
    indent 
    
    trueValue <- outputExpr e2 
    outputLine $ printf "%s = %s;" result trueValue 
    
    unindent 

    outputLine "} else {"
    indent 

    falseValue <- outputExpr e3
    outputLine $ printf "%s = %s;" result falseValue

    unindent 
    outputLine "}"

    return result 

  C0Declare n e letBody -> do 
    obj <- outputExpr e 
    outputLine $ printf "void* %s = %s;" (varName n) obj 
    outputExpr letBody 

  C0NativeFn n -> do 
    functionObjName <- freshName -- Not really a closure because it never captures anything 
    closureName <- freshName 
    outputLine $ printf "struct f0_closure* %s = alloc(struct f0_closure);" closureName 
    outputLine $ printf "%s->f = &%s;" closureName (wrappedNativeName n)
    
    boxedClosureName <- freshName 
    outputLine $ printf "void* %s = (void*)%s;" boxedClosureName closureName 
    return boxedClosureName

  C0MakeClosure functionIndex closureInfo -> do 
    closureName <- freshName 
    outputLine $ printf "struct f0_closure* %s = alloc(struct f0_closure);" closureName 
    outputLine $ printf "%s->f = &%s;" closureName (functionName functionIndex)
    
    let numCaptured = length closureInfo

    unless (numCaptured == 0) $ do 
      capturedArrayName <- freshName 
      outputLine $ printf "void*[] %s = alloc_array(void*, %d);" capturedArrayName numCaptured 
      outputLine $ printf "%s->captured = %s;" closureName capturedArrayName
      forM_ closureInfo $ \(sym, ref, i) -> do 
        let x = resolveRefClosure closureName ref 
        outputLine $ printf "%s[%d] = %s; // (capture '%s')" capturedArrayName i x (display sym)

    boxedClosureName <- freshName 
    outputLine $ printf "void* %s = (void*)%s;" boxedClosureName closureName 
    return boxedClosureName

  C0CallClosure e1 e2 -> do 
    boxedClosure <- outputExpr e1 
    arg <- outputExpr e2 

    unboxedClosure <- freshName
    outputLine $ printf "struct f0_closure* %s = (struct f0_closure*)%s;" unboxedClosure boxedClosure 
    
    result <- freshName 
    outputLine $ printf "void* %s = (*(%s->f))(%s, %s);" result unboxedClosure unboxedClosure arg 
    return result 

outputFunction :: PrintC0 m => (Int, C0Function) -> m () 
outputFunction (i, (C0Function _ e)) = do 
  outputLine $ printf "void* %s(struct f0_closure* closure, void* arg) {" (functionName i)
  
  indent 
  result <- outputExpr e 
  outputLine $ printf "return %s;" result 
  unindent 

  outputLine "}\n"

outputMain :: PrintC0 m => C0Expression -> m () 
outputMain e = do 
  outputLine "int main() {"

  indent 

  result <- outputExpr e 
  outputLine $ printf "return *(int*)%s;" result 

  unindent

  outputLine "}\n"

outputProgram :: (C0Expression, C0CodegenState) -> String 
outputProgram (mainE, C0CodegenState functionPool) =
  generalDecls ++ boxingHelpers ++ libs ++ runPrintC0 go  
  where go = do 
          forM_ (zip [0..] functionPool) outputFunction
          outputMain mainE  

        libs = concatMap mkLibraryWrapper libraryDefs

resolveRefIdent :: C0VariableReference -> String
resolveRefIdent = \case  
  C0ArgumentReference -> "arg"
  C0ClosureReference i -> printf "closure->captured[%d]" i
  C0ScopeReference s -> varName s 
  C0RecursiveReference -> "closure"

resolveRefClosure :: String -> C0VariableReference -> String
resolveRefClosure closureName = \case 
  C0RecursiveReference -> "(void*)" ++ closureName 
  other -> resolveRefIdent other 

spacesPerIndent :: Int 
spacesPerIndent = 4