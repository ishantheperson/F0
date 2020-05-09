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
import Codegen.Closure 
import Codegen.Symbolize 

import Text.Printf 
import Control.Monad.State.Strict

import Data.Maybe (mapMaybe)

generalDecls :: String 
generalDecls = unlines
  [
    "// The type of all F0 functions",
    "typedef void* f0_function(struct f0_closure* f0_closure, void* arg);",
    "",
    "// Contains the captured variables as well as the actual function to call",
    "struct f0_closure {",
    "  f0_function* f;",
    "  void*[] captured;",
    "};"
  ]

boxingHelpers :: String
boxingHelpers = unlines $ mkBoxingHelpers =<< [C0IntType, C0StringType, C0BoolType]
  where mkBoxingHelpers :: C0Type -> [String] 
        mkBoxingHelpers (printType -> t) = 
          [ printf "void* f0_box_%s(%s x) { %s* p = alloc(%s); *p = x; return (void*)p; }" t t t t 
          , printf "%s* f0_unbox_%s(void* p) { return *(%s*)p; }\n" t t t]

-- | Gets the canonical name for this function given its index
functionName :: Int -> String 
functionName = printf "f0_lambda%d"

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

type PrintC0 = MonadState PrintC0State
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
    outputLine $ printf "void* %s = f0_box_%s(%s);" result (printType t) unboxed
    return result 

  C0Unbox t e -> do 
    boxed <- outputExpr e 
    result <- freshName
    outputLine $ printf "%s %s = f0_unbox_%s(%s);" (printType t) result (printType t) boxed
    return result 

  C0Op op e1 e2 -> do 
    a <- outputExpr e1 
    b <- outputExpr e2 

    result <- freshName 

    let t = if op == Equals then C0BoolType else C0IntType
    outputLine $ printf "%s %s = %s %s %s;" (printType t) result a (printOp op) b 
    return result 

  C0Identifier ref -> do 
    result <- freshName 
    let x = resolveRef ref 
    outputLine $ printf "void* %s = %s;" result x
    return result 

  C0Literal l -> do 
    result <- freshName 
    let (t, x) = case l of 
                   C0IntLiteral i -> (C0IntType, show i)
                   C0StringLiteral s -> (C0StringType, show s)
                   C0BoolLiteral True -> (C0BoolType, "true")
                   C0BoolLiteral False -> (C0BoolType, "false")

    outputLine $ printf "%s %s = %s;" (printType t) result x 
    return result 

  C0Declare n e letBody -> do 
    obj <- outputExpr e 
    outputLine $ printf "void* %s = %s;" (varName n) obj 
    
    indent 
    result <- outputExpr letBody 
    unindent
    
    return result 

  C0MakeClosure functionIndex closureInfo -> do 
    closureName <- freshName 
    outputLine $ printf "struct f0_closure* %s = alloc(struct f0_closure);" closureName 
    outputLine $ printf "%s->f = &%s;" closureName (functionName functionIndex)
    
    let numCaptured = maximum $ mapMaybe (\(_, ref, _) -> case ref of C0ClosureReference i -> Just i; _ -> Nothing) closureInfo

    capturedArrayName <- freshName 
    outputLine $ printf "void*[] %s = alloc_array(void*, %d);" capturedArrayName numCaptured 
    outputLine $ printf "%s->captured = %s;" closureName capturedArrayName
    forM_ closureInfo $ \(name, ref, i) -> do 
      let x = resolveRef ref 
      outputLine $ printf "%s[%d] = %x;" capturedArrayName i x 

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

resolveRef :: C0VariableReference -> [Char]
resolveRef ref = case ref of 
              C0ArgumentReference -> "arg"
              C0ClosureReference i -> printf "closure->captured[%d]" i
              C0ScopeReference s -> varName s 
              C0RecursiveReference -> "closure"

printType :: C0Type -> String 
printType = \case 
  C0IntType -> "int"
  C0StringType -> "string"
  C0BoolType -> "bool"
  C0ClosureType -> "struct f0_closure*"

printOp :: F0Operator -> String 
printOp = \case 
  Equals -> "=="
  Plus -> "+"
  Times -> "*"

spacesPerIndent :: Int 
spacesPerIndent = 4