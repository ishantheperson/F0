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

generalDecls :: String 
generalDecls = unlines
  [
    "// The type of all F0 functions",
    "typedef void* f0_function(struct f0_closure* f0_closure, void* arg);",
    "",
    "// Contains the captured variables as well as the actual function to call",
    "struct f0_closure {",
    "  f0_function* f;",
    "  void*[] closure;",
    "};"
  ]

boxingHelpers :: String
boxingHelpers = unlines $ mkBoxingHelpers =<< [C0IntType, C0StringType, C0BoolType]
  where mkBoxingHelpers :: C0Type -> [String] 
        mkBoxingHelpers (printType -> t) = 
          [ printf "void* f0_box_%s(%s x) { %s* p = alloc(%s); *p = x; return (void*)p; }" t t t t 
          , printf "%s* f0_unbox_%s(void* p) { return *(%s*)p; }\n" t t t]

printType :: C0Type -> String 
printType = \case 
  C0IntType -> "int"
  C0StringType -> "string"
  C0BoolType -> "bool"
  C0ClosureType -> "struct f0_closure*"

-- | Gets the canonical name for this function given its index
functionName :: Int -> String 
functionName = printf "f0_lambda%d"

functionHeaders :: [C0Function] -> String 
functionHeaders = unlines . zipWith functionHeader [0..] 
  where functionHeader :: Int -> C0Function -> String 
        -- Right now all function information is discarded
        -- But it could be used to add some hints as to what is what in the generated code 
        functionHeader i _ = printf "void* %s(struct f0_closure* closure, void* arg);" (functionName i)


