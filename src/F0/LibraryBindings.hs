-- | Provides a mapping from 
-- available C0 function names
-- to their F0 types
module F0.LibraryBindings where 

import F0.Parser.AST 

import Data.Map (Map)
import qualified Data.Map.Strict as Map 

data C0LibraryBinding = C0LibraryBinding
  {
    f0LibFunctionName :: String, 
    f0LibFunctionType :: F0Type
  }
  deriving Show

libraryBindings :: Map String C0LibraryBinding
libraryBindings = Map.fromList 
  [
    ("println", C0LibraryBinding "println" $ f0StringT `F0Function` f0UnitT),
    ("print", C0LibraryBinding "print" $ f0StringT `F0Function` f0UnitT),
    ("printint", C0LibraryBinding "printint" $ f0IntT `F0Function` f0UnitT),


    ("string_join", C0LibraryBinding "string_join" $ F0TupleType [f0StringT, f0StringT] `F0Function` f0StringT),
    ("string_fromint", C0LibraryBinding "string_fromint" $ f0IntT `F0Function` f0StringT),
    ("string_length", C0LibraryBinding "string_length" $ f0StringT `F0Function` f0IntT),

    ("error", C0LibraryBinding "error" $ f0StringT `F0Function` f0UnitT),
    ("assert", C0LibraryBinding "assert" $ f0BoolT `F0Function` f0UnitT)
  ]

libraryDefs :: [C0LibraryBinding]
libraryDefs = Map.elems libraryBindings