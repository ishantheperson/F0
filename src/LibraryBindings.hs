-- | Provides a mapping from 
-- available C0 function names
-- to their F0 types
module LibraryBindings where 

import Parser.AST 
import Parser.ASTUtil 

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
    ("printint", C0LibraryBinding "printint" $ f0IntT `F0Function` f0UnitT)
  ]

libraryDefs :: [C0LibraryBinding]
libraryDefs = Map.elems libraryBindings