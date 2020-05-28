module Compiler.CompilerError where 

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

class CompilerError e where
  errorStage :: e -> String 
  errorMsg :: e -> String
  errorMsgIO :: e -> IO String 
  errorMsgIO = pure . errorMsg

instance CompilerError e => CompilerError (NonEmpty e) where 
  errorStage = errorStage . NonEmpty.head
  errorMsg = concat . NonEmpty.toList . NonEmpty.intersperse "\n" . fmap errorMsg
