{-# LANGUAGE ExistentialQuantification #-}
module F0.Compiler.CompilerM 
  ( PackedCompilerError(..)
  , CompilerM
  , liftCompiler 
  ) where 

import F0.Compiler.CompilerError
import F0.Display

data PackedCompilerError = forall e. CompilerError e => PackedCompilerError e

instance Display PackedCompilerError where 
  display (PackedCompilerError e) = 
    "When " ++ errorStage e ++ ":\n\n" ++ errorMsg e

  displayIO (PackedCompilerError e) = do 
    msg <- errorMsgIO e 
    return $ "When " ++ errorStage e ++ ":\n\n" ++ msg

-- | Really just for GHCi usage
instance Show PackedCompilerError where 
  show = display

type CompilerM a = Either PackedCompilerError a

liftCompiler :: CompilerError e => Either e a -> CompilerM a 
liftCompiler = either (Left . PackedCompilerError) Right