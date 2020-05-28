{-# LANGUAGE ExistentialQuantification #-}
module Compiler.CompilerM 
  ( PackedCompilerError(..)
  , CompilerM(runCompilerM)
  , liftCompiler 
  ) where 

import Compiler.CompilerError
import Display

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

data CompilerM a = CompilerM { runCompilerM :: Either PackedCompilerError a }

instance Functor CompilerM where 
  fmap f (CompilerM (Left e)) = CompilerM (Left e)
  fmap f (CompilerM (Right x)) = CompilerM (Right (f x))

instance Applicative CompilerM where 
  pure = CompilerM . Right 
  CompilerM (Left e) <*> _ = CompilerM (Left e)
  _ <*> CompilerM (Left e) = CompilerM (Left e)
  CompilerM (Right f) <*> CompilerM (Right x) = CompilerM (Right $ f x)

instance Monad CompilerM where 
  CompilerM (Right x) >>= f = f x 
  CompilerM (Left e) >>= _ = CompilerM (Left e)

liftCompiler :: CompilerError e => Either e a -> CompilerM a 
liftCompiler (Left e) = CompilerM . Left . PackedCompilerError $ e 
liftCompiler (Right x) = CompilerM . Right $ x
