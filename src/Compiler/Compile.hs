{-# LANGUAGE RecordWildCards #-}
module Compiler.Compile (
  module Compiler.CompilerM,
  module Compiler.CompilerError,

  CompilerOutput(..),
  compile,
) where 

import Parser.AST

import Compiler.CompilerM
import Compiler.CompilerError

import Text.Megaparsec
import Parser.Internal 
import Typechecker.Infer
import Codegen.Symbolize 
import Codegen.Closure
import Codegen.PrintC0 

import Data.Functor
import Data.Functor.Identity

data CompilerOutput = CompilerOutput 
  { 
    c1source :: String,
    typedAST :: [F0Declaration Symbol Identity],
    typeEnv :: TypeEnvironment,
    transformedAST :: (C0Expression, C0CodegenState)
  }
  deriving Show

-- | Compile @text@ from file @fileName@, keeping all data generated
compile :: FilePath -> String -> Either PackedCompilerError CompilerOutput
compile fileName text = runCompilerM $ do 
  parsedWithMain <- liftCompiler $ runParser (sc *> f0Decls <* eof) fileName text <&> (++[dummyMain])
  symbolized <- liftCompiler $ symbolize parsedWithMain 
  (typeEnv, typedAST) <- liftCompiler $ typecheck emptyEnv defaultState symbolized 
  let transformedAST = runCodegen . codegenExpr [] . programToExpression $ typedAST 
  let c1source = outputProgram transformedAST
  return $ CompilerOutput{..} 

-- | Compiler @text@ to a string which is C1 source code.
-- @fileName@ is used to report errors
compileSource :: FilePath -> String -> Either PackedCompilerError String
compileSource fileName text = runCompilerM $
      liftCompiler (runParser (sc *> f0Decls <* eof) fileName text)
  <&> (++[dummyMain]) 
  >>= liftCompiler . symbolize
  >>= liftCompiler . typecheck emptyEnv defaultState
  <&> outputProgram . runCodegen . codegenExpr [] . programToExpression . snd

dummyMain :: F0Declaration String Maybe 
dummyMain = F0Value "_main" (Just $ F0PrimitiveType F0IntType) (F0OpExp Plus [F0Literal $ F0IntLiteral 0, F0Identifier "main"])