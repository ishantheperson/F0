{-# LANGUAGE RecordWildCards #-}
module F0.Compiler.Compile (
  module F0.Compiler.CompilerM,
  module F0.Compiler.CompilerError,

  CompilerOutput(..),
  compile,
) where 

import F0.Parser.AST

import F0.Compiler.CompilerM
import F0.Compiler.CompilerError

import Text.Megaparsec
import F0.Parser.Internal 
import F0.Typechecker.Infer
import F0.Codegen.Symbolize 
import F0.Codegen.Closure
import F0.Codegen.PrintC0 

import Data.Functor
import Data.Functor.Identity

data CompilerOutput = CompilerOutput 
  { 
    c1source :: String,
    typedAST :: [F0Declaration Symbol Identity],
    typeEnv :: TypeEnvironment,
    transformedAST :: (C0Expression, C0CodegenState)
  }

-- | Compile @text@ from file @fileName@, keeping all data generated
compile :: FilePath -> String -> Either PackedCompilerError CompilerOutput
compile fileName text = do 
  -- Parser
  parsedWithMain <- liftCompiler $ runParser (sc *> f0Decls <* eof) fileName text <&> (++[dummyMain])
  -- Renaming
  symbolized <- liftCompiler $ symbolize parsedWithMain 
  -- Typechecking
  (typeEnv, typedAST) <- liftCompiler $ typecheck emptyEnv defaultState symbolized 
  -- Convert to a form where everything is explicit
  let transformedAST = runCodegen . codegenExpr [] . programToExpression $ typedAST 
  -- Generate C1 source text
  let c1source = outputProgram transformedAST
  return $ CompilerOutput{..} 

dummyMain :: F0Declaration String Maybe 
dummyMain = F0Value "_main" (Just $ F0PrimitiveType F0IntType) (F0OpExp Plus [F0Literal $ F0IntLiteral 0, F0Identifier "main"])