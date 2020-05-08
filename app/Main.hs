{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad 

import System.IO 
import System.Environment

import qualified Options.Applicative as Opts

import Text.Megaparsec
import Text.Show.Pretty 

import Parser.AST 
import Parser.Internal 

import Codegen.Symbolize
import Codegen.Closure 

import Typechecker.Infer 

data CompilerOptions = CompilerOptions 
  {
    printAst :: Bool,
    file :: FilePath
  }

compilerOptions = CompilerOptions 
  <$> Opts.switch (Opts.long "print-ast" <> Opts.help "print out the AST after parsing")
  <*> (Opts.argument Opts.str (Opts.metavar "<input file>"))

options = Opts.info (compilerOptions Opts.<**> Opts.helper) Opts.fullDesc

main :: IO ()
main = do 
  options <- Opts.execParser options 
  text <- readFile (file options)
  
  parseTree <- case runParser (sc *> f0Decls <* eof) (file options) text of 
                 Left errors -> do 
                   putStrLn $ errorBundlePretty errors 
                   fail "Parsing failed"

                 Right ast -> do 
                   when (printAst options) (pPrint ast)
                   return ast 

  -- Insert main value (_main)
  parseTree <- return $ parseTree ++ [F0Value "_main" (Just $ F0PrimitiveType F0IntType) (F0Identifier "main")]

  symbolized <- case symbolize parseTree of 
                  Left errors -> do 
                    mapM_ print errors 
                    fail "Symbolization failed"

                  Right ast -> do 
                    when (printAst options) (pPrint ast)
                    return ast 

  (typeAST, typeEnv) <- case typecheckDecls emptyEnv symbolized of 
                          Left errors -> do 
                            mapM_ print errors 
                            fail "Typechecking failed"

                          Right results -> return results 

  when (printAst options) (pPrint typeAST)

  let e = programToExpression typeAST 
  
  pPrint e 
  pPrint (runCodegen (codegenExpr [] e))

  putStr $ printEnv typeEnv 