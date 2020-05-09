{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad 

import System.IO 
import System.Environment
import System.FilePath.Posix

import qualified Options.Applicative as Opts

import Text.Megaparsec
import Text.Show.Pretty 

import Parser.AST 
import Parser.Internal 

import Codegen.Symbolize
import Codegen.Closure 
import Codegen.PrintC0 

import Typechecker.Infer 

data CompilerOptions = CompilerOptions 
  {
    printAst :: Bool,
    printTypes :: Bool,
    file :: FilePath
  }

compilerOptions = CompilerOptions 
  <$> Opts.switch (Opts.long "print-ast" <> Opts.help "print out the AST after parsing")
  <*> Opts.switch (Opts.long "print-types" <> Opts.help "print out the types of the top level decls")
  <*> (Opts.argument Opts.str (Opts.metavar "<input file>"))

options = Opts.info (compilerOptions Opts.<**> Opts.helper) Opts.fullDesc

main :: IO ()
main = do 
  options <- Opts.execParser options 
  let inputFile = file options 
  text <- readFile inputFile
  
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

  when (printAst options) $ pPrint typeAST
  when (printTypes options) $ putStr $ printEnv typeEnv 

  let e = programToExpression typeAST 
  
  let c0Program = runCodegen (codegenExpr [] e)

  let outputFileName = dropExtension inputFile ++ ".c1"

  writeFile outputFileName (outputProgram c0Program)
