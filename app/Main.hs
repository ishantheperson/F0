{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad 

import System.Environment
import System.Directory
import System.FilePath.Posix
import System.Process 

import qualified Options.Applicative as Opts

import Text.Megaparsec
import Text.Show.Pretty 

import Parser.AST 
import Parser.ASTUtil 
import Parser.Internal 

import Codegen.Symbolize
import Codegen.Closure 
import Codegen.PrintC0 

import Typechecker.Infer 

data CompilerOptions = CompilerOptions 
  {
    printAst :: Bool,
    printTypes :: Bool,
    printTransformed :: Bool,
    executeProgram :: Bool,
    keepC1 :: Bool,
    file :: FilePath
  }

compilerOptions = CompilerOptions 
  <$> Opts.switch (Opts.long "print-ast" <> Opts.help "print out the AST after parsing")
  <*> Opts.switch (Opts.long "print-types" <> Opts.help "print out the types of the top level decls")
  <*> Opts.switch (Opts.long "print-transformed" <> Opts.help "print out the transformed program")
  <*> Opts.switch (Opts.long "execute" <> Opts.short 'x' <> Opts.help "execute the program if it compiles")
  <*> Opts.switch (Opts.long "save-files" <> Opts.short 's' <> Opts.help "save the generated C1 code")
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
                    mapM_ (putStrLn . display) errors 
                    fail "Symbolization failed"

                  Right ast -> do 
                    when (printAst options) (pPrint ast)
                    return ast 

  -- typeAST has funs converted into vals 
  (typeEnv, typeAST) <- case typecheck emptyEnv defaultState symbolized of 
                          Left errors -> do 
                            putStrLn . display $ errors 
                            fail "Typechecking failed"

                          Right results -> return results 

  when (printAst options) $ pPrint typeAST
  when (printTypes options) $ putStr $ display typeEnv 

  let e = programToExpression typeAST 
  let c0Program = runCodegen (codegenExpr [] e)

  when (printTransformed options) $ pPrint c0Program

  -- Write output file 
  let basename = dropExtension inputFile
      outputFileName = basename ++ ".c1"
  writeFile outputFileName (outputProgram c0Program)

  -- Compile program using CC0 
  callProcess "cc0" ["-o", basename, outputFileName] 

  -- Avoid cluttering the directory by removing the generated code
  unless (keepC1 options) $ removeFile outputFileName

  -- If requested, execute the program
  when (executeProgram options) $ callProcess ("./" ++ basename) []
