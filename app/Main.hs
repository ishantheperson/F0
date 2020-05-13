{-# LANGUAGE RecordWildCards #-}
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
    optimize :: Bool,
    file :: FilePath
  }

compilerOptions = CompilerOptions 
  <$> Opts.switch (Opts.long "print-ast" <> Opts.help "print out the AST at various points during compilation")
  <*> Opts.switch (Opts.long "print-types" <> Opts.help "print out the types of the top level decls")
  <*> Opts.switch (Opts.long "print-transformed" <> Opts.help "print out the transformed program")
  <*> Opts.switch (Opts.long "execute" <> Opts.short 'x' <> Opts.help "execute the program if it compiles")
  <*> Opts.switch (Opts.long "save-files" <> Opts.short 's' <> Opts.help "save the generated C1 code")
  <*> Opts.switch (Opts.short 'O' <> Opts.help "optimize by passing -O2 to the C compiler")
  <*> (Opts.argument Opts.str (Opts.metavar "<input file>"))

options = Opts.info (compilerOptions Opts.<**> Opts.helper) Opts.fullDesc

main :: IO ()
main = do 
  CompilerOptions{..} <- Opts.execParser options 
  let inputFile = file 
  text <- readFile inputFile
  
  parseTree <- case runParser (sc *> f0Decls <* eof) file text of 
                 Left errors -> do 
                   putStrLn $ errorBundlePretty errors 
                   fail "Parsing failed"

                 Right ast -> do 
                   when printAst (pPrint ast)
                   return ast 

  -- Insert main value (_main)
  parseTree <- return $ parseTree ++ [F0Value "_main" (Just $ F0PrimitiveType F0IntType) (F0Identifier "main")]

  symbolized <- case symbolize parseTree of 
                  Left errors -> do 
                    mapM_ (putStrLn . display) errors 
                    fail "Symbolization failed"

                  Right ast -> do 
                    when printAst (pPrint ast)
                    return ast 

  -- typeAST has funs converted into vals 
  (typeEnv, typeAST) <- case typecheck emptyEnv defaultState symbolized of 
                          Left errors -> do 
                            putStrLn . display $ errors 
                            fail "Typechecking failed"

                          Right results -> return results 

  when printAst $ pPrint typeAST
  when printTypes $ putStr $ display typeEnv 

  let e = programToExpression typeAST 
  let c0Program = runCodegen (codegenExpr [] e)

  when printTransformed $ pPrint c0Program

  -- Write output file 
  let basename = dropExtension inputFile
      outputFileName = basename ++ ".c1"
  writeFile outputFileName (outputProgram c0Program)

  -- Compile program using CC0, optimizing
  callProcess "cc0" ["-c", "-O2", "-o", basename, outputFileName] 

  -- Avoid cluttering the directory by removing the generated code
  unless keepC1 $ removeFile outputFileName

  -- If requested, execute the program
  when executeProgram $ callProcess ("./" ++ basename) []
