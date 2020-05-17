{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Main where

import Control.Monad 

import System.Exit
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
    optimize :: Bool,
    executeProgram :: Bool,
    onlyTypecheck :: Bool,
    keepC1 :: Bool,
    extraCC0Opts :: [String],
    file :: FilePath
  }

compilerOptions = do 
  printAst <- Opts.switch (Opts.long "print-ast" <> Opts.help "print out the AST at various points during compilation")
  printTypes <- Opts.switch (Opts.long "print-types" <> Opts.help "print out the types of the top level decls")
  printTransformed <- Opts.switch (Opts.long "print-transformed" <> Opts.help "print out the transformed program")
  optimize <- Opts.switch (Opts.short 'O' <> Opts.help "optimize by passing -O2 to the C compiler")
  executeProgram <- Opts.switch (Opts.long "execute" <> Opts.short 'x' <> Opts.help "execute the program if it compiles")
  onlyTypecheck <- Opts.switch (Opts.long "only-typecheck" <> Opts.short 't' <> Opts.help "stop after typechecking. implies --print-types")
  keepC1 <- Opts.switch (Opts.long "save-files" <> Opts.short 's' <> Opts.help "save the generated C1 code")
  extraCC0Opts <- Opts.many (Opts.strOption (Opts.short 'c' <> Opts.metavar "<arg>" <> Opts.help "pass an option to CC0"))
  file <- (Opts.argument Opts.str (Opts.metavar "<input file>"))
  return $ CompilerOptions{..}

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
  parseTree <- return $ parseTree ++ [F0Value "_main" (Just $ F0PrimitiveType F0IntType) 
                                      (F0OpExp Plus [F0Literal $ F0IntLiteral 0, F0Identifier "main"])]

  symbolized <- case symbolize parseTree of 
                  Left errors -> do 
                    mapM_ (putStrLn <=< displayIO) errors 
                    fail "Symbolization failed"

                  Right ast -> do 
                    when printAst (pPrint ast)
                    return ast 

  -- typeAST has funs converted into vals 
  (typeEnv, typeAST) <- case typecheck emptyEnv defaultState symbolized of 
                          Left err -> do 
                            putStrLn =<< displayIO err  
                            fail "Typechecking failed"

                          Right results -> return results 

  when printAst $ pPrint typeAST
  when (printTypes || onlyTypecheck) $ putStr $ display typeEnv 

  when onlyTypecheck exitSuccess

  let e = programToExpression typeAST 
  let c0Program = runCodegen (codegenExpr [] e)

  when printTransformed $ pPrint c0Program

  -- Write output file 
  let basename = dropExtension inputFile
      outputFileName = basename ++ ".c1"
  writeFile outputFileName (outputProgram c0Program)

  -- Compile program using CC0
  -- callProcess throws an exception if the command fails,
  -- but if CC0 fails here then that is a bug in our program
  callProcess "cc0" 
    (
      (if optimize then ["-c", "-O2"] else []) ++
      ["-r", "unsafe"] ++ -- division/mod can be unsafe here, but not checking tags could give a big boost to performance
      ["-o", basename] ++
      extraCC0Opts ++
      [outputFileName]
    )

  -- Avoid cluttering the directory by removing the generated code
  unless keepC1 $ removeFile outputFileName

  -- If requested, execute the program
  when executeProgram $ callProcess ("./" ++ basename) []
