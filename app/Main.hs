{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Main where

import Control.Monad 

import Data.Maybe
import Data.Functor

import System.Exit (exitSuccess, exitFailure)
import System.Environment (lookupEnv)
import System.Directory (removeFile)
import System.FilePath.Posix (dropExtension)
import System.Process (callProcess)

import qualified Options.Applicative as Opts

import F0.Display
import Text.Show.Pretty 
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise

import F0.Parser.ASTUtil (removePositionInfo)
import F0.Compiler.Compile

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
    inputFile :: FilePath
  }

compilerOptions :: Opts.Parser CompilerOptions
compilerOptions = do 
  printAst         <- Opts.switch (Opts.long "print-ast" <> Opts.help "print out the AST at various points during compilation")
  printTypes       <- Opts.switch (Opts.long "print-types" <> Opts.help "print out the types of the top level decls")
  printTransformed <- Opts.switch (Opts.long "print-transformed" <> Opts.help "print out the transformed program")
  optimize         <- Opts.switch (Opts.short 'O' <> Opts.help "optimize by passing -O2 to the C compiler")
  executeProgram   <- Opts.switch (Opts.long "execute" <> Opts.short 'x' <> Opts.help "execute the program if it compiles")
  onlyTypecheck    <- Opts.switch (Opts.long "only-typecheck" <> Opts.short 't' <> Opts.help "don't create an executable. implies --print-types")
  keepC1           <- Opts.switch (Opts.long "save-files" <> Opts.short 's' <> Opts.help "save the generated C1 code")
  extraCC0Opts     <- Opts.many   (Opts.strOption (Opts.short 'c' <> Opts.metavar "<arg>" <> Opts.help "pass an option to CC0"))
  inputFile        <- Opts.argument Opts.str (Opts.metavar "<input file>")
  return $ CompilerOptions{..}

options :: Opts.ParserInfo CompilerOptions
options = Opts.info (Opts.helper <*> compilerOptions) Opts.fullDesc

main :: IO ()
main = do 
  CompilerOptions{..} <- Opts.execParser options 
  text <- readFile inputFile
  
  -- Allow usage of a different version of cc0 not on path
  cc0 <- lookupEnv "F0_CC0" <&> fromMaybe "cc0" 

  CompilerOutput{..} <- case compile inputFile text of 
                          Left err -> (putStrLn =<< displayIO err) >> exitFailure
                          Right output -> return output

  -- Display various generated information
  when printAst $ colorPrint (removePositionInfo typedAST)
  when (printTypes || onlyTypecheck) $ putStr $ display typeEnv 
  when printTransformed $ colorPrint transformedAST
  
  -- Stop before generating an output file if requested
  when onlyTypecheck exitSuccess

  -- Write output file 
  let basename = dropExtension inputFile
      outputFileName = basename ++ ".c1"
  
  writeFile outputFileName c1source

  -- Compile program using CC0
  -- callProcess throws an exception if the command fails,
  -- but if CC0 fails here then that is a bug in our program
  callProcess cc0 $ concat 
    [
      if optimize then ["-c", "-O2"] else [],
      ["-r", "unsafe"], -- not checking tags could give a big boost to performance/memory usage
      ["-o", basename], -- a.out is such an ugly name
      extraCC0Opts, -- user provided options
      [outputFileName]
    ]

  -- Avoid cluttering the directory by removing the generated code
  unless keepC1 $ removeFile outputFileName

  -- If requested, execute the program
  when executeProgram $ callProcess ("./" ++ basename) []

colorPrint :: Show a => a -> IO () 
colorPrint = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow 
