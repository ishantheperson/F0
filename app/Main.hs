module Main where

import Text.Megaparsec
import Parser.Internal 
import Text.Show.Pretty 
import Control.Monad.Writer.Strict 

import Parser.AST 
import Parser.ASTUtil
import Data.Either 
import Codegen.Symbolize

run p s = case runParser p "" s of 
  Right r -> pPrint r  
  Left e -> putStrLn (errorBundlePretty e) 

forceDecls :: String -> [F0Declaration String Maybe]
forceDecls s = fromRight undefined (runParser f0Decls "" s)

getExpr :: String -> F0Expression Symbol Maybe
getExpr s = case head (fromRight undefined $ symbolize $ forceDecls s) of 
  F0Value _ _ e -> e 
  _ -> error "not a value"

main :: IO ()
main = undefined 
