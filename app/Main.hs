{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Text.Megaparsec
import Text.Show.Pretty 
import Control.Monad.Writer.Strict 
import System.IO 
import Data.Either 

import Parser.AST 
import Parser.ASTUtil
import Parser.Internal 
import Codegen.Symbolize

import Typechecker.Infer 

run p s = case runParser p "" s of 
  Right r -> pPrint r  
  Left e -> putStrLn (errorBundlePretty e) 

forceDecls :: String -> [F0Declaration String Maybe]
forceDecls s = fromRight undefined (runParser f0Decls "" s)

getExpr :: String -> F0Expression Symbol Maybe
getExpr s = case head (fromRight undefined $ symbolize $ forceDecls s) of 
  F0Value _ _ e -> e 
  _ -> error "not a value"

forceSymbols :: String -> [F0Declaration Symbol Maybe]
forceSymbols s = fromRight undefined $ symbolize $ forceDecls ("val foo = " ++ s)

typecheckE :: String -> Either [TypeError] Scheme
typecheckE s = 
  let (F0Value _ _ e) = head $ fromRight undefined $ symbolize $ forceDecls ("val foo = " ++ s)
  in snd <$> typecheck e 

main :: IO ()
main = forever $ do 
  putStr "> "
  hFlush stdout
  input <- getLine 
  case typecheckE input of 
    Left errs -> print errs 
    Right (Forall _ t) -> putStrLn $ printType t 
